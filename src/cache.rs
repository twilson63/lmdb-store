use lru::LruCache;
use parking_lot::RwLock;
use std::num::NonZeroUsize;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Entry in the path cache
#[derive(Clone, Debug)]
struct CacheEntry {
    value: String,
    expires_at: Instant,
}

/// Thread-safe LRU cache for resolved paths
pub struct PathCache {
    cache: Arc<RwLock<LruCache<String, CacheEntry>>>,
    ttl: Duration,
}

impl PathCache {
    pub fn new(capacity: usize, ttl_seconds: u64) -> Self {
        let capacity = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(1000).unwrap());
        PathCache {
            cache: Arc::new(RwLock::new(LruCache::new(capacity))),
            ttl: Duration::from_secs(ttl_seconds),
        }
    }

    /// Get a value from the cache if it exists and hasn't expired
    pub fn get(&self, key: &str) -> Option<String> {
        let mut cache = self.cache.write();
        
        if let Some(entry) = cache.get_mut(key) {
            if entry.expires_at > Instant::now() {
                return Some(entry.value.clone());
            } else {
                // Entry expired, remove it
                cache.pop(key);
            }
        }
        
        None
    }

    /// Insert a value into the cache
    pub fn insert(&self, key: String, value: String) {
        let entry = CacheEntry {
            value,
            expires_at: Instant::now() + self.ttl,
        };
        
        self.cache.write().put(key, entry);
    }

    /// Clear the entire cache
    pub fn clear(&self) {
        self.cache.write().clear();
    }

    /// Invalidate specific keys
    pub fn invalidate(&self, key: &str) {
        self.cache.write().pop(key);
    }

    /// Invalidate all keys with a given prefix
    pub fn invalidate_prefix(&self, prefix: &str) {
        let mut cache = self.cache.write();
        let keys_to_remove: Vec<String> = cache
            .iter()
            .filter_map(|(k, _)| {
                if k.starts_with(prefix) {
                    Some(k.clone())
                } else {
                    None
                }
            })
            .collect();
        
        for key in keys_to_remove {
            cache.pop(&key);
        }
    }
}

// Global path resolution cache
use once_cell::sync::Lazy;
pub static PATH_CACHE: Lazy<PathCache> = Lazy::new(|| PathCache::new(10000, 300)); // 10k entries, 5 min TTL

/// Resolve a path with caching
pub fn resolve_path_cached<F>(path: &str, resolver: F) -> String 
where
    F: FnOnce(&str) -> String,
{
    // Check cache first
    if let Some(resolved) = PATH_CACHE.get(path) {
        return resolved;
    }
    
    // Not in cache, resolve it
    let resolved = resolver(path);
    
    // Cache the result if it's different from the input
    if resolved != path {
        PATH_CACHE.insert(path.to_string(), resolved.clone());
    }
    
    resolved
}

/// Invalidate cache entries when paths are modified
pub fn invalidate_path_cache(path: &str) {
    // Invalidate the exact path
    PATH_CACHE.invalidate(path);
    
    // Also invalidate any paths that might reference this as a link
    // For now, we'll be conservative and invalidate the parent path
    if let Some(pos) = path.rfind('/') {
        let parent = &path[..pos];
        if !parent.is_empty() {
            PATH_CACHE.invalidate_prefix(parent);
        }
    }
}

/// Invalidate cache entries when a link is created or modified
/// This needs to be more aggressive since links can affect many paths
pub fn invalidate_link_cache(_link_path: &str) {
    // Clear the entire cache when links change
    // This is conservative but ensures correctness
    // TODO: In the future, we could track link dependencies more precisely
    PATH_CACHE.clear();
}