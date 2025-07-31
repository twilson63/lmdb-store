use serde::{Deserialize, Serialize};

pub const PATH_SEPARATOR: &str = "/";
pub const GROUP_MARKER: &str = "__group__";
pub const LINK_MARKER: &str = "__link__";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PathType {
    Simple,
    Composite,
    Link(String),
}

pub fn normalize_path(path: &str) -> String {
    path.trim_matches('/').to_string()
}

pub fn join_paths(parts: &[String]) -> String {
    parts
        .iter()
        .filter(|p| !p.is_empty())
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .join(PATH_SEPARATOR)
}

pub fn split_path(path: &str) -> Vec<String> {
    path.split(PATH_SEPARATOR)
        .filter(|p| !p.is_empty())
        .map(|s| s.to_string())
        .collect()
}

pub fn is_group_key(key: &str) -> bool {
    key.ends_with(GROUP_MARKER)
}

pub fn make_group_key(path: &str) -> String {
    format!("{}{}", normalize_path(path), GROUP_MARKER)
}

pub fn is_link_key(key: &str) -> bool {
    key.ends_with(LINK_MARKER)
}

pub fn make_link_key(path: &str) -> String {
    format!("{}{}", normalize_path(path), LINK_MARKER)
}

pub fn path_to_key(path: &[String]) -> String {
    join_paths(path)
}

pub fn key_to_path(key: &str) -> Vec<String> {
    split_path(key)
}

pub fn parent_path(path: &str) -> Option<String> {
    let parts = split_path(path);
    if parts.len() <= 1 {
        None
    } else {
        Some(join_paths(&parts[..parts.len() - 1]))
    }
}

pub fn child_key(parent: &str, child: &str) -> String {
    if parent.is_empty() {
        child.to_string()
    } else {
        format!("{}{}{}", parent, PATH_SEPARATOR, child)
    }
}

pub fn extract_child_name(full_path: &str, parent_prefix: &str) -> Option<String> {
    if full_path.starts_with(parent_prefix) {
        let remainder = &full_path[parent_prefix.len()..];
        let remainder = remainder.trim_start_matches(PATH_SEPARATOR);
        
        // Only return the immediate child, not nested paths
        if let Some(slash_pos) = remainder.find(PATH_SEPARATOR) {
            Some(remainder[..slash_pos].to_string())
        } else if !remainder.is_empty() {
            Some(remainder.to_string())
        } else {
            None
        }
    } else {
        None
    }
}