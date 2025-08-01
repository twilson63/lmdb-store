use rustler::{Env, NifResult, Term};

mod atoms;
mod environment;
mod database;
mod transaction;
mod cursor;
mod error;
mod path_ops;
mod store;
mod batch;
mod cache;

rustler::init!(
    "hyper_lmdb",
    [
        nif_start,
        nif_stop,
        nif_reset,
        nif_read,
        nif_write,
        nif_type,
        nif_list,
        nif_make_group,
        nif_make_link,
        nif_path,
        nif_add_path,
        nif_resolve,
        nif_list_prefix,
        nif_sync,
        nif_begin_batch,
        nif_batch_write,
        nif_batch_make_group,
        nif_batch_make_link,
        nif_commit_batch
    ],
    load = on_load
);

fn on_load(env: Env, _info: Term) -> bool {
    environment::on_load(env) && batch::on_load(env)
}

#[rustler::nif]
fn nif_start<'a>(env: Env<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    store::start(env, store_opts)
}

#[rustler::nif]
fn nif_stop<'a>(env: Env<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    store::stop(env, store_opts)
}

#[rustler::nif]
fn nif_reset<'a>(env: Env<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    store::reset(env, store_opts)
}

#[rustler::nif]
fn nif_read<'a>(env: Env<'a>, store_opts: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    store::read(env, store_opts, key)
}

#[rustler::nif]
fn nif_write<'a>(env: Env<'a>, store_opts: Term<'a>, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
    store::write(env, store_opts, key, value)
}

#[rustler::nif]
fn nif_type<'a>(env: Env<'a>, store_opts: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    store::get_type(env, store_opts, key)
}

#[rustler::nif]
fn nif_list<'a>(env: Env<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    store::list(env, store_opts, path)
}

#[rustler::nif]
fn nif_make_group<'a>(env: Env<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    store::make_group(env, store_opts, path)
}

#[rustler::nif]
fn nif_make_link<'a>(env: Env<'a>, store_opts: Term<'a>, existing: Term<'a>, new: Term<'a>) -> NifResult<Term<'a>> {
    store::make_link(env, store_opts, existing, new)
}

#[rustler::nif]
fn nif_path<'a>(env: Env<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    store::path(env, store_opts, path)
}

#[rustler::nif]
fn nif_add_path<'a>(env: Env<'a>, store_opts: Term<'a>, path1: Term<'a>, path2: Term<'a>) -> NifResult<Term<'a>> {
    store::add_path(env, store_opts, path1, path2)
}

#[rustler::nif]
fn nif_resolve<'a>(env: Env<'a>, store_opts: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    store::resolve(env, store_opts, path)
}

#[rustler::nif]
fn nif_list_prefix<'a>(env: Env<'a>, store_opts: Term<'a>, prefix: Term<'a>, opts: Term<'a>) -> NifResult<Term<'a>> {
    store::list_prefix(env, store_opts, prefix, opts)
}

#[rustler::nif]
fn nif_sync<'a>(env: Env<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    store::sync(env, store_opts)
}

#[rustler::nif]
fn nif_begin_batch<'a>(env: Env<'a>, store_opts: Term<'a>) -> NifResult<Term<'a>> {
    batch::begin_batch(env, store_opts)
}

#[rustler::nif]
fn nif_batch_write<'a>(env: Env<'a>, batch: Term<'a>, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
    batch::batch_write(env, batch, key, value)
}

#[rustler::nif]
fn nif_batch_make_group<'a>(env: Env<'a>, batch: Term<'a>, path: Term<'a>) -> NifResult<Term<'a>> {
    batch::batch_make_group(env, batch, path)
}

#[rustler::nif]
fn nif_batch_make_link<'a>(env: Env<'a>, batch: Term<'a>, existing: Term<'a>, new: Term<'a>) -> NifResult<Term<'a>> {
    batch::batch_make_link(env, batch, existing, new)
}

#[rustler::nif]
fn nif_commit_batch<'a>(env: Env<'a>, batch: Term<'a>) -> NifResult<Term<'a>> {
    batch::commit_batch(env, batch)
}

// Direct environment functions removed - use store API instead