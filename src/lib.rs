use rustler::{Env, NifResult, Term};

mod atoms;
mod environment;
mod database;
mod transaction;
mod cursor;
mod error;
mod path_ops;
mod store;

rustler::init!(
    "hb_store_lmdb",
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
        nif_env_open,
        nif_env_close,
        nif_env_get,
        nif_env_put
    ],
    load = on_load
);

fn on_load(env: Env, _info: Term) -> bool {
    environment::on_load(env)
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
fn nif_env_open<'a>(env: Env<'a>, path: Term<'a>, opts: Term<'a>) -> NifResult<Term<'a>> {
    environment::env_open(env, path, opts)
}

#[rustler::nif]
fn nif_env_close<'a>(env: Env<'a>, env_resource: Term<'a>) -> NifResult<Term<'a>> {
    environment::env_close(env, env_resource)
}

#[rustler::nif]
fn nif_env_get<'a>(env: Env<'a>, env_resource: Term<'a>, db_name: Term<'a>, key: Term<'a>) -> NifResult<Term<'a>> {
    environment::env_get(env, env_resource, db_name, key)
}

#[rustler::nif]
fn nif_env_put<'a>(env: Env<'a>, env_resource: Term<'a>, db_name: Term<'a>, key: Term<'a>, value: Term<'a>) -> NifResult<Term<'a>> {
    environment::env_put(env, env_resource, db_name, key, value)
}