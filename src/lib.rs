use rustler::{Atom, Binary, Encoder, Env, Error, NifResult, OwnedBinary, Term};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use parking_lot::{Mutex, RwLock};
use once_cell::sync::Lazy;

mod atoms;
mod environment;
mod database;
mod transaction;
mod cursor;
mod error;
mod path_ops;
mod store;

use atoms::*;
use environment::*;
use database::*;
use transaction::*;
use cursor::*;
use error::*;
use path_ops::*;
use store::*;

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
        nif_add_path
    ],
    load = on_load
);

fn on_load(env: Env) -> bool {
    environment::on_load(env)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_start(env: Env, store_opts: Term) -> NifResult<Term> {
    store::start(env, store_opts)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_stop(env: Env, store_opts: Term) -> NifResult<Term> {
    store::stop(env, store_opts)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_reset(env: Env, store_opts: Term) -> NifResult<Term> {
    store::reset(env, store_opts)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_read(env: Env, store_opts: Term, key: Term) -> NifResult<Term> {
    store::read(env, store_opts, key)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_write(env: Env, store_opts: Term, key: Term, value: Term) -> NifResult<Term> {
    store::write(env, store_opts, key, value)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_type(env: Env, store_opts: Term, key: Term) -> NifResult<Term> {
    store::get_type(env, store_opts, key)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_list(env: Env, store_opts: Term, path: Term) -> NifResult<Term> {
    store::list(env, store_opts, path)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_make_group(env: Env, store_opts: Term, path: Term) -> NifResult<Term> {
    store::make_group(env, store_opts, path)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_make_link(env: Env, store_opts: Term, existing: Term, new: Term) -> NifResult<Term> {
    store::make_link(env, store_opts, existing, new)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_path(env: Env, store_opts: Term, path: Term) -> NifResult<Term> {
    store::path(env, store_opts, path)
}

#[rustler::nif(schedule = "DirtyCpu")]
fn nif_add_path(env: Env, store_opts: Term, path1: Term, path2: Term) -> NifResult<Term> {
    store::add_path(env, store_opts, path1, path2)
}