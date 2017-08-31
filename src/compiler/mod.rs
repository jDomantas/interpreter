// TODO: does it really need to be pub(crate) ?
pub(crate) mod builtins;
mod symbols;
mod alias_expansion;
mod precedence;
mod kind_check;
mod type_check;
mod def_grouping;
mod trait_check;
mod monomorphisation;
mod closure_fix;
mod compilation;
mod util;

use std::collections::BTreeMap;
use ast::Name;
use ast::parsed::Module;
use ast::resolved::Sym;
use errors::Errors;
use vm::{GlobalValue, Function};

pub fn compile(modules: &BTreeMap<Name, Module>, errors: &mut Errors)
    -> Result<(BTreeMap<u64, Function>, BTreeMap<Sym, GlobalValue>), ()>
{
    let items = symbols::resolve_symbols(modules, errors);
    if errors.have_errors() {
        return Err(());
    }

    let items = alias_expansion::expand_aliases(items, errors);
    if errors.have_errors() {
        return Err(());
    }

    let items = precedence::fix_items(items, errors);
    if errors.have_errors() {
        return Err(());
    }

    let res = kind_check::find_kind_errors(&items, errors);
    if errors.have_errors() {
        return Err(());
    }
    assert!(res.is_ok());

    let items = def_grouping::group_items(items);

    let mut items = type_check::infer_types(items, errors);
    if errors.have_errors() {
        return Err(());
    }

    trait_check::check_items(&mut items, errors);
    if errors.have_errors() {
        return Err(());
    }

    let mut items = monomorphisation::monomorphise(items);

    closure_fix::optimise(&mut items);
    
    let (fns, globals) = compilation::compile(items);

    Ok((fns, globals))
}
