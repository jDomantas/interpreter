pub mod builtins;
mod resolve_symbols;
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
use vm::Vm;
use CompileCtx;


pub(crate) fn compile(modules: &BTreeMap<Name, Module>, ctx: &mut CompileCtx)
    -> Result<Vm, ()>
{
    let items = resolve_symbols::resolve_symbols(modules, ctx);
    if ctx.reporter.have_errors() {
        return Err(());
    }

    let items = alias_expansion::expand_aliases(items, ctx);
    if ctx.reporter.have_errors() {
        return Err(());
    }

    let items = precedence::fix_items(items, ctx);
    if ctx.reporter.have_errors() {
        return Err(());
    }

    kind_check::find_kind_errors(&items, ctx);
    if ctx.reporter.have_errors() {
        return Err(());
    }
    
    let items = def_grouping::group_items(items);

    let mut items = type_check::infer_types(items, ctx);
    if ctx.reporter.have_errors() {
        return Err(());
    }

    trait_check::check_items(&mut items, ctx);
    if ctx.reporter.have_errors() {
        return Err(());
    }

    let mut items = monomorphisation::monomorphise(items, ctx);

    closure_fix::optimise(&mut items, ctx);

    let vm = compilation::compile(items);

    Ok(vm)
}
