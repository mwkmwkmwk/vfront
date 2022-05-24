use std::env;
use std::fs;
use std::io;
use vfront_basics::diag::{DiagPrinter, DiagRegistry, DiagSystem};
use vfront_basics::source::SourceManager;
use vfront_rtlil::diag;
use vfront_rtlil::parse::parse;

fn main() -> io::Result<()> {
    let sm = SourceManager::new();
    let args: Vec<_> = env::args().collect();
    let mut registry = DiagRegistry::new();
    diag::register_diags(&mut registry);
    let sink = DiagPrinter::new(&sm);
    for arg in &args[1..] {
        let text = fs::read_to_string(arg)?;
        let sc = sm.add_file(&arg[..], text);
        let diags = DiagSystem::new(&registry, &sink);
        let design = parse(sc, &diags);
        //println!("{:#?}", design);
        design.print();
    }
    Ok(())
}
