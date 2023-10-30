// the spec: https://docs.oracle.com/javase/specs/jvms/se20/jvms20.pdf
use std::env;
use std::fs::File;
use std::io::BufReader;

mod class;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let f = File::open(&args[1])?;
    let mut reader = BufReader::new(f);

    let class_file = class::parse_class_file(&mut reader)?;

    println!("{:#?}", class_file);

    Ok(())
}
