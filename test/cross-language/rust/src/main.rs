use std::io;
use std::io::{Read, Write};

use theta::avro::{FromAvro, ToAvro};

mod test_modules;
use test_modules::primitives::*;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut buffer = vec![];

    handle.read_to_end(&mut buffer)?;
    let v = Vec::<Primitives>::from_avro(&buffer);

    match v {
        Ok((_, result)) => {
            let mut stdout = io::stdout();
            stdout.write_all(&result.to_avro())?;
            stdout.flush()?;
        }
        Err(_) => panic!("Parsing input failed."),
    }
    Ok(())
}
