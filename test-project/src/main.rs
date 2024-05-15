use std::{env, io};

fn main() {
    let mut args = env::args();

    if args.len() == 1 {
        println!("***run not interactive");
    } else {
        match args.nth(1).unwrap().as_str() {
            "interactive" => {
                let mut line = String::new();

                io::stdin()
                    .read_line(&mut line)
                    .expect("Failed to read line");

                println!("***run interactive: {line}");
            }

            _ => {
                panic!("unexpected argument");
            }
        }
    }
}
