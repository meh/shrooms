#![feature(collections)]

mod crypt;
use crypt::{crypt, salt};

fn main() {
	println!("{}", crypt(b"a", &salt(b"H.")));

	//let salt = salt(b"H.");

	//for _ in 0 .. 100_000 {
		//crypt(b"a", &salt);
	//}
}
