mod chunk;
mod compiler;
mod error;
mod instruction;
mod object;
mod r#ref;
mod value;
mod vm;
mod native;

pub(crate) use error::VmResult;

fn main() {
    println!("Hello, world!");
}
