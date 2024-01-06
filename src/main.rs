use std::{io::BufReader, path::Path};

use obj::Obj;

use crate::linker::Linker;

mod linker;
mod obj;

fn main() {
  let obj_files = [
    "objs/main.o",
    "objs/calif.o",
    "objs/mass.o",
    "objs/newyork.o"
  ];

  let objs = obj_files.iter().map(read_obj).collect();
  let mut linker = Linker::new(objs);

  let obj = linker.link();

  let mut out = std::fs::OpenOptions::new()
    .write(true)
    .create(true)
    .truncate(true)
    .open("out_sample")
    .unwrap();
  obj.dump(&mut out).unwrap();
}

fn read_obj(path: impl AsRef<Path>) -> Obj {
  let file = std::fs::File::open(path.as_ref()).unwrap();
  let mut read = BufReader::new(file);
  let obj = Obj::from_reader(&mut read).unwrap();
  obj
}
