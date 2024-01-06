use std::io::BufReader;

use obj::Obj;

mod obj;

fn main() {
  let file = std::fs::File::open("sample").unwrap();
  let mut read = BufReader::new(file);
  let obj = Obj::from_reader(&mut read).unwrap();
  println!("{:?}", obj);
  let mut out = std::fs::OpenOptions::new()
    .write(true)
    .create(true)
    .truncate(true)
    .open("out_sample")
    .unwrap();
  obj.dump(&mut out).unwrap();
}
