//! Object file format definition in linkers and loaders:
//! LINK -- Magic Number
//! nsegs nsyms nrels -- Number of segments, number of symbols, number of relocation entries
//! -- segments --
//! -- symbols --
//! -- rels --
//! -- data --
//!
//! Segment definition:
//! name start_addr length property
//! .text 1000 2500 RP -- R: Read, W: Write, P: Present in file
//! .data 4000 C00 RWP
//! .bss 5000 1900 RW
//!
//! Symbol definition:
//! name value seg type
//!
//! Relocation entry definition:
//! loc seg ref type
//!
//! Object data are hex string followed by new line. Two hex digits represent a byte
//!

use std::{io::BufRead, io::Write};

pub const MAGIC_NUMBER: &str = "LINK";

#[derive(Debug, Clone, Copy)]
pub enum RelEntryType {
  /// 4-byte absolute address:
  /// The four bytes at loc are an absolute reference to segment ref.
  /// Operation: Add the relocated address of the corresponding segment to the original
  /// value in the 4-byte location.
  A4,
  /// 4-byte relative address:
  /// The four bytes at loc are a relative reference to segment ref.
  /// Operation: Add the (relocated address of the ref_ segment - relocated address of the seg segment) to the original
  /// value in the 4-byte location.
  R4,
  /// Absolute symbol reference.
  /// The four bytes at loc are an abso- lute reference to symbol ref,
  /// with the addend being the value al- ready stored at loc.
  /// Operation: Add the relocated address of the corresponding symbol to the original
  /// value in the 4-byte location.
  AS4,
  /// Relative symbol reference.
  /// The four bytes at loc are a relative reference to symbol ref,
  /// with the addend being the value already stored at loc. (The addend is usually zero.)
  /// Operation: Add the (relocated address of the ref_ symbol - relocated address of the seg segment - loc) to the original
  /// value in the 4-byte location.
  RS4,
  /// Upper half reference. The two bytes at loc are the most significant two bytes of a reference to symbol ref.
  /// Put the upper two bits of the relocated address of the symbol to the 2-bytes starting at loc
  U2,
  /// Lower half reference. The two bytes at loc are the least significant two bytes of a reference to symbol ref.
  /// Put the lower two bits of the relocated address of the symbol to the 2-bytes starting at loc
  L2,
}

impl TryFrom<&'_ str> for RelEntryType {
  type Error = String;

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "A4" => Ok(Self::A4),
      "R4" => Ok(Self::R4),
      "AS4" => Ok(Self::AS4),
      "RS4" => Ok(Self::RS4),
      "U2" => Ok(Self::U2),
      "L2" => Ok(Self::L2),
      s => Err(s.to_string()),
    }
  }
}

impl RelEntryType {
  pub fn to_str(self) -> &'static str {
    match self {
      Self::A4 => "A4",
      Self::R4 => "R4",
      Self::AS4 => "AS4",
      Self::RS4 => "RS4",
      Self::U2 => "U2",
      Self::L2 => "L2",
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct RelEntry {
  /// loc is an offset into the segment data, where the 4/2 bytes starting from that offset
  /// contains an address that needs to be relocated based on type
  pub loc: u64,
  /// The ordinal number of the segment in which the entry is defined
  pub seg: u64,
  /// The reference the entry is making to, it could be a segment number or a symbol number
  /// defined within the object file.
  pub ref_: u64,
  /// Entry type
  pub type_: RelEntryType,
}

impl RelEntry {
  pub fn parse_definition(s: &str) -> Option<Self> {
    let mut split = s.split(" ");
    let loc: u64 = split
      .next()
      .and_then(|loc| u64::from_str_radix(loc, 16).ok())?;
    let seg: u64 = split
      .next()
      .and_then(|seg| u64::from_str_radix(seg, 16).ok())?;
    let ref_: u64 = split
      .next()
      .and_then(|ref_| u64::from_str_radix(ref_, 16).ok())?;
    let type_ = split
      .next()
      .and_then(|type_| RelEntryType::try_from(type_).ok())?;
    Some(Self {
      loc,
      seg,
      ref_,
      type_,
    })
  }
}

#[derive(Debug)]
pub struct Symbol {
  /// Symbol name
  pub name: String,
  /// Hex value of symbol, which is the address of the 
  /// source code symbol relative to segment start address
  pub value: u64,
  /// Segment number where symbol is defined
  pub seg: u64,
  /// If the symbol is defined in current module (object file)
  pub defined: bool,
}

impl Symbol {
  pub fn parse_definition(s: &str) -> Option<Self> {
    let mut split = s.split(" ");
    let name = split.next()?.to_string();
    let value = split
      .next()
      .and_then(|value| u64::from_str_radix(value, 16).ok())?;
    let seg = split
      .next()
      .and_then(|seg| u64::from_str_radix(seg, 16).ok())?;
    let defined = split.next()? == "D";
    Some(Self {
      name,
      value,
      seg,
      defined,
    })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SegmentType {
  /// Read and Present
  RP,
  /// Read, Write and Present
  RWP,
  /// Read and Write
  RW,
}

impl TryFrom<&'_ str> for SegmentType {
  type Error = String;

  fn try_from(value: &'_ str) -> Result<Self, Self::Error> {
    match value {
      "RP" => Ok(Self::RP),
      "RWP" => Ok(Self::RWP),
      "RW" => Ok(Self::RW),
      s => Err(s.to_string()),
    }
  }
}

impl SegmentType {
  pub fn to_str(self) -> &'static str {
    match self {
      Self::RP => "RP",
      Self::RWP => "RWP",
      Self::RW => "RW",
    }
  }
}

#[derive(Debug)]
pub struct Segment {
  /// Segment name
  pub name: String,
  /// Start address
  pub logical_start: u64,
  /// Segment length in byte
  pub length: u64,
  pub type_: SegmentType,
  /// Segment text data
  pub data: Vec<u8>,
  /// The corresponding output segment number, only used when
  /// linking
  pub output: u64,
}

impl Segment {
  pub fn new_empty(name: String, type_: SegmentType) -> Self {
    Self {
      name,
      logical_start: 0,
      length: 0,
      type_,
      data: vec![],
      output: u64::MAX,
    }
  }

  pub fn parse_definition(s: &str) -> Option<Self> {
    let mut split = s.split(" ");
    let name = split.next()?.to_string();
    let start_addr = split
      .next()
      .and_then(|start_addr| u64::from_str_radix(start_addr, 16).ok())?;
    let length = split
      .next()
      .and_then(|length| u64::from_str_radix(length, 16).ok())?;
    let type_ = split.next()?.try_into().ok()?;
    Some(Segment {
      name,
      logical_start: start_addr,
      length,
      type_,
      data: Vec::new(),
      output: u64::MAX,
    })
  }

  pub fn set_data(&mut self, data: Vec<u8>) {
    self.data = data;
  }

  pub fn parse_data(s: &str) -> Option<Vec<u8>> {
    (0..s.len())
      .step_by(2)
      .map(|i| u8::from_str_radix(&s[i..i + 2], 16).ok())
      .collect()
  }

  pub fn dump_data<W: Write>(&self, w: &mut W) -> std::io::Result<()> {
    const MASK: u8 = (1 << 4) - 1;
    // each byte gives two hex digit
    for &byte in &self.data {
      let upper = byte >> 4;
      let lower = byte & MASK;
      write!(w, "{:x}{:x}", upper, lower)?;
    }
    writeln!(w)?;
    Ok(())
  }
}

#[derive(Debug)]
pub struct Obj {
  pub segments: Vec<Segment>,
  pub symbols: Vec<Symbol>,
  pub rels: Vec<RelEntry>,
}

impl Obj {
  pub fn from_reader<R: BufRead>(reader: &mut R) -> Option<Self> {
    let mut lines = reader.lines();

    let magic = lines.next()?.ok()?;

    if magic != MAGIC_NUMBER {
      return None;
    }

    // read number of segments, symbols and relocation entries
    let numbers = lines.next()?.ok()?;

    let mut split = numbers.split(" ");
    let nseg = split
      .next()
      .and_then(|nseg| u64::from_str_radix(nseg, 16).ok())?;
    let nsyms = split
      .next()
      .and_then(|nsyms| u64::from_str_radix(nsyms, 16).ok())?;
    let nrels = split
      .next()
      .and_then(|nrels| u64::from_str_radix(nrels, 16).ok())?;

    // read segment definitions
    let mut segments = Vec::with_capacity(nseg as usize);
    for _ in 0..nseg {
      let buffer = lines.next()?.ok()?;
      let seg = Segment::parse_definition(&buffer)?;
      segments.push(seg);
    }

    // read symbol definitions
    let mut symbols = Vec::with_capacity(nsyms as usize);
    for _ in 0..nsyms {
      let buffer = lines.next()?.ok()?;
      let sym = Symbol::parse_definition(&buffer)?;
      symbols.push(sym);
    }

    // read relocation entry definitions
    let mut rels = Vec::with_capacity(nrels as usize);
    for _ in 0..nrels {
      let buffer = lines.next()?.ok()?;
      let rel = RelEntry::parse_definition(&buffer)?;
      rels.push(rel);
    }

    // read segment data
    for i in 0..nseg {
      let buffer = lines.next().map(|r| r.unwrap()).unwrap_or_default();
      let data = Segment::parse_data(&buffer)?;
      segments[i as usize].set_data(data);
    }

    Some(Obj {
      segments,
      symbols,
      rels,
    })
  }

  pub fn dump<W: Write>(&self, w: &mut W) -> std::io::Result<()> {
    writeln!(w, "LINK")?;
    // write numbers
    writeln!(
      w,
      "{:x} {:x} {:x}",
      self.segments.len(),
      self.symbols.len(),
      self.rels.len()
    )?;
    // write segments
    for seg in &self.segments {
      writeln!(
        w,
        "{} {:x} {:x} {}",
        &seg.name,
        seg.logical_start,
        seg.length,
        seg.type_.to_str()
      )?;
    }
    // write symbols
    for sym in &self.symbols {
      writeln!(
        w,
        "{} {:x} {:x} {}",
        &sym.name,
        sym.value,
        sym.seg,
        if sym.defined { "D" } else { "U" }
      )?;
    }
    // write rels
    for rel in &self.rels {
      writeln!(
        w,
        "{:x} {:x} {:x} {}",
        rel.loc,
        rel.seg,
        rel.ref_,
        rel.type_.to_str()
      )?;
    }
    // write data
    for seg in &self.segments {
      seg.dump_data(w)?;
    }

    Ok(())
  }
}
