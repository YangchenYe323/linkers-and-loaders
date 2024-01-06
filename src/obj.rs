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
  /// 4-byte absolute address
  A4,
  /// 4-byte relative address
  R4,
}

impl TryFrom<&'_ str> for RelEntryType {
  type Error = String;

  fn try_from(value: &str) -> Result<Self, Self::Error> {
    match value {
      "A4" => Ok(Self::A4),
      "R4" => Ok(Self::R4),
      s => Err(s.to_string()),
    }
  }
}

impl RelEntryType {
  pub fn to_str(self) -> &'static str {
    match self {
      Self::A4 => "A4",
      Self::R4 => "R4",
    }
  }
}

#[derive(Debug)]
pub struct RelEntry {
  /// Address of the entry
  pub loc: u64,
  /// Segment where the entry is defined
  pub seg: u64,
  /// Symbol number of this entry
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
  /// Hex value of symbol
  pub value: u64,
  /// Segment where symbol is defined
  pub seg: u64,
  /// U - undefined, D - defined
  pub type_: String,
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
    let type_ = split.next()?.to_string();
    Some(Self {
      name,
      value,
      seg,
      type_,
    })
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
  /// Segment readable
  pub read: bool,
  /// Segment writable
  pub write: bool,
  /// Segment present in file
  pub present: bool,
  /// Segment text data
  pub data: Vec<u8>,
}

impl Segment {
  pub fn parse_definition(s: &str) -> Option<Self> {
    let mut split = s.split(" ");
    let name = split.next()?.to_string();
    let start_addr = split
      .next()
      .and_then(|start_addr| u64::from_str_radix(start_addr, 16).ok())?;
    let length = split
      .next()
      .and_then(|length| u64::from_str_radix(length, 16).ok())?;
    let properties = split.next()?;
    let read = properties.contains('R');
    let write = properties.contains('W');
    let present = properties.contains('P');
    Some(Segment {
      name,
      logical_start: start_addr,
      length,
      read,
      write,
      present,
      data: Vec::new(),
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
      let buffer = lines.next()?.ok()?;
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
      let mut property = String::new();
      if seg.read {
        property.push('R');
      }
      if seg.write {
        property.push('W');
      }
      if seg.present {
        property.push('P');
      }
      writeln!(
        w,
        "{} {:x} {:x} {}",
        &seg.name, seg.logical_start, seg.length, property
      )?;
    }
    // write symbols
    for sym in &self.symbols {
      writeln!(
        w,
        "{} {:x} {:x} {}",
        &sym.name, sym.value, sym.seg, &sym.type_
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
