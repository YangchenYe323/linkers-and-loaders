use std::collections::{BTreeMap, HashMap};

use crate::obj::{Obj, Segment, SegmentType, Symbol};

use self::relocation::execute_rel_entry;

const GLOBAL_START: u64 = 0x1000;

/// The module table serves as the single database of the input object information
/// for the linker. It serves the function of retrieving segment by [SegmentId] and
/// retrieving Symbols by [SymbolId]
#[derive(Debug)]
pub struct ModuleTable(Vec<Obj>);

impl std::ops::Index<SegmentId> for ModuleTable {
  type Output = Segment;

  fn index(&self, index: SegmentId) -> &Self::Output {
    let SegmentId(obj, seg) = index;
    &self.0[obj].segments[seg - 1]
  }
}

impl std::ops::IndexMut<SegmentId> for ModuleTable {
  fn index_mut(&mut self, index: SegmentId) -> &mut Self::Output {
    let SegmentId(obj, seg) = index;
    &mut self.0[obj].segments[seg - 1]
  }
}

impl std::ops::Index<SymbolId> for ModuleTable {
  type Output = Symbol;

  fn index(&self, index: SymbolId) -> &Self::Output {
    let SymbolId(obj, sym) = index;
    &self.0[obj].symbols[sym]
  }
}

impl std::ops::IndexMut<SymbolId> for ModuleTable {
  fn index_mut(&mut self, index: SymbolId) -> &mut Self::Output {
    let SymbolId(obj, sym) = index;
    &mut self.0[obj].symbols[sym]
  }
}

/// In a linker, an input segment is uniquely identified by (nth object file, nth segment) pair,
/// where the first is 0-indexed and the second is 1-indexed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SegmentId(usize, usize);

/// Wrapper around [Segment] which keeps track of the input segments that get merged
/// to the output segment. After the address of the output segment is assigned, we will
/// use this mapping to
#[derive(Debug)]
pub struct OutputSegment {
  /// The final output segment, which is all the input segment combined
  seg: Segment,
  /// A list of input segments
  inputs: Vec<SegmentId>,
}

impl OutputSegment {
  pub fn new_empty(name: String, type_: SegmentType) -> Self {
    Self {
      seg: Segment::new_empty(name, type_),
      inputs: vec![],
    }
  }
}

impl OutputSegment {
  pub fn resolve_input_segment_addresses_and_output(
    &self,
    self_idx: u64,
    inputs: &mut ModuleTable,
  ) {
    let mut start_addr = self.seg.logical_start;
    for segment_id in &self.inputs {
      let input_seg = &mut inputs[*segment_id];
      input_seg.logical_start = start_addr;
      input_seg.output = self_idx + 1;
      start_addr += input_seg.length;
    }
  }
}

/// In a linker, an input symbol is uniquely identified by (nth object file, nth symbol) pair,
/// where the first is 0-indexed and the second is 1-indexed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SymbolId(usize, usize);

/// Linker's global symbol table entry, keeps track of if a symbol is defined in multiple
/// places or undefined.
#[derive(Debug, Default)]
pub struct LinkerSymbolTableEntry {
  value: u64,
  // The output segment current entry belongs to
  out_seg: u64,
  defining_inputs: Vec<SymbolId>,
  /// Modules in which the symbol is referred to but not defined (an external)
  referring_inputs: Vec<SymbolId>,
}

#[derive(Debug)]
pub struct Linker {
  /// Module table, the ith object file in the table has object number i,
  /// which is used to identify segment and symbol
  module_table: ModuleTable,
}

impl Linker {
  pub fn new(inputs: Vec<Obj>) -> Self {
    Self {
      module_table: ModuleTable(inputs),
    }
  }

  pub fn link(&mut self) -> Obj {
    let mut rp_segments = BTreeMap::new();
    let mut rwp_segments = BTreeMap::new();
    let mut rw_segments = BTreeMap::new();

    let mut global_symbol_table = HashMap::new();

    // In the first pass we accumulates size information for the merged output
    // segments and global symbol table
    for (iobj, obj) in self.module_table.0.iter().enumerate() {
      for (iseg, seg) in obj.segments.iter().enumerate() {
        let group = match seg.type_ {
          SegmentType::RP => &mut rp_segments,
          SegmentType::RWP => &mut rwp_segments,
          SegmentType::RW => &mut rw_segments,
        };

        // This is the merged new segment
        let new_seg = group
          .entry(seg.name.clone())
          .or_insert_with(|| OutputSegment::new_empty(seg.name.clone(), seg.type_));
        new_seg.seg.length += seg.length;
        new_seg.inputs.push(SegmentId(iobj, iseg + 1));
        // TODO: segment data
      }

      for (isym, sym) in obj.symbols.iter().enumerate() {
        let entry = global_symbol_table
          .entry(sym.name.clone())
          .or_insert_with(LinkerSymbolTableEntry::default);
        // println!("{:?}", sym);
        if sym.defined {
          if !entry.defining_inputs.is_empty() {
            panic!("Multiple definition of symbol {}", &sym.name);
          }
          entry.value = sym.value;
          entry.defining_inputs.push(SymbolId(iobj, isym));
        } else {
          entry.referring_inputs.push(SymbolId(iobj, isym));
        }
      }
    }

    let mut cur_addr = GLOBAL_START;
    // In the second pass we:
    // Step 1: assign addresses to each output segment in turn
    // RP segments come first
    for (_, seg) in &mut rp_segments {
      seg.seg.logical_start = cur_addr;
      cur_addr += seg.seg.length;
      // round up to word boundary for each segment
      cur_addr = utils::next_multile_of_four(cur_addr);
    }

    // Round up to 1000 for RWP segments
    cur_addr = utils::next_multiple_of_a_thousand(cur_addr);
    for (_, seg) in &mut rwp_segments {
      seg.seg.logical_start = cur_addr;
      cur_addr += seg.seg.length;
      // round up to word boundary for each segment
      cur_addr = utils::next_multile_of_four(cur_addr);
    }

    // Round up to 4 for RW segments
    cur_addr = utils::next_multile_of_four(cur_addr);
    for (_, seg) in &mut rw_segments {
      // put common blocks at the end of the bss segment
      seg.seg.logical_start = cur_addr;
      cur_addr += seg.seg.length;
      cur_addr = utils::next_multile_of_four(cur_addr);
    }

    // all_segments is the canonical list of all the output segments
    let mut all_segments: Vec<_> = rp_segments
      .into_values()
      .chain(rwp_segments.into_values())
      .chain(rw_segments.into_values())
      .collect();

    // Step 2: Assign address to each input segment based on output segment's base address
    // and establish mapping from input segment to output segment
    for (self_index, seg) in all_segments.iter().enumerate() {
      seg.resolve_input_segment_addresses_and_output(self_index as u64, &mut self.module_table);
    }

    // Step 3: By this point every input segment should have a logical start address resolved
    // and an output segment mapped we propagate this information to calculate the logical address
    // and output segment of the symbols.
    for (name, entry) in &mut global_symbol_table {
      if entry.defining_inputs.is_empty() {
        panic!("Symbol undefined: {}", name);
      }
      let defining_symbol = entry.defining_inputs[0];
      let input_segment = self.module_table[defining_symbol].seg;
      let offset = self.module_table[defining_symbol].value;
      let defining_segment = SegmentId(defining_symbol.0, input_segment as usize);
      let final_addr = self.module_table[defining_segment].logical_start + offset;
      entry.value = final_addr;
      let out_seg = self.module_table[defining_segment].output;
      entry.out_seg = out_seg;

      // Propagate back to all the inputs
      for symbol_id in &entry.defining_inputs {
        self.module_table[*symbol_id].value = final_addr;
      }
      for symbol_id in &entry.referring_inputs {
        self.module_table[*symbol_id].value = final_addr;
      }
    }

    // Step 4: By this point every input segment and symbol has a final address assignment, and we could start
    // executing relocation entries.
    for iobj in 0..self.module_table.0.len() {
      for entry in self.module_table.0[iobj].rels.clone() {
        execute_rel_entry(&mut self.module_table, iobj as u64, entry);
      }
    }

    // Step 5: Now relocation entries have been applied and we could add up final segment data
    for segment in &mut all_segments {
      for input_id in &segment.inputs {
        segment
          .seg
          .data
          .append(&mut self.module_table[*input_id].data);
      }
    }

    let segments = all_segments.into_iter().map(|out| out.seg).collect();
    let symbols = global_symbol_table
      .into_iter()
      .map(|(name, entry)| Symbol {
        name,
        value: entry.value,
        seg: entry.out_seg,
        defined: true,
      })
      .collect();

    Obj {
      segments,
      symbols,
      rels: vec![],
    }
  }
}

mod relocation {
  use std::io::Cursor;

  use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

  use crate::obj::{RelEntry, RelEntryType};

  use super::{ModuleTable, SegmentId, SymbolId};

  pub fn execute_rel_entry(module_table: &mut ModuleTable, obj: u64, entry: RelEntry) {
    let origin_seg = SegmentId(obj as usize, entry.seg as usize);
    // final_addr = target_addr - relational_addr + offset
    let target_addr = find_target_address(entry.type_, entry.ref_, obj, module_table);
    let offset = match entry.type_ {
      RelEntryType::A4 | RelEntryType::R4 => {
        let seg = &mut module_table[origin_seg];
        let data = &seg.data;
        let mut cursor = Cursor::new(&data[entry.loc as usize..(entry.loc + 4) as usize]);
        cursor.read_u32::<BigEndian>().unwrap()
      }
      _ => 0,
    };
    let relational_addr = match entry.type_ {
      RelEntryType::R4 => module_table[origin_seg].logical_start,
      RelEntryType::RS4 => module_table[origin_seg].logical_start + entry.loc,
      _ => 0,
    };
    let value = target_addr + offset as u64 - relational_addr;
    let value = value as u32;
    match entry.type_ {
      RelEntryType::A4 | RelEntryType::AS4 | RelEntryType::R4 | RelEntryType::RS4 => {
        let seg = &mut module_table[origin_seg];
        let mut cursor = Cursor::new(&mut seg.data[entry.loc as usize..(entry.loc + 4) as usize]);
        cursor.write_u32::<BigEndian>(value).unwrap()
      }
      RelEntryType::U2 => {
        let upper = (value >> 16) as u16;
        let seg = &mut module_table[origin_seg];
        let mut cursor = Cursor::new(&mut seg.data[entry.loc as usize..(entry.loc + 2) as usize]);
        cursor.write_u16::<BigEndian>(upper).unwrap()
      }
      RelEntryType::L2 => {
        let lower = (value & 0xffff) as u16;
        let seg = &mut module_table[origin_seg];
        let mut cursor = Cursor::new(&mut seg.data[entry.loc as usize..(entry.loc + 2) as usize]);
        cursor.write_u16::<BigEndian>(lower).unwrap()
      }
    }
  }

  fn find_target_address(
    type_: RelEntryType,
    ref_: u64,
    obj: u64,
    module_table: &ModuleTable,
  ) -> u64 {
    match type_ {
      RelEntryType::R4 | RelEntryType::A4 => {
        let segment_id = SegmentId(obj as usize, ref_ as usize);
        module_table[segment_id].logical_start
      }
      _ => {
        let symbol_id = SymbolId(obj as usize, ref_ as usize);
        module_table[symbol_id].value
      }
    }
  }
}

mod utils {
  pub fn next_multile_of_four(number: u64) -> u64 {
    (number + 3) & (!0x03)
  }

  pub fn next_multiple_of_a_thousand(number: u64) -> u64 {
    ((number + 999) / 1000) * 1000
  }
}

#[test]
fn t() {
  println!("{}", utils::next_multile_of_four(18000));
}
