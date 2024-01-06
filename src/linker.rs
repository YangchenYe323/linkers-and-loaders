use std::collections::BTreeMap;

use crate::obj::{Obj, Segment, SegmentType};

const GLOBAL_START: u64 = 0x1000;

#[derive(Debug)]
pub struct Linker {
  inputs: Vec<Obj>,
}

#[derive(Debug, Default)]
pub struct CommonBlock {
  // address
  pub loc: u64,
  pub size: u64,
}

impl Linker {
  pub fn new(inputs: Vec<Obj>) -> Self {
    Self { inputs }
  }

  pub fn link(&mut self) -> Obj {
    let mut rp_segments = BTreeMap::new();
    let mut rwp_segments = BTreeMap::new();
    let mut rw_segments = BTreeMap::new();

    // common blocks: name -> CommonBlock
    let mut common_blocks: BTreeMap<String, CommonBlock> = BTreeMap::new();

    // In the first pass we accumulates size information for the merged
    // segment and the common blocks
    for obj in &self.inputs {
      for seg in &obj.segments {
        let group = match seg.type_ {
          SegmentType::RP => &mut rp_segments,
          SegmentType::RWP => &mut rwp_segments,
          SegmentType::RW => &mut rw_segments,
        };

        // This is the merged new segment
        let new_seg = group
          .entry(seg.name.clone())
          .or_insert_with(|| Segment::new_empty(seg.name.clone(), seg.type_));
        new_seg.length += seg.length;
        // TODO: segment data
      }

      for sym in &obj.symbols {
        let is_common_block = !sym.defined && sym.value != 0;
        if is_common_block {
          let entry = common_blocks.entry(sym.name.clone()).or_default();
          entry.size = 8;
        }
      }
    }

    // In the second pass we assign addresses to each segment in turn
    let mut cur_addr = GLOBAL_START;
    // RP segments come first
    for (_, seg) in &mut rp_segments {
      seg.logical_start = cur_addr;
      cur_addr += seg.length;
      // round up to word boundary for each segment
      cur_addr = utils::next_multile_of_four(cur_addr);
    }

    // Round up to 1000 for RWP segments
    cur_addr = utils::next_multiple_of_a_thousand(cur_addr);
    for (_, seg) in &mut rwp_segments {
      seg.logical_start = cur_addr;
      cur_addr += seg.length;
      // round up to word boundary for each segment
      cur_addr = utils::next_multile_of_four(cur_addr);
    }

    // Round up to 4 for RW segments
    cur_addr = utils::next_multile_of_four(cur_addr);
    for (_, seg) in &mut rw_segments {
      // put common blocks at the end of the bss segment
      seg.logical_start = cur_addr;
      if seg.name == "bss" {
        for (_, block) in &mut common_blocks {
          block.loc = seg.logical_start + seg.length;
          seg.length += block.size;
        }
      }
      cur_addr += seg.length;
      cur_addr = utils::next_multile_of_four(cur_addr);
    }

    // TODO: Set up symbols and relocation entries
    let segments = rp_segments
      .into_values()
      .chain(rwp_segments.into_values())
      .chain(rw_segments.into_values())
      .collect();
    Obj {
      segments,
      symbols: vec![],
      rels: vec![],
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
