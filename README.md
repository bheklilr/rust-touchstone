# rust-touchstone

A pure Rust crate for parsing Touchstone 2.0 files using `nom`.

# Motivation

This project is primarily for the purpose of learning more Rust, specifically
parsing text and manipulating arrays of data.  This project is not meant to be
used in any sort of production setting.  If you think it's good enough for
production use then please let me know so I can turn it into more than a side
project for learning (i.e adding proper tests, benchmarks, extra features).

# Examples

Current this project is not fully functional, but the intention is to be able to
use it something like

```rust

extern crate touchstone;

use ts;

let DATA &'static str = include_str!("data.s4p");

pub fn main() {
    let data = ts::parse_touchstone(DATA).unwrap();
    println!("{:?}", data.num_ports);  // prints "4"
    println!("{:?}", data.option_line.freq_units);  // prints "DB"
    println!("{:?}", data.length());  // prints 2000
}
```

This currently doesn't work, because I haven't finished it yet.  So far it can
(mostly) parse the header of a Touchstone 2.0 file, which has been more
complicated than expected.
