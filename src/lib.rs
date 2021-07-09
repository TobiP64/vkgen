// MIT License
//
// Copyright (c) 2019-2021 Tobias Pfeiffer
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

//! The library version of this crate, to be used in build scripts.
//!
//! Example 1:
//! ```rust
//! use std::{fs, io};
//!
//! fn main() {
//! 	vkgen::generate(
//! 		fs::File::open("vk.xml")
//! 			.map(io::BufReader::new)
//! 			.expect("failed to open input file"),
//! 		fs::OpenOptions::new()
//! 			.write(true)
//! 			.open("vk.rs")
//! 			.map(io::BufWriter::new)
//! 			.expect("failed to open output file"),
//! 		fs::OpenOptions::new()
//! 			.write(true)
//! 			.open("vk.toml")
//! 			.map(io::BufWriter::new)
//! 			.expect("failed to open output file"),
//! 		true
//! 	).unwrap();
//!
//! 	vkgen::fix_ptr_types(vkgen::Api::Vulkan, "vk.rs")
//! 		.expect("failed to fix pointer types");
//! }
//! ```
//!
//! Example 2:
//! ```rust
//! use {vkgen::*, std::{fs, io}};
//!
//! fn main() {
//! 	let Registry { api, elements, exts } = xml::deserialize(fs::File::open("vk.xml")
//! 		.map(io::BufReader::new)
//! 		.expect("failed to open input file"))
//!  		.expect("failed to deserialize registry");
//!
//! 	gen(&mut fs::OpenOptions::new()
//! 		.write(true)
//! 		.open("vk.rs")
//! 		.map(io::BufWriter::new)
//! 		.expect("failed to open input file"), elements, api, true)
//! 		.expect("failed to write the generated code");
//!
//! 	gen_cargo(&mut fs::OpenOptions::new()
//! 		.write(true)
//! 		.open("vk.toml")
//! 		.map(io::BufWriter::new)
//! 		.expect("failed to open output file"), exts)
//!  		.expect("failed to write the generated code");
//!
//! 	fix_ptr_types(api, "vk.rs")
//! 		.expect("failed to fix pointer types");
//! }
//! ```

use std::{io::{self, *}, fs::OpenOptions, path::Path};

pub use {parse::*, gen::*};

pub mod xml;
pub mod parse;
pub mod gen;

pub fn generate(
	reader:           impl BufRead + std::fmt::Debug,
	mut writer:       impl Write,
	mut writer_cargo: impl Write,
	mangle:           bool
) -> io::Result<()> {
	let Registry { api, elements, exts } = xml::deserialize(reader)
		.map_err(|e| Error::new(ErrorKind::InvalidData, e))?;
	
	gen(&mut writer, elements, api, mangle)?;
	gen_cargo(&mut writer_cargo, exts)?;
	Ok(())
}

/// Fixes void pointers/unit references
pub fn fix_ptr_types(api: Api, path: impl AsRef<Path>) -> io::Result<()> {
	let mut buf = String::new();
	let mut f = OpenOptions::new()
		.read(true)
		.write(true)
		.open(path)?;
	
	f.read_to_string(&mut buf)?;
	
	let buf = buf.replace("&()", if api == Api::Vulkan { "VkAnyRef" } else { "XrAnyRef" })
		.replace("&'a ()", if api == Api::Vulkan { "VkAnyRef<'a>" } else { "XrAnyRef<'a>" })
		.replace("&mut ()", if api == Api::Vulkan { "VkAnyMut" } else { "XrAnyMut" })
		.replace("&'a mut ()", if api == Api::Vulkan { "VkAnyMut<'a>" } else { "XrAnyMut<'a>" })
		.replace("&[()]", "&[u8]")
		.replace("&mut [()]", "&mut [u8]")
		.replace("&'a [()]", "&'a [u8]")
		.replace("&'a mut [()]", "&'a mut [u8]");
	
	f.seek(SeekFrom::Start(0))?;
	f.write_all(buf.as_bytes())?;
	f.flush()
}
