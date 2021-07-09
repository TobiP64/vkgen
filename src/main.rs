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

use {vkgen::*, std::{io::{self, Write}, fs::{File, OpenOptions}}};

//mod xml;
//mod parse;
//mod gen;

const HELP: &str = r#"
vkgen
Usage: vkgen <input file (optional)> [options]

If no input file is specified, the registry will be read from stdin.

Options:
--help, -h                       - display this help page
--out=<output file>,
-o=<output file>                 - specify the output file, if no output file is specified, the generated code will be written to `<input file>.rs`
--out-cargo=<output cargo file>,
-oc=<output cargo file>          - specify the output cargo file, if no output file is specified, the generated code will be written to `<input file>.toml`

For more information, see https://crates.io/crates/vkgen
"#;

fn main() {
	let mut file_in = None;
	let mut file_out = None;
	let mut file_out_cargo = None;
	
	for arg in std::env::args().skip(1) {
		if arg.starts_with('-') {
			if arg.starts_with("-o=") || arg.starts_with("--out=") {
				file_out = Some(arg.trim_start_matches("-o=")
					.trim_start_matches("--out=").to_string());
			} else if arg.starts_with("-oc=") || arg.starts_with("--out-cargo=") {
				file_out_cargo = Some(arg.trim_start_matches("-oc=")
					.trim_start_matches("--out-cargo=").to_string());
			} else if arg == "-h" || arg == "--help" {
				println!("{}", HELP);
				return;
			} else {
				println!("ignored unknown option: {}", arg)
			}
		} else {
			file_in = Some(arg);
		}
	}
	
	let r = match &file_in {
		Some(file_in) => {
			print!("parsing file `{}` ... ", file_in);
			std::io::stdout().flush().unwrap_or_default();
			xml::deserialize(File::open(file_in)
				.map(io::BufReader::new)
				.expect("failed to open input file"))
		}
		None => {
			print!("parsing from stdin ... ");
			std::io::stdout().flush().unwrap_or_default();
			xml::deserialize(io::BufReader::new(io::stdin()))
		}
	};
	
	let Registry { api, elements, exts } = match r {
		Ok(v) => {
			println!("\x1b[32mok\x1b[0m");
			v
		}
		Err(e) => {
			println!("\x1b[31mfailed\x1b[0m\nError: {}", e);
			std::process::exit(100);
		}
	};
	
	let file_out = file_out
		.or_else(|| file_in.as_ref().map(|s| s.replace(".xml", ".rs")))
		.unwrap_or_else(|| match api {
			Api::Vulkan => "vk.rs",
			Api::OpenXr => "xr.rs"
		}.to_string());
	
	print!("generating code (dst: `{}`) ... ", &file_out);
	std::io::stdout().flush().unwrap_or_default();
	
	let r = OpenOptions::new()
		.write(true)
		.create(true)
		.truncate(true)
		.open(&file_out)
		.and_then(|writer| gen::gen(&mut io::BufWriter::new(writer), elements.into_iter(), api, true));
	
	match r {
		Ok(()) => println!("\x1b[32mok\x1b[0m"),
		Err(e) => {
			println!("\x1b[31mfailed\x1b[0m\nError: {}", e);
			std::process::exit(100);
		}
	}
	
	let file_out_cargo = file_out_cargo
		.or_else(|| file_in.as_ref().map(|s| s.replace(".xml", ".toml")))
		.unwrap_or_else(|| match api {
			Api::Vulkan => "vk.toml",
			Api::OpenXr => "xr.toml"
		}.to_string());
	
	print!("generating cargo file (dst: `{}`) ... ", &file_out_cargo);
	std::io::stdout().flush().unwrap_or_default();
	
	let r = OpenOptions::new()
		.write(true)
		.create(true)
		.truncate(true)
		.open(&file_out_cargo)
		.and_then(|writer| gen::gen_cargo(&mut io::BufWriter::new(writer), exts));
	
	match r {
		Ok(()) => println!("\x1b[32mok\x1b[0m"),
		Err(e) => {
			println!("\x1b[31mfailed\x1b[0m\nError: {}", e);
			std::process::exit(100);
		}
	}
	
	if BITFIELDS.load(std::sync::atomic::Ordering::Relaxed) {
		println!("WARN: the registry contains C bitfield structs, these structs are not generated correctly! See the README for more information.");
	}
	
	print!("fixing pointer types ... ");
	std::io::stdout().flush().unwrap_or_default();
	match vkgen::fix_ptr_types(api, &file_out) {
		Ok(()) => println!("\x1b[32mok\x1b[0m"),
		Err(e) => {
			println!("\x1b[31mfailed\x1b[0m\nError: {}", e);
			std::process::exit(100);
		}
	}
}