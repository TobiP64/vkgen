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

use {self::{RegistryElement::*, parse::*}, std::{io::{self, {Read, Seek, SeekFrom, Write}}, fs::{File, OpenOptions}}};

mod xml;
mod parse;
mod gen;

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
	
	let Registry { api, vec, exts, consts }: Registry = match &file_in {
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
	}.expect("failed to parse registry");
	
	let iter = match api {
		Api::Vulkan => get_vk_items(),
		Api::OpenXr => get_xr_items()
	}.into_iter()
		.chain(vec.into_iter())
		.chain(std::iter::once(Enums(KhrEnums {
			name:    "Extension Constants".to_string(),
			r#type:  KhrEnumsType::None,
			comment: None,
			enums:   consts
		})));
	
	let file_out = file_out
		.or_else(|| file_in.as_ref().map(|s| s.replace(".xml", ".rs")))
		.unwrap_or_else(|| match api {
			Api::Vulkan => "vk.rs",
			Api::OpenXr => "xr.rs"
		}.to_string());
	print!("\x1b[32mok\x1b[0m\ngenerating code (dst: `{}`) ... ", &file_out);
	std::io::stdout().flush().unwrap_or_default();
	
	OpenOptions::new()
		.write(true)
		.create(true)
		.truncate(true)
		.open(&file_out)
		.and_then(|writer| gen::gen(&mut io::BufWriter::new(writer), iter, api, true))
		.expect("failed output generated code");
	
	let file_out_cargo = file_out_cargo
		.or_else(|| file_in.as_ref().map(|s| s.replace(".xml", ".toml")))
		.unwrap_or_else(|| match api {
			Api::Vulkan => "vk.toml",
			Api::OpenXr => "xr.toml"
		}.to_string());
	print!("\x1b[32mok\x1b[0m\ngenerating cargo file (dst: `{}`) ... ", &file_out_cargo);
	
	OpenOptions::new()
		.write(true)
		.create(true)
		.truncate(true)
		.open(&file_out_cargo)
		.and_then(|writer| gen::gen_cargo(&mut io::BufWriter::new(writer), exts))
		.expect("failed output generated cargo file");
	
	println!("\x1b[32mok\x1b[0m");
	
	if BITFIELDS.load(std::sync::atomic::Ordering::Relaxed) {
		println!("WARN: the registry contained C bitfield structs, these structs were not generated correctly! See the README for more information.");
	}
	
	let mut buf = String::new();
	
	let mut f = OpenOptions::new()
		.read(true)
		.write(true)
		.open(&file_out)
		.unwrap();
	
	f.read_to_string(&mut buf).unwrap();
	
	let buf = buf.replace("&()", if api == Api::Vulkan { "VkAnyRef" } else { "XrAnyRef" })
		.replace("&'a ()", if api == Api::Vulkan { "VkAnyRef<'a>" } else { "XrAnyRef<'a>" })
		.replace("&mut ()", if api == Api::Vulkan { "VkAnyMut" } else { "XrAnyMut" })
		.replace("&'a mut ()", if api == Api::Vulkan { "VkAnyMut<'a>" } else { "XrAnyMut<'a>" })
		.replace("&[()]", "&[u8]")
		.replace("&mut [()]", "&mut [u8]")
		.replace("&'a [()]", "&'a [u8]")
		.replace("&'a mut [()]", "&'a mut [u8]");
	
	f.seek(SeekFrom::Start(0)).unwrap();
	f.write_all(buf.as_bytes()).unwrap();
}

fn get_vk_items() -> Vec<RegistryElement> {
	vec![
		Macro {
			name: "VK_MAKE_VERSION".to_string(),
			content: "(major: u32, minor: u32, patch: u32) -> u32 { (major << 22) | (minor << 12) | patch }".to_string()
		},
		Macro {
			name: "VK_VERSION_MAJOR".to_string(),
			content: "(version: u32) -> u32 { version >> 22 }".to_string()
		},
		Macro {
			name: "VK_VERSION_MINOR".to_string(),
			content: "(version: u32) -> u32 { version >> 12 & 0x3ff }".to_string()
		},
		Macro {
			name: "VK_VERSION_PATCH".to_string(),
			content: "(version: u32) -> u32 { version & 0xfff }".to_string()
		},
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkInternalAllocationNotification".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  vec![
				KhrCommandParam { name: "pUserData".to_string(),       r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "size".to_string(),            r#type: "usize".to_string(), ..Default::default() },
				KhrCommandParam { name: "allocationType".to_string(),  r#type: "VkInternalAllocationType".to_string(), ..Default::default() },
				KhrCommandParam { name: "allocationScope".to_string(), r#type: "VkSystemAllocationScope".to_string(), ..Default::default() }
			]
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkInternalFreeNotification".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  vec![
				KhrCommandParam { name: "pUserData".to_string(),       r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "size".to_string(),            r#type: "usize".to_string(), ..Default::default() },
				KhrCommandParam { name: "allocationType".to_string(),  r#type: "VkInternalAllocationType".to_string(), ..Default::default() },
				KhrCommandParam { name: "allocationScope".to_string(), r#type: "VkSystemAllocationScope".to_string(), ..Default::default() }
			]
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkReallocationFunction".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  vec![
				KhrCommandParam { name: "pUserData".to_string(),       r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "pOriginal".to_string(),       r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "size".to_string(),            r#type: "usize".to_string(), ..Default::default() },
				KhrCommandParam { name: "alignment".to_string(),       r#type: "usize".to_string(), ..Default::default() },
				KhrCommandParam { name: "allocationScope".to_string(), r#type: "VkSystemAllocationScope".to_string(), ..Default::default() }
			]
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkAllocationFunction".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  vec![
				KhrCommandParam { name: "pUserData".to_string(),       r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "size".to_string(),            r#type: "usize".to_string(), ..Default::default() },
				KhrCommandParam { name: "alignment".to_string(),       r#type: "usize".to_string(), ..Default::default() },
				KhrCommandParam { name: "allocationScope".to_string(), r#type: "VkSystemAllocationScope".to_string(), ..Default::default() }
			]
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkFreeFunction".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  vec![
				KhrCommandParam { name: "pUserData".to_string(), r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "pMemory".to_string(),   r#type: "*const u8".to_string(), ..Default::default() }
			]
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkVoidFunction".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  Vec::new()
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkDebugReportCallbackEXT".to_string(),
			comment: None,
			result:  "VkBool32".to_string(),
			params:  vec![
				KhrCommandParam { name: "flags".to_string(),        r#type: "VkDebugReportFlagsEXT".to_string(), ..Default::default() },
				KhrCommandParam { name: "objectType".to_string(),   r#type: "VkDebugReportObjectTypeEXT".to_string(), ..Default::default() },
				KhrCommandParam { name: "object".to_string(),       r#type: "u64".to_string(), ..Default::default() },
				KhrCommandParam { name: "location".to_string(),     r#type: "usize".to_string(), ..Default::default() },
				KhrCommandParam { name: "messageCode".to_string(),  r#type: "i32".to_string(), ..Default::default() },
				KhrCommandParam { name: "pLayerPrefix".to_string(), r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "pMessage".to_string(),     r#type: "*const u8".to_string(), ..Default::default() },
				KhrCommandParam { name: "pUserData".to_string(),    r#type: "*const u8".to_string(), ..Default::default() }
			]
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_vkDebugUtilsMessengerCallbackEXT".to_string(),
			comment: None,
			result:  "VkBool32".to_string(),
			params:  vec![KhrCommandParam { name: "messageSeverity".to_string(), r#type: "VkDebugUtilsMessageSeverityFlagBitsEXT".to_string(), ..Default::default() },
						  KhrCommandParam { name: "messageTypes".to_string(),    r#type: "VkDebugUtilsMessageTypeFlagsEXT".to_string(), ..Default::default() },
						  KhrCommandParam { name: "pCallbackData".to_string(),   r#type: "*const VkDebugUtilsMessengerCallbackDataEXT".to_string(), ..Default::default() },
						  KhrCommandParam { name: "pUserData".to_string(),       r#type: "*const u8".to_string(), ..Default::default() }]
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name: "PFN_vkDeviceMemoryReportCallbackEXT".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  vec![
				KhrCommandParam { name: "pCallbackData".to_string(), r#type: "*const VkDeviceMemoryReportCallbackDataEXT".to_string(), ..Default::default() },
				KhrCommandParam { name: "pUserData".to_string(),     r#type: "*mut u8".to_string(), ..Default::default() }
			]
		}))
	]
}

fn get_xr_items() -> Vec<RegistryElement> {
	vec![
		Macro {
			name: "XR_MAKE_VERSION".to_string(),
			content: "(major: u64, minor: u64, patch: u64) -> XrVersion { (major & 0xffff << 48) | (minor & 0xffff << 32) | (patch & 0xffffffff) }".to_string()
		},
		Macro {
			name: "XR_VERSION_MAJOR".to_string(),
			content: "(version: XrVersion) -> u64 { version >> 48 & 0xffff }".to_string()
		},
		Macro {
			name: "XR_VERSION_MINOR".to_string(),
			content: "(version: XrVersion) -> u64 { version >> 32 & 0xffff }".to_string()
		},
		Macro {
			name: "XR_VERSION_PATCH".to_string(),
			content: "(version: XrVersion) -> u64 { version & 0xffffffff }".to_string()
		},
		Macro {
			name: "XR_SUCCEEDED".to_string(),
			content: "(result: XrResult) -> bool { result as i32 >= 0 }".to_string()
		},
		Macro {
			name: "XR_UNQUALIFIED_SUCCESS".to_string(),
			content: "(result: XrResult) -> bool { result as i32 == 0 }".to_string()
		},
		Macro {
			name: "XR_FAILED".to_string(),
			content: "(result: XrResult) -> bool { (result as i32) < 0 }".to_string()
		},
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_xrVoidFunction".to_string(),
			comment: None,
			result:  "()".to_string(),
			params:  Vec::new()
		})),
		Type(KhrType::FuncPtr(KhrTypeFuncPtr {
			name:    "PFN_xrDebugUtilsMessengerCallbackEXT".to_string(),
			comment: None,
			result:  "XrBool32".to_string(),
			params:  vec![
				KhrCommandParam { r#type: "XrDebugUtilsMessageSeverityFlagsEXT".to_string(),         name: "messageSeverity".to_string(), ..Default::default() },
				KhrCommandParam { r#type: "XrDebugUtilsMessageTypeFlagsEXT".to_string(),             name: "messageTypes".to_string(), ..Default::default() },
				KhrCommandParam { r#type: "*const XrDebugUtilsMessengerCallbackDataEXT".to_string(), name: "callbackData".to_string(), ..Default::default() },
				KhrCommandParam { r#type: "*const u8".to_string(),                                   name: "pUserData".to_string(), ..Default::default() },
			]
		})),
	]
}
