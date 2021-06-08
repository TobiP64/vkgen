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

use {
	crate::parse::*,
	std::{io, ops::Deref, rc::Rc, cell::RefCell, collections::{HashMap, HashSet, hash_map::Entry}}
};

macro_rules! writeKhrImpl {
	( @init
    	$writer:expr,
    	$lib_name:expr,
    	$lib_abbr:expr,
    	$fn_table:expr,
    	$fn_uninit:expr,
    	$fn_init:expr
    ) => { writeln!($writer, r#"
static mut LIB_{2}: Lib{1} = Lib{1} {{
	lib                                         : None,
	pfn_{3}GetInstanceProcAddr                   : {{ extern "C" fn load(arg0: u64, arg1: *const u8) -> PFN_{3}VoidFunction {{ unsafe {{ LIB_{2}.load(); (LIB_{2}.pfn_{3}GetInstanceProcAddr)(arg0, arg1) }} }} load }},{4}
}};

struct Lib{1} {{
	lib                                         : Option<libloading::Library>,
	pfn_{3}GetInstanceProcAddr                   : extern "C" fn(u64, *const u8) -> PFN_{3}VoidFunction,{5}
}}

impl Lib{1} {{
	unsafe fn load(&mut self) {{
		let lib                                          = libloading::Library::new(LIB).expect("failed to load `lib{0}`");
		self.pfn_{3}GetInstanceProcAddr                   = *lib.get(b"{3}GetInstanceProcAddr\0").expect("failed to load `{3}GetInstanceProcAddr`");{6}
		self.lib                                         = Some(lib);
		log::trace!("loaded `lib{0}`");
	}}
}}"#, $lib_name.to_lowercase(), $lib_name, $lib_name.to_uppercase(), $lib_abbr, $fn_table, $fn_uninit, $fn_init) };
	(@table $writer:expr, $name:expr, $fn_table:expr, $fn_init:expr, $parent:expr ) => { write!($writer, r#"
pub struct {0}Table {{
	{1}
}}

impl {0}Table {{
	fn load(handle: {0}, table: &{3}Table) -> Arc<Self> {{
		Arc::new(unsafe {{ Self {{
			{2}
		}} }})
	}}
}}

impl fmt::Debug for {0}Table {{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {{
		f.debug_struct("{0}Table").finish()
	}}
}}

"#, $name, $fn_table, $fn_init, $parent) };
    (@impl $writer:expr, $handle:expr, $table:expr ) => { write!($writer, r#"
/// Wrapper for a `{0}` handle
#[derive(Clone, Debug)]
pub struct {0}Impl {{
	pub handle: {0},
	pub table:  Arc<{1}Table>
}}

impl ops::Deref for {0}Impl {{
	type Target = {0};
	
	#[inline]
	fn deref(&self) -> &{0} {{ &self.handle }}
}}

impl fmt::Display for {0}Impl {{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {{
		write!(f, "{{:#x}}", self.handle)
	}}
}}

impl {0}Impl {{
"#, $handle, $table) };
}

#[derive(Clone, Debug)]
struct Alias {
	name:     String,
	comment:  Option<String>,
	r#type:   Rc<RefCell<Type>>
}

#[derive(Clone, Debug)]
struct Struct {
	name:     String,
	comment:  Option<String>,
	category: KhrTypeStructCategory,
	members:  Vec<StructMember>
}

impl Struct {
	fn has_lifetime(&self) -> bool {
		self.members.iter().any(|m| match (&*m.r#type.borrow(), &m.len) {
			(Type::Ref(ty), Some(_)) | (Type::Mut(ty), Some(_)) => ty.borrow().has_lifetime(),
			(ty, _) => ty.has_lifetime()
		})
	}
	
	fn is_copy(&self) -> bool {
		self.members.iter().all(|m| m.r#type.borrow().is_copy())
	}
	
	fn is_debug(&self) -> bool {
		self.category != KhrTypeStructCategory::Union && self.members.iter()
			.all(|m| m.r#type.borrow().is_debug())
	}
	
	fn is_default(&self) -> bool {
		self.category != KhrTypeStructCategory::Union && self.members.iter()
			.all(|m| m.r#type.borrow().is_default() && m.len.is_none())
	}
}

#[derive(Clone, Debug)]
struct StructMember {
	name:     String,
	comment:  Option<String>,
	r#type:   Rc<RefCell<Type>>,
	optional: Option<String>,
	len:      Option<String>
}

#[derive(Clone, Debug)]
struct FnPtr {
	params: Vec<Param>,
	result: Vec<Rc<RefCell<Type>>>
}

#[derive(Clone, Debug)]
enum Type {
	Ref(Rc<RefCell<Type>>),
	Mut(Rc<RefCell<Type>>),
	Arr(Rc<RefCell<Type>>, Option<Value>),
	FnPtr(Rc<FnPtr>),
	Alias(Rc<Alias>),
	Struct(Rc<Struct>),
	Enums(Rc<KhrEnums>),
	Primitive(String),
	Undefined(String)
}

impl Type {
	fn from_str(s: &str, lookup: &mut impl FnMut(&str) -> Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
		let s = s.trim();
		if let Some(s) = s.strip_prefix("*const ") {
			Rc::new(RefCell::new(Self::Ref(Self::from_str(s, lookup))))
		} else if let Some(s) = s.strip_prefix("*mut ") {
			Rc::new(RefCell::new(Self::Mut(Self::from_str(s, lookup))))
		} else if let (Some(s), true) = (s.strip_prefix('['), s.ends_with(s)) {
			Rc::new(RefCell::new(match s.rfind(';') {
				Some(i) => Self::Arr(Self::from_str(&s[..i], lookup), Some(Value::Literal(s[i + 1..s.len() - 1].to_string(), lookup("usize")))),
				None => {
					Self::Arr(Self::from_str(s, lookup), None)
				}
			}))
		} else {
			lookup(s)
		}
	}
	
	fn has_lifetime(&self) -> bool {
		match self {
			Self::Ref(_) | Self::Mut(_) => true,
			Self::Struct(s)             => s.has_lifetime(),
			Self::Arr(ty, _)            => ty.borrow().has_lifetime(),
			Self::Alias(v)              => v.r#type.borrow().has_lifetime(),
			Self::Primitive(_) | Self::FnPtr(_) | Self::Enums(_) | Self::Undefined(_) => false
		}
	}
	
	fn is_copy(&self) -> bool {
		match self {
			Self::Ref(_) | Self::Primitive(_) | Self::FnPtr(_) | Self::Enums(_) => true,
			Self::Mut(_) | Self::Undefined(_) => false,
			Self::Struct(s)                   => s.is_copy(),
			Self::Arr(ty, _)                  => ty.borrow().is_copy(),
			Self::Alias(v)                    => v.r#type.borrow().is_copy()
		}
	}
	
	fn is_debug(&self) -> bool {
		match self {
			Self::Ref(v) | Self::Mut(v) => v.borrow().is_debug(),
			Self::Struct(_s) => /*s.is_debug()*/false,
			Self::FnPtr(_)| Self::Undefined(_) | Self::Arr(_, Some(_)) => false,
			Self::Primitive(_) | Self::Enums(_) | Self::Arr(_, None) => true,
			Self::Alias(v) => v.r#type.borrow().is_debug()
		}
	}
	
	fn is_default(&self) -> bool {
		match self {
			Self::Ref(_) | Self::Mut(_) | Self::Arr(_, None) | Self::Primitive(_) => true,
			Self::FnPtr(_) | Self::Enums(_) | Self::Undefined(_) | Self::Arr(_, Some(_)) => false,
			Self::Struct(_s)             => /*s.is_default()*/false,
			Self::Alias(v)              => v.r#type.borrow().is_default()
		}
	}
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) ->  std::fmt::Result {
		match (self, f.alternate(), f.sign_plus()) {
			(Self::Ref(ty), false, false)            => write!(f, "&{}", ty.borrow()),
			(Self::Ref(ty), false, true)             => write!(f, "&'a {:+}", ty.borrow()),
			(Self::Ref(ty), true, false)             => write!(f, "*const {:#}", ty.borrow()),
			(Self::Ref(ty), true, true)              => write!(f, "*const {:+#}", ty.borrow()),
			(Self::Mut(ty), false, false)            => write!(f, "&mut {}", ty.borrow()),
			(Self::Mut(ty), false, true)             => write!(f, "&'a mut {:+}", ty.borrow()),
			(Self::Mut(ty), true, false)             => write!(f, "*mut {:#}", ty.borrow()),
			(Self::Mut(ty), true, true)              => write!(f, "*mut {:+#}", ty.borrow()),
			(Self::Arr(ty, None), false, false)      => write!(f, "[{}]", ty.borrow()),
			(Self::Arr(ty, None), false, true)       => write!(f, "[{:+}]", ty.borrow()),
			(Self::Arr(ty, None), true, _)           => write!(f, "{}", ty.borrow()),
			(Self::Arr(ty, Some(len)), false, false) => write!(f, "[{}; {} as usize]", ty.borrow(), len),
			(Self::Arr(ty, Some(len)), false, true)  => write!(f, "[{:+}; {} as usize]", ty.borrow(), len),
			(Self::Arr(ty, Some(len)), true, _)      => write!(f, "[{:#}; {} as usize]", ty.borrow(), len),
			(Self::Alias(alias), _, lt)              => write!(f, "{}{}", &alias.name, if lt && alias.r#type.borrow().has_lifetime() { "<'a>" } else { "" }),
			(Self::Struct(s), _, lt)                 => write!(f, "{}{}", &s.name, if lt && s.has_lifetime() { "<'a>" } else { "" }),
			(Self::Enums(enums), ..)                 => write!(f, "{}", &enums.name),
			(Self::Primitive(s), ..) | (Self::Undefined(s), ..) => f.write_str(s),
			(Self::FnPtr(fn_ptr), _, _) => {
				let FnPtr { params, result } = &**fn_ptr;
				
				f.write_str("extern fn(\n")?;
				
				for param in params {
					match param {
						Param { name, ty, optional: Some(s), .. } if s == "true" => writeln!(f, "\t{:<45}: Option<{}>,", name, ty.borrow()),
						Param { name, ty, .. } => writeln!(f, "\t{:<45}: {},", name, ty.borrow())
					}?;
				}
				
				match result.as_slice() {
					[] => f.write_str(")"),
					[ty] => writeln!(f, ") -> {}", ty.borrow()),
					result => {
						f.write_str(") -> (\n")?;
						
						for ty in result {
							writeln!(f, "{},", ty.borrow())?;
						}
						
						f.write_str(")")
					}
				}
			}
		}
	}
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
enum Value {
	Literal(String, Rc<RefCell<Type>>),
	Enum(Rc<KhrEnums>, usize)
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Literal(val, _) => f.write_str(val),
			Self::Enum(_enums, _idx) => unimplemented!()/*write!(f, "{}::{}", &enums.name, &enums.enums[idx])*/
		}
	}
}

#[derive(Clone, Debug)]
struct DispatchableHandle {
	name:     Option<String>,
	commands: Vec<Command>
}

#[derive(Clone, Debug)]
struct Command {
	name:    String,
	comment: Option<String>,
	feature: Option<String>,
	result:  Rc<RefCell<Type>>,
	params:  Vec<Param>
}

#[derive(Clone, Debug)]
struct Param {
	name:     String,
	comment:  Option<String>,
	ty:       Rc<RefCell<Type>>,
	len:      Option<String>,
	optional: Option<String>
}

#[derive(Clone, Debug)]
enum Item {
	Comment(String),
	Type(Rc<RefCell<Type>>),
	Macro { name: String, content: String }
}

/// generates rust source code from the given `vec` and writes it to `writer`
#[allow(clippy::cognitive_complexity)]
pub fn gen(
	writer:   &mut impl io::Write,
	elements: impl IntoIterator<Item = RegistryElement>,
	api:      Api,
	mangle:   bool
) -> io::Result<()> {
	use std::fmt::Write;
	
	let [beginning, api_name, api_name_upper, api_name_lower, null_handle, load_func, api_suffix_lower, api_suffix] = match api {
		Api::Vulkan => VK_STRINGS,
		Api::OpenXr => XR_STRINGS
	};
	
	// methods that should be skipped, because they are added manually
	let skip_methods = SKIPPED_METHODS.iter()
		.copied()
		.collect::<HashSet<&str>>();
	
	let mut types = PRIMITIVE_TYPES.iter()
		.cloned()
		.map(|ty| (ty.to_string(), Rc::new(RefCell::new(Type::Primitive(ty.to_string())))))
		.collect::<HashMap<_, _>>();
	
	let mut tables         = HashMap::new();
	let mut static_uninit  = String::new();
	let mut handles        = HashMap::new();
	
	handles.insert(None, DispatchableHandle { name: None, commands: Vec::new() });
	tables.extend(match api {
		Api::Vulkan => vec![
			(None, (String::new(), String::new(), load_func.trim_start_matches("pfn_"), "")),
			(Some("VkInstance"), (String::new(), String::new(), "vkGetInstanceProcAddr", "")),
			(Some("VkDevice"),   (String::new(), String::new(), "vkGetDeviceProcAddr", "VkInstance")),
		],
		Api::OpenXr => vec![
			(None, (String::new(), String::new(), load_func.trim_start_matches("pfn_"), "")),
			(Some("XrInstance"), (String::new(), String::new(), "xrGetInstanceProcAddr", "")),
		],
	}.into_iter());
	
	// allocate some buffers for command generation
	let mut proto  = String::new();
	let mut header = String::new();
	let mut call   = String::new();
	let mut uninit = String::new();
	let mut uninit_call = String::new();
	
	let items = elements.into_iter().filter_map(|e| Some(match e {
		RegistryElement::Type(ty)                => {
			let (name, ty) = match ty {
				KhrType::Alias(KhrTypeAlias { category, name, alias, body, .. }) => {
					// collect all handles
					if category == KhrTypeAliasCategory::Handle {
						handles.insert(Some(name.clone()), DispatchableHandle {
							name:     Some(name.clone()),
							commands: Vec::new()
						});
					}
					
					(name.clone(), Type::Alias(Rc::new(Alias {
						name,
						comment: None,
						r#type:  resolve_type(&convert_c_type(alias.as_ref(), &body), &mut types),
					})))
				},
				KhrType::Struct(KhrTypeStruct { category, name, members, comment }) => (name.clone(), Type::Struct(Rc::new(Struct {
					name,
					comment,
					category,
					members: members.into_iter().filter_map(|m| match m {
						KhrTypeStructMemberVariant::Comment(_) => None,
						KhrTypeStructMemberVariant::Member(KhrTypeStructMember { r#type, name, optional, len, comment, }) => Some(StructMember {
							name,
							comment,
							r#type: resolve_type(&r#type, &mut types),
							optional,
							len
						})
					}).collect()
				}))),
				KhrType::FuncPtr(KhrTypeFuncPtr { name, comment, result, params }) => (name.clone(), Type::Alias(Rc::new(Alias {
					name,
					comment,
					r#type: Rc::new(RefCell::new(Type::FnPtr(Rc::new(FnPtr {
						params: params.into_iter().map(|param| Param {
							name:     param.name,
							comment:  param.comment,
							ty:       resolve_type(&param.r#type, &mut types),
							len:      param.len,
							optional: param.optional
						}).collect(),
						result: vec![resolve_type(&result, &mut types)]
					}))))
				}))),
				_ => return None
			};
			
			Item::Type(insert_type(name, ty, &mut types))
		}
		RegistryElement::Enums(enums)            => {
			Item::Type(insert_type(enums.name.clone(), Type::Enums(Rc::new(enums)), &mut types))
		}
		RegistryElement::Command(cmd)            => return match cmd {
			KhrCommand::Command { proto, param, comment, feature } if !skip_methods.contains(proto.name.as_str()) => {
				match handles.get_mut(&param.first().map(|p| p.r#type.clone())) {
					Some(v) => v,
					None => handles.get_mut(&None).unwrap()
				}.commands.push(Command {
					name: proto.name,
					comment,
					feature,
					result: resolve_type(&proto.r#type, &mut types),
					params: param.into_iter().map(|KhrCommandParam { r#type, name, optional, len, comment, }| Param {
						name,
						comment,
						ty: resolve_type(&r#type, &mut types),
						len,
						optional
					}).collect()
				});
				
				None
			},
			KhrCommand::Alias { name, ..} if !skip_methods.contains(name.as_str()) => {
				// TODO
				None
			},
			_ => None
		},
		RegistryElement::Macro { name, content } => Item::Macro { name, content },
		RegistryElement::Comment(comment)        => Item::Comment(comment)
	})).collect::<Vec<_>>();
	
	writer.write_all(beginning.as_bytes())?;
	
	for item in items {
		match item {
			Item::Comment(comment) => for l in comment.lines() { writeln!(writer, "// {}", l.trim())?; },
			Item::Macro { name, content } => writeln!(writer, "#[inline] pub const fn {}{}", name, content)?,
			Item::Type(ty) => match &*ty.borrow() {
				Type::Alias(alias) => {
					let Alias { name, comment, r#type } = &**alias;
					let ty = r#type.borrow();
					
					writeln!(writer)?;
					write_comment(comment, writer)?;
					writeln!(writer, "pub type {0:<45}{2} = {1:+};", name, ty, if ty.has_lifetime() { "<'a>" } else { "" })?;
				},
				Type::Struct(r#struct) => {
					let derive_debug   = r#struct.is_debug();
					let derive_default = r#struct.is_default();
					let derive_clone   = r#struct.is_copy();
					let lifetime       = r#struct.has_lifetime();
					let a_lifetime     = if lifetime { "<'_>" } else { "" };
					
					let Struct { name, comment, category, members } = &**r#struct;
					
					writeln!(writer)?;
					write_comment(comment, writer)?;
					
					let derives = [
						derive_clone.then(|| "Copy, Clone"),
						derive_debug.then(|| "Debug"),
						derive_default.then(|| "Default")
					].iter()
						.copied()
						.flatten()
						.fold(String::new(), |mut buf, s| { buf.push_str(s); buf.push_str(", "); buf });
					
					writeln!(writer, "#[repr(C)]\n#[derive({})]\npub {} {}{} {{", derives.trim_end_matches(", "), category, name, if lifetime { "<'a>" } else { "" })?;
					
					for member in members {
						let StructMember { name, comment, r#type, optional, len } = member;
						
						if let Some(comment) = comment {
							writeln!(writer, "\t/// {}", comment)?;
						}
						
						match (&*r#type.borrow(), optional.as_deref() == Some("true"), len) {
							(ty @ Type::Ref(_), _, Some(_)) => writeln!(writer, "\tpub {:<45}: {:+#},", name, ty),
							(ty @ Type::Mut(_), _, Some(_)) => writeln!(writer, "\tpub {:<45}: {:+#},", name, ty),
							(ty @ Type::Ref(_), true, _) | (ty @ Type::Mut(_), true, _) =>
								writeln!(writer, "\tpub {:<45}: Option<{:+}>,", name, ty),
							(ty, ..) if  *category == KhrTypeStructCategory::Union && !ty.is_copy() =>
								writeln!(writer, "\tpub {:<45}: std::mem::ManuallyDrop<{:+}>,", name, ty),
							(ty, ..) =>
								writeln!(writer, "\tpub {:<45}: {:+},", name, ty)
						}?;
					}
					
					writeln!(writer, "}}\n\nunsafe impl Send for {0}{1} {{}}\nunsafe impl Sync for {0}{1} {{}}\n",
							 name, a_lifetime)?;
					
					if !derive_debug {
						writeln!(writer, "impl fmt::Debug for {0}{1} {{\n\tfn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {{\n\t\tf.debug_struct(\"{0}\").finish()\n\t}}\n}}\n",
								 name, a_lifetime)?;
					}
					
					if !derive_default {
						write!(writer, "impl Default for {0}{1} {{ \n\tfn default() -> Self {{\n\t\tunsafe {{ MaybeUninit::zeroed().assume_init() }}\n\t}}\n}}\n",
							   name, a_lifetime)?;
					}
				}
				Type::Enums(enums) => {
					let KhrEnums { name, r#type, comment, enums } = &**enums;
					
					writeln!(writer)?;
					write_comment(comment, writer)?;
					
					let enum_name = &name;
					match r#type {
						KhrEnumsType::Enum | KhrEnumsType::Bitmask => {
							writeln!(writer, "pub use self::{0}::*;\n\n#[repr(C)]\n#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]\npub enum {0} {{", name)?;
							
							let default = enums.iter().find_map(|e| match e {
								KhrEnumsVariant::Enum { name, .. } => Some(name.as_str()),
								_ => None
							}).unwrap_or_else(|| {
								writeln!(writer, "\t__Default__ = 0,").unwrap();
								"__Default__"
							});
							
							for e in enums {
								match e {
									KhrEnumsVariant::Comment(comment) => write_comment_pre("\t //", Some(comment), writer)?,
									KhrEnumsVariant::Enum { name, value, comment, .. } => {
										if let KhrEnumValue::Alias(_) = value { continue; }
										write_comment_pre("\t ///", comment.as_ref().map(String::as_str), writer)?;
										writeln!(writer, "\t{:<80} = {},", name, value)?;
									},
									_ => ()
								}
							}
							
							writeln!(writer, "}}\n\nimpl Default for {} {{\n\tfn default() -> Self {{\n\t\tSelf::{}\n\t}}\n}}",
									 name, default)?;
							
							let mut first = true;
							for e in enums {
								if let KhrEnumsVariant::Enum { name, value: KhrEnumValue::Alias(alias), comment, .. } = e {
									if first { writeln!(writer)?; first = false; }
									write_comment_pre("///", comment.as_ref().map(String::as_str), writer)?;
									writeln!(writer, "pub const {:<80}: {} = {};", name, enum_name, alias)?;
								}
							}
						}
						_ => {
							writeln!(writer)?;
							
							for e in enums {
								match e {
									KhrEnumsVariant::Comment(comment) => write_comment_pre("\t //", Some(comment), writer)?,
									KhrEnumsVariant::Enum { name, value, comment, .. } => {
										write_comment(comment, writer)?;
										let __tmp__;
										writeln!(writer, "pub const {:<50}: {} = {};", name, if name == "VK_QUEUE_FAMILY_EXTERNAL_KHR" || name == "VK_SHADER_UNUSED_NV" {
											"u32"
										} else {
											__tmp__ = value.to_string();
											parse_type(&__tmp__)
										}, value)?;
									},
									_ => ()
								}
							}
						}
					}
				}
				_ => println!("ERROR: invalid type declaration")
			}
		}
	}
	
	for handle in handles.values() {
		let DispatchableHandle { name: handle, commands } = handle;
		let handle = handle.as_deref();
		
		let table_handle = match (api, handle) {
			(_, None)                                 => None,
			(Api::Vulkan, Some("VkInstance"))
			| (Api::Vulkan, Some("VkPhysicalDevice")) => Some("VkInstance"),
			(Api::Vulkan, Some(_))                    => Some("VkDevice"),
			(Api::OpenXr, Some(_))                    => Some("XrInstance")
		};
		
		let (table, init, load_fn, _) = tables.get_mut(&table_handle).expect("failed to find table");
		
		let table_handle = table_handle.unwrap_or("LibVulkan");
		
		match handle {
			Some(handle @ "VkInstance") | Some(handle @ "XrInstance") => {
				writeKhrImpl!(@impl writer, handle, table_handle)?;
				writeln!(writer, "\tpub fn new(handle: {0}) -> Self {{\n\t\tSelf {{ handle, table: {0}Table::load(handle) }}\n\t}}", handle)
			}
			Some(handle @ "VkDevice") => {
				writeKhrImpl!(@impl writer, handle, table_handle)?;
				writeln!(writer, "\tpub fn new(handle: {0}, parent: &VkInstanceImpl) -> Self {{\n\t\tSelf {{ handle, table: {0}Table::load(handle, &parent.table) }}\n\t}}", handle)
			}
			Some(handle) => {
				writeKhrImpl!(@impl writer, handle, table_handle)?;
				writeln!(writer, "\tpub fn new(handle: {0}, parent: &{1}Impl) -> Self {{\n\t\tSelf {{ handle, table: parent.table.clone() }}\n\t}}", handle, table_handle)
			},
			None    => writeln!(writer, "impl {}InstanceImpl {{", api_suffix)
		}?;
		
		for Command { name, comment, feature, result, params } in commands {
			if skip_methods.contains(&name.as_str()) { continue; }
			
			// mangle
			let mut mangled_name = name.clone();
			if mangle {
				mangled_name = name.replace(handle.unwrap_or("Instance")
					.trim_start_matches("Vk").trim_start_matches("Xr"), "");
				
				// remove `vk`/`xr`
				mangled_name.remove(0);
				mangled_name.remove(0);
				
				// fist char to lower case
				let ch = mangled_name.remove(0).to_ascii_lowercase();
				mangled_name.insert(0, ch);
			}
			
			let params = &params[if handle.is_none() { 0 } else { 1 }..];
			let result = result.borrow();
			
			// get array lengths
			let lens = params.iter()
				.filter_map(|param| param.len.as_deref()
					.filter(|_| matches!(&*param.ty.borrow(), Type::Ref(_) | Type::Mut(_)))
					.map(|len| (len, param)))
				.collect::<HashMap<_, _>>();
			
			// clear buffers
			proto.clear();  // table struct entry
			header.clear(); // rust function declaration
			call.clear();   // ffi call
			uninit.clear();
			uninit_call.clear();
			
			// init buffers
			if let Some(handle) = &handle {
				proto.push_str(handle);
				proto.push_str(", ");
				header.push_str("\n\t\t&self,");
				call.push_str("self.handle, ");
			}
			
			// write buffers
			for Param { name, ty, comment, len, optional, .. } in params {
				let ty = ty.borrow();
				let mut name     = &*name;
				let ffi_name     = name;
				let mut optional = optional.as_deref() == Some("true");
				let has_len      = len.as_ref().map_or(false, |len| lens.contains_key(&&**len));
				let len          = lens.contains_key(&&**name) && matches!(&*ty, Type::Primitive(_));
				
				if len {
					optional = lens.get(&&**name).unwrap().optional.as_deref() == Some("true");
					name = &lens.get(&&**name).unwrap().name;
				}
				
				match &*ty {
					Type::Ref(ty) | Type::Mut(ty) => if let Type::Primitive(v) = &*ty.borrow() {
						if v == "()" && !has_len { call.push('('); }
					} else {},
					_ => ()
				}
				
				call.push_str(name);
				call.push_str(match (&*ty, has_len, len, optional) {
					(_, _, true, true)                 => ".as_ref().map_or(0, |v| v.len() as _), ",
					(_, _, true, false)                => ".len() as _, ",
					(Type::Ref(_), false, false, true) => ".map_or(std::ptr::null(), |v| v as _) as _, ",
					(Type::Mut(_), false, false, true) => ".map_or(std::ptr::null_mut(), |v| v as _) as _, ",
					(Type::Ref(_), true, false, true)  => ".map_or(std::ptr::null(), <[_]>::as_ptr) as _, ",
					(Type::Mut(_), true, false, true)  => ".map_or(std::ptr::null_mut(), <[_]>::as_mut_ptr) as _, ",
					(Type::Ref(_), true, false, false) => ".as_ptr() as _, ",
					(Type::Mut(_), true, false, false) => ".as_mut_ptr() as _, ",
					_                                  => ", "
				});
				
				if !has_len && !optional {
					match &*ty {
						Type::Ref(ty2) | Type::Mut(ty2) => if let Type::Primitive(v) = &*ty2.borrow() {
							if v == "()" {
								if call.ends_with(", ") { call.truncate(call.len() - 2); }
								call.push_str(").into(), ");
							}
						} else {
							if call.ends_with(", ") { call.truncate(call.len() - 2); }
							match &*ty {
								Type::Ref(_) => call.push_str(" as *const _ as _, "),
								Type::Mut(_) => call.push_str(" as *mut _ as _, "),
								_ => unreachable!()
							}
						},
						_ => ()
					}
				}
				
				write!(proto, "{:#}, ", ty).unwrap();
				
				if handle.is_none() {
					write!(uninit, "{}: {:#}, ", ffi_name, ty).unwrap();
					write!(uninit_call, "{}, ", ffi_name).unwrap();
				}
				
				if len {
					continue;
				}
				
				if let Some(comment) = comment {
					write!(header, " // {}", comment).unwrap();
				}
				
				let ty_str = match (&*ty, optional, has_len) {
					(Type::Ref(ty), false, false) => format!("&{}",               ty.borrow()),
					(Type::Mut(ty), false, false) => format!("&mut {}",           ty.borrow()),
					(Type::Ref(ty), false, true)  => format!("&[{}]",             ty.borrow()),
					(Type::Mut(ty), false, true)  => format!("&mut [{}]",         ty.borrow()),
					(Type::Ref(ty), true, false)  => format!("Option<&{}>",       ty.borrow()),
					(Type::Mut(ty), true, false)  => format!("Option<&mut {}>",   ty.borrow()),
					(Type::Ref(ty), true, true)   => format!("Option<&[{}]>",     ty.borrow()),
					(Type::Mut(ty), true, true)   => format!("Option<&mut [{}]>", ty.borrow()),
					//(Type::Ref(ty), _, true) => "*const u8".to_string(),
					//(Type::Mut(ty), _, true) => "*mut u8".to_string(),
					(ty, ..)                      => format!("{}", ty)
				};
				
				write!(header, "\n\t\t{:<45}: {},", name, ty_str).unwrap();
			};
			
			// remove tailing colon
			let proto  = proto.trim_end_matches(", ");
			let header = header.trim_end_matches(',');
			let call   = call.trim_end_matches(", ");
			
			// write comments
			if let Some(comment) = comment {
				for l in comment.lines() {
					write!(writer, "\n\t/// {}", l).unwrap();
				}
			}
			
			// write feature
			if let Some(feature) = feature {
				write!(writer, "\n\t#[cfg(feature = \"{}\")]", feature).unwrap();
				write!(table, "\n\t#[cfg(feature = \"{}\")]", feature).unwrap();
				if handle.is_some() {
					write!(init, "\n\t\t\t#[cfg(feature = \"{}\")]", feature).unwrap();
				} else {
					write!(init, "\n\t\t#[cfg(feature = \"{}\")]\n\t\t{{ ", feature).unwrap();
					write!(static_uninit, "\n\t#[cfg(feature = \"{}\")]", feature).unwrap();
				}
			}
			
			// write command, table entry and loading code
			write!(table, "\n\tpfn_{:<40}: extern \"C\" fn({}) -> {},",
				   name, proto, result).unwrap();
			if handle.is_some() {
				write!(writer, "\n\tpub fn {}({}\n\t) -> {} {{\n\t\t(self.table.pfn_{})({})\n\t}}\n",
					   mangled_name, &header, result, name, &call).unwrap();
				
				write!(init, "\n\t\t\tpfn_{:<60}: transmute((table.pfn_{})(handle, b\"{0}\\0\".as_ptr())),",
					   name, load_fn).unwrap();
			} else {
				write!(writer, "\n\tpub fn {}({}\n\t) -> {} {{\n\t\tunsafe {{ (LIB_{}.pfn_{})({}) }}\n\t}}\n",
					   mangled_name, &header, result, api_name_upper, name, &call).unwrap();
				
				write!(static_uninit, "\n\tpfn_{0:<40}: {{ extern \"C\" fn load({2}) -> {1} {{ unsafe {{ LIB_{4}.load(); (LIB_{4}.pfn_{0})({3}) }} }} load }},",
					   name, result, uninit, uninit_call, api_name_upper).unwrap();
				write!(init, "\n\t\t\tself.pfn_{:<40}= transmute((self.{})({}, b\"{0}\\0\".as_ptr()));",
					   name, load_func, null_handle).unwrap();
				
				if feature.is_some() {
					init.push_str("\n\t\t}");
				}
			}
		}
		
		writeln!(writer, "}}")?;
	}
	
	if let Some((inst_table, inst_init, ..)) = tables.get_mut(&Some("VkInstance")) {
		inst_table.push_str("\n\tpfn_vkGetDeviceProcAddr                                    : extern fn(VkDevice, *const u8) -> PFN_vkVoidFunction,");
		inst_init.push_str("\n\t\t\tpfn_vkGetDeviceProcAddr                                         : transmute((LIB_VULKAN.pfn_vkGetInstanceProcAddr)(handle, b\"vkGetDeviceProcAddr\\0\".as_ptr())),");
	}
	
	for (name, (table, init, _, parent)) in tables {
		match name {
			None => write!(writer, r#"
static mut LIB_{5}: Lib{0} = Lib{0} {{
	lib: None,
	pfn_{3}GetInstanceProcAddr: {{ extern fn load(handle: {7}Instance, name: *const u8) -> PFN_{3}VoidFunction {{ unsafe {{ LIB_{5}.load(); (LIB_{5}.pfn_{3}GetInstanceProcAddr)(handle, name) }} }} load }},{6}
}};

pub struct Lib{0} {{
	#[allow(dead_code)]
	lib: Option<libloading::Library>,
	pfn_{3}GetInstanceProcAddr: extern fn({7}Instance, *const u8) -> PFN_{3}VoidFunction,{1}
}}

impl Lib{0} {{
	unsafe fn load(&mut self) {{
		let lib                                          = libloading::Library::new(LIB).expect("failed to load `lib{4}`");
		self.pfn_{3}GetInstanceProcAddr                   = *lib.get(b"{3}GetInstanceProcAddr\0").expect("failed to load `{3}GetInstanceProcAddr`");
		self.lib                                         = Some(lib);
		{2}
	}}
}}

"#, api_name, table, init, api_suffix_lower, api_name_lower, api_name_upper, static_uninit, api_suffix),
			Some(name) if name.ends_with("Instance") => write!(writer, r#"
pub struct {0}Table {{
	{1}
}}

impl {0}Table {{
	fn load(handle: {0}) -> Arc<Self> {{
		let table = unsafe {{ &LIB_{3} }};
		Arc::new(unsafe {{ Self {{
			{2}
		}} }})
	}}
}}

impl fmt::Debug for {0}Table {{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {{
		f.debug_struct("{0}Table").finish()
	}}
}}

"#, name, table, init, api_name_upper),
			Some(name) => writeKhrImpl!(@table writer, name, table, init, parent)
		}?;
	}
	
	Ok(())
}

pub fn gen_cargo(
	writer:   &mut impl io::Write,
	elements: impl IntoIterator<Item = (String, Vec<String>)>
) -> io::Result<()> {
	let mut extensions = elements.into_iter()
		.collect::<Vec<_>>();
	
	writeln!(writer, "[dependencies]\nlog = \"0.*\"\nlibloading = \"0.*\"\n\n[features]\ndefault = [{}]", extensions.iter()
		.filter(|(name, _)| name.starts_with("VK_VERSION") | name.starts_with("XR_VERSION"))
		.map(|(name, _)| format!("\"{}\", ", name))
		.collect::<String>()
		.trim_start_matches(", "))?;
	
	extensions.sort_by_key(|v| v.0.clone());
	
	for (name, requires) in extensions {
		use std::fmt::Write;
		let mut dependencies = String::new();
		for s in requires { write!(&mut dependencies, "\"{}\", ", s).unwrap(); }
		writeln!(writer, "{:<50}= [{}]", name, dependencies.trim_end_matches(", "))?;
	}
	
	Ok(())
}

fn resolve_type(s: &str, types: &mut HashMap<String, Rc<RefCell<Type>>>) -> Rc<RefCell<Type>> {
	Type::from_str(s, &mut |s| types
		.entry(s.to_string())
		.or_insert_with(|| Rc::new(RefCell::new(Type::Undefined(s.to_string()))))
		.clone())
}

fn insert_type(name: String, ty: Type, types: &mut HashMap<String, Rc<RefCell<Type>>>) -> Rc<RefCell<Type>> {
	match types.entry(name) {
		Entry::Vacant(e) => { e.insert(Rc::new(RefCell::new(ty))).clone() },
		Entry::Occupied(e) => {
			*e.get().borrow_mut() = ty;
			e.get().clone()
		}
	}
}

fn write_comment(comment: &Option<impl Deref<Target = str>>, writer: &mut impl io::Write) -> io::Result<()> {
	write_comment_pre("///", comment.as_deref(), writer)
}

fn write_comment_pre(pre: &str, comment: Option<&str>, writer: &mut impl io::Write) -> io::Result<()> {
	if let Some(comment) = comment {
		for l in comment.lines() {
			writeln!(writer, "{} {}", pre, l.trim())?;
		}
	}
	Ok(())
}

static VK_STRINGS: [&str; 8] = [VK_BEGINNING, "Vulkan", "VULKAN", "vulkan", "VK_NULL_HANDLE", "pfn_vkGetInstanceProcAddr", "vk", "Vk"];
static XR_STRINGS: [&str; 8] = [XR_BEGINNING, "OpenXr", "OPENXR", "openxr", "XR_NULL_HANDLE", "pfn_xrGetInstanceProcAddr", "xr", "Xr"];

static SKIPPED_METHODS: &[&str] = &[
	"vkGetInstanceProcAddr",
	"vkGetDeviceProcAddr",
	"xrGetInstanceProcAddr"
];

static PRIMITIVE_TYPES: &[&str] = &[
	"()",
	"usize",
	"isize",
	"u8",
	"i8",
	"u16",
	"i16",
	"u32",
	"i32",
	"u64",
	"i64",
	"u128",
	"i128",
	"f32",
	"f64",
	"char"
];

static VK_BEGINNING: &str = r#"//! MACHINE GENERATED FILE, DO NOT EDIT
#![feature(try_trait, const_fn, const_fn_transmute, stmt_expr_attributes)]
#![warn(clippy::all)]
#![allow(
	non_snake_case,
	non_upper_case_globals,
	non_camel_case_types,
	unused_attributes,
	unused_parens,
	invalid_value,
	clippy::unused_unit,
	clippy::too_many_arguments,
	clippy::enum_clike_unportable_variant,
	clippy::unnecessary_cast,
	clippy::missing_safety_doc,
	clippy::from_over_into,
	clippy::upper_case_acronyms
)]

use {
	core::{mem::{transmute, MaybeUninit}, ops, fmt},
	std::sync::Arc,
	self::external::*
};

pub use types::*;

/// Contains some useful types
mod types {
	#[derive(Copy, Clone, Debug)]
	#[repr(transparent)]
	pub struct VkAnyRef<'b>(&'b ());
	
	impl<'a> VkAnyRef<'a> {
		#[allow(clippy::transmute_ptr_to_ptr)]
		pub const fn new<T>(v: &'a T) -> Self {
			Self(unsafe { std::mem::transmute(v) })
		}
	}
	
	impl<'a, T> From<&'a T> for VkAnyRef<'a> {
		fn from(v: &'a T) -> Self {
			unsafe { Self((v as *const T as *const ()).as_ref().unwrap()) }
		}
	}
	
	impl<'a, T> Into<*const T> for VkAnyRef<'a> {
		fn into(self) -> *const T {
			self.0 as *const () as *const T
		}
	}
	
	#[derive(Debug)]
	#[repr(transparent)]
	pub struct VkAnyMut<'b>(&'b mut ());
	
	impl<'a> VkAnyMut<'a> {
		pub fn new<T>(v: &'a mut T) -> Self {
			Self(unsafe { &mut*(v as *mut T as *mut ()) })
		}
	}
	
	impl<'a, T> From<&'a mut T> for VkAnyMut<'a> {
		fn from(v: &'a mut T) -> Self {
			unsafe { Self((v as *mut T as *mut ()).as_mut().unwrap()) }
		}
	}
	
	impl<'a, T> Into<*mut T> for VkAnyMut<'a> {
		fn into(self) -> *mut T {
			self.0 as *mut () as *mut T
		}
	}
}

pub mod external {
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum wl_display {}
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum wl_surface {}
	
	impl wl_display {
		pub unsafe fn from_ptr<'a, T>(ptr: *mut T) -> Option<&'a mut Self> {
			(ptr as *mut Self).as_mut()
		}
	}
	
	impl wl_surface {
		pub unsafe fn from_ptr<'a, T>(ptr: *mut T) -> Option<&'a mut Self> {
			(ptr as *mut Self).as_mut()
		}
	}
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum xcb_connection_t {}
	pub type xcb_window_t   = u32;
	pub type xcb_visualid_t = u32;
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum Display {}
	pub type VisualID = u32;
	pub type Window   = u32;
	pub type RROutput = u32;
	
	pub type GgpStreamDescriptor = usize;
	pub type GgpFrameToken       = usize;
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum ANativeWindow {}
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum AHardwareBuffer {}
	
	pub type zx_handle_t = usize;
	
	pub type HINSTANCE = Option<std::ptr::NonNull<()>>;
	pub type HWND      = Option<std::ptr::NonNull<()>>;
	pub type HMONITOR  = Option<std::ptr::NonNull<()>>;
	pub type HANDLE    = Option<std::ptr::NonNull<()>>;
	pub type DWORD     = u32;
	pub type LPCWSTR   = Option<std::ptr::NonNull<u16>>;
	
	#[derive(Debug, Copy, Clone, Eq, PartialEq)]
	pub struct SECURITY_ATTRIBUTES {
		pub nLength:              DWORD,
		pub lpSecurityDescriptor: Option<std::ptr::NonNull<()>>,
		pub bInheritHandle:       i32
	}
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum CAMetalLayer {}
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum IDirectFB {}

	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum IDirectFBSurface {}
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum _screen_context {}
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum _screen_window {}
}

pub const VK_NULL_HANDLE        : u64 = 0;
/// Vulkan 1.0 version number
pub const VK_API_VERSION_1_0    : u32 = VK_MAKE_VERSION(1, 0, 0);
/// Vulkan 1.1 version number
pub const VK_API_VERSION_1_1    : u32 = VK_MAKE_VERSION(1, 1, 0);
/// Vulkan 1.1 version number
pub const VK_API_VERSION_1_2    : u32 = VK_MAKE_VERSION(1, 2, 0);

#[cfg(unix)]
const LIB: &str = "libvulkan.so.1";
#[cfg(windows)]
const LIB: &str = "vulkan-1.dll";

impl ops::Try for VkResult {
	type Ok = VkResult;
	type Error = VkResult;
	
	fn into_result(self) -> Result<Self::Ok, Self::Error> {
		if self as i32 >= 0 {
			Ok(self)
		} else {
			Err(self)
		}
	}
	
	fn from_error(v: Self::Error) -> Self {
		v
	}
	
	fn from_ok(v: Self::Ok) -> Self {
		v
	}
}

impl std::error::Error for VkResult {}

impl fmt::Display for VkResult {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(self, f)
	}
}
"#;

static XR_BEGINNING: &str = r#"//! MACHINE GENERATED FILE, DO NOT EDIT
#![feature(try_trait, const_fn, const_fn_transmute, stmt_expr_attributes)]
#![warn(clippy::all)]
#![allow(
	non_snake_case,
	non_upper_case_globals,
	non_camel_case_types,
	unused_attributes,
	unused_parens,
	invalid_value,
	clippy::unused_unit,
	clippy::too_many_arguments,
	clippy::enum_clike_unportable_variant,
	clippy::unnecessary_cast,
	clippy::missing_safety_doc,
	clippy::from_over_into,
	clippy::upper_case_acronyms
)]

use {
	core::{mem::{transmute, MaybeUninit}, ops, fmt},
	std::sync::Arc,
	self::external::*,
	vk::*,
};
pub use types::*;

/// Contains some useful types
mod types {
	#[derive(Copy, Clone, Debug)]
	#[repr(transparent)]
	pub struct XrAnyRef<'b>(&'b ());
	
	impl<'a> XrAnyRef<'a> {
		#[allow(clippy::transmute_ptr_to_ptr)]
		pub const fn new<T>(v: &'a T) -> Self {
			Self(unsafe { std::mem::transmute(v) })
		}
	}
	
	impl<'a, T> From<&'a T> for XrAnyRef<'a> {
		fn from(v: &'a T) -> Self {
			unsafe { Self((v as *const T as *const ()).as_ref().unwrap()) }
		}
	}
	
	impl<'a, T> Into<*const T> for XrAnyRef<'a> {
		fn into(self) -> *const T {
			self.0 as *const () as *const T
		}
	}
	
	#[derive(Debug)]
	#[repr(transparent)]
	pub struct XrAnyMut<'b>(&'b mut ());
	
	impl<'a> XrAnyMut<'a> {
		pub fn new<T>(v: &'a mut T) -> Self {
			Self(unsafe { &mut*(v as *mut T as *mut ()) })
		}
	}
	
	impl<'a, T> From<&'a mut T> for XrAnyMut<'a> {
		fn from(v: &'a mut T) -> Self {
			unsafe { Self((v as *mut T as *mut ()).as_mut().unwrap()) }
		}
	}
	
	impl<'a, T> Into<*mut T> for XrAnyMut<'a> {
		fn into(self) -> *mut T {
			self.0 as *mut () as *mut T
		}
	}
}

pub mod external {
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum wl_display {}
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum xcb_connection_t {}
	pub type xcb_window_t       = u32;
	pub type xcb_visualid_t     = u32;
	pub type xcb_glx_fbconfig_t = u32;
	pub type xcb_glx_drawable_t = u32;
	pub type xcb_glx_context_t  = u32;
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum Display {}
	pub type GLXFBConfig = u64;
	pub type GLXDrawable = u64;
	pub type GLXContext  = u64;
	
	pub type EGLDisplay = u64;
	pub type EGLConfig  = u64;
	pub type EGLContext = u64;
	pub type PFNEGLGETPROCADDRESSPROC = extern fn();
	
	pub type HDC   = *mut ();
	pub type HGLRC = *mut ();
	pub type LUID  = u64;
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum ID3D11Device {}
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum ID3D11Texture2D {}
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum ID3D12Device {}
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum ID3D12CommandQueue {}
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum ID3D12Resource {}
	#[repr(C)]
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum D3D_FEATURE_LEVEL { Variant }
	
	#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
	pub enum IUnknown {}

	/// This typedef only exists in vulkan.h, but not in the Vulkan registry, so it is defined here.
	pub type PFN_vkGetInstanceProcAddr = extern fn(vk::VkInstance, *const u8) -> vk::PFN_vkVoidFunction;
}

pub const XR_NULL_HANDLE          : u64        = 0;
pub const XR_CURRENT_API_VERSION  : XrVersion  = XR_MAKE_VERSION(1, 0, 5);
pub const XR_NULL_PATH            : XrPath     = 0;
pub const XR_NO_DURATION          : XrDuration = 0;
pub const XR_INFINITE_DURATION    : XrDuration = 0x7fffffffffffffff;
pub const XR_MIN_HAPTIC_DURATION  : XrDuration = -1;
pub const XR_FREQUENCY_UNSPECIFIED: i32        = 0;
pub const XR_MAX_EVENT_DATA_SIZE  : usize      = std::mem::size_of::<XrEventDataBuffer>();

#[cfg(unix)]
const LIB: &str = "libopenxr.so.1";
#[cfg(windows)]
const LIB: &str = "openxr-1.dll";

impl ops::Try for XrResult {
	type Ok = XrResult;
	type Error = XrResult;
	
	fn into_result(self) -> Result<Self::Ok, Self::Error> {
		if self as i32 >= 0 {
			Ok(self)
		} else {
			Err(self)
		}
	}
	
	fn from_error(v: Self::Error) -> Self {
		v
	}
	
	fn from_ok(v: Self::Ok) -> Self {
		v
	}
}

impl std::error::Error for XrResult {}

impl fmt::Display for XrResult {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(self, f)
	}
}

"#;