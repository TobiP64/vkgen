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

use {serde::{*, de::Error}, crate::xml::Body, self::RegistryElement::*, std::sync::atomic::AtomicBool};

pub static BITFIELDS: AtomicBool = AtomicBool::new(false);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Api {
	Vulkan,
	OpenXr
}

pub struct Registry {
	pub api:    Api,
	pub vec:    Vec<RegistryElement>,
	pub exts:   Vec<(String, Vec<String>)>,
	pub consts: Vec<KhrEnumsVariant>
}

#[derive(Debug)]
pub enum RegistryElement {
	Type(KhrType),
	Enums(KhrEnums),
	Command(KhrCommand),
	Macro { name: String, content: String },
	Comment(String)
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrRegistryVariant {
	Comment(Body<String>),
	Types(Vec<KhrTypesVariants>),
	Enums(#[serde(deserialize_with = "deserialize_enums")] KhrEnums),
	Commands(Vec<KhrCommandsVariant>),
	Feature(KhrFeature),
	Extensions(Vec<KhrExtensionsVariant>),
	#[serde(other)]
	Other
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrTypesVariants {
	Type(KhrType),
	Comment(CommentVariant)
}

#[derive(Debug)]
pub enum KhrType {
	Alias(KhrTypeAlias),
	Struct(KhrTypeStruct),
	FuncPtr(KhrTypeFuncPtr),
	Other
}

#[derive(Debug)]
pub struct KhrTypeAlias {
	pub category: KhrTypeAliasCategory,
	pub name:     String,
	pub alias:    String,
	pub body:     String,
	pub parent:   Option<String>
}

#[derive(Debug)]
pub struct KhrTypeStruct {
	pub category: KhrTypeStructCategory,
	pub name:     String,
	pub members:  Vec<KhrTypeStructMemberVariant>,
	pub comment:  Option<String>
}

#[derive(Debug)]
pub struct KhrTypeFuncPtr {
	pub name:    String,
	pub comment: Option<String>,
	pub result:  String,
	pub params:  Vec<KhrCommandParam>
}

#[derive(Debug, Deserialize, Eq, PartialEq, Copy, Clone)]
#[serde(rename_all = "lowercase")]
pub enum KhrTypeAliasCategory {
	Handle,
	Enum,
	Bitmask,
	Basetype,
	Struct
}

#[derive(Debug, Deserialize, Eq, PartialEq, Copy, Clone)]
#[serde(rename_all = "lowercase")]
pub enum KhrTypeStructCategory {
	Struct,
	Union
}

impl std::fmt::Display for KhrTypeStructCategory {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Struct => f.write_str("struct"),
			Self::Union => f.write_str("union")
		}
	}
}

#[derive(Debug)]
pub enum KhrTypeStructMemberVariant {
	Member(KhrTypeStructMember),
	Comment(String)
}

#[derive(Debug)]
pub struct KhrTypeStructMember {
	pub r#type:   String,
	pub name:     String,
	pub optional: Option<String>,
	pub len:      Option<String>,
	pub comment:  Option<String>
}

#[derive(Debug)]
pub struct KhrEnums {
	pub name:    String,
	pub r#type:  KhrEnumsType,
	pub comment: Option<String>,
	pub enums:   Vec<KhrEnumsVariant>
}

#[derive(Debug, Deserialize, Eq, PartialEq, Copy, Clone)]
#[serde(rename_all = "lowercase")]
pub enum KhrEnumsType {
	Enum,
	Bitmask,
	#[serde(other)]
	None
}

impl Default for KhrEnumsType {
	fn default() -> Self {
		Self::None
	}
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrEnumsVariant {
	Enum {
		name:    String,
		#[serde(flatten)]
		value:   KhrEnumValue,
		comment: Option<String>
	},
	Comment(String),
	#[serde(other)]
	Other
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrEnumValue {
	Value(#[serde(deserialize_with = "deserialize_value")] String),
	BitPos(String),
	Alias(String)
}

impl std::fmt::Display for KhrEnumValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Value(value) => f.write_str(value),
			Self::BitPos(bit)  => write!(f, "{:#x}", 1usize << bit.parse::<usize>().unwrap()),
			Self::Alias(alias) => f.write_str(alias)
		}
	}
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrCommandsVariant {
	Comment(String),
	Command(KhrCommand)
}

#[derive(Debug, Clone)]
pub enum KhrCommand {
	Alias {
		name:    String,
		alias:   String,
		comment: Option<String>,
		feature: Option<String>
	},
	Command {
		proto:   KhrCommandProto,
		param:   Vec<KhrCommandParam>,
		comment: Option<String>,
		feature: Option<String>
	}
}

#[derive(Debug, Default, Clone)]
pub struct KhrCommandProto {
	pub r#type: String,
	pub name:   String
}

#[derive(Debug, Default, Clone)]
pub struct KhrCommandParam {
	pub r#type:     String,
	pub name:       String,
	pub optional:   Option<String>,
	pub len:        Option<String>,
	pub comment:    Option<String>
}

#[derive(Debug)]
pub struct KhrFeature {
	pub api:     String,
	pub name:    String,
	pub number:  String,
	pub comment: Option<String>,
	pub require: Vec<Vec<KhrRequireVariant>>
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrExtensionsVariant {
	Comment(String),
	Extension(KhrExtension)
}

#[derive(Debug)]
pub struct KhrExtension {
	pub name:      String,
	pub number:    usize,
	pub r#type:    Option<String>,
	pub supported: String,
	pub requires:  Vec<String>,
	pub platform:  Option<String>,
	pub require:   Vec<Vec<KhrRequireVariant>>
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrRequireVariant {
	Enum {
		name:      String,
		#[serde(flatten)]
		value:     Option<KhrRequireEnumVal>,
		extends:   Option<String>,
		extnumber: Option<usize>,
		comment:   Option<String>,
		dir:       Option<String>
	},
	Type { name: String },
	Command { name: String },
	Comment(String),
	#[serde(other)]
	Other
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KhrRequireEnumVal {
	Value(#[serde(deserialize_with = "deserialize_value")] String),
	Offset(String),
	Bitpos(String),
	Alias(String)
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum CommentVariant {
	Attr(String),
	Body(Body<String>)
}

#[allow(clippy::from_over_into)]
impl Into<String> for CommentVariant {
	fn into(self) -> String {
		match self {
			Self::Attr(v) => v,
			Self::Body(v) => v.value
		}
	}
}

fn deserialize_value<'de, D: Deserializer<'de>>(de: D) -> Result<String, D::Error> {
	String::deserialize(de).map(|s| convert_c_value(&s))
}

fn deserialize_list<'de, D: Deserializer<'de>>(de: D) -> Result<Vec<String>, D::Error> {
	String::deserialize(de)
		.map(|s| s.split(',').map(String::from).collect())
}

impl<'de> Deserialize<'de> for Registry {
	#[allow(clippy::cognitive_complexity)]
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error>  {
		use KhrRequireEnumVal::*;
		
		let registry = Vec::<KhrRegistryVariant>::deserialize(deserializer)?;
		let mut api = None;
		let mut vec = Vec::new();
		let mut consts = Vec::new();
		let mut exts = Vec::new();
		
		for e in registry {
			match e {
				KhrRegistryVariant::Comment(v) => vec.push(RegistryElement::Comment(v.value)),
				KhrRegistryVariant::Types(types) => vec.extend(types.into_iter()
					.map(|r#type| match r#type {
						KhrTypesVariants::Comment(v) => RegistryElement::Comment(v.into()),
						KhrTypesVariants::Type(r#type) => RegistryElement::Type(r#type)
					})),
				KhrRegistryVariant::Enums(mut enums) => {
					if &enums.name == "API Constants" {
						for e in &mut enums.enums {
							if let KhrEnumsVariant::Enum { name, value: KhrEnumValue::Value(ref mut v), .. } = e {
								if ["VK_TRUE", "VK_FALSE"].contains(&name.as_str()) {
									api = Some(Api::Vulkan);
									v.push_str("u32");
								} else if ["XR_TRUE", "XR_FALSE"].contains(&name.as_str()) {
									api = Some(Api::OpenXr);
									v.push_str("u32");
								}
							}
						}
					}
					
					vec.push(Enums(enums))
				}
				KhrRegistryVariant::Commands(commands) => vec.extend(
					commands.iter().map(|e| match e {
						KhrCommandsVariant::Command(KhrCommand::Alias { name, alias, comment, feature }) => {
							let mut alias = alias;
							'outer: loop {
								for command in &commands {
									match command {
										KhrCommandsVariant::Command(KhrCommand::Command { proto, param, .. })
										if *alias == proto.name => break 'outer Command(KhrCommand::Command {
											proto: KhrCommandProto { r#type: proto.r#type.clone(), name: name.clone() },
											param: param.clone(),
											comment: comment.clone(),
											feature: feature.clone()
										}),
										KhrCommandsVariant::Command(KhrCommand::Alias { name: name_, alias: alias2, .. })
										if alias == name_ => {
											alias = alias2;
											continue 'outer;
										}
										_ => ()
									}
								}
								panic!("failed to find command alias `{}` for command `{}`", alias, name)
							}
						}
						KhrCommandsVariant::Command(v) => Command(v.clone()),
						KhrCommandsVariant::Comment(v) => Comment(v.clone())
					})),
				KhrRegistryVariant::Extensions(mut extensions) => {
					let extensions = extensions.iter_mut().filter_map(|e| match e {
						KhrExtensionsVariant::Extension(e) if e.supported != "disabled" => Some(e),
						_ => None
					}).collect::<Vec<_>>();
					
					extensions.iter()
						.flat_map(|ext| ext.require.iter())
						.flat_map(|req| req.iter())
						.for_each(|e| if let KhrRequireVariant::Enum { name, value, extends: Option::None, .. } = e {
							consts.push(KhrEnumsVariant::Enum {
								name:    name.clone(),
								value:   match value {
									Some(Alias(v))  => KhrEnumValue::Alias(v.clone()),
									Some(Value(v))  => KhrEnumValue::Value(v.clone()),
									Some(Bitpos(v)) => KhrEnumValue::BitPos(v.clone()),
									_ => return
								},
								comment: None
							})
						});
					
					vec.iter_mut().for_each(|e| match e {
						RegistryElement::Enums(ref mut enums) => extensions.iter()
							.flat_map(|ext| ext.require.iter().map(move |e| (ext, e)))
							.flat_map(|(ext, req)| req.iter().map(move |e| (ext, e)))
							.for_each(|(ext, e)| match e {
								KhrRequireVariant::Enum { name, value, dir, extends: Some(extends), extnumber, .. }
								if extends == &enums.name && !enums.enums.iter()
									.any(|v| if let KhrEnumsVariant::Enum { name: ref v, .. } = v { v == name } else { false })
								=> enums.enums.push(KhrEnumsVariant::Enum {
									name:  name.clone(),
									value: match value {
										Some(Alias(v))       => KhrEnumValue::Alias(v.clone()),
										Some(Value(v))       => KhrEnumValue::Value(v.clone()),
										Some(Bitpos(v))      => KhrEnumValue::BitPos(v.clone()),
										Some(Offset(offset)) => KhrEnumValue::Value(dir.clone().unwrap_or_default() + &(
											1_000_000_000 + (extnumber.unwrap_or(ext.number) - 1)
												* 1_000 + offset.parse::<usize>().unwrap()).to_string()),
										_ => return
									},
									comment: None
								}),
								_ => ()
							}),
						RegistryElement::Command(KhrCommand::Command { feature: ref mut feat, proto: KhrCommandProto { name, .. }, .. })
						| RegistryElement::Command(KhrCommand::Alias { feature: ref mut feat, name, .. }) => extensions.iter()
							.flat_map(|ext| ext.require.iter().map(move |e| (ext, e)))
							.flat_map(|(ext, req)| req.iter().map(move |e| (ext, e)))
							.for_each(|(ext, e)| match e {
								KhrRequireVariant::Command { name: feature_cmd_name } if feature_cmd_name == &*name  => *feat = Some(ext.name.clone()),
								_ => ()
							}),
						_ => ()
					});
					
					exts.extend(extensions.into_iter().map(|e| (e.name.clone(), e.requires.clone())));
				}
				KhrRegistryVariant::Feature(feature) => {
					feature.require.iter()
						.flat_map(|req| req.iter())
						.for_each(|e| if let KhrRequireVariant::Enum { name, value, extends: Option::None, .. } = e {
							consts.push(KhrEnumsVariant::Enum {
								name:    name.clone(),
								value:   match value {
									Some(Alias(v))  => KhrEnumValue::Alias(v.clone()),
									Some(Value(v))  => KhrEnumValue::Value(v.clone()),
									Some(Bitpos(v)) => KhrEnumValue::BitPos(v.clone()),
									_ => return
								},
								comment: None
							})
						});
					
					vec.iter_mut().for_each(|e| match e {
						RegistryElement::Enums(ref mut enums) => feature.require.iter()
							.flat_map(|req| req.iter())
							.for_each(|e| match e {
								KhrRequireVariant::Enum { name, value, dir, extends: Some(extends), extnumber, .. }
								if extends == &enums.name && !enums.enums.iter()
									.any(|v| if let KhrEnumsVariant::Enum { name: ref v, .. } = v { v == name } else { false })
								=> enums.enums.push(KhrEnumsVariant::Enum {
									name:  name.clone(),
									value: match value {
										Some(Alias(v))       => KhrEnumValue::Alias(v.clone()),
										Some(Value(v))       => KhrEnumValue::Value(v.clone()),
										Some(Bitpos(v))      => KhrEnumValue::BitPos(v.clone()),
										Some(Offset(offset)) => KhrEnumValue::Value(dir.clone().unwrap_or_default() + &(
											1_000_000_000 + (extnumber.unwrap() - 1)
												* 1_000 + offset.parse::<usize>().unwrap()).to_string()),
										_ => return
									},
									comment: None
								}),
								_ => ()
							}),
						RegistryElement::Command(KhrCommand::Command { feature: ref mut feat, proto: KhrCommandProto { name, .. }, .. })
						| RegistryElement::Command(KhrCommand::Alias { feature: ref mut feat, name, .. }) => feature.require.iter()
							.flat_map(|req| req.iter())
							.for_each(|e| match e {
								KhrRequireVariant::Command { name: feature_cmd_name } if feature_cmd_name == name  => *feat = Some(feature.name.clone()),
								_ => ()
							}),
						_ => ()
					});
					
					exts.push((feature.name, Vec::new()));
				}
				_ => ()
			}
		}
		
		let api = api.expect("cannot determine API type");
		
		Ok(Self { api, vec, exts, consts })
	}
}

impl<'de> Deserialize<'de> for KhrType {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(rename_all = "lowercase")]
		enum Val {
			Category(Category),
			Name(CommentVariant),
			Member(KhrTypeStructMember),
			#[serde(alias = "type")]
			Alias(CommentVariant),
			Parent(String),
			Comment(CommentVariant),
			#[serde(rename = "$value")]
			Body(String),
			#[serde(other)]
			Other
		}
		
		#[derive(Debug, Deserialize)]
		#[serde(untagged)]
		enum Category {
			Struct(KhrTypeStructCategory),
			Alias(KhrTypeAliasCategory),
			Other(String)
		}
		
		let vec = Vec::<Val>::deserialize(deserializer);
		let vec = vec?;
		let body = vec.iter()
			.filter_map(|e| match e { Val::Body(v) => Some(v.as_str()), _ => None })
			.collect::<String>();
		let mut category = None;
		let mut name = None;
		let mut alias = None;
		let mut comment = None;
		let mut parent = None;
		let mut members = Vec::new();
		
		for e in vec {
			match e {
				Val::Category(v) => category = Some(v),
				Val::Name(v) => name = Some(v),
				Val::Alias(v) => alias = Some(v.into()),
				Val::Parent(v) => parent = Some(v),
				Val::Comment(CommentVariant::Attr(v)) => comment = Some(v),
				Val::Comment(CommentVariant::Body(v)) => members.push(KhrTypeStructMemberVariant::Comment(v.value)),
				Val::Member(v) => members.push(KhrTypeStructMemberVariant::Member(v)),
				_ => ()
			}
		}
		
		Ok(match (category, name, alias) {
			(
				Some(Category::Struct(category)),
				Some(CommentVariant::Attr(name)),
				None
			) => KhrType::Struct(KhrTypeStruct {
				category,
				name,
				members,
				comment
			}),
			(
				Some(Category::Alias(category)),
				Some(name),
				Some(alias)
			) => KhrType::Alias(KhrTypeAlias {
				category,
				name: name.into(),
				alias,
				body,
				parent
			}),
			(
				Some(Category::Struct(_)),
				Some(name),
				Some(alias)
			) => KhrType::Alias(KhrTypeAlias {
				category: KhrTypeAliasCategory::Struct,
				name: name.into(),
				alias,
				body,
				parent
			}),
			_ => KhrType::Other
		})
	}
}

impl<'de> Deserialize<'de> for KhrTypeStructMember {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(rename_all = "lowercase")]
		enum Val {
			Type(Body<String>),
			Name(Body<String>),
			Comment(Body<String>),
			#[serde(rename = "$value")]
			Body(String),
			Optional(String),
			Len(String),
			Enum(Body<String>),
			#[serde(other)]
			Other
		}
		
		let vec = Vec::<Val>::deserialize(deserializer)?;
		let body = vec.iter().filter_map(|e| match e {
			Val::Body(v) => Some(v.as_str()),
			Val::Enum(v) => Some(&v.value),
			_ => None
		}).collect::<String>();
		let mut r#type = None;
		let mut name = None;
		let mut optional = None;
		let mut len = None;
		let mut comment = None;
		
		for e in vec {
			match e {
				Val::Type(v) => r#type = Some(convert_c_type(&v.value, &body)),
				Val::Name(v) => name = Some(convert_name(&v.value)),
				Val::Comment(v) => comment = Some(v.value),
				Val::Len(v) => len = Some(v),
				Val::Optional(v) => optional = Some(v),
				_ => ()
			}
		}
		
		Ok(Self {
			r#type: r#type.ok_or_else(|| D::Error::missing_field("type"))?,
			name: name.ok_or_else(|| D::Error::missing_field("name"))?,
			optional,
			len,
			comment,
		})
	}
}

impl<'de> Deserialize<'de> for KhrCommand {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(rename_all = "lowercase")]
		enum Val {
			Name(String),
			Alias(String),
			Proto(KhrCommandProto),
			Param(KhrCommandParam),
			Comment(String),
			#[serde(other)]
			Other
		}
		
		let mut name = None;
		let mut alias = None;
		let mut proto = None;
		let mut param = Vec::new();
		let mut comment = None;
		
		for e in Vec::<Val>::deserialize(deserializer)? {
			match e {
				Val::Name(v) => name = Some(v),
				Val::Alias(v) => alias = Some(v),
				Val::Proto(v) => proto = Some(v),
				Val::Param(v) => param.push(v),
				Val::Comment(v) => comment = Some(v),
				_ => ()
			}
		}
		
		Ok(match (name, alias, proto) {
			(Some(name), Some(alias), None) => KhrCommand::Alias {
				name,
				alias,
				comment,
				feature: None
			},
			(None, None, Some(proto)) => KhrCommand::Command {
				proto,
				param,
				comment,
				feature: None
			},
			_ => return Err(D::Error::custom("data did not match any variant of `KhrCommand`"))
		})
		
	}
}

impl<'de> Deserialize<'de> for KhrCommandProto {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
		#[derive(Deserialize)]
		struct Val {
			r#type: Body<String>,
			name:   Body<String>
		}
		
		Val::deserialize(deserializer).map(|v| KhrCommandProto {
			r#type: convert_c_type(&v.r#type.value, ""),
			name:   v.name.value
		})
	}
}

impl<'de> Deserialize<'de> for KhrCommandParam {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(rename_all = "lowercase")]
		enum Val {
			Type(Body<String>),
			Name(Body<String>),
			Comment(Body<String>),
			#[serde(rename = "$value")]
			Body(String),
			Optional(String),
			Len(String),
			Enum(Body<String>),
			#[serde(other)]
			Other
		}
		
		let vec = Vec::<Val>::deserialize(deserializer)?;
		let body = vec.iter().filter_map(|e| match e {
			Val::Body(v) => Some(v.as_str()),
			Val::Enum(v) => Some(&v.value),
			_ => None
		}).collect::<String>();
		let mut r#type = None;
		let mut name = None;
		let mut comment = None;
		let mut optional = None;
		let mut len = None;
		
		for e in vec {
			match e {
				Val::Type(v) => r#type = Some(convert_c_type(&v.value, &body)),
				Val::Name(v) => name = Some(convert_name(&v.value)),
				Val::Comment(v) => comment = Some(v.value),
				Val::Optional(v) => optional = Some(v),
				Val::Len(v) => len = Some(v),
				_ => ()
			}
		}
		
		Ok(Self {
			r#type: r#type.ok_or_else(|| D::Error::missing_field("type"))?,
			name: name.ok_or_else(|| D::Error::missing_field("name"))?,
			optional,
			len,
			comment,
		})
	}
}

impl<'de> Deserialize<'de> for KhrFeature {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(rename_all = "lowercase")]
		enum Val {
			Api(String),
			Name(String),
			Number(String),
			Comment(String),
			Require(Vec<KhrRequireVariant>),
			#[serde(other)]
			Other
		}
		
		let mut api = None;
		let mut name = None;
		let mut number = None;
		let mut comment = None;
		let mut require = Vec::new();
		
		for e in Vec::<Val>::deserialize(deserializer)? {
			match e {
				Val::Api(v) => api = Some(v),
				Val::Name(v) => name = Some(v),
				Val::Number(v) => number = Some(v),
				Val::Comment(v) => comment = Some(v),
				Val::Require(v) => require.push(v),
				_ => ()
			}
		}
		
		Ok(Self {
			api:    api.ok_or_else(|| D::Error::missing_field("api"))?,
			name:   name.ok_or_else(|| D::Error::missing_field("name"))?,
			number: number.ok_or_else(|| D::Error::missing_field("number"))?,
			comment,
			require
		})
	}
}

impl<'de> Deserialize<'de> for KhrExtension {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(rename_all = "lowercase")]
		enum Val {
			Name(String),
			Number(usize),
			Type(String),
			Supported(String),
			Requires(#[serde(deserialize_with = "deserialize_list", default)] Vec<String>),
			#[serde(alias = "protect")]
			Platform(String),
			Require(Vec<KhrRequireVariant>),
			#[serde(other)]
			Other
		}
		
		let mut name = None;
		let mut number = None;
		let mut r#type = None;
		let mut supported = None;
		let mut requires = None;
		let mut platform = None;
		let mut require = Vec::new();
		
		for e in Vec::<Val>::deserialize(deserializer)? {
			match e {
				Val::Name(v) => name = Some(v),
				Val::Number(v) => number = Some(v),
				Val::Type(v) => r#type = Some(v),
				Val::Supported(v) => supported = Some(v),
				Val::Requires(v) => requires = Some(v),
				Val::Platform(v) => platform = Some(v),
				Val::Require(v) => require.push(v),
				_ => ()
			}
		}
		
		Ok(Self {
			name:      name.ok_or_else(|| D::Error::missing_field("name"))?,
			number:    number.ok_or_else(|| D::Error::missing_field("number"))?,
			r#type,
			supported: supported.ok_or_else(|| D::Error::missing_field("supported"))?,
			requires:  requires.unwrap_or_else(Vec::new),
			platform,
			require
		})
	}
}

fn deserialize_enums<'de, D: Deserializer<'de>>(deserializer: D) -> Result<KhrEnums, D::Error> {
	#[derive(Deserialize)]
	#[serde(rename_all = "lowercase")]
	enum Val {
		Name(String),
		Type(KhrEnumsType),
		Comment(CommentVariant),
		Enum {
			name: String,
			#[serde(flatten)]
			value: KhrEnumValue,
			comment: Option<String>
		},
		#[serde(other)]
		Other
	}
	
	let mut name = None;
	let mut r#type = None;
	let mut comment = None;
	let mut enums = Vec::new();
	
	for e in Vec::<Val>::deserialize(deserializer)? {
		match e {
			Val::Name(v) => name = Some(v),
			Val::Type(v) => r#type = Some(v),
			Val::Comment(CommentVariant::Attr(v)) => comment = Some(v),
			Val::Comment(CommentVariant::Body(v)) => enums.push(KhrEnumsVariant::Comment(v.value)),
			Val::Enum { name, value, comment } => enums.push(
				KhrEnumsVariant::Enum { name, value, comment }),
			_ => ()
		}
	}
	
	Ok(KhrEnums {
		name: name.ok_or_else(|| D::Error::missing_field("name"))?,
		r#type: r#type.unwrap_or(KhrEnumsType::None),
		comment,
		enums
	})
}

/// parses the type of the given value (e.g. 1u32 -> u32) or returns a default type (i32)
pub fn parse_type(s: &str) -> &str {
	if      s.contains("u128")   { "u128" }
	else if s.contains("u64")    { "u64"  }
	else if s.contains("u32")    { "u32"  }
	else if s.contains("u16")    { "u16"  }
	else if s.contains("u8")     { "u8"   }
	else if s.contains("i128")   { "i128" }
	else if s.contains("i64")    { "i64"  }
	else if s.contains("i32")    { "i32"  }
	else if s.contains("i16")    { "i16"  }
	else if s.contains("i8")     { "i8"   }
	else if s.contains("f64")    { "f64"  }
	else if s.contains("f32")    { "f32"  }
	else if s.contains("usize")  { "usize" }
	else if s.contains("isize")  { "isize" }
	else if s.contains("char")   { "char" }
	else if s.starts_with('\"') && s.ends_with('\"') { "&str" }
	else if s.starts_with("b\"") && s.ends_with("\\0\".as_ptr()") { "*const u8" }
	else { "i32" }
}

/// converts C-types to Rust-types (e.g. uint32_t -> u32)
pub fn convert_c_type(ty: &str, outer: &str) -> String {
	#[allow(clippy::never_loop)]
		let ty = 'outer: loop {
		for (k, v) in C_TYPES.as_ref() {
			if *k == ty { break 'outer *v; }
		}
		break ty;
	};
	
	let outer = match outer.trim() {
		"*"       | "struct*"        => "*mut",
		"**"      | "struct**"       => "*mut *mut",
		"const*"  | "const struct*"  => "*const",
		"const**" | "const struct**" => "*mut *const",
		"const* const*"              => "*const *const",
		"" | "typedef;" | "()"       => return ty.to_string(),
		ty if ty.starts_with(':') => {
			BITFIELDS.store(true, std::sync::atomic::Ordering::Relaxed);
			""
		},
		r#type                  => return parse_array(ty, r#type.trim_start_matches("const")),
	};
	let mut s = String::new();
	s.push_str(outer);
	s.push(' ');
	s.push_str(ty);
	s
}

fn parse_array(ty: &str, outer: &str) -> String {
	match (outer.strip_prefix('['), outer.find(']')) {
		(Some(t), Some(end)) => format!("[{}; {}]", parse_array(ty, &outer[end + 1..]), &t[..end - 1]),
		_ => ty.to_string()
	}
}

/// converts C-style values to Rust-style values (e.g.: 1ULL -> 1u64)
pub fn convert_c_value(name: &str) -> String {
	if name.starts_with('\"') && name.ends_with('\"') {
		let mut name = String::from(name);
		name.insert(0, 'b');
		name.truncate(name.len() - 1);
		name.push_str("\\0\".as_ptr()");
		return name;
	}
	
	let mut s = String::new();
	let mut i = 0;
	let name = name.as_bytes();
	
	while i < name.len() {
		if name[i] >= b'0' && name[i] <= b'9' {
			s.push(name[i] as char);
			if name[i + 1..].starts_with(b"ULL") {
				s.push_str("u64");
				i += 3;
			} else if i + 1 < name.len() && name[i + 1] == b'U' {
				s.push_str("u32");
				i += 1;
			} else if i + 1 < name.len() && name[i + 1] == b'f' {
				s.push_str("f32");
				i += 1;
			}
		} else if i + 1 < name.len() && name[i] == b'~' && name[i + 1] >= b'0' && name[i + 1] <= b'9'{
			s.push('!');
		} else {
			s.push(name[i] as char);
		}
		i += 1;
	}
	s
}

/// converts names that contain Rust keywords (e.g.: type -> r#type)
pub fn convert_name(name: &str) -> String {
	match name {
		name if KEYWORDS.contains(&name) => format!("r#{}", name),
		name => name.to_string()
	}
}


static C_TYPES: [(&str, &str); 18] = [
	("int8_t",   "i8"),
	("int16_t",  "i16"),
	("int32_t",  "i32"),
	("int64_t",  "i64"),
	("uint8_t",  "u8"),
	("uint16_t", "u16"),
	("uint32_t", "u32"),
	("uint64_t", "u64"),
	("float",    "f32"),
	("double",   "f64"),
	("size_t",   "usize"),
	("int",      "i32"),
	("char",     "u8"),
	("void",     "()"),
	("VK_DEFINE_HANDLE", "u64"),
	("VK_DEFINE_NON_DISPATCHABLE_HANDLE", "u64"),
	("XR_DEFINE_HANDLE", "u64"),
	("XR_DEFINE_ATOM", "u64")
];

static KEYWORDS: [&str; 50] = [
	"as",
	"break",
	"const",
	"continue",
	"crate",
	"dyn",
	"else",
	"enum",
	"extern",
	"false",
	"fn",
	"for",
	"if",
	"impl",
	"in",
	"let",
	"loop",
	"match",
	"mod",
	"move",
	"mut",
	"pub",
	"ref",
	"return",
	"Self",
	"self",
	"static",
	"struct",
	"super",
	"trait",
	"true",
	"type",
	"unsafe",
	"use",
	"where",
	"while",
	"abstract",
	"async",
	"become",
	"box",
	"do",
	"final",
	"macro",
	"override",
	"priv",
	"try",
	"typeof",
	"unsized",
	"virtual",
	"yield"
];