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
	std::{str::FromStr, io, mem, fmt, marker::PhantomData},
	serde::{*, de::{self, *}, forward_to_deserialize_any},
	self::State::*
};

const UNICODE_FILE_MAGIC_BYTES: [u8; 3] = [0xEF, 0xBB, 0xBF];

pub fn deserialize<'de, T: Deserialize<'de>, R: io::BufRead + fmt::Debug>(reader: R) -> Result<T, Error> {
	T::deserialize(&mut Deserializer::from_reader(reader))
}

#[derive(Debug, Deserialize)]
pub struct Body<T> {
	#[serde(rename = "$value")]
	pub value: T
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum State {
	/// The outer tag is yet to be parsed.
	Uninit,
	/// Reading a tag's name.
	TagKey,
	/// Indicates that the next value is a map.
	TagValue(bool),
	/// Searching for attributes in a tag.
	AttrKey,
	/// An attribute value is to be parsed
	AttrValue,
	/// Reading the body of an xml tag while searching for inner tags.
	BodyValue,
	BodyKey
}

fn invalid_state(state: State, file: &'static str, line: u32) -> Error {
	Error::Custom(format!("invalid state: {:?}, {}:{}", state, file, line))
}

#[derive(Debug)]
pub enum Error {
	Custom(String),
	InvalidType(String, String),
	InvalidValue(String, String),
	InvalidLength(usize, String),
	UnknownVariant(String, &'static [&'static str]),
	UnknownField(String, &'static [&'static str]),
	MissingField(&'static str),
	DuplicateField(&'static str),
	Io(io::Error),
	Utf8(std::str::Utf8Error),
	InvalidToken(String, String),
	Parse(String)
}

impl Error {
	pub(crate) fn invalid_token(unexp: &str, exp: &str) -> Self {
		Self::InvalidToken(unexp.to_string(), exp.to_string())
	}
	
	pub(crate) fn unexpected_eof(msg: &str) -> Self {
		Self::Io(io::Error::new(io::ErrorKind::UnexpectedEof, msg))
	}
}

struct DisplayImpl<'a>(&'a dyn Expected);

impl fmt::Display for DisplayImpl<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.0.fmt(f)
	}
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Custom(s)                  => f.write_str(s),
			Self::InvalidType(unexp, exp)    => write!(f, "invalid type `{}`, expected `{}`", unexp, exp),
			Self::InvalidValue(unexp, exp)   => write!(f, "invalid value `{}`, expected `{}`", unexp, exp),
			Self::InvalidLength(unexp, exp)  => write!(f, "invalid length `{}`, expected `{}`", *unexp, exp),
			Self::UnknownVariant(unexp, exp) => write!(f, "unknown variant `{}`, expected variants `{:?}`", unexp, exp),
			Self::UnknownField(unexp, exp)   => write!(f, "unknown field `{}`, expected fields `{:?}`", unexp, exp),
			Self::MissingField(exp)          => write!(f, "missing field `{}`", exp),
			Self::DuplicateField(field)      => write!(f, "duplicate field `{}`", field),
			Self::Io(err)                    => write!(f, "io error: {}", err),
			Self::Utf8(err)                  => write!(f, "utf8 error: {}", err),
			Self::InvalidToken(unexp, exp)   => write!(f, "invalid token: {}, expected {}", unexp, exp),
			Self::Parse(err)            => write!(f, "parse error: {}", err)
		}
	}
}

impl de::Error for Error {
	fn custom<U: fmt::Display>(msg: U) -> Self {
		Self::Custom(msg.to_string())
	}
	
	fn invalid_type(unexp: Unexpected, exp: &dyn Expected) -> Self {
		Self::InvalidType(unexp.to_string(), DisplayImpl(exp).to_string())
	}
	
	fn invalid_value(unexp: Unexpected, exp: &dyn Expected) -> Self {
		Self::InvalidValue(unexp.to_string(), DisplayImpl(exp).to_string())
	}
	
	fn invalid_length(len: usize, exp: &dyn Expected) -> Self {
		Self::InvalidLength(len, DisplayImpl(exp).to_string())
	}
	
	fn unknown_variant(variant: &str, expected: &'static [&'static str]) -> Self {
		Self::UnknownVariant(variant.to_string(), expected)
	}
	
	fn unknown_field(field: &str, expected: &'static [&'static str]) -> Self {
		Self::UnknownField(field.to_string(), expected)
	}
	
	fn missing_field(field: &'static str) -> Self {
		Self::MissingField(field)
	}
	
	fn duplicate_field(field: &'static str) -> Self {
		Self::DuplicateField(field)
	}
}

impl From<io::Error> for Error {
	fn from(e: io::Error) -> Self {
		Self::Io(e)
	}
}

impl From<std::str::Utf8Error> for Error {
	fn from(e: std::str::Utf8Error) -> Self {
		Self::Utf8(e)
	}
}

struct IgnoredIdSeed;

impl<'de> de::DeserializeSeed<'de> for IgnoredIdSeed {
	type Value = IgnoredAny;
	
	fn deserialize<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
		deserializer.deserialize_identifier(de::IgnoredAny)
	}
}

struct IgnoredAnySeed;

impl<'de> de::DeserializeSeed<'de> for IgnoredAnySeed {
	type Value = IgnoredAny;
	
	fn deserialize<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
		deserializer.deserialize_ignored_any(de::IgnoredAny)
	}
}

struct NoopSeed;

impl<'de> de::DeserializeSeed<'de> for NoopSeed {
	type Value = ();
	
	fn deserialize<D: de::Deserializer<'de>>(self, _deserializer: D) -> Result<Self::Value, D::Error> {
		Ok(())
	}
}

#[derive(Debug)]
pub struct Deserializer<T: io::BufRead> {
	reader: T,
	state:  State,
	stack:  Vec<String>,
	buf:    String
}

impl<T: io::BufRead> Deserializer<T> {
	pub fn from_reader(mut reader: T) -> Self {
		// consume magic bytes for unicode text files
		if reader.fill_buf().unwrap_or(&[]).starts_with(&UNICODE_FILE_MAGIC_BYTES) {
			reader.consume(3);
		}
		
		Self {
			reader,
			state:  Uninit,
			stack:  Vec::new(),
			buf:    String::new(),
		}
	}
	
	fn next(&mut self) -> Result<char, Error> {
		let buf = self.reader.fill_buf()?;
		let len = if buf[0] & 0x80 == 0 {
			1
		} else if buf[0] & 0xE0 == 0xC0 {
			2
		} else if buf[0] & 0xF0 == 0xE0 {
			3
		} else {
			4
		};
		
		let mut buf = [0u8; 4];
		self.reader.read_exact(&mut buf[..len])?;
		Ok(std::str::from_utf8(&buf[..len])?.chars().next().unwrap())
	}
	
	fn read_until(&mut self, mut f: impl FnMut(u8) -> bool, peek: bool) -> Result<u8, Error> {
		loop {
			let buf = self.reader.fill_buf()?;
			
			if buf.is_empty() {
				return Err(Error::unexpected_eof(""));
			}
			
			let (i, &ch) = buf.iter()
				.enumerate()
				.find(|(_, c)| f(**c))
				.unwrap_or((buf.len(), &0));
			
			self.buf.push_str(std::str::from_utf8(&buf[..i])?);
			
			if i != buf.len() {
				self.reader.consume(i + if peek { 0 } else { 1 });
				return Ok(ch);
			} else {
				let __tmp__ = buf.len();
				self.reader.consume(__tmp__);
			}
		}
	}
	
	fn next_value<U: FromStr>(&mut self) -> Result<U, Error> {
		self.state = match self.state {
			AttrValue => self.read_until(|ch| ch == b'"', false).map(|_| AttrKey)?,
			BodyValue => self.read_until(|ch| ch == b'<', true).map(|_| BodyKey)?,
			TagValue(true) => {
				Body::<String>::deserialize(&mut*self)?;
				self.state
			},
			state => return Err(invalid_state(state, file!(), line!()))
		};
		
		self.dbg_val(&self.buf);
		let v = convert_xml(self.buf.trim()).parse::<U>()
			.map_err(|_| Error::Parse(format!("failed to parse {}", std::any::type_name::<U>())));
		self.buf.clear();
		v
	}
	
	fn dbg_key(&self, key: &str) {
		if cfg!(feature = "xml_dbg") {
			print!("\n{}{:10}", "\t".repeat(self.stack.len()), key);
		}
	}
	
	fn dbg_val(&self, val: &str) {
		if cfg!(feature = "xml_dbg") {
			print!(" = \"{}\"", val);
		}
	}
}

impl<'a, 'de, T: io::BufRead> serde::Deserializer<'de> for &'a mut Deserializer<T> {
	type Error = Error;
	
	fn deserialize_identifier<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		match self.state {
			// parse a tag's name
			TagKey => {
				let ch = self.read_until(|ch| ch.is_ascii_whitespace() || ch == b'>', false)?;
				self.state = TagValue(ch == b'>');
				self.dbg_key(&self.buf);
				let r = visitor.visit_str(self.buf.as_str());
				self.stack.push(mem::take(&mut self.buf));
				r
			}
			// parse attribute name
			AttrKey => {
				self.read_until(|ch| ch.is_ascii_whitespace() || ch == b'=', false)?;
				self.state = AttrValue;
				self.dbg_key(&self.buf);
				let r = visitor.visit_str(self.buf.as_str());
				self.buf.clear();
				r
			}
			// return value identifier
			BodyKey => {
				self.state = BodyValue;
				self.dbg_key("$value");
				visitor.visit_str("$value")
			},
			state => Err(invalid_state(state, file!(), line!()))
		}
	}
	
	fn deserialize_map<V: Visitor<'de>>(mut self, visitor: V) -> Result<V::Value, Self::Error> {
		struct Seed<'de, T: Visitor<'de>>(T, PhantomData<&'de ()>);
		
		impl<'de, T: Visitor<'de>> de::DeserializeSeed<'de> for Seed<'de, T> {
			type Value = T::Value;
			
			fn deserialize<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
				deserializer.deserialize_map(self.0)
			}
		}
		
		match self.state {
			TagValue(new_state) => {
				self.state = if new_state { BodyKey } else { AttrKey };
				visitor.visit_map(self)
			}
			Uninit => {
				self.state = BodyKey;
				self.next_entry_seed(IgnoredIdSeed, Seed(visitor, PhantomData))
					.transpose()
					.expect("XML document was empty")
					.map(|(_, v)| v)
			}
			state => Err(invalid_state(state, file!(), line!()))
		}
	}
	
	fn deserialize_seq<V: Visitor<'de>>(mut self, visitor: V) -> Result<V::Value, Self::Error> {
		struct Seed<'de, T: Visitor<'de>>(T, PhantomData<&'de ()>);
		
		impl<'de, T: Visitor<'de>> de::DeserializeSeed<'de> for Seed<'de, T> {
			type Value = T::Value;
			
			fn deserialize<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
				deserializer.deserialize_seq(self.0)
			}
		}
		
		match self.state {
			TagValue(new_state) => {
				self.state = if new_state { BodyKey } else { AttrKey };
				visitor.visit_seq(self)
			}
			Uninit => {
				self.state = BodyKey;
				self.next_entry_seed(IgnoredIdSeed, Seed(visitor, PhantomData))
					.transpose()
					.expect("XML document was empty")
					.map(|(_, v)| v)
			}
			state => Err(invalid_state(state, file!(), line!()))
		}
	}
	
	fn deserialize_enum<V: Visitor<'de>>(
		self,
		_name:     &'static str,
		_variants: &'static [&'static str],
		visitor:   V
	) -> Result<V::Value, Self::Error> {
		match self.state {
			AttrValue | BodyValue => visitor.visit_enum(
				self.next_value::<String>()?.into_deserializer()),
			Uninit | TagValue(_) => {
				self.deserialize_map(IgnoredAny)?;
				visitor.visit_enum(self)
			},
			state => Err(invalid_state(state, file!(), line!()))
		}
	}
	
	fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		match self.state {
			TagKey | AttrKey | BodyKey => self.deserialize_identifier(visitor),
			Uninit | TagValue(_) => self.deserialize_map(visitor),
			AttrValue | BodyValue =>  visitor.visit_string(self.next_value()?)
		}
	}
	
	fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_bool(self.next_value()?)
	}
	
	fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_i8(self.next_value()?)
	}
	
	fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_i16(self.next_value()?)
	}
	
	fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_i32(self.next_value()?)
	}
	
	fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_i64(self.next_value()?)
	}
	
	fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_u8(self.next_value()?)
	}
	
	fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_u16(self.next_value()?)
	}
	
	fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_u32(self.next_value()?)
	}
	
	fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_u64(self.next_value()?)
	}
	
	fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_f32(self.next_value()?)
	}
	
	fn deserialize_f64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_f64(self.next_value()?)
	}
	
	fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_char(self.next_value()?)
	}
	
	fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_string(self.next_value()?)
	}
	
	fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_string(self.next_value()?)
	}
	
	fn deserialize_bytes<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_byte_buf(self.next_value::<String>()?.into_bytes())
	}
	
	fn deserialize_byte_buf<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_byte_buf(self.next_value::<String>()?.into_bytes())
	}
	
	fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_some(self)
	}
	
	fn deserialize_unit<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.next_value::<String>()?;
		visitor.visit_unit()
	}
	
	fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Self::Error> where
		V: Visitor<'de> {
		self.deserialize_unit(visitor)
	}
	
	fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Self::Error> where
		V: Visitor<'de> {
		visitor.visit_newtype_struct(self)
	}
	
	fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error> where
		V: Visitor<'de> {
		self.deserialize_seq(visitor)
	}
	
	fn deserialize_tuple_struct<V>(self, _name: &'static str, _len: usize, visitor: V) -> Result<V::Value, Self::Error> where
		V: Visitor<'de> {
		self.deserialize_seq(visitor)
	}
	
	fn deserialize_struct<V>(self, _name: &'static str, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> where
		V: Visitor<'de> {
		self.deserialize_map(visitor)
	}
	
	fn deserialize_ignored_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.deserialize_any(visitor)
	}
	
	fn is_human_readable(&self) -> bool {
		true
	}
}

impl<'a, 'de, R: io::BufRead> de::MapAccess<'de> for &'a mut Deserializer<R> {
	type Error = Error;
	
	fn next_key_seed<K: DeserializeSeed<'de>>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error> {
		let ch = 'outer: loop {
			match self.state {
				AttrKey => loop {
					match self.next()? {
						ch if ch.is_whitespace() => (),
						'>' => {
							self.state = BodyKey;
							break;
						}
						'/' => return match self.next()? {
							'>' => {
								self.state = BodyKey;
								self.stack.pop().unwrap();
								Ok(None)
							}
							ch => Err(Error::invalid_token(&format!("'{}'", ch as char), "'>'"))
						},
						ch => break 'outer ch
					}
				}
				BodyKey => {
					loop {
						match self.next()? {
							ch if ch.is_whitespace() => (),
							'<' => break,
							ch => break 'outer ch
						}
					}
					
					match self.next()? {
						// closing tag
						'/' => loop {
							match self.next()? {
								ch if ch.is_whitespace() => (),
								'>' => return match self.stack.pop() {
									Some(tag) if tag == self.buf => {
										self.buf.clear();
										Ok(None)
									},
									tag => Err(Error::Custom(format!(
										"invalid closing tag: `{}`, expected `{:?}`",
										mem::take(&mut self.buf), tag
									)))
								},
								ch => self.buf.push(ch)
							}
						}
						// comment/doctype, skip
						'!' => loop {
							while self.next()? != '-' {}
							if self.next()? == '-' && self.next()? == '>' { break; }
						}
						'?' => loop {
							while self.next()? != '?' {}
							if self.next()? == '>' { break; }
						}
						// opening tag
						ch => {
							self.state = TagKey;
							break 'outer ch;
						}
					}
				}
				state => return Err(invalid_state(state, file!(), line!()))
			}
		};
		
		self.buf.push(ch);
		seed.deserialize(&mut**self).map(Some)
	}
	
	fn next_value_seed<V: DeserializeSeed<'de>>(&mut self, seed: V) -> Result<V::Value, Self::Error>{
		match self.state {
			TagValue(_) | BodyValue => (),
			AttrValue => loop {
				match self.next()? {
					'"' => break,
					ch if ch.is_whitespace() || ch == '=' => (),
					ch => return Err(Error::invalid_token(&format!("'{}'", ch),  "'\"', '=' or whitespace"))
				}
			}
			state => return Err(invalid_state(state, file!(), line!()))
		}
		
		seed.deserialize(&mut**self)
	}
}

impl<'a, 'de, R: io::BufRead> de::EnumAccess<'de> for &'a mut Deserializer<R> {
	type Error   = Error;
	type Variant = Self;
	
	fn variant_seed<V: DeserializeSeed<'de>>(mut self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error> {
		match self.next_entry_seed(seed, NoopSeed)? {
			Some((k, _)) => Ok((k, self)),
			None => panic!("no key was found")
		}
	}
}

impl<'a, 'de, R: io::BufRead> de::VariantAccess<'de> for &'a mut Deserializer<R> {
	type Error = Error;
	
	fn unit_variant(self) -> Result<(), Self::Error> {
		use de::Deserializer;
		self.deserialize_ignored_any(IgnoredAny).map(|_| ())
	}
	
	fn newtype_variant_seed<T: DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value, Self::Error> {
		seed.deserialize(self)
	}
	
	fn tuple_variant<V: Visitor<'de>>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error> {
		use de::Deserializer;
		self.deserialize_tuple(len, visitor)
	}
	
	fn struct_variant<V: Visitor<'de>>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value, Self::Error> {
		use de::Deserializer;
		self.deserialize_map(visitor)
	}
}

impl<'a, 'de, R: io::BufRead> de::SeqAccess<'de> for &'a mut Deserializer<R> {
	type Error = Error;
	
	fn next_element_seed<T: DeserializeSeed<'de>>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error> {
		
		struct Visitor;
		
		impl<'de> de::Visitor<'de> for Visitor {
			type Value = String;
			
			fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
				write!(formatter, "an identifier")
			}
			
			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
				Ok(v.to_string())
			}
		}
		
		struct Seed;
		
		impl<'de> de::DeserializeSeed<'de> for Seed {
			type Value = String;
			
			fn deserialize<D: de::Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
				deserializer.deserialize_identifier(Visitor)
			}
		}
		
		match self.next_key_seed(Seed)? {
			Some(id) => seed.deserialize(&mut SeqDeserializer {
				inner: Some(&mut**self),
				id:    Some(id.as_str())
			}).map(Some),
			None => Ok(None)
		}
	}
}

struct SeqDeserializer<'a, R: io::BufRead> {
	inner: Option<&'a mut Deserializer<R>>,
	id:    Option<&'a str>,
}

impl<'a, 'de, R: io::BufRead> de::Deserializer<'de> for &'a mut SeqDeserializer<'a, R> {
	type Error = Error;
	
	fn deserialize_any<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error> where
		V: Visitor<'de> {
		self.id.unwrap();
		visitor.visit_map(self)
	}
	
	fn deserialize_enum<V: Visitor<'de>>(
		self,
		_name:     &'static str,
		_variants: &'static [&'static str],
		visitor:   V
	) -> Result<V::Value, Self::Error> {
		visitor.visit_enum(self)
	}
	
	fn deserialize_identifier<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value, Self::Error> where
		V: Visitor<'de> {
		visitor.visit_str(self.id.take().unwrap())
	}
	
	forward_to_deserialize_any!(
		bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes byte_buf option unit
		unit_struct newtype_struct seq tuple tuple_struct map struct ignored_any
	);
}

impl<'a, 'de, R: io::BufRead> de::MapAccess<'de> for SeqDeserializer<'a, R> {
	type Error = Error;
	
	fn next_key_seed<K: DeserializeSeed<'de>>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error> {
		match self.inner.as_ref() {
			Some(_) => seed.deserialize::<&mut Self>(unsafe {
				std::mem::transmute::<&mut Self, _>(self) }).map(Some),
			None => Ok(None)
		}
	}
	
	fn next_value_seed<V: DeserializeSeed<'de>>(&mut self, seed: V) -> Result<V::Value, Self::Error> {
		self.inner.take().unwrap().next_value_seed(seed)
	}
}

impl<'a, 'de, R: io::BufRead> de::EnumAccess<'de> for &'a mut SeqDeserializer<'a, R> {
	type Error   = Error;
	type Variant = &'a mut Deserializer<R>;
	
	fn variant_seed<V: DeserializeSeed<'de>>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error> {
		let mut __tmp__ = self.inner.take().unwrap();
		match seed.deserialize(self) {
			Ok(v) => {
				__tmp__.next_value_seed(NoopSeed)?;
				Ok((v, __tmp__))
			},
			Err(e) => Err(e)
		}
	}
}

/// converts xml expressions (e.g.: &quot; -> ")
fn convert_xml(mut s: &str) -> String {
	let mut buf = String::new();
	while let Some(off) = s.find('&') {
		let ext = s.find(';').unwrap();
		buf.push_str(&s[..off]);
		buf.push(match &s[off + 1..ext] {
			"exclamation" => '!',
			"quot"        => '"',
			"percent"     => '%',
			"amp"         => '&',
			"apos"        => '\'',
			"add"         => '+',
			"lt"          => '<',
			"equal"       => '=',
			"gt"          => '>',
			_             => panic!()
		});
		s = &s[ext + 1..];
	}
	buf.push_str(s);
	buf
}