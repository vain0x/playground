/*

TODOs:

- [ ] conversion from NaN/Inf f64, i64 or usize
- [ ] parse string
    - [ ] \u+FFFF
    - [ ] \b, \f
- [ ] serialize
    - [ ] pretty print
    - [ ] \u+FFFF
- [ ] error reporting
- [ ] input::cur, line
- [ ] i64 support
- [ ] support file stream
- [ ] methods of Value
- [ ] partial_cmp for obj
- [ ] refactoring

*/

use std::collections::BTreeMap;
use std::iter::FromIterator;
use std::iter::*;
use std::vec::*;

// static indent_width: i32 = 2;

type Error = String;

pub type Array = Vec<Value>;

pub type Object = BTreeMap<String, Value>;

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub enum Type {
    Null,
    Boolean,
    Number,
    String,
    Array,
    Object,
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Array),
    Object(Object),
}

macro_rules! impl_value_as {
    ($n:ident, $m:ident, $t:ty) => {
        pub fn $n(&self) -> Option<&$t> {
            self.try_as::<$t>()
        }

        pub fn $m(&mut self) -> Option<&mut $t> {
            self.try_as_mut::<$t>()
        }
    }
}

impl Value {
    pub fn null() -> Value {
        Value::Null
    }

    pub fn array() -> Value {
        Value::Array(Vec::new())
    }

    pub fn object() -> Value {
        Value::Object(BTreeMap::new())
    }

    fn from<T: ValueLike>(value: T) -> Value {
        value.into_value()
    }

    fn try_from_number(value: f64) -> Result<Value, &'static str> {
        if value.is_nan() {
            Err("NaN can't be a json value.")
        } else if value.is_infinite() {
            Err("Infinite value can't be a json value.")
        } else {
            Ok(Value::Number(value))
        }
    }

    fn from_number(value: f64) -> Value {
        Value::try_from_number(value).unwrap()
    }

    pub fn try_as<T: ValueKind>(&self) -> Option<&T> {
        T::try_borrow(self)
    }

    pub fn try_as_mut<T: ValueKind>(&mut self) -> Option<&mut T> {
        T::try_borrow_mut(self)
    }

    pub fn is_of<T: ValueKind>(&self) -> bool {
        self.try_as::<T>().is_some()
    }

    pub fn is_null(&self) -> bool {
        self.is_of::<()>()
    }

    impl_value_as!(as_bool, as_bool_mut, bool);
    impl_value_as!(as_number, as_number_mut, f64);
    impl_value_as!(as_string, as_string_mut, String);
    impl_value_as!(as_array, as_array_mut, Array);
    impl_value_as!(as_object, as_object_mut, Object);

    pub fn serialize(&self) -> String {
        JsonSerializer::serialize_to_string(self)
    }

    pub fn pretty_print(&self) -> String {
        "".to_string()
    }

    pub fn is_empty(&self) -> bool {
        true
    }

    pub fn contains_key(&self, _index: i64) -> bool {
        false
    }

    pub fn get<K>(&self, _key: &K) -> Option<&Value> {
        None
    }

    pub fn get_mut<K, V>(&mut self, _key: &K) -> Option<&mut Value> {
        None
    }
}

pub trait ValueKind: Clone + std::fmt::Debug + ValueLike {
    fn try_borrow(value: &Value) -> Option<&Self>;

    fn try_borrow_mut(value: &mut Value) -> Option<&mut Self>;

    fn into_value(self) -> Value {
        <Self as ValueLike>::into_value(self)
    }
}

impl ValueKind for () {
    fn try_borrow(value: &Value) -> Option<&Self> {
        match value {
            &Value::Null => Some(&()),
            _ => None,
        }
    }

    fn try_borrow_mut(value: &mut Value) -> Option<&mut Self> {
        static mut UNIT: () = ();
        match value {
            &mut Value::Null => Some(unsafe { &mut UNIT }),
            _ => None,
        }
    }
}

impl ValueKind for bool {
    fn try_borrow(value: &Value) -> Option<&Self> {
        match value {
            &Value::Boolean(ref it) => Some(it),
            _ => None,
        }
    }

    fn try_borrow_mut(value: &mut Value) -> Option<&mut Self> {
        match value {
            &mut Value::Boolean(ref mut it) => Some(it),
            _ => None,
        }
    }
}

impl ValueKind for f64 {
    fn try_borrow(value: &Value) -> Option<&Self> {
        match value {
            &Value::Number(ref it) => Some(it),
            _ => None,
        }
    }

    fn try_borrow_mut(value: &mut Value) -> Option<&mut Self> {
        match value {
            &mut Value::Number(ref mut it) => Some(it),
            _ => None,
        }
    }
}

impl ValueKind for String {
    fn try_borrow(value: &Value) -> Option<&Self> {
        match value {
            &Value::String(ref it) => Some(it),
            _ => None,
        }
    }

    fn try_borrow_mut(value: &mut Value) -> Option<&mut Self> {
        match value {
            &mut Value::String(ref mut it) => Some(it),
            _ => None,
        }
    }
}

impl ValueKind for Array {
    fn try_borrow(value: &Value) -> Option<&Self> {
        match value {
            &Value::Array(ref it) => Some(it),
            _ => None,
        }
    }

    fn try_borrow_mut(value: &mut Value) -> Option<&mut Self> {
        match value {
            &mut Value::Array(ref mut it) => Some(it),
            _ => None,
        }
    }
}

impl ValueKind for Object {
    fn try_borrow(value: &Value) -> Option<&Self> {
        match value {
            &Value::Object(ref it) => Some(it),
            _ => None,
        }
    }

    fn try_borrow_mut(value: &mut Value) -> Option<&mut Self> {
        match value {
            &mut Value::Object(ref mut it) => Some(it),
            _ => None,
        }
    }
}

/// Represents a type of JSON value.
pub trait ValueLike: Clone + std::fmt::Debug {
    fn into_value(self) -> Value;
}

impl ValueLike for Value {
    fn into_value(self) -> Value {
        self
    }
}

impl ValueLike for () {
    fn into_value(self) -> Value {
        Value::Null
    }
}

impl ValueLike for bool {
    fn into_value(self) -> Value {
        Value::Boolean(self)
    }
}

impl ValueLike for f64 {
    fn into_value(self) -> Value {
        Value::from_number(self)
    }
}

impl ValueLike for i32 {
    fn into_value(self) -> Value {
        Value::Number(self as f64)
    }
}

impl ValueLike for String {
    fn into_value(self) -> Value {
        Value::String(self)
    }
}

fn array_from_iter<V, I>(iter: I) -> Array
where
    V: ValueLike,
    I: IntoIterator<Item = V>,
{
    iter.into_iter().map(|v| v.into_value()).collect::<Array>()
}

fn object_from_iter<K, V, I>(iter: I) -> Object
where
    K: ToString,
    V: ValueLike,
    I: IntoIterator<Item = (K, V)>,
{
    iter.into_iter()
        .map(|(k, v)| (k.to_string(), v.into_value()))
        .collect::<BTreeMap<_, _>>()
}

pub trait ValueCollectionItem: Clone + std::fmt::Debug {
    fn collect<I: IntoIterator<Item = Self>>(iter: I) -> Value;
}

impl<V: ValueLike> ValueCollectionItem for V {
    fn collect<I: IntoIterator<Item = V>>(iter: I) -> Value {
        Value::Array(array_from_iter(iter))
    }
}

impl<K, V> ValueCollectionItem for (K, V)
where
    K: ToString + Clone + std::fmt::Debug,
    V: ValueLike,
{
    fn collect<I: IntoIterator<Item = Self>>(iter: I) -> Value {
        Value::Object(object_from_iter(iter))
    }
}

impl<'a> ValueLike for &'a str {
    fn into_value(self) -> Value {
        Value::String(self.to_string())
    }
}

impl<T: ValueCollectionItem> ValueLike for Vec<T>
where
    T: Clone + std::fmt::Debug,
{
    fn into_value(self) -> Value {
        <T as ValueCollectionItem>::collect(self)
    }
}

impl<K, V> ValueLike for BTreeMap<K, V>
where
    K: ToString + Clone + std::fmt::Debug,
    V: ValueLike,
{
    fn into_value(self) -> Value {
        Value::Object(object_from_iter(self))
    }
}

impl<T: ValueCollectionItem> FromIterator<T> for Value {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        T::collect(iter)
    }
}

struct Input<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    end: bool,
    consumed: bool,
    line: i32,
}

impl<'a> Input<'a> {
    fn new(s: &'a str) -> Input<'a> {
        Input {
            chars: s.chars().peekable(),
            end: false,
            consumed: false,
            line: 1,
        }
    }

    fn getc(&mut self) -> Option<char> {
        if self.consumed {
            if let Some(&'\n') = self.chars.peek() {
                self.line += 1;
            }

            let next = self.chars.next();
            if next == None {
                self.end = true;
            }
        }

        if self.end {
            self.consumed = false;
            return None;
        }

        self.consumed = true;
        return self.chars.peek().cloned();
    }

    fn ungetc(&mut self) {
        self.consumed = false;
    }

    fn skip_ws(&mut self) {
        loop {
            match self.getc() {
                Some(' ') | Some('\t') | Some('\r') | Some('\n') => continue,
                _ => {
                    self.ungetc();
                    break;
                }
            }
        }
    }

    fn expect(&mut self, ex: char) -> bool {
        self.skip_ws();
        match self.getc() {
            Some(c) if c == ex => true,
            _ => {
                self.ungetc();
                false
            }
        }
    }

    fn does_match(&mut self, pattern: &str) -> bool {
        for ex in pattern.chars() {
            match self.getc() {
                Some(c) if c == ex => continue,
                _ => {
                    self.ungetc();
                    return false;
                }
            }
        }
        true
    }
}

trait ParseContext {
    fn set_null(&mut self) -> bool;
    fn set_bool(&mut self, value: bool) -> bool;
    fn set_number(&mut self, value: f64) -> bool;
    fn set_string(&mut self, value: String) -> bool;
    fn parse_array_start(&mut self) -> bool;
    fn parse_array_item<'i>(&mut self, input: &mut Input<'i>, size: usize) -> bool;
    fn parse_array_stop(&mut self, size: usize) -> bool;
    fn parse_object_start(&mut self) -> bool;
    fn parse_object_item<'i>(&mut self, input: &mut Input<'i>, key: String) -> bool;
}

struct DefaultParseContext<'a> {
    out: &'a mut Value,
}

impl<'a> DefaultParseContext<'a> {
    fn new(out: &'a mut Value) -> DefaultParseContext<'a> {
        DefaultParseContext { out }
    }
}

impl<'a> ParseContext for DefaultParseContext<'a> {
    fn set_null(&mut self) -> bool {
        *self.out = Value::Null;
        true
    }

    fn set_bool(&mut self, value: bool) -> bool {
        *self.out = Value::from(value);
        true
    }

    fn set_number(&mut self, value: f64) -> bool {
        *self.out = Value::from(value);
        true
    }

    fn set_string(&mut self, value: String) -> bool {
        *self.out = Value::from(value);
        true
    }

    fn parse_array_start(&mut self) -> bool {
        *self.out = Value::Array(Array::new());
        true
    }

    fn parse_array_item<'i>(&mut self, input: &mut Input<'i>, _size: usize) -> bool {
        let mut value = Value::Null;
        let ok = {
            let mut subcontext = DefaultParseContext::new(&mut value);
            parse_input(&mut subcontext, input)
        };

        if !ok {
            return false;
        }

        let vec = self.out.as_array_mut().unwrap();
        vec.push(value);
        true
    }

    fn parse_array_stop(&mut self, _size: usize) -> bool {
        true
    }

    fn parse_object_start(&mut self) -> bool {
        *self.out = Value::Object(Object::new());
        true
    }

    fn parse_object_item<'i>(&mut self, input: &mut Input<'i>, key: String) -> bool {
        let mut value = Value::Null;
        let ok = {
            let mut subcontext = DefaultParseContext::new(&mut value);
            parse_input(&mut subcontext, input)
        };

        if !ok {
            return false;
        }

        let obj = self.out.as_object_mut().unwrap();
        obj.insert(key, value);
        true
    }
}

fn _read_digits<'a>(input: &mut Input<'a>) -> String {
    let mut num_str = Vec::new();
    loop {
        match input.getc() {
            Some(ch)
                if ('0' <= ch && ch <= '9') || ch == '+' || ch == '-' || ch == 'e' || ch == 'E'
                    || ch == '.' =>
            {
                num_str.push(ch);
            }
            _ => {
                input.ungetc();
                break;
            }
        }
    }
    return num_str.into_iter().collect::<String>();
}

fn _parse_number<'a, C: ParseContext>(ch: char, context: &mut C, input: &mut Input<'a>) -> bool {
    if ('0' <= ch && ch <= '9') || ch == '-' {
        input.ungetc();

        let num_str = _read_digits(input);
        if num_str.is_empty() {
            return false;
        }

        match num_str.parse::<f64>() {
            Ok(value) => {
                context.set_number(value);
                true
            }
            Err(_) => false,
        }
    } else {
        false
    }
}

fn _parse_string<'a>(out: &mut String, input: &mut Input<'a>) -> bool {
    loop {
        match input.getc() {
            None => {
                input.ungetc();
                return false;
            }
            Some(ch) if ch < ' ' => {
                input.ungetc();
                return false;
            }
            Some('"') => {
                return true;
            }
            Some('\\') => {
                match input.getc() {
                    Some('"') => {
                        out.push('\"');
                    }
                    Some('\\') => {
                        out.push('\\');
                    }
                    // Some('/') => {
                    //     out.push('/');
                    // }
                    // Some ('b') => {
                    //     out.push('\b');
                    // }
                    // Some ('f') => {
                    //     out.push('\f');
                    // }
                    Some('n') => {
                        out.push('\n');
                    }
                    Some('r') => {
                        out.push('\r');
                    }
                    Some('t') => {
                        out.push('\t');
                    }
                    Some('u') => {
                        panic!("not implemented");
                    }
                    _ => {
                        return false;
                    }
                }
            }
            Some(ch) => {
                out.push(ch);
            }
        }
    }
}

fn _parse_array<'a, C: ParseContext>(ctx: &mut C, input: &mut Input<'a>) -> bool {
    if !ctx.parse_array_start() {
        return false;
    }

    let mut index = 0;
    if input.expect(']') {
        return ctx.parse_array_stop(index);
    }

    loop {
        if !ctx.parse_array_item(input, index) {
            return false;
        }

        index += 1;
        if !input.expect(',') {
            break;
        }
    }

    if !input.expect(']') {
        return false;
    }

    if !ctx.parse_array_stop(index) {
        return false;
    }

    true
}

fn _parse_object<'a, C: ParseContext>(context: &mut C, input: &mut Input<'a>) -> bool {
    if !context.parse_object_start() {
        return false;
    }

    if input.expect('}') {
        return true;
    }

    loop {
        let mut key = String::new();
        if !input.expect('"') || !_parse_string(&mut key, input) || !input.expect(':') {
            return false;
        }

        if !context.parse_object_item(input, key) {
            return false;
        }

        if !input.expect(',') {
            break;
        }
    }

    if !input.expect('}') {
        return false;
    }

    true
}

fn parse_input<'a, C: ParseContext>(ctx: &mut C, input: &mut Input<'a>) -> bool {
    input.skip_ws();
    match input.getc() {
        Some('n') => {
            if input.does_match("ull") {
                ctx.set_null();
                true
            } else {
                false
            }
        }
        Some('t') => {
            if input.does_match("rue") {
                ctx.set_bool(true);
                true
            } else {
                false
            }
        }
        Some('f') => {
            if input.does_match("alse") {
                ctx.set_bool(false);
                true
            } else {
                false
            }
        }
        Some('[') => _parse_array(ctx, input),
        Some('{') => _parse_object(ctx, input),
        Some(ch) if ('0' <= ch && ch <= '9') || ch == '-' => _parse_number(ch, ctx, input),
        Some('"') => {
            let mut out = String::new();
            if _parse_string(&mut out, input) {
                ctx.set_string(out)
            } else {
                false
            }
        }
        _ => {
            input.ungetc();
            false
        }
    }
}

pub fn parse_string(s: &str) -> Result<Value, Error> {
    let mut out = Value::null();
    let ok = {
        let mut context = DefaultParseContext::new(&mut out);
        let mut input = Input::new(s);
        parse_input(&mut context, &mut input)
    };

    if ok {
        Ok(out)
    } else {
        Err("ERROR".to_string())
    }
}

fn is_first(value: &mut bool) -> bool {
    let old_value = *value;
    *value = false;
    old_value
}

struct JsonSerializer<W> {
    writer: W,
}

impl<W: std::io::Write> JsonSerializer<W> {
    fn new(writer: W) -> Self {
        JsonSerializer { writer }
    }

    fn write_char(&mut self, c: u8) {
        self.writer.write(&[c]).unwrap();
    }

    fn write_str(&mut self, s: &str) {
        self.writer.write(s.as_bytes()).unwrap();
    }

    fn serialize_string(&mut self, value: &str) {
        self.write_char(b'"');

        for c in value.chars() {
            match c {
                '"' => self.write_str("\\\""),
                '\n' => self.write_str("\\n"),
                '\r' => self.write_str("\\r"),
                '\t' => self.write_str("\\t"),
                '\\' => self.write_str("\\\\"),
                _ => {
                    fn is_u8(n: i32) -> bool {
                        std::u8::MIN as i32 <= n && n <= std::u8::MAX as i32
                    }
                    if is_u8(c as i32) {
                        self.write_char(c as u8);
                    } else {
                        unimplemented!("\\u+10FFFF")
                    }
                }
            }
        }

        self.write_char(b'"');
    }

    fn serialize_array(&mut self, array: &Array) {
        if array.is_empty() {
            self.write_str("[]");
        } else {
            self.write_char(b'[');
            let mut first = true;
            for item in array {
                if !is_first(&mut first) {
                    self.write_char(b',');
                }
                self.serialize_core(item);
            }
            self.write_char(b']');
        }
    }

    fn serialize_object(&mut self, object: &Object) {
        if object.is_empty() {
            self.write_str("{}");
        } else {
            self.write_char(b'{');
            let mut first = true;
            for (key, item) in object.iter() {
                if !is_first(&mut first) {
                    self.write_char(b',');
                }

                self.serialize_string(key);
                self.write_char(b':');
                self.serialize_core(item);
            }
            self.write_char(b'}');
        }
    }

    fn serialize_core(&mut self, value: &Value) {
        match value {
            &Value::Null => self.write_str("null"),
            &Value::Boolean(true) => self.write_str("true"),
            &Value::Boolean(false) => self.write_str("false"),
            &Value::Number(ref value) => self.write_str(&value.to_string()),
            &Value::String(ref value) => self.serialize_string(value),
            &Value::Array(ref array) => self.serialize_array(array),
            &Value::Object(ref object) => self.serialize_object(object),
        }
    }
}

impl JsonSerializer<()> {
    fn serialize_to_string(value: &Value) -> String {
        let mut buffer = Vec::<u8>::new();
        JsonSerializer::new(&mut buffer).serialize_core(value);
        String::from_utf8(buffer).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Ordering;

    #[test]
    fn test_object_comparison() {
        let obj1 = object_from_iter(vec![("a", 1)]);
        let obj2 = object_from_iter(vec![("a", Value::Null)]);

        assert_eq!(obj1, obj1);
        assert_eq!(obj1.partial_cmp(&obj2), Some(Ordering::Greater));
    }

    #[test]
    fn test_value_from() {
        assert_eq!(Value::from(true), Value::Boolean(true));
        assert_eq!(Value::from(42), Value::Number(42.0));
        assert_eq!(Value::from(3.14), Value::Number(3.14));
    }

    #[test]
    fn test_input_getc() {
        let source = "12";
        let mut input = Input::new(source);
        assert_eq!(input.getc(), Some('1'));
        assert_eq!(input.getc(), Some('2'));
        assert_eq!(input.getc(), None);
    }

    #[test]
    fn test_parse_number() {
        assert_eq!(parse_string("1"), Ok(Value::Number(1.0)));
        assert_eq!(parse_string("-3.14"), Ok(Value::Number(-3.14)));
        assert_eq!(parse_string("1e-9"), Ok(Value::Number(1e-9)));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse_string(r#""hello""#),
            Ok(Value::String("hello".to_string()))
        );

        // Empty.
        assert_eq!(parse_string(r#""""#), Ok(Value::String("".to_string())));

        // Escapse sequences.
        assert_eq!(
            parse_string(r#" "Hello,\r\n\tworld!" "#),
            Ok(Value::String("Hello,\r\n\tworld!".to_string()))
        );

        // Unknown escapse sequence.
        assert!(parse_string(r#" "\q" "#).is_err());

        // Not closed.
        assert!(parse_string(r#" "hello-- "#).is_err());
    }

    #[test]
    fn test_parse_string_as_array() {
        assert_eq!(parse_string("[]"), Ok(Value::array()));
        assert_eq!(
            parse_string("[true, false, true]"),
            Ok(Value::from(vec![true, false, true]))
        );
    }

    #[test]
    fn test_parse_object_empty() {
        assert_eq!(parse_string("{}"), Ok(Value::object()));
    }

    #[test]
    fn test_parse_object_simple() {
        let json = r#"{
            "user_id": 42,
            "name": "John Doe"
        }"#;
        let expected = Value::from(vec![
            ("user_id", Value::from(42)),
            ("name", Value::from("John Doe")),
        ]);
        assert_eq!(parse_string(json), Ok(expected));
    }

    #[test]
    fn test_parse_object_nested() {
        let json = r#"{
            "array": [1, 2, 3],
            "object": { "foo": true }
        }"#;
        let expected = Value::from(vec![
            ("array", Value::from(vec![1, 2, 3])),
            ("object", Value::from(vec![("foo", true)])),
        ]);
        assert_eq!(parse_string(json), Ok(expected));
    }

    #[test]
    fn test_parse_object_rejects_dangling_comma() {
        assert!(parse_string(r#"{ "foo": true, }"#).is_err());
    }

    #[test]
    fn test_parse_bool_ok() {
        assert_eq!(parse_string("true"), Ok(Value::Boolean(true)));
        assert_eq!(parse_string("false"), Ok(Value::Boolean(false)));

        // Ignore leading whitespaces.
        assert_eq!(parse_string("\t\n\r  true"), Ok(Value::Boolean(true)));
    }

    #[test]
    fn test_parse_bool_err() {
        // Typo.
        assert!(parse_string("ture").is_err());

        // Case sensitive.
        assert!(parse_string("TRUE").is_err());
    }

    #[test]
    fn test_serialize_string_simple() {
        assert_eq!(Value::from("hello").serialize(), r#""hello""#);
    }

    #[test]
    fn test_serialize_string_escaped() {
        let source = "\"\"\"\n\tHello!\n\"\"\"";
        let expected = r#""\"\"\"\n\tHello!\n\"\"\"""#;
        assert_eq!(Value::from(source).serialize(), expected);
    }

    #[test]
    #[ignore]
    fn test_serialize_string_unicode() {
        assert_eq!(Value::from("你好").serialize(), r#""\u4f60\u597d""#);
    }
}

#[cfg(test)]
mod ported_tests {
    use super::*;
    use std::*;

    #[test]
    fn test_constructor() {
        let table = vec![
            (Value::from(true), "true"),
            (Value::from(false), "false"),
            (Value::from(42.0), "42"),
            (Value::from("hello"), "\"hello\""),
        ];
        for (value, json) in table {
            let actual = value.serialize();
            assert_eq!(actual, json);
        }
    }

    #[test]
    fn test_double_reserialization() {
        /// Serialize and deserialize a number.
        fn f(r: f64) -> f64 {
            let json = Value::from(r).serialize();
            let value = parse_string(&json).unwrap();
            value.as_number().cloned().unwrap()
        }

        for n in 1..53 {
            let x = (1_i64 << n) as f64;
            assert_eq!(f(x), x);
        }

        for n in 53..1024 {
            let x = 1_f64.powf(n as f64);
            let y = f(x);
            assert!((x - y).abs() / y <= 1e-8);
        }
    }

    #[test]
    #[cfg(not_impl)]
    fn test_correct_output() {
        fn test<T>(source: String, expected: T) {
            let value = parse_string(source).unwrap();
            assert!(value.is_of<T>())
            // value: has type T
            // parsed completely
        }

        test("false", false);
        test("true", true);
        test("90.5", 90.5_f64);
        test("1.7976931348623157e+308", 1.7976931348623157e+308_f64);
        test("\"hello\"", "hello".to_string());
        test("\"\\\"\\\\\\/\\n\\r\\t\"", "\"\\/\n\r\t".to_string());
    }

    #[test]
    #[cfg(not_impl)]
    fn test_value_is_empty() {
        assert!(parse_string("[]").expect("Parse success").is_empty());
        assert!(parse_string("{}").expect("Parse success").is_empty());
    }

    #[test]
    #[cfg(not_impl)]
    fn test_value_array() {
        let a = parse_string("[1,true,\"hello,\"]").expect("Should parse an array");

        assert_eq!(a.as_array().unwrap().unwrap().len(), 3);

        assert_eq!(a.contains_key(0), true, "First element should exist.");
        assert_eq!(a.get(0).unwrap().as_number().unwrap(), 1.0);

        assert_eq!(a.contains_key(1), true, "Second element should exist.");
        assert_eq!(a.get(1).unwrap().as_bool().unwrap(), true);

        assert_eq!(a.contains_key(2), true, "Third element should exist.");
        assert_eq!(a.get(2).unwrap().as_string().unwrap(), "hello".to_string());

        assert!(!a.contains_key(3));
    }

    #[test]
    #[cfg(not_impl)]
    fn test_value_object() {
        let v = parse_string(r#"{ "a": true }"#).expect("Should parse an object");

        assert_eq!(v.as_object().unwrap().unwrap().len(), 1);
        assert!(v.contains_key("a"), true, "Should has a as key.");
        assert!(v.get("a").unwrap(), true);

        assert!(!a.contains_key("z"));
    }

    #[test]
    #[cfg(not_impl)]
    fn test_value_object_modification() {
        let v = Value::object();

        *(v.get_mut("foo").unwrap()) = Value::from("bar".to_string());

        v.insert("hoge", Value::array());
        v.get_mut("hoge").unwrap().push(Value::from(42.0));

        v.insert("baz", Value::object());
        let v2 = v.get_mut("baz").unwrap();
        v2.insert("piyo", Value::from(3.14));

        let json = v.serialize();
        assert_eq!(json, r#"{"foo":"bar","hoge":[42],"baz":{"piyo":3.14}}"#);
    }

    #[test]
    #[cfg(not_impl)]
    fn test_error_message() {
        fn test(source: &str, line: i32, near: &str) {
            let actual = parse_string(source).expect_err("Expected an error.");
            let expected = format!("Syntax error at line {} near: {}", line, near);
            assert!(actual, expected);
        }

        test("falsoa", 1, "oa");
        test("{]", 1, "]");
        test("\n\ttab", 2, "tab");
        test("\"abc\nd\"", 1, "");
    }

    #[test]
    fn test_deep_comparison_equal() {
        let l = parse_string(r#"{ "b": true, "a": [1, 2, "three"], "d": 2 }"#);
        let r = parse_string(r#"{ "d": 2.0, "b": true, "a": [1, 2, "three"] }"#);
        assert_eq!(l, r);
    }

    #[test]
    #[should_panic]
    fn test_deep_comparison_not_equal_array() {
        let l = parse_string(r#"{ "b": true, "a": [1, 2, "three"], "d": 2 }"#);
        let r = parse_string(r#"{ "b": true, "a": [1,    "three"], "d": 2 }"#);
        assert_eq!(l, r);
    }

    #[test]
    #[should_panic]
    fn test_deep_comparison_not_equal_boolean() {
        let l = parse_string(r#"{ "b": true, "a": [1, 2, "three"], "d": 2 }"#);
        let r = parse_string(r#"{ "b":false, "a": [1, 2, "three"], "d": 2 }"#);
        assert_eq!(l, r);
    }

    #[test]
    #[cfg(not_impl)]
    fn test_erase() {
        let obj = parse_string(r#"{ "b": true, "a": [1, 2, "three"], "d": 2 }"#).unwrap();
        obj.erase("b");
        let expected = parse_string(r#"{ a": [1, 2, "three"], "d": 2 }"#).unwrap();
        assert_eq!(obj, expected);
    }

    #[test]
    fn test_serialize_integer() {
        assert_eq!(Value::from(2.0).serialize(), "2");
    }

    fn serialization_sample() -> Value {
        parse_string(
            r#"{
            "a": 1,
            "b": [2, { "b1": "abc" }],
            "c": {},
            "d": []
        }"#,
        ).unwrap()
    }

    #[test]
    fn test_serialize_object_minimum() {
        let actual = serialization_sample().serialize();
        assert_eq!(actual, r#"{"a":1,"b":[2,{"b1":"abc"}],"c":{},"d":[]}"#);
    }

    #[test]
    #[ignore]
    fn test_serialize_object_pretty() {
        let actual = serialization_sample().pretty_print();
        let expected = r#"{
  "a": 1,
  "b": [
    2,
    {
      "b1": "abc"
    }
  ],
  "c": {},
  "d": []
  }"#;
        assert_eq!(actual, expected);
    }

    #[test]
    #[ignore]
    #[should_panic]
    fn test_reject_nan() {
        Value::from(f64::NAN);
    }

    #[test]
    #[ignore]
    #[should_panic]
    fn test_reject_infinity() {
        Value::from(f64::INFINITY);
    }

    #[test]
    fn test_cast() {
        assert_eq!(Value::from(3.14).as_bool(), None);
    }

    #[test]
    #[cfg(not_impl)]
    fn test_simple_api() {
        let v = parse_string(r#"[ 1, "abc" ]"#).unwrap();
        let a = v.as_array().unwrap();
        assert_eq!(a.len(), 2);
        assert_eq!(a[0].as_number().unwrap(), 1);
        assert_eq!(a[1].as_string().unwrap(), "abc");
    }
}
