/*

TODOs:

- [ ] parse string
    - [ ] \u+FFFF
    - [ ] \b, \f
- [ ] error reporting
- [ ] serialize
- [ ] input::cur, line
- [ ] i64 support
- [ ] support file stream
- [ ] methods of Value
- [ ] test codes from picojson
- [ ] helper methods to build object
- [ ] partial_cmp for obj
- [ ] refactoring

*/

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::iter::FromIterator;

// static indent_width: i32 = 2;

type Error = String;

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
pub enum Type {
    Null,
    Boolean,
    Number,
    String,
    Array,
    Object,
}

pub enum SerializeStyle {
    Minimum,
    Pretty,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Array(Vec<Value>);

impl Array {
    pub fn new() -> Array {
        Array(Vec::new())
    }

    pub fn unwrap(&self) -> &Vec<Value> {
        &self.0
    }

    pub fn unwrap_mut(&mut self) -> &mut Vec<Value> {
        &mut self.0
    }
}

impl IntoIterator for Array {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> std::vec::IntoIter<Value> {
        let Array(vec) = self;
        vec.into_iter()
    }
}

impl PartialOrd for Array {
    fn partial_cmp(&self, other: &Array) -> Option<Ordering> {
        let (&Array(ref ls), &Array(ref rs)) = (self, other);

        // Lexicographical ordering.
        if let Some(ordering) = ls.len().partial_cmp(&rs.len()) {
            return Some(ordering);
        }

        for (l, r) in ls.iter().zip(rs) {
            if let Some(ordering) = l.partial_cmp(r) {
                return Some(ordering);
            }
        }

        Some(Ordering::Equal)
    }
}

impl<V: Into<Value>> FromIterator<V> for Array {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Array {
        Array(iter.into_iter().map(|v| v.into()).collect())
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Object(BTreeMap<String, Value>);

impl Object {
    pub fn new() -> Object {
        Object(BTreeMap::new())
    }

    // fn unwrap(&self) -> &BTreeMap<String, Value> {
    //     &self.0
    // }

    fn unwrap_mut(&mut self) -> &mut BTreeMap<String, Value> {
        &mut self.0
    }

    pub fn insert<K: ToString, V>(&mut self, key: K, value: V) -> Option<Value>
    where
        String: From<K>,
        Value: From<V>,
    {
        self.unwrap_mut()
            .insert(From::from(key), Value::from(value))
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Object) -> Option<Ordering> {
        let (&Object(ref lt), &Object(ref rt)) = (self, other);

        if lt == rt {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

impl<K: ToString, V: Into<Value>> FromIterator<(K, V)> for Object {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Object {
        Object(BTreeMap::from_iter(
            iter.into_iter().map(|(k, v)| (k.to_string(), v.into())),
        ))
    }
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

impl Value {
    pub fn null() -> Value {
        Value::Null
    }

    pub fn array() -> Value {
        Value::Array(Array::new())
    }

    pub fn object() -> Value {
        Value::Object(Object::new())
    }

    fn from<T>(value: T) -> Value
    where
        Value: From<T>,
    {
        From::from(value)
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            &Value::Boolean(value) => Some(value),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match self {
            &Value::Number(number) => Some(number),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            &Value::String(ref s) => Some(s),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Array> {
        match self {
            &Value::Array(ref array) => Some(array),
            _ => None,
        }
    }

    pub fn as_array_mut(&mut self) -> Option<&mut Array> {
        match self {
            &mut Value::Array(ref mut array) => Some(array),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<&Object> {
        match self {
            &Value::Object(ref obj) => Some(obj),
            _ => None,
        }
    }

    pub fn as_object_mut(&mut self) -> Option<&mut Object> {
        match self {
            &mut Value::Object(ref mut obj) => Some(obj),
            _ => None,
        }
    }

    pub fn serialize(&self, _mode: SerializeStyle) -> String {
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

macro_rules! impl_from_for_value {
    ($t:ty, $f:expr) => {
        impl From<$t> for Value {
            fn from(value: $t) -> Self {
                $f(value)
            }
        }
    };
}

impl_from_for_value!(bool, Value::Boolean);
impl_from_for_value!(f64, Value::Number);
impl_from_for_value!(String, Value::String);
impl_from_for_value!(Array, Value::Array);
impl_from_for_value!(Object, Value::Object);

impl_from_for_value!(i32, |value: i32| Value::Number(value as f64));

impl<V: Into<Value>> FromIterator<V> for Value {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Value::Array(<Array as FromIterator<V>>::from_iter(iter))
    }
}

impl<K: ToString, V: Into<Value>> FromIterator<(K, V)> for Value {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Value::Object(<Object as FromIterator<(K, V)>>::from_iter(iter))
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

        let vec = self.out.as_array_mut().unwrap().unwrap_mut();
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

        let obj = self.out.as_object_mut().unwrap().unwrap_mut();
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
                    Some('/') => {
                        out.push('/');
                    }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn array<V: Into<Value>, I: IntoIterator<Item = V>>(iter: I) -> Array {
        iter.into_iter().collect()
    }

    fn obj<K: ToString, V: Into<Value>, I: IntoIterator<Item = (K, V)>>(iter: I) -> Object {
        iter.into_iter().collect()
    }

    #[test]
    fn test_object_comparison() {
        let obj1 = obj(vec![("a", 1)]);
        let obj2 = obj(vec![("a", Value::Null)]);

        assert_eq!(obj1, obj1);
        assert_eq!(obj1.partial_cmp(&obj2), None);
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
        assert_eq!(parse_string("[]"), Ok(Value::Array(Array::new())));
        assert_eq!(
            parse_string("[true, false, true]"),
            Ok(Value::Array(array(vec![true, false, true])))
        );
    }

    #[test]
    fn test_parse_object_empty() {
        assert_eq!(parse_string("{}"), Ok(Value::Object(Object::new())));
    }

    #[test]
    fn test_parse_object_simple() {
        let json = r#"{
            "user_id": 42,
            "name": "John Doe"
        }"#;
        let expected = Value::from(obj(vec![
            ("user_id", Value::from(42)),
            ("name", Value::String("John Doe".to_string())),
        ]));
        assert_eq!(parse_string(json), Ok(expected));
    }

    #[test]
    fn test_parse_object_nested() {
        let json = r#"{
            "array": [1, 2, 3],
            "object": { "foo": true }
        }"#;
        let expected = Value::from(obj(vec![
            ("array", Value::Array(array(vec![1, 2, 3]))),
            (
                "object",
                Value::Object(obj(vec![("foo", Value::from(true))])),
            ),
        ]));
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
}

#[cfg(test)]
mod ported_tests {
    use super::*;
    use std::*;

    // [ ] constructors
    // [ ] parse
    // [ ] array
    // [ ] object
    // [ ] equality
    // [ ] swap
    // [ ] serialize pretty print
    // [ ] reject NaN/Inf

    // [ ] unicode string

    #[test]
    #[ignore]
    fn test_construtor() {
        let table = vec![
            (Value::from(true), "true"),
            (Value::from(false), "false"),
            (Value::from(42.0), "42"),
            (Value::from("hello".to_string()), "\"hello\""),
        ];
        for (value, json) in table {
            let actual = value.serialize(SerializeStyle::Minimum);
            assert_eq!(actual, json);
        }
    }

    #[test]
    #[ignore]
    fn test_double_reserialization() {
        /// Serialize and deserialize a number.
        fn f(r: f64) -> f64 {
            let json = Value::from(r).serialize(SerializeStyle::Minimum);
            let value = parse_string(&json).unwrap();
            value.as_number().unwrap()
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
        let v = parse_string(r#"{"a": true }"#).expect("Should parse an object");

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

        let json = v.serialize(SerializeStyle::Minimum);
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
    #[ignore]
    fn test_serialize_integer() {
        assert_eq!(Value::from(2.0).serialize(SerializeStyle::Minimum), "2.0");
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
    #[ignore]
    fn test_serialize_object_minimum() {
        let actual = serialization_sample().serialize(SerializeStyle::Minimum);
        assert_eq!(actual, r#"{"a":1,"b":[2,{"b1":"abc"}],"c":{},"d":[]}"#);
    }

    #[test]
    #[ignore]
    fn test_serialize_object_pretty() {
        let actual = serialization_sample().serialize(SerializeStyle::Pretty);
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
