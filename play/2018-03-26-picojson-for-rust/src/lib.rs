/*

TODOs:

- [ ] parse string
    - [ ] \u+FFFF
    - [ ] \b, \f
- [ ] error reporting
- [ ] serialize
- [ ] input::cur, line
- [ ] i64 support
- [ ] methods of Value
- [ ] test codes from picojson
- [ ] helper methods to build object
- [ ] partial_cmp for obj
- [ ] refactoring

*/

use std::collections::BTreeMap;
use std::cmp::Ordering;

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
    fn null() -> Value {
        Value::Null
    }

    fn from<T>(value: T) -> Value
    where
        Value: From<T>,
    {
        From::from(value)
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
}

macro_rules! impl_from_for_value {
    ($t: ty, $f: expr) => {
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

    #[test]
    fn test_object_comparison() {
        let obj1 = {
            let mut obj = Object::new();
            obj.insert("a", 1);
            obj
        };

        let obj2 = {
            let mut obj = Object::new();
            obj.insert("a", Value::null());
            obj
        };

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
            Ok(Value::Array(Array(
                vec![true, false, true]
                    .into_iter()
                    .map(|b| Value::Boolean(b))
                    .collect::<Vec<_>>()
            )))
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
        let expected = {
            let mut obj = Object::new();
            obj.insert("user_id", Value::Number(42.0));
            obj.insert("name", Value::String("John Doe".to_string()));
            Value::Object(obj)
        };
        assert_eq!(parse_string(json), Ok(expected));
    }

    #[test]
    fn test_parse_object_nested() {
        let json = r#"{
            "array": [1, 2, 3],
            "object": { "foo": true }
        }"#;
        let expected = {
            let mut obj = Object::new();
            obj.insert(
                "array",
                Value::Array(Array((1..4).map(From::from).collect::<Vec<_>>())),
            );
            obj.insert("object", {
                let mut obj = Object::new();
                obj.insert("foo", Value::Boolean(true));
                Value::Object(obj)
            });
            Value::Object(obj)
        };
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
