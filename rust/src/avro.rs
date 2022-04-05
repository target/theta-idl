use std::cmp::min;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::marker::Sized;

use chrono::{Date, DateTime, Datelike, NaiveDate, NaiveDateTime, NaiveTime, Utc};
use integer_encoding::VarInt;
use time::Duration;
use uuid::Uuid;

use nom::{
    branch::alt,
    bytes::streaming::{tag, take, take_till},
    combinator::{map, map_res},
    error::{context, ErrorKind},
    multi::{count, many_till},
    number::streaming::{le_u32, le_u64, le_u8},
    Err, IResult,
};

// ToAvro

/// A trait for types that can be serialized directly to the Avro
/// binary format.
pub trait ToAvro {
    /// Convert this value to Avro's binary format, pushing it to the
    /// end of the given buffer.
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>);

    /// Convert this value to Avro's binary format, returning a fresh
    /// vector with the content written.
    ///
    /// You can override this method to be more efficient by
    /// initializing the returned vector at a size that will fit your
    /// encoded value.
    fn to_avro(&self) -> Vec<u8> {
        let mut buffer = Vec::new();

        self.to_avro_buffer(&mut buffer);

        buffer
    }
}

// Instances for primitive types

impl ToAvro for bool {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        if *self {
            buffer.push(1);
        } else {
            buffer.push(0);
        }
    }
}

impl ToAvro for Vec<u8> {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let length = self.len() as i64;
        length.to_avro_buffer(buffer);

        buffer.append(&mut self.clone());
    }
}

impl ToAvro for i32 {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        buffer.append(&mut self.encode_var_vec())
    }

    fn to_avro(&self) -> Vec<u8> {
        self.encode_var_vec()
    }
}

impl ToAvro for i64 {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        buffer.append(&mut self.encode_var_vec())
    }

    fn to_avro(&self) -> Vec<u8> {
        self.encode_var_vec()
    }
}

impl ToAvro for f32 {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        buffer.append(&mut self.to_avro())
    }

    fn to_avro(&self) -> Vec<u8> {
        self.to_bits().to_le_bytes().to_vec()
    }
}

impl ToAvro for f64 {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        buffer.append(&mut self.to_avro())
    }

    fn to_avro(&self) -> Vec<u8> {
        self.to_bits().to_le_bytes().to_vec()
    }
}

impl ToAvro for String {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let string_bytes = &mut self.as_bytes().to_vec();
        let length = string_bytes.len() as i64;

        length.to_avro_buffer(buffer);
        buffer.append(string_bytes);
    }
}

impl ToAvro for Date<Utc> {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let epoch = NaiveDate::from_ymd(1970, 1, 1);
        let from_ce = self.num_days_from_ce();
        let from_epoch = from_ce - epoch.num_days_from_ce();

        from_epoch.to_avro_buffer(buffer);
    }
}

impl ToAvro for DateTime<Utc> {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let epoch = DateTime::from_utc(NaiveDate::from_ymd(1970, 1, 1).and_hms(0, 0, 0), Utc);
        let from_epoch = (*self - epoch).num_microseconds();

        match from_epoch {
            Some(value) => value.to_avro_buffer(buffer),

            // This is a panic because it represents a bug in our implementation.
            None => panic!("Microsecond time value overflow when parsing DateTime."),
        }
    }
}

impl ToAvro for Uuid {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        self.to_hyphenated().to_string().to_avro_buffer(buffer);
    }
}

impl ToAvro for NaiveTime {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let midnight = NaiveTime::from_hms(0, 0, 0);
        let max_micros = (24 * 60 * 60 * 1_000_000) - 1;
        match (*self - midnight).num_microseconds() {
            Some(since_midnight) =>
                min(since_midnight, max_micros).to_avro_buffer(buffer),

            // This is a panic because it represents a bug in our implementation.
            None => panic!("Microsecond time value overflowed when parsing Time.")
        }
    }
}

impl ToAvro for NaiveDateTime {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let epoch = NaiveDate::from_ymd(1970, 1, 1).and_hms(0, 0, 0);
        let from_epoch = (*self - epoch).num_microseconds();

        match from_epoch {
            Some(value) => value.to_avro_buffer(buffer),

            // This is a panic because it represents a bug in our implementation.
            None => panic!("Microsecond time value overflow when parsing DateTime."),
        }
    }
}

/// Arrays are currently always encoded as a single block.
impl<A: ToAvro> ToAvro for Vec<A> {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let length = self.len() as i64;

        if length > 0 {
            length.to_avro_buffer(buffer);
            for item in self {
                item.to_avro_buffer(buffer);
            }
        }

        buffer.push(0); // mark end of array
    }
}

/// Maps are currently always encoded as a single block.
impl<A: ToAvro> ToAvro for HashMap<String, A> {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        let length = self.len() as i64;

        if length > 0 {
            length.to_avro_buffer(buffer);
            let mut pairs = self.iter().collect::<Vec<_>>();
            pairs.sort_by_key(|k| k.0);
            for (key, value) in pairs {
                key.to_avro_buffer(buffer);
                value.to_avro_buffer(buffer);
            }
        }

        buffer.push(0); // mark end of map
    }
}

impl<A: ToAvro> ToAvro for Option<A> {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        match self {
            None => buffer.push(0),
            Some(a) => {
                1.to_avro_buffer(buffer);
                a.to_avro_buffer(buffer);
            }
        }
    }
}

// FromAvro

pub trait FromAvro: Sized {
    /// A parser that consumes an array of bytes and produces a value
    /// of the given type.
    fn from_avro(input: &[u8]) -> IResult<&[u8], Self>;
}

// Instances for primitive types

impl FromAvro for bool {
    fn from_avro(input: &[u8]) -> IResult<&[u8], bool> {
        context(
            "Boolean",
            alt((map(tag([0]), |_| false), map(tag([1]), |_| true))),
        )(input)
    }
}

impl FromAvro for Vec<u8> {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
        context("Bytes", |input| {
            let (input, length) = i64::from_avro(input)?;

            if length >= 0 {
                map(take(length as usize), |slice: &[u8]| slice.to_vec())(input)
            } else {
                Err(Err::Error((input, ErrorKind::Tag)))
            }
        })(input)
    }
}

impl FromAvro for i32 {
    fn from_avro(input: &[u8]) -> IResult<&[u8], i32> {
        context("Int", |input| {
            let mut vec = vec![];

            let (input, bytes) = take_till(|byte| byte & 0x80 == 0)(input)?;
            let (input, end) = le_u8(input)?;

            vec.extend_from_slice(bytes);
            vec.push(end);

            let (i, _) = i32::decode_var(&vec);

            Ok((input, i))
        })(input)
    }
}

impl FromAvro for i64 {
    fn from_avro(input: &[u8]) -> IResult<&[u8], i64> {
        context("Long", |input| {
            let mut vec = vec![];

            let (input, bytes) = take_till(|byte| byte & 0x80 == 0)(input)?;
            let (input, end) = le_u8(input)?;

            vec.extend_from_slice(bytes);
            vec.push(end);

            let (i, _) = i64::decode_var(&vec);

            Ok((input, i))
        })(input)
    }
}

impl FromAvro for f32 {
    fn from_avro(input: &[u8]) -> IResult<&[u8], f32> {
        context("Float", map(le_u32, |bits| f32::from_bits(bits)))(input)
    }
}

impl FromAvro for f64 {
    fn from_avro(input: &[u8]) -> IResult<&[u8], f64> {
        context("Double", map(le_u64, |bits| f64::from_bits(bits)))(input)
    }
}

impl FromAvro for String {
    fn from_avro(input: &[u8]) -> IResult<&[u8], String> {
        context("String", |input| {
            let (input, length) = i64::from_avro(input)?;

            if length >= 0 {
                map_res(take(length as usize), |slice: &[u8]| {
                    String::from_utf8(slice.to_vec())
                })(input)
            } else {
                Err(Err::Error((input, ErrorKind::Tag)))
            }
        })(input)
    }
}

impl FromAvro for Date<Utc> {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Date<Utc>> {
        context("Date", |input| {
            let (input, days) = i32::from_avro(input)?;

            let offset = NaiveDate::from_ymd(1970, 1, 1).num_days_from_ce();
            let naive = NaiveDate::from_num_days_from_ce(days + offset);

            Ok((input, Date::from_utc(naive, Utc)))
        })(input)
    }
}

impl FromAvro for DateTime<Utc> {
    fn from_avro(input: &[u8]) -> IResult<&[u8], DateTime<Utc>> {
        context("DateTime", |input| {
            let (input, microseconds) = i64::from_avro(input)?;
            let epoch = DateTime::from_utc(NaiveDate::from_ymd(1970, 1, 1).and_hms(0, 0, 0), Utc);
            Ok((input, epoch + Duration::microseconds(microseconds)))
        })(input)
    }
}

impl FromAvro for Uuid {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Uuid> {
        context("UUID", |input| {
            let (input, string) = String::from_avro(input)?;
            match Uuid::parse_str(&string) {
                Ok(uuid) => Ok((input, uuid)),
                Err(_) => Err(Err::Error((input, ErrorKind::Tag))),
            }
        })(input)
    }
}

impl FromAvro for NaiveTime {
    fn from_avro(input: &[u8]) -> IResult<&[u8], NaiveTime> {
        context("Time", |input| {
            let (input, microseconds) = i64::from_avro(input)?;

            // Special case for leap seconds, which Theta explicitly
            // does not support:
            let microseconds = min(microseconds, (24 * 60 * 60 * 1_000_000) - 1);

            let midnight = NaiveTime::from_hms(0, 0, 0);
            Ok((input, midnight + Duration::microseconds(microseconds)))
        })(input)
    }
}

impl FromAvro for NaiveDateTime {
    fn from_avro(input: &[u8]) -> IResult<&[u8], NaiveDateTime> {
        context("LocalDatetime", |input| {
            let (input, microseconds) = i64::from_avro(input)?;
            let epoch = NaiveDate::from_ymd(1970, 1, 1).and_hms(0, 0, 0);
            Ok((input, epoch + Duration::microseconds(microseconds)))
        })(input)
    }
}

impl<A: FromAvro + Clone> FromAvro for Vec<A> {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Vec<A>> {
        context("Array", decode_blocks)(input)
    }
}

/// Parse a key-value pair as the key string followed by the value.
///
/// This instance is needed for the HashMap instance.
impl<A: FromAvro> FromAvro for (String, A) {
    fn from_avro(input: &[u8]) -> IResult<&[u8], (String, A)> {
        context("Map key-value pair", |input| {
            let (input, key) = String::from_avro(input)?;
            let (input, value) = A::from_avro(input)?;
            Ok((input, (key, value)))
        })(input)
    }
}

impl<A: FromAvro + Clone> FromAvro for HashMap<String, A> {
    fn from_avro(input: &[u8]) -> IResult<&[u8], HashMap<String, A>> {
        fn from_vec<A>(vec: Vec<(String, A)>) -> HashMap<String, A> {
            HashMap::from_iter(vec.into_iter())
        }

        context("Map", map(decode_blocks, from_vec))(input)
    }
}

impl<A: FromAvro> FromAvro for Option<A> {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Option<A>> {
        let parse = |input| {
            let (input, tag) = i64::from_avro(input)?;
            match tag {
                0 => Ok((input, Option::None)),
                1 => {
                    let (input, value) = A::from_avro(input)?;
                    Ok((input, Option::Some(value)))
                }
                _ => Err(Err::Error((input, ErrorKind::Tag))),
            }
        };

        context("Option", parse)(input)
    }
}

/// Decode a series of blocks terminated by an empty block.
fn decode_blocks<A>(input: &[u8]) -> IResult<&[u8], Vec<A>>
where
    A: FromAvro + Clone,
{
    context(
        "Map/Array blocks",
        map(many_till(nonempty_block, tag([0])), |(blocks, _)| {
            blocks.concat()
        }),
    )(input)
}

/// Decode a single block from an array or map, using the given parser
/// to decode each element inside the block.
///
/// Each block starts with a count of the elements in the block. A
/// series of blocks is always terminated with an empty block (encoded
/// as a 0).
fn nonempty_block<A>(input: &[u8]) -> IResult<&[u8], Vec<A>>
where
    A: FromAvro,
{
    context("Map/Array block", |input| {
        let (input, items) = i64::from_avro(input)?;

        match items {
            // Fail on empty block.
            0 => Err(Err::Error((&[0], ErrorKind::Tag))),

            // Negative items means the count of items is followed by the
            // count of *bytes* in the block. We can use this to optimize
            // the parsing process, but we ignore it for now.
            items if items < 0 => {
                let (input, _bytes) = i64::from_avro(input)?;
                count(A::from_avro, items.abs() as usize)(input)
            }

            items => count(A::from_avro, items as usize)(input),
        }
    })(input)
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use super::*;

    use quickcheck::*;

    #[test]
    fn test_bool() {
        assert_eq!(false.to_avro(), vec![0]);
        assert_eq!(true.to_avro(), vec![1]);
    }

    #[test]
    fn test_bytes() {
        let empty: Vec<u8> = vec![];
        assert_eq!(empty.to_avro(), vec![0]);

        let short: Vec<u8> = vec![0];
        assert_eq!(short.to_avro(), vec![2, 0]); // 2 is 1 in zig-zag

        let long: Vec<u8> = [0; 512].to_vec();
        let mut long_expected = vec![0x80, 0x08]; // [0x80, 0x08] is 512 in zig-zag
        long_expected.extend_from_slice(&[0; 512]);
        assert_eq!(long.to_avro(), long_expected);

        check_valid_parse(long_expected.as_slice(), long);
    }

    #[test]
    fn test_int() {
        assert_eq!(0i32.to_avro(), vec![0]);
        assert_eq!((-1i32).to_avro(), vec![1]);
        assert_eq!(1i32.to_avro(), vec![2]);
        assert_eq!(512i32.to_avro(), vec![0x80, 0x08]);
    }

    #[test]
    fn test_long() {
        assert_eq!(0i64.to_avro(), vec![0]);
        assert_eq!((-1i64).to_avro(), vec![1]);
        assert_eq!(1i64.to_avro(), vec![2]);
        assert_eq!(512i64.to_avro(), vec![0x80, 0x08]);
    }

    quickcheck! {
        fn test_float(f: f32) -> bool {
            let check_to = f.to_avro() == f.to_bits().to_le_bytes();

            let check_from = match f32::from_avro(&(f.to_bits().to_le_bytes())) {
                Ok(([], result)) => result == f,
                Ok((_, _)) => false, // did not consume all input
                Err(_) => false,
            };

            check_to && check_from
        }

        fn test_double(f: f64) -> bool {
            let check_to = f.to_avro() == f.to_bits().to_le_bytes();

            let check_from = match f64::from_avro(&(f.to_bits().to_le_bytes())) {
                Ok(([], result)) => result == f,
                Ok((_, _)) => false, // did not consume all input
                Err(_) => false,
            };

            check_to && check_from
        }
    }

    #[test]
    fn test_string() {
        // length as long followed by utf-8 encoding
        assert_eq!("foo".to_string().to_avro(), vec![0x06, 0x66, 0x6f, 0x6f]);

        let long = String::from_utf8([0x66; 512].to_vec()).unwrap();
        let mut long_expected = vec![0x80, 0x08]; // [0x80, 0x08] is 512 in zig-zag
        long_expected.extend_from_slice(&[0x66; 512]);
        assert_eq!(long.to_avro(), long_expected);
    }

    #[test]
    fn test_date() {
        let encode = |y, m, d| Date::from_utc(NaiveDate::from_ymd(y, m, d), Utc).to_avro();

        assert_eq!(encode(1970, 01, 01), vec![0]);
        assert_eq!(encode(1967, 04, 07), vec![0xcf, 0xf]); // -1000 in zig-zag
        assert_eq!(encode(1972, 09, 27), vec![0xd0, 0xf]); // 1000 in zig-zag
    }

    #[test]
    fn test_datetime() {
        let encode = |y, m, d, h| {
            DateTime::from_utc(NaiveDate::from_ymd(y, m, d).and_hms(h, 0, 0), Utc).to_avro()
        };

        assert_eq!(encode(1970, 1, 1, 0), vec![0]);
        assert_eq!(encode(1970, 1, 1, 3), vec![0x80, 0xb0, 0xd7, 0xbb, 0x50]);
        // 648000000000 (3 hours in microseconds) in zig-zag
        assert_eq!(encode(1969, 12, 31, 21), vec![0xff, 0xaf, 0xd7, 0xbb, 0x50]);
        // -648000000000 (3 hours in microseconds) in zig-zag
    }

    #[test]
    fn test_time() {
        assert_eq!(NaiveTime::from_hms(0, 0, 0).to_avro(), vec![0]);
        assert_eq!(
            NaiveTime::from_hms(1, 0, 0).to_avro(),
            (60i64 * 60 * 1_000_000).to_avro()
        );

        // Special leap second handling: round down to 23:59:59.999999
        assert_eq!(
            NaiveTime::from_hms_micro(23, 59, 59, 1_123_456).to_avro(),
            NaiveTime::from_hms_micro(23, 59, 59, 999_999).to_avro()
        )
    }

    #[test]
    fn test_array() {
        let empty: Vec<i32> = vec![];
        assert_eq!(empty.to_avro(), vec![0]);

        // length as long followed by items encoded as normal,
        // terminated with a 0 byte
        assert_eq!(vec![0i32, 1i32].to_avro(), vec![4, 0, 2, 0]);

        let long = [1i32; 512].to_vec();
        let mut long_expected = vec![0x80, 0x08]; // [0x80, 0x08] is 512 in zig-zag
        long_expected.extend_from_slice(&[0x02; 512]);
        long_expected.push(0);
        assert_eq!(long.to_avro(), long_expected);

        // Test parsing empty array
        check_valid_parse::<Vec<i32>>(&[0], vec![]);

        // Test parsing long array
        check_valid_parse::<Vec<i32>>(long_expected.as_slice(), long);
    }

    #[test]
    fn test_map() {
        let mut map = HashMap::new();
        map.insert("foo".to_string(), 512i32);

        let mut encoded = vec![0x02]; // length of first block (1 in zig-zag)
        encoded.append(&mut vec![0x06, 0x66, 0x6f, 0x6f]); // "foo"
        encoded.append(&mut vec![0x80, 0x08]); // 512
        encoded.push(0); // 0 block to terminate map
        assert_eq!(map.to_avro(), encoded);
    }

    #[test]
    fn test_optional() {
        assert_eq!(None::<i32>.to_avro(), vec![0]);
        check_valid_parse::<Option<i32>>(&[0], None);

        assert_eq!(Some(1i32).to_avro(), vec![0x02, 0x02]);
        check_valid_parse::<Option<i32>>(&[0x02, 0x02], Some(1i32));

        let invalid: &[u8] = &[0x04, 0x02]; // not an option (tag out of bounds)
        match Option::<i32>::from_avro(invalid) {
            Ok((_, result)) => assert!(
                false,
                "Parse should have failed, but returned {:?} instead.",
                result
            ),
            Err(_) => (),
        }
    }

    // fromAvro ∘ toAvro = id
    quickcheck! {
        fn prop_bool(b: bool) -> bool {
            check_encoding(b)
        }

        fn prop_bytes(b: Vec<u8>) -> bool {
            check_encoding(b)
        }

        fn prop_int(i: i32) -> bool {
            check_encoding(i)
        }

        fn prop_long(l: i64) -> bool {
            check_encoding(l)
        }

        fn prop_float(f: f32) -> bool {
            check_encoding(f)
        }

        fn prop_double(d: f64) -> bool {
            check_encoding(d)
        }

        fn prop_string(s: String) -> bool {
            check_encoding(s)
        }

        fn prop_date(days: i32) -> bool {
            let date = Date::from_utc(NaiveDate::from_num_days_from_ce(days), Utc);
            check_encoding(date)
        }

        fn prop_datetime(days: i32, h: u32, m: u32, s: u32) -> bool {
            let date = Date::from_utc(NaiveDate::from_num_days_from_ce(days), Utc);
            let datetime = date.and_hms(h % 24, m % 60, s % 60);
            check_encoding(datetime)
        }

        fn prop_uuid(bytes: u128) -> bool {
            let uuid = Uuid::from_u128(bytes);
            check_encoding(uuid)
        }

        fn prop_time(h: u32, m: u32, s: u32) -> bool {
            let time = NaiveTime::from_hms(h % 24, m % 60, s % 60);
            check_encoding(time)
        }

        fn prop_local_datetime(days: i32, h: u32, m: u32, s: u32) -> bool {
            let date = NaiveDate::from_num_days_from_ce(days);
            let datetime = date.and_hms(h % 24, m % 60, s % 60);
            check_encoding(datetime)
        }

        fn prop_array(values: Vec<i32>) -> bool {
            check_encoding(values)
        }

        fn prop_map(map: HashMap<String, i32>) -> bool {
            check_encoding(map)
        }

        fn prop_option(option: Option<i32>) -> bool {
            check_encoding(option)
        }
    }

    fn check_encoding<A: ToAvro + FromAvro + PartialEq>(value: A) -> bool {
        match FromAvro::from_avro(value.to_avro().as_slice()) {
            Ok(([], result)) => value == result,
            Ok((_, _)) => false, // did not consume all input
            Err(_) => false,
        }
    }

    // Check that the given parser parses to the given value, with no
    // errors or left-over inputs.
    fn check_valid_parse<A: PartialEq + FromAvro + Debug>(input: &[u8], expected: A) {
        match A::from_avro(input) {
            Ok(([], result)) => assert_eq!(result, expected),
            Ok((remainder, _)) => assert!(
                false,
                "Parser did not consume all input. {} bytes left over.",
                remainder.len()
            ),
            Err(_) => assert!(false, "Parser failed with error."),
        }
    }
}

#[cfg(test)]
mod benchmarks {
    use super::*;

    extern crate test;
    use test::{black_box, Bencher};

    use rand::distributions::{Distribution, Standard};
    use rand::prelude::{thread_rng, Rng};

    #[bench]
    fn array_to_avro(b: &mut Bencher) {
        b.iter(|| {
            black_box({
                generate_vec::<i64>(1_000_000).to_avro();
            });
        });
    }

    #[bench]
    fn array_to_avro_sized_buffer(b: &mut Bencher) {
        b.iter(|| {
            black_box({
                let size = 1_000_000;

                // max size of variable-encoded i64 is 10 bytes
                let mut buffer = Vec::with_capacity(size * 10);

                generate_vec::<i64>(size).to_avro_buffer(&mut buffer);
            });
        });
    }

    fn generate_vec<T>(n: usize) -> Vec<T>
    where
        Standard: Distribution<T>,
    {
        let mut rng = thread_rng();
        (0..n).map(|_| rng.gen()).collect()
    }
}
