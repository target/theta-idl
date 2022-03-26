use chrono::{Date, DateTime, NaiveTime, Utc};
use std::collections::HashMap;
use theta::avro::{FromAvro, ToAvro};
use nom::{IResult, Err, error::{context, ErrorKind}};
use uuid::{Uuid};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Newtype(pub i32);

impl ToAvro for Newtype {
  fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
    self.0.to_avro_buffer(buffer);
  }
}

impl FromAvro for Newtype {
  fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
    context("newtype.Newtype", |input| {
      let (input, value) = i32::from_avro(input)?;
      Ok((input, Newtype(value)))
    })(input)
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NewtypeRecord {
  pub foo: newtype::Newtype,
}

impl ToAvro for NewtypeRecord {
  fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
     self.foo.to_avro_buffer(buffer);
  }
}

impl FromAvro for NewtypeRecord {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
        context("newtype.NewtypeRecord", |input| {
            let (input, foo) = newtype::Newtype::from_avro(input)?;
            Ok((input, NewtypeRecord { foo: foo }))
        })(input)
    }
}
