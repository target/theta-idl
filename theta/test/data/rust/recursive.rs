use chrono::{Date, DateTime, Utc};
use std::collections::HashMap;
use theta::avro::{FromAvro, ToAvro};
use nom::{IResult, Err, error::{context, ErrorKind}};
use uuid::{Uuid};

#[derive(Clone, Debug, PartialEq)]
pub struct AMutual(pub recursive::MutualA);

impl ToAvro for AMutual {
  fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
    self.0.to_avro_buffer(buffer);
  }
}

impl FromAvro for AMutual {
  fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
    context("recursive.AMutual", |input| {
      let (input, value) = recursive::MutualA::from_avro(input)?;
      Ok((input, AMutual(value)))
    })(input)
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MutualA {
    pub mutual: Box<recursive::MutualB>,
}

impl ToAvro for MutualA {
  fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
     self.mutual.to_avro_buffer(buffer);
  }
}

impl FromAvro for MutualA {
  fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
    context("recursive.MutualA", |input| {
      let (input, mutual) = recursive::MutualB::from_avro(input)?;
      Ok((input, MutualA { mutual: Box::new(mutual) }))
    })(input)
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MutualB {
    pub mutual: Box<recursive::Wrapper>,
}

impl ToAvro for MutualB {
  fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
     self.mutual.to_avro_buffer(buffer);
  }
}

impl FromAvro for MutualB {
  fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
    context("recursive.MutualB", |input| {
      let (input, mutual) = recursive::Wrapper::from_avro(input)?;
      Ok((input, MutualB { mutual: Box::new(mutual) }))
    })(input)
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Recursive {
    Nil {
    },
    Recurse {
        contents: i32,
        boxed: Box<recursive::Recursive>,
        unboxed: HashMap<String, Vec<recursive::Recursive>>,
    },
}

impl ToAvro for Recursive {
  fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
    match self {
      Recursive::Nil {  } => {
        0i64.to_avro_buffer(buffer);
      },

      Recursive::Recurse { contents, boxed, unboxed } => {
        1i64.to_avro_buffer(buffer);
        contents.to_avro_buffer(buffer);
        boxed.to_avro_buffer(buffer);
        unboxed.to_avro_buffer(buffer);
      },
    };
  }
}

impl FromAvro for Recursive {
  fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
    context("recursive.Recursive", |input| {
      let (input, tag) = i64::from_avro(input)?;
      match tag {
        0 => {
          Ok((input, Recursive::Nil {  }))
        },
        1 => {
          let (input, contents) = i32::from_avro(input)?;
          let (input, boxed) = recursive::Recursive::from_avro(input)?;
          let (input, unboxed) = HashMap::from_avro(input)?;
          Ok((input, Recursive::Recurse { contents: contents, boxed: Box::new(boxed), unboxed: unboxed }))
        },
        _ => Err(Err::Error((input, ErrorKind::Tag))),
      }
    })(input)
  }
}

pub type Wrapper = Option<recursive::AMutual>;
