use chrono::{Date, DateTime, NaiveDateTime, NaiveTime, Utc};
use std::collections::HashMap;
use theta::avro::{FromAvro, ToAvro};
use nom::{IResult, Err, error::{context, ErrorKind}};
use uuid::{Uuid};

#[derive(Clone, Debug, PartialEq)]
pub enum SimpleEnum {
    Simple,
}

impl ToAvro for SimpleEnum {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        match self {
            SimpleEnum::Simple => 0i64.to_avro_buffer(buffer),
        }
    }
}

impl FromAvro for SimpleEnum {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
        context("enums.SimpleEnum", |input| {
            let (input, tag) = i64::from_avro(input)?;
            match tag {
                0 => Ok((input, SimpleEnum::Simple)),
                _ => Err(Err::Error((input, ErrorKind::Tag))),
            }
        })(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TrickyEnum {
    Sym,
    Sym_,
    Sym__,
}

impl ToAvro for TrickyEnum {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        match self {
            TrickyEnum::Sym => 0i64.to_avro_buffer(buffer),
            TrickyEnum::Sym_ => 1i64.to_avro_buffer(buffer),
            TrickyEnum::Sym__ => 2i64.to_avro_buffer(buffer),
        }
    }
}

impl FromAvro for TrickyEnum {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
        context("enums.TrickyEnum", |input| {
            let (input, tag) = i64::from_avro(input)?;
            match tag {
                0 => Ok((input, TrickyEnum::Sym)),
                1 => Ok((input, TrickyEnum::Sym_)),
                2 => Ok((input, TrickyEnum::Sym__)),
                _ => Err(Err::Error((input, ErrorKind::Tag))),
            }
        })(input)
    }
}
