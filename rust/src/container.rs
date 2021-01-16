#![allow(non_snake_case)]
#![allow(unused_imports)]

use crate::avro::{FromAvro, ToAvro};

use std::collections::HashMap;
use std::io::{Read, Write};

use libflate::deflate::{Decoder, Encoder};
use nom::{
    bytes::streaming::{tag, take},
    combinator::{complete, map},
    multi::{count, many0},
    Err, IResult,
};
use rand::Rng;

/// Parse all the objects defined in all the blocks of a container.
pub fn from_container<A: FromAvro + Clone>(input: &[u8]) -> IResult<&[u8], Vec<A>> {
    let (input, header) = Header::from_avro(input)?;

    let parse_block = |input| header.decode_block(input);
    let (input, blocks) = many0(complete(parse_block))(input)?;

    Ok((input, blocks.concat()))
}

/// Serialize all the given objects into a container, including the
/// given schema (which should be UTF-8-encoded JSON.
///
/// Currently, this function will put every single object into a
/// single block, but this may change in the future. Readers of the
/// container should not rely on this property.
pub fn to_container<A: ToAvro>(objects: &[A], schema: &[u8], codec: Codec) -> Vec<u8> {
    let mut buffer = Vec::new();
    to_container_buffer(objects, schema, codec, &mut buffer);
    buffer
}

/// Like `to_container` but allows specifying the avro sync marker that should
/// be used.
pub fn to_container_sync<A: ToAvro>(
    objects: &[A],
    schema: &[u8],
    codec: Codec,
    sync: &[u8],
) -> Vec<u8> {
    let mut buffer = Vec::new();
    to_container_buffer_sync(objects, schema, codec, sync, &mut buffer);
    buffer
}

/// Serialize all the given objects into a container, including the
/// given schema (which should be UTF-8-encoded JSON).
///
/// Currently, this function will put every single object into a
/// single block, but this may change in the future. Readers of the
/// container should not rely on this property.
pub fn to_container_buffer<A: ToAvro>(
    objects: &[A],
    schema: &[u8],
    codec: Codec,
    buffer: &mut Vec<u8>,
) {
    let mut rng = rand::thread_rng();

    let mut sync = Vec::<u8>::with_capacity(16);
    for _ in 0..16 {
        sync.push(rng.gen());
    }

    to_container_buffer_sync(objects, schema, codec, &sync, buffer);
}

/// Like `to_container_buffer` but allows specifying the avro sync marker that
/// should be used.
pub fn to_container_buffer_sync<A: ToAvro>(
    objects: &[A],
    schema: &[u8],
    codec: Codec,
    sync: &[u8],
    buffer: &mut Vec<u8>,
) {
    assert_eq!(
        sync.len(),
        16,
        "The sync marker must consist of 16 bytes, found {}.",
        sync.len()
    );

    let header = Header {
        sync: sync.to_vec(),
        schema: schema.to_vec(),
        codec,
    };
    header.to_avro_buffer(buffer);

    header.encode_block(objects, buffer);
}

/// The header of an Avro container. This contains the following
/// information:
///
///   * schema: the schema of objects in the container, encoded as
///   JSON in UTF-8.
///
///   * codec: either `Deflate` or `Null`, depening on whether
///   compression is enabled.
///
///   * sync: a 16-byte sequence used to separate blocks in the
///   container
///
/// The `ToAvro` and `FromAvro` instances of this type correspond to
/// how the header has to be encoded in binary container files. A full
/// container file is encoded as a `Header` in its Avro format
/// followed by any number of blocks (see the `Block` type).
#[derive(Clone, PartialEq, Debug)]
pub struct Header {
    /// The schema, encoded as a JSON string in UTF-8.
    pub schema: Vec<u8>,

    pub codec: Codec,

    /// The sync marker used to separate blocks in the container. Most
    /// of the time, this is a random sequence of 16 bytes.
    pub sync: Vec<u8>,
}

impl ToAvro for Header {
    fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
        // Magic header (Obj1)
        buffer.extend_from_slice(&[0x4f, 0x62, 0x6a, 0x01]);

        let mut map: HashMap<String, Vec<u8>> = HashMap::new();
        map.insert("avro.schema".to_string(), self.schema.clone());
        map.insert("avro.codec".to_string(), self.codec.as_bytes().to_vec());
        map.to_avro_buffer(buffer);

        buffer.append(&mut self.sync.clone());
    }
}

impl FromAvro for Header {
    fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
        // Magic header (Obj1)
        let (input, _) = tag(&[0x4f, 0x62, 0x6a, 0x01])(input)?;

        // schema and codec
        let (input, meta) = HashMap::<_, Vec<_>>::from_avro(input)?;

        // sync marker
        let (input, sync) = take(16usize)(input)?;

        let codec = match meta.get("avro.codec") {
            Some(bytes) => Codec::from_bytes(bytes).unwrap(),
            None => Codec::Null,
        };

        let schema = meta.get("avro.schema").unwrap().to_vec();

        Ok((
            input,
            Header {
                schema,
                codec,
                sync: sync.to_vec(),
            },
        ))
    }
}

impl Header {
    /// Encode a single block with any number of objects.
    ///
    /// In Avro, a block is made up of the following components in order:
    ///
    ///   1. the number of objects in the block, as an Avro long (ie zig-zag)
    ///
    ///   2. the number of bytes the objects take up as a long (after
    ///      compression, if any)
    ///
    ///   3. the objects themselves (compressed if specified)
    ///
    ///   4. the 16-byte sync marker for the container
    ///
    /// A container file is made up of a header followed by any number
    /// of blocks encoded like this.
    pub fn encode_block<A: ToAvro>(&self, objects: &[A], out_buffer: &mut Vec<u8>) {
        let count = objects.len() as i64;
        count.to_avro_buffer(out_buffer);

        let object_buffer = &mut Vec::new();
        for object in objects {
            object.to_avro_buffer(object_buffer);
        }

        match self.codec {
            Codec::Null => {
                let byte_size = object_buffer.len() as i64;
                byte_size.to_avro_buffer(out_buffer);

                out_buffer.append(object_buffer);
            }
            Codec::Deflate => {
                let mut encoder = Encoder::new(Vec::new());
                encoder.write_all(object_buffer.as_slice()).unwrap();

                let mut compressed = encoder.finish().into_result().unwrap();

                let byte_size = compressed.len() as i64;
                byte_size.to_avro_buffer(out_buffer);

                out_buffer.append(&mut compressed);
            }
        }

        out_buffer.append(&mut self.sync.clone());
    }

    /// Decode a single Avro block. See documentation on
    /// `encode_block` for details.
    ///
    /// This method returns a Nom-style parser. You can combine it
    /// with other combinators: `count(header.decode_block(),
    /// 10)(input)`.
    pub fn decode_block<'a, A: FromAvro>(&self, input: &'a [u8]) -> IResult<&'a [u8], Vec<A>> {
        let (input, count_objects) = i64::from_avro(input)?;
        let (input, count_bytes) = i64::from_avro(input)?;
        let (input, encoded_objects) = take(count_bytes as usize)(input)?;
        let (input, _sync) = tag(self.sync.as_slice())(input)?;

        let mut buffer = Vec::new();
        let decoded_objects = match self.codec {
            Codec::Null => encoded_objects,
            Codec::Deflate => {
                let mut decoder = Decoder::new(encoded_objects);
                decoder.read_to_end(&mut buffer).unwrap();
                buffer.as_slice()
            }
        };

        let parsed = count(A::from_avro, count_objects as usize)(decoded_objects);

        let result = match parsed {
            Ok((_, objects)) => Ok((input, objects)),
            Err(Err::Incomplete(needed)) => Err(Err::Incomplete(needed)),
            Err(Err::Error((_, kind))) => Err(Err::Failure((input, kind))),
            Err(Err::Failure((_, kind))) => Err(Err::Failure((input, kind))),
        };

        result
    }
}

/// How to compress objects in this container.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Codec {
    Deflate,
    Null,
}

impl Codec {
    fn as_bytes(&self) -> &[u8] {
        let as_str = match self {
            Codec::Deflate => "deflate",
            Codec::Null => "null",
        };

        as_str.as_bytes()
    }

    fn from_bytes(bytes: &[u8]) -> Option<Codec> {
        let s = String::from_utf8(bytes.to_vec());
        match s {
            Ok(s) => {
                if s == "null" {
                    Some(Codec::Null)
                } else if s == "deflate" {
                    Some(Codec::Deflate)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
