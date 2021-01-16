from __future__ import annotations

from dataclasses import dataclass
import io
import json
import random
from typing import Callable, Iterator, List, TypeVar
import zlib

from theta import avro

A = TypeVar("A")

def decode_container(decoder: avro.Decoder, object_type) -> Iterator[A]:
    """
    Parse a container that contains the given type of object.
    """
    header = Header.decode_avro(decoder)

    while True:
        try:
            block_objects = header.decode_block(decoder, object_type.decode_avro)
            yield from block_objects
        except avro.EmptyStream as e:
            break

def encode_container(encoder: avro.Encoder, objects: List[A],
                     codec: str="deflate", sync_marker: Optional[bytes]=None,
                     object_type=None):
    """
    Encode the given list of objects into a container, with all the objects
    in a single block.

    This will default to the `"deflate"` codec unless `"null"` is
    explicitly specified.

    This will use the schema of the first object in the list unless the
    list is empty, in which case it will use the schema of the given
    `object_type`. If the list is empty and an `object_type` was not
    passed in, this will raise an exception.
    """
    schema_json_object = \
        objects[0].avro_schema if len(objects) > 0 else object_type.avro_schema

    header = Header(json.dumps(schema_json_object), codec,
                    sync_marker or random_sync_marker())

    header.encode_avro(encoder)
    header.encode_block(encoder, objects)

def random_sync_marker() -> bytes:
    """
    Generate a random 16-byte sequence that can be used as a container's
    sync marker.
    """
    return bytes(random.getrandbits(8) for _ in range(0, 16))

@dataclass
class Header:
    """
    The header of an Avro container. The header can be described as an Avro
    record itself with the following fields:

      1. A 4-byte fixed field (which is expected to be the string Obj1)
      2. Metadata stored as an Avro map of `bytes` values, with:
         a. An "avro.codec" key specifying the compression scheme used by
            the container, if any. If missing, defaults to `"null"`.
         b. An "avro.schema" key specifying the Avro schema for the records
            in the container, stored as UTF-8 encoded JSON.
      3. A 16-byte fixed field specifying the container's sync marker.
    """
    schema: str
    codec: str
    sync_marker: bytes = random_sync_marker()

    def to_avro(self, out):
        """
        Serialize the header to the Avro binary format on the given
        output stream.
        """
        self.encode_avro(avro.Encoder(out))

    def from_avro(self, in_):
        """
        Parse the header from the given binary input stream.
        """
        return self.decode_avro(avro.Decoder(in_))

    def encode_avro(self, encoder: avro.Encoder):
        """
        Serialize the header using the given `avro.Encoder`.
        """
        # Magic header (Obj1)
        encoder.raw(b"Obj\x01")

        # Schema and code (an Avro hash map containing bytestrings)
        metadata = {
            "avro.schema": self.schema.encode("Utf8"),
            "avro.codec": self.codec.encode("Utf8")
        }
        encoder.map(metadata, encoder.bytes)

        # Sync marker (fixed 16-byte field):
        encoder.raw(self.sync_marker)

    @staticmethod
    def decode_avro(decoder: avro.Decoder) -> Header:
        """
        Parse a header using the given `avro.Decoder`.
        """
        # Magic header (Obj1)
        assert(decoder.raw(4) == b"Obj\x01")

        # Schema and codec (an Avro hash map containing bytestrings)
        metadata = decoder.map(decoder.bytes)

        # Sync marker (fixed 16-byte field)
        sync_marker = decoder.raw(16)

        codec = metadata.get("avro.codec").decode("utf8") or "null"

        schema = metadata["avro.schema"].decode("utf8")

        return Header(schema, codec, sync_marker)

    def encode_block(self, encoder: avro.Encoder, objects: List[A]):
        """
        Encode a single Avro block.

        A block has the following components:

          * An integer specifying how many objects it contains.
          * An integer specifying the block's size in bytes, *after* compression.
          * The corresponding number of objects, encoded with the container's
            schema.
          * The container's 16-byte sync marker to mark the end of the block.
        """
        encoder.integral(len(objects))

        object_buffer = io.BytesIO()
        object_encoder = avro.Encoder(object_buffer)

        for obj in objects:
            obj.encode_avro(object_encoder)

        encoded = object_buffer.getvalue()

        if self.codec == "null":
            encoder.integral(len(encoded))
            encoder.raw(encoded)
        elif self.codec == "deflate":
            compressed = deflate(encoded)

            encoder.integral(len(compressed))
            encoder.raw(compressed)
        else:
            codec = self.codec
            raise Exception(f"Unknown compression format specified: '{codec}'.")

        encoder.raw(self.sync_marker)

    def decode_block(self, decoder: avro.Decoder,
                     decode_object: Callable[[A], None]) -> Iterator[A]:
        """
        Decode a single block, return a list of objects.

        A block has the following components:

          * An integer specifying how many objects it contains.
          * An integer specifying the block's size in bytes, *after* compression.
          * The corresponding number of objects, encoded with the container's
            schema.
          * The container's 16-byte sync marker to mark the end of the block.
        """
        object_count = decoder.integral()
        bytes_count = decoder.integral()
        encoded_objects = decoder.raw(bytes_count)
        assert(decoder.raw(16) == self.sync_marker)

        if self.codec == "deflate":
            encoded_objects = inflate(encoded_objects)
        elif self.codec != "null":
            raise Exception(f"Unknown compression format specified: '{self.codec}'.")

        object_decoder = avro.Decoder(io.BytesIO(encoded_objects))

        for _ in range(0, object_count):
            yield decode_object(object_decoder)

def deflate(data: bytes) -> bytes:
    # We need to throw away header (2 bytes at beginning) and
    # checksum (4 bytes at end) from result of zlib.compress
    return zlib.compress(data)[2:-4]

def inflate(data: bytes) -> bytes:
    decompress = zlib.decompressobj(-zlib.MAX_WBITS)
    inflated = decompress.decompress(data)
    inflated += decompress.flush()
    return inflated
