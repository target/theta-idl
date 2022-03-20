from dataclasses import dataclass
from datetime import date, datetime, timedelta
import io
import math
import struct
from typing import Callable, Dict, List, Optional, TypeVar, Union
from uuid import UUID

from hypothesis import example, given
from hypothesis.strategies import binary, booleans, dates, datetimes, dictionaries, floats, integers, lists, text
import unittest

class Encoder:
    def __init__(self, out):
        """
        Build an object that can write Theta values to an output stream in
        the Avro binary format.

        The output stream has to be able to handle binary data. If
        you're writing to a file, make sure to open in binary mode (ie
        `open('wb', file)`); if you're writing to STDOUT, use
        `sys.stdout.buffer`.
        """
        self._out = out

    def raw(self, raw: bytes):
        """
        Write raw binary data to the output stream.
        """
        self._out.write(raw)

    # Primitive Types
    def bool(self, x: bool):
        """
        Write a Theta Bool to the output stream. Booleans are encoded in Avro as
        a single byte with 0 for false and 1 for true.
        """
        if x:
            self.raw(struct.pack("B", 1))
        else:
            self.raw(struct.pack("B", 0))

    def bytes(self, x: bytes):
        """
        Write a Theta Bytes value to the output stream. Bytes are written as a long
        specifying the length followed by that many bytes of raw data.
        """
        self.integral(len(x))
        self.raw(x)

    def integral(self, x: int):
        """
        Write a Theta Int or Long to the output stream.

        Both "long" and "int" are encoded the same way in Avro and Python does
        not have different types, so both are encoded with the same method.

        Note that this function does not do bounds checking and is not defined
        for numbers outside the bounds for a long.
        """
        # zig-zag encoding
        x = (x >> 63) ^ (x << 1)

        # variable-length 7-bit encoding
        while (x & ~0x7f) != 0:
            self.raw(struct.pack("B", (x & 0x7f) | 0x80))
            x = x >> 7
        self.raw(struct.pack("B", x))

    def float(self, x: float):
        """
        Write a Theta float to the output stream. Floats are encoded as
        four bytes in little-endian order.
        """
        self.raw(struct.pack("<f", x))

    def double(self, x: float):
        """
        Write a Theta double to the output stream. Doubles are encoded as
        eight bytes in little-endian order.
        """
        self.raw(struct.pack("<d", x))

    def string(self, x: str):
        """
        Write a Theta String to the output stream.
        """
        self.bytes(x.encode("utf-8"))

    def date(self, x: date):
        """
        Write a Theta Date to the output stream.

        Dates are encoded as the number of days since the epoch (1970-01-01).
        """
        epoch = date(1970, 1, 1)
        self.integral((x - epoch).days)

    def datetime(self, x: datetime):
        """
        Write a Theta Datetime to the output stream.

        Datetimes are encoded as the number of microseconds since the epoch
        (1970-01-01:00:00:00).
        """
        epoch = datetime(1970, 1, 1)
        delta = x - epoch

        micros = delta.days * 24 * 60 * 60 * 1000 * 1000
        micros += delta.seconds * 1000 * 1000
        micros += delta.microseconds

        self.integral(micros)

    def uuid(self, uuid: UUID):
        """
        Write a UUID to the output stream.

        UUIDs are encoded as strings following the RFC 4122 format.

        Example: f81d4fae-7dec-11d0-a765-00a0c91e6bf6
        """
        self.string(str(uuid))

    # Containers

    A = TypeVar("A")

    def array(self, x: List[A], encode):
        """
        Write all the elements of the given list as an Avro array, using the
        provided callback to encode each element.

        All the elements in the list are encoded into a single block in Avro.

        We need to take a callback in this function to differentiate between
        types that are the same in Python but different in Avro (like float
        and double), as well as types that contain those (containers, records...
        etc).
        """
        if len(x) > 0:
            self.integral(len(x))
            for item in x:
                encode(item)

        # Mark the end of the array:
        self.integral(0)

    def map(self, x: Dict[str, A], encode):
        """
        Write all the elements of the given dictionary as an Avro map,
        using the provided callback to encode each element.

        Keys in the dictionary are processed in an unspecified order.

        All the elements in the dictionary are encoded into a single block
        in Avro.

        We need to take a callback in this function to differentiate between
        types that are the same in Python but different in Avro (like float
        and double), as well as types that contain those (containers, records...
        etc).
        """
        if len(x) > 0:
            self.integral(len(x))
            for key, value in x.items():
                self.string(key)
                encode(value)

        # Mark the end of the map:
        self.integral(0)

    def optional(self, x: Optional[A], encode):
        """
        Write a possibly-null value to Avro using the provided callback to
        encode the element (if present).

        We need to take a callback in this function to differentiate between
        types that are the same in Python but different in Avro (like float
        and double), as well as types that contain those (containers, records...
        etc).
        """
        if x is None:
            self.integral(0)
        else:
            self.integral(1)
            encode(x)

class EmptyStream(Exception):
    """
    An exception that signals that a decoder's underlying stream is empty.
    """
    pass

class Decoder:
    def __init__(self, stream):
        self._stream = stream

    def raw(self, n: int):
        """
        Read the given number of bytes from the underlying stream.
        """
        xs = self._stream.read(n)

        if len(xs) < n:
            raise EmptyStream()
        else:
            return xs

    def bool(self):

        """
        Read a Bool value. Booleans are encoded as a single byte
        with 0 for false and 1 for true.
        """
        byte = struct.unpack("B", self.raw(1))[0]
        if byte == 0:
            return False
        elif byte == 1:
            return True
        else:
            raise Exception(f"Invalid format for Bool field. Expected 0 or 1 but got '{byte}'.")

    def bytes(self):
        """
        Read a Bytes value. Bytes are encoded as a long specifying the length,
        followed by that many bytes of raw data.
        """
        length = self.integral()
        return self.raw(length)

    def integral(self):
        """
        Read an Int or a Long value. These are encoded the same way in Avro
        (variable-length zig-zag encoding) and represented with the same value
        in Python.
        """
        result = 0
        count = 0

        while True:
            byte = ord(self.raw(1))
            result |= (byte & 0x7f) << (count * 7)
            count += 1

            if byte & 0x80 == 0:
                return (result >> 1) ^ -(result & 1)

    def float(self):
        """
        Read a Float value. Floats are encoded as four bytes in little-endian
        order.
        """
        return struct.unpack("<f", self.raw(4))[0]

    def double(self):
        """
        Read a Double value. Doubles are encoded as eight bytes in
        little-endian order.
        """
        return struct.unpack("<d", self.raw(8))[0]

    def string(self):
        """
        Decode a String value. Strings are encoded as a long specifying a length
        (in bytes), followed by that many bytes of UTF-8.
        """
        length = self.integral()
        utf8_bytes = self.raw(length)
        return utf8_bytes.decode("utf-8")

    def date(self):
        """
        Decode a Date value. Dates are stored as ints, counting the number of
        days since the epoch (1970-01-01).
        """
        epoch = date(1970, 1, 1)
        return epoch + timedelta(days=self.integral())

    def datetime(self):
        """
        Decode a Datetime value. Datetimes are stored as longs, counting the
        number of microseconds since the epoch (1970-01-01).
        """
        epoch = datetime(1970, 1, 1)
        return epoch + timedelta(microseconds=self.integral())


    def uuid(self) -> UUID:
        """
        Decode a UUID.  UUIDs have to be represented as strings
        following the RFC 4122 format (example:
        "ef81d4fae-7dec-11d0-a765-00a0c91e6bf6").

        Raises an exception if the encoding does not conform to RFC 4122.
        """
        return UUID(self.string())

    A = TypeVar("A")

    def array(self, decode):
        """
        Decode an array of decodable values. Each value is decoded using the
        given lambda.

        Arrays are encoded as a series of blocks. Each block starts with a
        long encoding its *count*:

          1. A count of 0 marks the end of the array.
          2. A count > 0 means the block has that many elements.
          3. A count < 0 means the block has abs(count) elements, and that
             there is a long following the count specifying the number of
             bytes in the block.

        We currently don't use blocks with negative counts when *encoding*,
        but we need to handle them when *decoding* to cover all valid Avro
        data.
        """
        count = self.integral()

        elements = []
        while count != 0:
            if count < 0:
                self.integral()
                count = -count

            for _ in range(0, count):
                elements += [decode()]

            count = self.integral()

        return elements

    def map(self, decode):
        """
        Decode a key-value map where the keys are strings and the values are
        any decodable value.

        Maps are encoded as arrays of key-value pairs. That is, a map of A
        values is encoded as an array of { key: String, value: A } records.
        """
        def decode_key_value():
            key = self.string()
            value = decode()
            return (key, value)

        return dict(self.array(decode_key_value))

    def optional(self, decode):
        """
        Decode an optional value (a value that can be None).

        Optional values are encoded as an Avro union:

          1. A tag of 0 means None.
          2. A tag of 1 means its followed by the encoded value.
        """
        tag = self.integral()

        if tag == 0:
            return None
        if tag == 1:
            return decode()
        else:
            raise Exception(f"Invalid encoding: expected 0 or 1 tag but got {tag}.")


# Tests

class TestEncoder(unittest.TestCase):
    def setUp(self):
        self.stream = io.BytesIO()
        self.encoder = Encoder(self.stream)

    def test_bool(self):
        self.encoder.bool(True)
        self.encoder.bool(False)
        assert(self.stream.getvalue() == bytes([1, 0]))

    def test_bytes_empty(self):
        self.encoder.bytes(b"")
        assert(self.stream.getvalue() == bytes([0]))

    def test_bytes_short(self):
        self.encoder.bytes(b"foo")
        assert(self.stream.getvalue() == b"\x06foo")

    def test_bytes_long(self):
        string = b"0" * 512
        length = b"\x80\x08" # 512 in zig-zag encoding
        self.encoder.bytes(string)
        assert(self.stream.getvalue() == length + string)

    def test_integral(self):
        self.encoder.integral(0)
        self.encoder.integral(-1)
        self.encoder.integral(1)
        self.encoder.integral(512)

        assert(self.stream.getvalue() == b"\x00\x01\x02\x80\x08")

    @given(floats(width=32))
    def test_float(self, x):
        self.setUp() # needed each time the function is called by Hypothesis

        self.encoder.float(x)
        assert(self.stream.getvalue() == struct.pack("<f", x))

    @given(floats(width=64))
    def test_double(self, x):
        self.setUp() # needed each time the function is called by Hypothesis

        self.encoder.double(x)
        assert(self.stream.getvalue() == struct.pack("<d", x))

    def test_string_empty(self):
        self.encoder.string("")
        assert(self.stream.getvalue() == b"\x00")

    def test_string_short(self):
        self.encoder.string("foo")
        assert(self.stream.getvalue() == b"\x06foo")

    def test_string_long(self):
        string = "x" * 512
        length = b"\x80\x08" # 512 in zig-zag encoding
        self.encoder.string(string)
        assert(self.stream.getvalue() == length + (b"x" * 512))

    def test_date(self):
        epoch = b"\x00"
        before = b"\xcf\x0f" # -1000 in zig-zag encoding
        after = b"\xd0\x0f" # 1000 in zig-zag encoding

        self.encoder.date(date(1970, 1, 1))
        self.encoder.date(date(1967, 4, 7)) # epoch - 1000 days
        self.encoder.date(date(1972, 9, 27)) # epoch + 1000 days

        assert(self.stream.getvalue() == epoch + before + after)

    def test_datetime(self):
        epoch = b"\x00"

        # -3 hours in microseconds in zig-zag encoding
        before = b"\xff\xaf\xd7\xbb\x50"

        # 3 hours in microseconds in zig-zag encoding
        after = b"\x80\xb0\xd7\xbb\x50"

        self.encoder.datetime(datetime(1970, 1, 1))
        self.encoder.datetime(datetime(1969, 12, 31, 21)) # epoch - 3 hours
        self.encoder.datetime(datetime(1970, 1, 1, 3)) # epoch + 3 hours

        assert(self.stream.getvalue() == epoch + before + after)

    def test_array_empty(self):
        self.encoder.array([], self.should_not_be_called)
        assert(self.stream.getvalue() == b"\x00")

    def test_array_short(self):
        self.encoder.array([0, 1], self.encoder.integral)

        # length (in zig-zag), [0, 1] in zig-zag, 0 byte to end
        assert(self.stream.getvalue() == b"\x04" + b"\x00\x02" + b"\x00")

    def test_array_long(self):
        length = b"\x80\x08" # 512 in zig-zag encoding
        elements = b"\x02" * 512

        self.encoder.array([1] * 512, self.encoder.integral)

        # length (in zig-zag), 1 in zig-zag 512 times, 0 byte to end
        assert(self.stream.getvalue() == length + elements + b"\x00")

    def test_map_empty(self):
        self.encoder.map({}, self.should_not_be_called)
        assert(self.stream.getvalue() == b"\x00")

    def test_map(self):
        key = b"\x06foo"
        value = b"\x06bar"

        self.encoder.map({"foo" : "bar"}, self.encoder.string)

        # length (in zig-zag), key, value, 0 to end map
        assert(self.stream.getvalue() == b"\x02" + key + value + b"\x00")

    def test_optional(self):
        none = b"\x00"

        # tag (1 in zig-zag), length of string, string itself
        some = b"\x02\x06foo"

        self.encoder.optional(None, self.should_not_be_called)
        self.encoder.optional("foo", self.encoder.string)

        assert(self.stream.getvalue() == none + some)

    def should_not_be_called(self):
        raise "Should not be called."

class TestDecoder(unittest.TestCase):
    def decode(self, decode, input):
        decoder = Decoder(io.BytesIO(input))
        return decode(decoder)

    def test_bool(self):
        assert(self.decode(Decoder.bool, b"\x00") == False)
        assert(self.decode(Decoder.bool, b"\x01") == True)

        try:
            self.decode(Decoder.bool, b"\x02")
            assert(False)
        except:
            pass

    def test_bytes_empty(self):
        assert(self.decode(Decoder.bytes, b"\x00") == b"")

    def test_bytes_short(self):
        assert(self.decode(Decoder.bytes, b"\x06foo") == b"foo")

    def test_bytes_long(self):
        string = b"0" * 512
        length = b"\x80\x08" # 512 in zig-zag encoding
        assert(self.decode(Decoder.bytes, length + string) == b"0" * 512)

    def test_integral(self):
        assert(self.decode(Decoder.integral, b"\x00") == 0)
        assert(self.decode(Decoder.integral, b"\x01") == -1)
        assert(self.decode(Decoder.integral, b"\x02") == 1)
        assert(self.decode(Decoder.integral, b"\x80\x08") == 512)

    @given(floats(width=32))
    def test_float(self, x):
        encoded = struct.pack("<f", x)
        decoded = self.decode(Decoder.float, encoded)

        if math.isnan(x):
            assert(math.isnan(decoded))
        else:
            assert(x == decoded)

    @given(floats(width=64))
    def test_double(self, x):
        encoded = struct.pack("<d", x)
        decoded = self.decode(Decoder.double, encoded)

        if math.isnan(x):
            assert(math.isnan(decoded))
        else:
            assert(x == decoded)

    def test_string_empty(self):
        assert(self.decode(Decoder.string, b"\x00") == "")

    def test_string_short(self):
        assert(self.decode(Decoder.string, b"\x06foo") == "foo")

    def test_string_long(self):
        length = b"\x80\x08" # 512 in zig-zag encoding
        assert(self.decode(Decoder.string, length + b"x" * 512) == "x" * 512)

    def test_date(self):
        epoch = b"\x00"
        before = b"\xcf\x0f" # -1000 in zig-zag encoding
        after = b"\xd0\x0f" # 1000 in zig-zag encoding

        assert(self.decode(Decoder.date, epoch) == date(1970, 1, 1))
        assert(self.decode(Decoder.date, before) == date(1967, 4, 7))
        assert(self.decode(Decoder.date, after) == date(1972, 9, 27))

    def test_datetime(self):
        epoch = b"\x00"

        # -3 hours in microseconds in zig-zag encoding
        before = b"\xff\xaf\xd7\xbb\x50"

        # 3 hours in microseconds in zig-zag encoding
        after = b"\x80\xb0\xd7\xbb\x50"

        assert(self.decode(Decoder.datetime, epoch) == datetime(1970, 1, 1))
        assert(self.decode(Decoder.datetime, before) == datetime(1969, 12, 31, 21))
        assert(self.decode(Decoder.datetime, after) == datetime(1970, 1, 1, 3))

    def test_array_empty(self):
        def decode(decoder):
            return decoder.array(self.should_not_be_called)

        assert(self.decode(decode, b"\x00") == [])

    def test_array_short(self):
        def decode(decoder):
            return decoder.array(decoder.integral)

        # length (in zig-zag), [0, 1] in zig-zag, 0 byte to end
        assert(self.decode(decode, b"\x04" + b"\x00\x02" + b"\x00") == [0, 1])

    def test_array_long(self):
        def decode(decoder):
            return decoder.array(decoder.integral)

        length = b"\x80\x08" # 512 in zig-zag encoding
        elements = b"\x02" * 512

        assert(self.decode(decode, length + elements + b"\x00") == [1] * 512)

    def test_map_empty(self):
        def decode(decoder):
            return decoder.map(self.should_not_be_called)

        assert(self.decode(decode, b"\x00") == {})

    def test_map(self):
        def decode(decoder):
            return decoder.map(decoder.string)

        key = b"\x06foo"
        value = b"\x06bar"

        assert(self.decode(decode, b"\x02" + key + value + b"\x00") ==
               { "foo" : "bar" })

    def test_optional_none(self):
        def decode(decoder):
            return decoder.optional(self.should_not_be_called)

        assert(self.decode(decode, b"\x00") == None)

    def test_optional(self):
        def decode(decoder):
            return decoder.optional(decoder.string)

        assert(self.decode(decode, b"\x02\x06foo") == "foo")

    def should_not_be_called(self):
        raise Exception("Should not be called.")

class TestRoundTrip(unittest.TestCase):
    @given(booleans())
    def test_bool(self, x):
        self.check_encoding(x, Encoder.bool, Decoder.bool)

    @given(binary())
    def test_bytes(self, x):
        self.check_encoding(x, Encoder.bytes, Decoder.bytes)

    # within range of 64-bit numbers
    @given(integers(min_value=-0x8000000000000000, max_value=0x7FFFFFFFFFFFFFFF))
    def test_integral(self, x):
        self.check_encoding(x, Encoder.integral, Decoder.integral)

    @given(floats(width=32))
    def test_float(self, x):
        self.check_float_encoding(x, Encoder.float, Decoder.float)

    @given(floats(width=64))
    def test_double(self, x):
        self.check_float_encoding(x, Encoder.double, Decoder.double)

    @given(text())
    def test_string(self, x):
        self.check_encoding(x, Encoder.string, Decoder.string)

    @given(dates())
    def test_date(self, x):
        self.check_encoding(x, Encoder.date, Decoder.date)

    @given(datetimes())
    def test_datetime(self, x):
        self.check_encoding(x, Encoder.datetime, Decoder.datetime)

    @given(lists(dates()))
    def test_array(self, x):
        def encode(encoder, value):
            encoder.array(x, encoder.date)

        def decode(decoder):
            return decoder.array(decoder.date)

        self.check_encoding(x, encode, decode)

    @given(dictionaries(text(), dates()))
    def test_map(self, x):
        def encode(encoder, value):
            encoder.map(x, encoder.date)

        def decode(decoder):
            return decoder.map(decoder.date)

        self.check_encoding(x, encode, decode)

    @given(dates())
    @example(None)
    def test_optional(self, x):
        def encode(encoder, value):
            encoder.optional(x, encoder.date)

        def decode(decoder):
            return decoder.optional(decoder.date)

        self.check_encoding(x, encode, decode)

    def check_encoding(self, x, encode, decode):
        encode_stream = io.BytesIO()
        encoder = Encoder(encode_stream)
        encode(encoder, x)

        decode_stream = io.BytesIO(encode_stream.getvalue())
        decoder = Decoder(decode_stream)
        assert(decode(decoder) == x)

    def check_float_encoding(self, x, encode, decode):
        encode_stream = io.BytesIO()
        encoder = Encoder(encode_stream)
        encode(encoder, x)

        decode_stream = io.BytesIO(encode_stream.getvalue())
        decoder = Decoder(decode_stream)
        decoded = decode(decoder)

        if math.isnan(x):
            assert(math.isnan(decoded))
        else:
            assert(x == decoded)

if __name__ == "__main__":
    unittest.main()
