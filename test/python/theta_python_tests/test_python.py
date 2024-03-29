import datetime
import io
from hypothesis import given
from hypothesis.strategies import binary, booleans, builds, dates, datetimes, \
     dictionaries, floats, integers, lists, none, one_of, text, uuids, times
import unittest

from theta import avro

from theta_python_tests.container import ContainerRecord
from theta_python_tests.enum import EnumWrapper, SimpleEnum
from theta_python_tests.primitives import Containers, Primitives
from theta_python_tests.importing import Foo
from theta_python_tests.variant import Either, I, J, S
from theta_python_tests.newtype import NewtypeRecord

import theta_python_tests.shadowing as shadowing

import theta_python_tests.com.example.nested as nested
import theta_python_tests.com.example.importing_nested as importing_nested

ints = integers(min_value=-0x80000000, max_value=0x7FFFFFFF)
longs = integers(min_value=-0x8000000000000000, max_value=0x7FFFFFFFFFFFFFFF)

# TODO: Add some kind of hypothesis method to the types generated by
# Theta?

# Generators for types define in the test Theta schemas.
#
# Floating-point fields are configured not to generate NaNs so that we
# don't need a special case for equality. NaN parsing/serializing is
# tested in theta/avro.py anyway.

# primitives.theta
primitives = builds(Primitives,
                    booleans(),
                    binary(),
                    ints,
                    longs,
                    floats(width=32, allow_nan=False),
                    floats(width=64, allow_nan=False),
                    text(),
                    dates(),
                    datetimes(),
                    uuids(),
                    times(),
                    datetimes(),
                    binary(min_size=1, max_size=1),
                    binary(min_size=3, max_size=3))

containers = builds(Containers,
                    lists(booleans()),
                    dictionaries(text(), booleans()),
                    one_of(booleans(), none()),
                    dictionaries(text(), lists(one_of(booleans(), none()))))

foo = builds(Foo, lists(primitives))

newtype = builds(NewtypeRecord, ints)

# variant.theta
i = builds(I, ints)
j = builds(J, ints)
s = builds(S, text(), ints)
either = one_of(i, j, s)

class TestPrimitives(unittest.TestCase):
    @given(primitives)
    def test_primitives(self, record):
        round_trip(record, Primitives)


class TestContainers(unittest.TestCase):
    @given(containers)
    def test_containers(self, record):
        round_trip(record, Containers)


class TestEnum(unittest.TestCase):
    def test_enum(self):
        round_trip(EnumWrapper(SimpleEnum.SymbolA), EnumWrapper)
        round_trip(EnumWrapper(SimpleEnum.SymbolB), EnumWrapper)


class TestVariant(unittest.TestCase):
    @given(i)
    def test_branch(self, record):
        round_trip(record, Either)

    @given(either)
    def test_variant(self, record):
        round_trip(record, Either)


class TestFoo(unittest.TestCase):
    @given(foo)
    def test_foo(self, record):
        round_trip(record, Foo)


class TestNewtype(unittest.TestCase):
    @given(newtype)
    def test_newtype(self, record):
        round_trip(record, NewtypeRecord)


class TestShadowing(unittest.TestCase):
    @given(primitives)
    def test_primitives(self, record):
        wrapped = shadowing.Primitives(underlying=record)
        round_trip(wrapped, shadowing.Primitives)


class TestImportingNested(unittest.TestCase):
    @given(ints)
    def test_importing_nested(self, i):
        nested_record = nested.TestRecord(i)
        wrapped = importing_nested.ImportingNested(nested_record)
        round_trip(wrapped, importing_nested.ImportingNested)


class TestFixedContainers(unittest.TestCase):
    def test_container(self):
        date = datetime.date(2010, 10, 10)
        time = datetime.datetime(2010, 10, 10, 10, 10, 10)
        expected_records = [
            ContainerRecord(True, b"foo", 1, 42, 1.0, 2.3, "blarg", date, time),
            ContainerRecord(True, b"foo", 2, 42, 1.0, 2.3, "blarg", date, time),
            ContainerRecord(True, b"foo", 3, 42, 1.0, 2.3, "blarg", date, time)
        ]

        # A container created using Haskell with no compression.
        with open("avro/container.avro", "rb") as container:
            records = ContainerRecord.read_container(container)
            for (got, expected) in zip(records, expected_records):
                assert(got == expected)

        # A container created using Haskell with deflate compression.
        with open("avro/container-deflate.avro", "rb") as container:
            records = ContainerRecord.read_container(container)
            for (got, expected) in zip(records, expected_records):
                assert(got == expected)


class TestContainerRoundTrip(unittest.TestCase):
    @given(lists(primitives))
    def test_primitives_container_deflate(self, records):
        round_trip_container(Primitives, records, "deflate")

    @given(lists(primitives))
    def test_primitivesx_container_null(self, records):
        round_trip_container(Primitives, records, "null")

    @given(lists(either))
    def test_variant_container_deflate(self, records):
        round_trip_container(Either, records, "deflate")

    @given(lists(either))
    def test_variant_container_null(self, records):
        round_trip_container(Either, records, "null")


def round_trip(record, theta_class):
    """
    Given a record of a type generated by Theta, this tests if serializing
    and then deserializing the record produces the same value.

    from_avro ∘ to_avro = id
    """
    # Round-trip with low-level encode/decode methods:
    encode_stream = io.BytesIO()
    encoder = avro.Encoder(encode_stream)
    record.encode_avro(encoder)

    decode_stream = io.BytesIO(encode_stream.getvalue())
    decoder = avro.Decoder(decode_stream)
    result = theta_class.decode_avro(decoder)

    assert(result == record)

    # Round-trip with high-level to/from methods:
    to_stream = io.BytesIO()
    record.to_avro(to_stream)

    from_stream = io.BytesIO(to_stream.getvalue())
    result = record.from_avro(from_stream)

    assert(result == record)


def round_trip_container(object_type, records, codec):
    out_stream = io.BytesIO()
    object_type.write_container(records, out_stream, codec=codec)

    in_stream = io.BytesIO(out_stream.getvalue())
    parsed_records = object_type.read_container(in_stream)
    assert(list(parsed_records) == records)


def generate_test_containers():
    """
    Generate a couple of containers of Primitives records that can 
    be used to test whether other systems can parse these containers.
    """
    date = datetime.date(2010, 10, 10)
    time = datetime.datetime(2010, 10, 10, 10, 10, 10)
    primitives = [
        Primitives(True, b"foo", 1, 42, 1.0, 2.3, "blarg", date, time),
        Primitives(True, b"foo", 2, 42, 1.0, 2.3, "blarg", date, time),
        Primitives(True, b"foo", 3, 42, 1.0, 2.3, "blarg", date, time)
    ]
    with open("primitives-container-python.avro", "wb") as f:
        Primitives.write_container(primitives, f, codec="null")

    with open("primitives-container-python-deflate.avro", "wb") as f:
        Primitives.write_container(primitives, f, codec="deflate")
