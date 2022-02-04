# Python Tests

This directory defines a Nix derivation that tests the Python code that Theta generates.

## Adding Test Modules

If you want to add a new module `example` to the Python tests, you need to take three steps:

  1. Add `example.theta` to `modules`.
  2. Add `import example` to `modules/python_tests.theta`.
  3. Import and test the corresponding Python module in `theta_python_tests/test_python.py`.
  
Most of the tests use Hypothesis to randomly generate values of the class and then ensure that they can be encoded to Avro and decoded back to the same value. In the future, Theta itself might generate Hypothesis strategies for its types, but for now you have to do it manually. If `example.theta` had the following record:

```
type Foo = { foo : Int }
```

Here's how you might test it in `test_python.py`:
  
```
import theta_python_tests.example as example

example_foo = builds(example.Foo, ints)

class TestExample(unittest.TestCase):
    @given(example_foo)
    def test_foo(self, record):
        round_trip(record, example.Foo)
```

Of course, feel free to add more specific generators and test cases!

## Errors

If your test doesn't work, double-check the following:

  1. Did you import your new module from `python_tests.theta`?
  2. Did you import the corresponding `theta_python_tests.*.py` in `test_python.py`?
