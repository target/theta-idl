# Rust Tests

This directory uses Nix to test the Rust code that Theta generates. The Nix setup is a bit of a hack; see comments in `default.nix` for details.

The tests pick up every module in `modules`, generate a corresponding Rust file then compile and run tests for the whole package (ie `lib.rs` + generated modules).

## Adding Test Modules

If you want to add a new module named `foo` to the tests here, you need to do two things:

  1. Add `foo.theta` to the `modules` directory.
  2. Add `pub mod foo;` to `lib.rs`.
  
This will ensure that the module is built and typechecked.

If you want to do other tests with the types from the new module—like round-tripping encoding/decoding—you will need to add test code for it to the `tests` module in `lib.rs`.

Note: do not add a module called `lib.theta`! This will overwrite `lib.rs` and break things.

## Adding Test Dependencies

The `Cargo.toml` file *for the tests package* is generated from `default.nix`. If you want access to a new library for testing, you'll have to add it there as well as updating the `Cargo.lock` file in this directory. The whole Nix setup is something of a hack—I can't even guarantee that this will work!
