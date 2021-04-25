# purescript-mote

[![Latest release](http://img.shields.io/github/release/garyb/purescript-mote.svg)](https://github.com/garyb/purescript-mote/releases) 
![Build Status](https://github.com/garyb/purescript-mote/actions/workflows/ci.yml/badge.svg)

A library for describing test suites / specs.

Mote cannot actually run tests itself - nor does it have an opinion of what a test even is! It just provides a means of describing tests and groups, and then generating a plan to resolve which tests should be skipped when using combinators like `only` and `skip`. The plan can then be interpreted to do something to actually run the tests.

While we already have the perfectly good [`purescript-spec`](https://github.com/owickstrom/purescript-spec) and [`purescript-test-unit`](https://github.com/bodil/purescript-test-unit) libraries, these are restrictive in how they allow tests to be defined. Mote provides a more flexible DSL, `MoteT`, that allows effects to be performed while building up the test suite... this opens the door to trouble, but sometimes you might need to access some kind of `Reader`-based environment or generate tests from filesystem inputs, now you have the option to do this amidst the test definitions.

Mote test/group descriptions also accommodate the of bracketing - describing pre/post actions to allow some kind of setup/teardown if necessary. The pre-action can generate a value that the post-action will later consume.

Some demos of interpreting a Mote plan as a `Spec` or `TestSuite` are provided in the [`examples`](examples/) directory.

## Installation

```
bower install purescript-mote
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-mote).
