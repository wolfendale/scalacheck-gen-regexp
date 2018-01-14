[![Build Status](https://travis-ci.org/wolfendale/scalacheck-gen-regexp.svg?branch=master)](https://travis-ci.org/wolfendale/scalacheck-gen-regexp)

# scalacheck-gen-regexp

A library for creating [scalacheck](https://www.scalacheck.org/) generators from regular expressions

Cross-built for Scala 2.11/2.12

##### Installation

In your `build.sbt`
```scala
resolvers += Resolver.bintrayRepo("wolfendale", "maven")

libraryDependencies += "wolfendale" %% "scalacheck-gen-regexp" % "[VERSION]"
```

##### Quick start

```scala
import wolfendale.scalacheck.regexp.RegexpGen

val generator: Gen[String] = RegexpGen.from("[1-9]\\d?(,\\d{3})+")
```

##### Supported syntax

| Feature           | Example                                     | Notes                                                                        |
|-------------------|---------------------------------------------|------------------------------------------------------------------------------|
| Literals          | `a`, `\\w`, `7`                             | Literals are transformed into constant generators                            |
| Character Classes | `[abc]`, `[^abc]`, `[a-zA-Z0-9]`            | Character classes are transformed with `Gen.oneOf`                           |
| Default Classes   | `\w`, `\d`, `\S`, `.`                       | These are treated as predefined character classes                            |
| Quantifiers       | `a?`, `b+`, `c*`, `d{3}`, `e{4,5}`, `f{5,}` | These use `Gen.listOfN` to create sized lists of the preceding term          |
| Groups            | `(abc)`, `(?:def)`                          | Backreferences are not supported, groups can only be used for grouping terms |
| Alternates        | `a\|b\|c`, `a(b\|c)d`                             | Alternates are also transformed with `Gen.oneOf`                             |
| Boundaries        | `^`, `$`, `\b`                              | Although these will be parsed they do not modify the generator output        |

##### Unsupported syntax

| Feature                        | Example                     | Notes                                                                                                              |
|--------------------------------|-----------------------------|--------------------------------------------------------------------------------------------------------------------|
| Backreferences                 | `([ab]\1)`                  | With the current implementation there's no simple way to do this, definitely in consideration for a future release |
| Octal / Hex / Special Literals | `\012`, `\xF1`, `\p{Lower}` | Most of these should be simple to implement but I wanted to get an initial release created first                   |
| Character Class Intersection   | `[a&&[b]]`, `[a[b]]`        | Difficult to implement currently but not impossible, definite consideration for a future release                   |

##### Tips for usage

* In order to represent _any character_, `RegexpGen#from` takes an implicit `Arbitrary[Char]`.
There is a default instance provided by scalacheck however for most uses you probably want to
provide your own.

* If you use the `+` or `*` quantifiers you'll end up getting huge variance in string sizes.
If this isn't what you want, consider bounding the lengths of certain string segments with the
`{min,max}` quantifier.

* Negated character classes / default classes are implemented by generating an arbitrary `Char`
within certain bounds via `suchThat`, because of this you can end up throwing away a lot of 
cases and in certain circumstances your tests may fail. Try to refactor out negative cases.

* In character classes each option is given equal weighting. If you'd prefer to weight a
particular entry you can add it multiple times, this is made easier with string interpolation:
```s"[${"a-z"*5}\s]"```. In the example case the generator is 5 times more likely to generate
an alpha character than a space.
