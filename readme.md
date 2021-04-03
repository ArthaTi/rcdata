# rcdata

rcdata is a small collection of parsers and serializers.

## json

rcdata.json is a simple JSON pull parser written according to [RFC](https://tools.ietf.org/html/rfc8259).

### Why another?

* It's a pull parser, making it more flexible.
* Reasonably quick, should be faster than `std.json`, but probably won't reach world records.
* Range-based and lazy.
* Boost licensed.
* May eventually implement serialization in a similar fashion.

## bin

rcserial.bin is a serializer and pull parser for a custom binary format. The format is meant to be simple, so it might
lack features. It does not store field names or type information.

As a feature, it can do both serialization and parsing with the same code.

## TODOs

* Make the API consistent between libraries, reuse methods for eg. struct serialization.
