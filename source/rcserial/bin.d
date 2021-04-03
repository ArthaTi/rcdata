/// a serializer and pull parser for a custom binary format. The format is meant to be simple, so it might lack
/// features. It does not store field names or type information.
module rcserial.bin;

import std.range;
import std.traits;
import std.bitmanip;

// Implementation notes:
// * Use `ulong` instead of `size_t` to keep the data consistent between computers.
// * Numbers should be stored in little ending using std.bitmanip.nativeToLittleEndian.

/// Make an rcbin parser.
RCBin!(T, true) rcbinParser(T)(ref T range) {

    return RCBin!(T, true)(range);

}

/// Make an rcbin serializer.
RCBin!(T, false) rcbinSerializer(T)(ref T range) {

    return RCBin!(T, false)(range);

}

/// Check if the given type is an RCBin struct.
enum isRCBin(T) = is(T == RCBin!(A, B), A, alias B);

/// Struct for parsing and serializing into binary.
/// Params:
///     T        = A range. If parsing, this must be an input range, or if serializing, an output range.
///     isParser = If true this should be a parser
struct RCBin(T, bool isParser) {

    // Checks for parser
    static if (isParser) {

        static assert(isInputRange!T, "RCBin parser argument is not an input range.");
        static assert(is(ElementType!T == ubyte), "RCBin range must have ubyte as element type.");
        enum isSerializer = false;

    }

    // Checks for serializer
    else {

        static assert(isOutputRange!(T, ubyte), "RCBin serializer argument is not an output range.");
        enum isSerializer = true;

    }


    /// Range to read or write to.
    T* range;

    /// Create a parser or serializer with the given range.
    this(ref T range) {

        this.range = &range;

    }

    /// Read or write a number to the stream.
    static if (isParser)
    void getNumber(T)(ref T target)
    if (isNumeric!T) {

        // Read the value
        target = takeExactly(*range, T.sizeof)
            .staticArray!(T.sizeof)
            .littleEndianToNative!T;

        // Advance the range
        popFrontN(*range, T.sizeof);

    }

    static if (isSerializer)
    void getNumber(T)(T input)
    if (isNumeric!T) {

        // Write the value
        multiput(*range, input.nativeToLittleEndian[]);

    }

    /// Get a value.
    void get(T)(ref T target) {

        static if (isNumeric!T) getNumber(target);

    }

}

unittest {

    struct Foo {

        int id;
        float value;
        string name;
        string[] arguments;

    }

    void getData(T)(ref T bin, ref Foo target)
    if (isRCBin!T) {

        bin.get(target.id);
        bin.get(target.value);
        //bin.get!string(target.name);
        //bin.get!(string[])(target.arguments);

    }

    Foo foo = {
        id: 123,
        value: 42.01,
        name: "John Doe",
        arguments: ["a", "ab", "b"],
    };

    auto data = appender!(ubyte[]);
    auto serializer = rcbinSerializer(data);
    getData(serializer, foo);

    ubyte[] buffer = data[];

    Foo newFoo;
    auto parser = rcbinParser(buffer);
    getData(parser, newFoo);

    assert(newFoo.id == 123);
    assert(newFoo.value == 42.01f);
    assert(newFoo.name == "John Doe");
    assert(newFoo.arguments == ["a", "ab", "b"]);

}

// std.range's put sucks and does some really unexpected and weird behavior for arrays
/// Append input to output. Input can be a single value or a range.
private void multiput(R, E)(ref R output, E input) {

    // Add all array items
    static if (isInputRange!E) {

        foreach (element; input) {

            output.put(element);

        }

    }

    // Add one element
    else output.put(input);

}
