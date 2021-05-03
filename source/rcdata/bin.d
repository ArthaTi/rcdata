/// A serializer and pull parser for a custom binary format. The format is meant to be simple, so it might lack
/// features. It does not store field names or type information.
module rcdata.bin;

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

    /// Get a value.
    /// Returns: the current `RCBin` instance to allow chaining
    static if (isParser)
    auto get(T)(ref T target) {

        return getImpl(target);

    }

    /// Ditto
    static if (isSerializer)
    auto get(T)(const T input) {

        // Safe cast: getImpl won't modify the clone
        T clone = cast(T) input;
        return getImpl(clone);

    }

    private auto getImpl(T)(ref T target) {

        // Boolean
        static if (is(T == bool)) getBoolean(target);

        // Basic types
        else static if (isNumeric!T || isSomeChar!T) getNumber(target);

        // Arrays
        else static if (isArray!T) getArray(target);

        // Structs
        else static if (is(T == struct)) getStruct(target);

        // Other types
        else static assert(0, "Unsupported type " ~ fullyQualifiedName!T);

        return this;

    }

    /// Read and return a value. Should be avoided to enable making serializers.
    static if (isParser)
    T read(T)() {

        T value;
        get(value);
        return value;

    }

    /// Read or write a boolean to the stream.
    void getBoolean(ref bool target) {

        // Read the value
        ubyte tempTarget = target;
        getNumber(tempTarget);
        target = cast(bool) tempTarget;

    }


    /// Read or write a number to the stream.
    static if (isParser)
    void getNumber(T)(ref T target)
    if (isNumeric!T || isSomeChar!T) {

        // Read the value
        target = takeExactly(*range, T.sizeof)
            .staticArray!(T.sizeof)
            .littleEndianToNative!T;

        // Advance the range
        popFrontN(*range, T.sizeof);

    }

    /// Ditto
    static if (isSerializer)
    void getNumber(T)(const T input)
    if (isNumeric!T || isSomeChar!T) {

        // Write the value
        multiput(*range, input.nativeToLittleEndian[]);

    }

    /// Read or write a dynamic array to the stream.
    static if (isParser)
    void getArray(T)(ref T[] target) {

        // Make a new array in case T is const
        Unconst!T[] clone;

        // Get array length
        clone.length = read!ulong;

        // Fill each item
        foreach (ref item; clone) {

            item = read!(Unconst!T);

        }

        // Apply to target
        target = cast(T[]) clone;

    }

    /// Ditto
    static if (isSerializer)
    void getArray(T)(const T[] input) {

        // Write array length
        get(cast(ulong) input.length);
        // target.length would be uint on 32 bit machines, so we cast it to ulong

        // Write each item
        foreach (item; input) {

            get(item);

        }

    }

    /// Read or write a static array to the stream.
    static if (isParser)
    void getArray(T, size_t size)(ref T[size] target) {

        // TODO const(T) support? if that works for static arrays...

        foreach (ref item; target) {

            item = read!T;

        }

    }

    /// Ditto
    static if (isSerializer)
    void getArray(T, size_t size)(const T[size] input) {

        // TODO const(T) support? if that works for static arrays...

        foreach (item; input) {

            get(item);

        }

    }

    /// Read or write all struct fields.
    static if (isParser)
    void getStruct(T)(ref T target)
    if (is(T == struct)) {

        // Iterate on each field
        static foreach (fieldName; FieldNameTuple!T) {

            get(mixin("target." ~ fieldName));

        }

    }

    /// Ditto
    static if (isSerializer)
    void getStruct(T)(const T target)
    if (is(T == struct)) {

        // Iterate on each field
        static foreach (fieldName; FieldNameTuple!T) {

            get(mixin("target." ~ fieldName));

        }

    }

}

///
unittest {

    struct Foo {

        int id;
        float value;
        string name;
        string[] arguments;
        string unicodeString;
        int[] numbers;
        int[3] staticArray;

    }

    void getData(T)(ref T bin, ref Foo target)
    if (isRCBin!T) {

        bin.get(target.id)
           .get(target.value)
           .get(target.name)
           .get(target.arguments)
           .get(target.unicodeString)
           .get(target.numbers)
           .get(target.staticArray);

    }

    Foo foo = {
        id: 123,
        value: 42.01,
        name: "John Doe",
        arguments: ["a", "ab", "b"],
        unicodeString: "Ich fühle mich gut.",
        numbers: [1, 2, 3, 4],
        staticArray: [1, 2, 3],
    };

    // Serialize the data
    auto data = appender!(ubyte[]);
    auto serializer = rcbinSerializer(data);
    getData(serializer, foo);

    // Read the data
    ubyte[] buffer = data[];
    Foo newFoo;
    auto parser = rcbinParser(buffer);
    getData(parser, newFoo);

    assert(newFoo.id == 123);
    assert(newFoo.value == 42.01f);
    assert(newFoo.name == "John Doe");
    assert(newFoo.arguments == ["a", "ab", "b"]);
    assert(newFoo.unicodeString == "Ich fühle mich gut.");
    assert(newFoo.numbers == [1, 2, 3, 4]);
    assert(newFoo.staticArray == [1, 2, 3]);

    // Or even better — serialize the whole struct
    auto data2 = appender!(ubyte[]);
    rcbinSerializer(data2)
        .getStruct(foo);

    auto buffer2 = data2[];
    assert(data[] == buffer2);

    // And read the data later
    Foo anotherFoo;
    rcbinParser(buffer2)
        .getStruct(anotherFoo);

    assert(foo == anotherFoo);

}

unittest {

    char test = 'a';

    auto data = appender!(ubyte[]);
    rcbinSerializer(data)
        .get(test);
    auto buffer = data[];

    char newTest;
    auto parser = rcbinParser(buffer)
        .get(newTest);

    assert(newTest == 'a');

}

unittest {

    auto data = appender!(ubyte[]);
    rcbinSerializer(data)
        .get(true)
        .get(false);

    auto buffer = data[];

    bool foo, bar;
    auto parser = rcbinParser(buffer)
        .get(foo)
        .get(bar);

    assert(foo);
    assert(!bar);

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
