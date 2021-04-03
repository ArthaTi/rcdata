/// A simple JSON pull parser written according to RFC 8259.
module rcserial.json;

import std.range;
import std.traits;
import std.format;
import std.algorithm;
import std.exception;

/// Struct for parsing JSON.
struct JSONParser {

    /// Type of a JSON value
    enum Type {

        null_,
        boolean,
        number,
        string,
        array,
        object

    }

    /// Input taken by the parser.
    ForwardRange!dchar input;

    /// Current line number.
    size_t lineNumber = 1;

    @disable this();

    /// Start parsing using the given object, converting it to an InputRange.
    this(T)(T input)
    if (isForwardRange!T) {

        this.input = input.inputRangeObject;

    }

    /// Check the next type in the document.
    /// Returns: Type of the object.
    Type peekType() {

        skipSpace();

        // Nothing left
        if (input.empty) {

            throw new Exception("Unexpected end of file.");

        }

        with (Type)
        switch (input.front) {

            // Valid types
            case 'n': return null_;

            case 't':
            case 'f': return boolean;

            case '-':
            case '0':
                ..
            case '9': return number;

            case '"': return string;

            case '[': return array;
            case '{': return object;

            // Errors
            case ']':
            case '}':

                throw new Exception(
                    failMsg(input.front.format!"Unexpected '%s' (maybe there's a comma before it?)")
                );

            // Other errors
            default:

                throw new Exception(
                    failMsg(input.front.format!"Unexpected character '%s'")
                );

        }

    }

    // TODO: switchType

    /// Get a value of the matching type.
    /// Params:
    ///     T = Built-in type expected to be returned, or an element of the `Type` enum.
    template get(T) {

        // Boolean
        static if (is(T : bool) || (is(T == enum) && T == Type.boolean)) {
            alias get = getBoolean;
        }

        // Number 1
        else static if (isNumeric!T) {
            alias get = getNumber!T;
        }

        // Number 2
        else static if (is(T == enum) && T == Type.number) {
            alias get = getNumber!float;
        }

        // String
        else static if (isSomeString!T) {
            T get() {
                import std.conv : to;
                return getString.to!T;
            }
        }

        // String 2
        else static if (is(T == enum) && T == Type.string) {
            alias get = getString;
        }

        // Arrays
        else static if (is(T == U[], U)) {
            alias get = getArray!U;
        }

        // Static arrays
        else static if (is(T == U[N], U, size_t N)) {
            alias get = getArray!T;
        }

        // Associative arrays
        else static if (is(T == U[wstring], U)) {
            alias get = getAssoc!U;
        }

        else static if (is(T == U[Y], U, Y) && isSomeString!Y) {
            T get() {
                import std.conv : to;
                return getAssoc!U.to!T;
            }
        }

        // Objects
        else static if (is(T == struct) || is(T == class)) {
            alias get = getStruct!T;
        }

        else static assert(0, fullyQualifiedName!T.format!"Type %s is currently unsupported by get()");

    }

    /// Skip the next value in the JSON.
    /// Throws: `JSONException` on syntax error.
    void skipValue() {

        const nextType = peekType();
        final switch (nextType) {

            case Type.null_:
                getNull();
                break;

            case Type.boolean:
                getBoolean();
                break;

            case Type.number:
                getNumber!string;
                break;

            case Type.string:
                getString();
                break;

            case Type.array:

                // Skip all values
                foreach (index; getArray) skipValue();
                break;

            case Type.object:

                // Skip all values
                foreach (key; getObject) skipValue();
                break;

        }

    }

    /// Expect the next value to be null and skip to the next value.
    ///
    /// Despite the name, this function doesn't return.
    ///
    /// Throws: `JSONException` if the next item isn't a null.
    void getNull() {

        skipSpace();

        // Check the values
        enforce!JSONException(input.skipOver("null"), failFoundMsg("Expected null"));

    }

    /// Get a boolean and skip to the next value.
    /// Throws: `JSONException` if the next item isn't a boolean.
    /// Returns: The parsed boolean.
    bool getBoolean() {

        skipSpace();

        // Check the values
        if (input.skipOver("true")) return true;
        else if (input.skipOver("false")) return false;

        // Or fail
        else throw new JSONException(failFoundMsg("Expected boolean"));

    }

    /// Get the next number.
    ///
    /// The number will be verified according to the JSON spec, but is parsed using std. Because of this, you can
    /// request a string return value, in order to perform manual conversion if needed.
    ///
    /// Implementation note: If the number contains an uppercase E, it will be converted to lowercase.
    ///
    /// Params:
    ///     T = Type of the returned number, eg. `int` or `real`. Can also be a `string` type, if conversion should be
    ///         done manually, or the number is expected to be big.
    /// Throws: `JSONException` if the next item isn't a number.
    /// Returns: The matched number.
    T getNumber(T)()
    if (isNumeric!T || isSomeString!T) {

        skipSpace();

        // Match the string
        dstring number = input.skipOver("-") ? "-" : "";

        /// Push the current character, plus following digits, to the result string.
        /// Returns: Length of the matched string.
        size_t pushDigit() {

            size_t length;

            do {
                number ~= input.front;
                input.popFront;
                length++;
            }
            while (!input.empty && '0' <= input.front && input.front <= '9');

            return length;

        }

        // Check the first digit
        enforce!JSONException(!input.empty && '0' <= input.front && input.front <= '9',
            failFoundMsg("Expected number"));

        // Parse integer part
        const leadingZero = input.front == '0';
        const digits = pushDigit();

        // Check for leading zeros
        enforce!JSONException(!leadingZero || digits == 1,
                failMsg("Numbers cannot have leading zeros, found"));

        // Fractal part
        if (!input.empty && input.front == '.') pushDigit();

        // Exponent
        if (!input.empty && (input.front == 'e' || input.front == 'E')) {

            // Add the E
            number ~= 'e';
            input.popFront;

            // EOF?
            enforce!JSONException(!input.empty, "Unexpected EOF in exponent");

            // Check for sign
            if (input.front == '-' || input.front == '+') {
                number ~= input.front;
                input.popFront;
            }

            // Push the numbers
            // RFC 8259 actually allows leading zeros here
            enforce!JSONException(
                '0' <= input.front && input.front <= '9',
                failMsg(input.front.format!"Unexpected character '%s' in exponent")
            );

            // Push the digits
            pushDigit();

        }

        import std.conv : to;
        return number.to!T;

    }

    /// Get the next string.
    /// Throws: `JSONException` if the next item isn't a string.
    /// Returns: The matched string in UTF-16, because JSON uses it to encode strings.
    wstring getString() {

        skipSpace();

        wstring result;
        size_t startLine = lineNumber;

        // Require a quotation mark
        enforce!JSONException(input.skipOver(`"`), failFoundMsg("Expected string"));

        // Read next characters
        loop: while (true) {

            enforce!JSONException(!input.empty, startLine.format!"Unclosed string starting at line %s");

            // Don't accept control codes
            enforce!JSONException(input.front != 10,
                failMsg("JSON strings cannot contain line feeds, use \n instead."));
            enforce!JSONException(input.front >= 20,
                failMsg("Illegal control point in a string, use an escape code instead"));

            switch (input.front) {

                // Closing the string
                case '"':

                    input.popFront;
                    break loop;

                // Escape code
                case '\\':

                    result ~= getEscape();
                    break;

                // Other characters
                default:

                    result ~= input.front;
                    input.popFront();

            }

        }

        return result;

    }

    /// Get array elements by iterating over them.
    ///
    /// Note: You must read exactly one array item per iteration, otherwise the generator will crash.
    ///
    /// Throws: `JSONException` if the next item isn't an array or there's a syntax error.
    /// Returns: A generator range yielding current array index until all the items are read.
    auto getArray() {

        import std.concurrency : Generator, yield;

        skipSpace();

        // Expect an array opening
        enforce!JSONException(input.skipOver("["), failFoundMsg("Expected an array"));

        return new Generator!size_t({

            size_t index;

            // Skip over space
            skipSpace();

            // Check the contents
            while (!input.skipOver("]")) {

                // Require a comma after non-zero indexes
                enforce!JSONException(
                    !index || input.skipOver(","),
                    failFoundMsg("Expected a comma between array elements")
                );

                // Expect an item
                yield(index++);

                skipSpace();

            }

        });

    }

    /// Get an array of elements matching the type.
    ///
    /// Throws: `JSONException` if there's a type mismatch or syntax error.
    T[] getArray(T)() {

        T[] result;
        foreach (index; getArray) {

            result ~= get!T;

        }

        return result;

    }

    ///
    unittest {

        auto json = JSONParser(q{ ["test", "foo", "bar"] });
        assert(json.getArray!string == ["test", "foo", "bar"]);

    }

    /// Get a static array of elements matching the type.
    /// Throws: `JSONException` if there's a type mismatch or syntax error.
    T getArray(T : Element[Size], Element, size_t Size)() {

        T result;
        foreach (index; getArray) {

            result[index] = get!Element;

        }
        return result;

    }

    ///
    unittest {

        auto text = q{ [1, 2, 3] };

        {
            auto json = JSONParser(text);
            auto values = json.getArray!(uint[3]);

            static assert(is(typeof(values) == uint[3]));
            assert(values == [1, 2, 3]);
        }

        {
            auto json = JSONParser(text);
            auto values = json.getArray!(uint, 3);

            static assert(is(typeof(values) == uint[3]));
            assert(values == [1, 2, 3]);

        }

    }

    /// Get a static array of elements matching the types.
    /// Throws: `JSONException` if there's a type mismatch or syntax error.
    Element[Size] getArray(Element, size_t Size)() {

        return getArray!(Element[Size]);

    }

    /// Get an associative array from the JSON.
    /// Throws: `JSONException` on type mismatch or syntax error.
    /// Returns: The requested associative array.
    T[wstring] getAssoc(T)() {

        T[wstring] result;
        foreach (key; getObject) {

            result[key] = get!T;

        }
        return result;

    }

    ///
    unittest {

        auto json = JSONParser(q{
            {
                "hello": 123,
                "foo": -123,
                "test": 42.123
            }
        });

        auto assoc = json.getAssoc!float;

        assert(assoc["hello"] == 123);
        assert(assoc["foo"] == -123);
        assert(assoc["test"] == 42.123f);

    }

    /// Get object contents by iterating over them.
    ///
    /// Note: You must read exactly one item per key, otherwise the generator will crash.
    ///
    /// Throws: `JSONException` on type mismatch or syntax error.
    /// Returns: A generator yielding the found key, in document order.
    auto getObject() {

        import std.concurrency : Generator, yield;

        skipSpace();

        // Expect an array opening
        enforce!JSONException(input.skipOver("{"), failFoundMsg("Expected an object"));

        return new Generator!wstring({

            skipSpace();

            bool first = true;

            // Check the contents
            while (!input.skipOver("}")) {

                // If this isn't the first item
                if (!first) {

                    // Require a comma
                    enforce!JSONException(input.skipOver(","), failFoundMsg("Expected a comma between object items"));

                }
                else first = false;

                // Read the key
                auto key = getString();

                // Expect a colon
                skipSpace();
                enforce!JSONException(input.skipOver(":"), failFoundMsg("Expected a colon after object key"));

                // Pass the key to the item
                yield(key);

                // Skip space
                skipSpace();

            }

        });

    }

    /// Push object contents into a struct or class.
    ///
    /// The object doesn't have to contain all fields defined in the struct or class.
    ///
    /// Fields that share names with D reserved keywords can be suffixed with `_`, as according to the
    /// $(LINK2 https://dlang.org/dstyle.html#naming_keywords, D style).
    ///
    /// The struct or class must have a no argument constructor available.
    ///
    /// Throws: `JSONException` if there's a type mismatch or syntax error.
    /// Params:
    ///     T = Type of the struct.
    ///     obj = Instance of the object to modify. Classes are edited in place, structs are not.
    ///     fallback = Function to call if a field doesn't exist. Otherwise, it will be ignored.
    /// Returns:
    ///     1. If T is a struct, a copy of the given object with updated properties.
    ///     2. If T is a class, a reference to the given object. (ret is obj)
    T updateStruct(T)(T obj, void delegate(ref T, wstring) fallback = null)
    if (is(T == struct) || is(T == class)) {

        // Expect an object
        foreach (key; getObject) {

            import std.conv : to;
            import std.string : chomp;
            import std.meta : AliasSeq, staticMap;

            // Check parents
            static if (is(T == class)) {
                alias FullT = AliasSeq!(BaseClassesTuple!T, T);
            }
            else {
                alias FullT = T;
            }

            alias FieldTypes = staticMap!(Fields, FullT);

            // Match struct fields
            fields: switch (key.to!string) {

                static foreach (i, field; staticMap!(FieldNameTuple, FullT)) {{

                    alias FieldType = FieldTypes[i];
                    static if (!hasUDA!(mixin("T." ~ field), JSONExclude)) {

                        case field.chomp("_"):

                            __traits(getMember, obj, field) = get!FieldType;
                            break fields;

                    }

                }}

                default:

                    // If the fallback isn't null, call it
                    if (fallback !is null) fallback(obj, key);

                    // Otherwise just skip the value
                    else skipValue();

            }

        }

        return obj;

    }

    /// Ditto
    T getStruct(T)(void delegate(ref T, wstring) fallback = null) {

        // Create the object
        static if (is(T == struct)) {
            auto obj = T();
        }
        else {
            auto obj = new T();
        }

        return updateStruct!T(obj, fallback);

    }

    ///
    unittest {

        struct Example {
            string name;
            int version_;
            string[] contents;
        }

        auto json = JSONParser(q{
            {
                "name": "rcjson",
                "version": 123,
                "contents": ["json-parser"]
            }
        });
        const obj = json.getStruct!Example;
        assert(obj.name == "rcjson");
        assert(obj.version_ == 123);
        assert(obj.contents == ["json-parser"]);

    }

    /// Using fallback
    unittest {

        struct Table {

            string tableName;

            @JSONExclude
            string[string] attributes;

        }

        auto json = JSONParser(q{
            {
                "tableName": "Player",
                "id": "PRIMARY KEY INT",
                "name": "VARCHAR(30)",
                "xp": "INT",
                "attributes": "VARCHAR(60)"
            }
        });

        auto table = json.getStruct!Table((ref Table table, wstring key) {

            import std.conv : to;
            table.attributes[key.to!string] = json.getString.to!string;

        });

        assert(table.tableName == "Player");
        assert(table.attributes["id"] == "PRIMARY KEY INT");
        assert(table.attributes["xp"] == "INT");
        assert(table.attributes["attributes"] == "VARCHAR(60)");

    }

    /// Copy the parser. Useful to keep document data for later.
    JSONParser save() {

        return this;

    }

    /// Saving parser state
    unittest {
        auto json = JSONParser(q{
            [
                {
                    "name": "A",
                    "ability": "doFoo",
                    "health": 30
                },
                {
                    "name": "B",
                    "ability": "doBar"
                },
                {
                    "name": "C",
                    "inherits": ["A", "B"],
                    "ability": "doTest"
                }
            ]
        });

        static class EntityMeta {
            string name;
            string[] inherits;
        }

        static class Entity : EntityMeta {
            string ability;
            int health = 100;
        }

        JSONParser[string] states;
        Entity[string] entities;
        foreach (index; json.getArray) {

            // Get the metadata and save the state
            auto state = json.save;
            auto meta = json.getStruct!EntityMeta;  // Efficient and quick way to fetch the two attributes
            states[meta.name] = state.save;

            // Create the object
            auto entity = new Entity();

            // Inherit properties
            foreach (parent; meta.inherits) {

                // Get the parent
                auto parentState = states[parent].save;

                // Inherit its values
                parentState.updateStruct(entity);
                // Note: We're operating on classes. Use `entity = state.getStruct(entity)` on structs

            }

            // Now, add local data
            state.updateStruct(entity);

            entities[entity.name] = entity;

        }

        const a = entities["A"];
        assert(a.name == "A");
        assert(a.ability == "doFoo");
        assert(a.health == 30);

        const b = entities["B"];
        assert(b.name == "B");
        assert(b.ability == "doBar");
        assert(b.health == 100);

        const c = entities["C"];
        assert(c.name == "C");
        assert(c.ability == "doTest");
        assert(c.health == 30);  // A had explicitly stated health, B did not — inherited from A.
                                 // Otherwise impossible without saving states.

    }

    /// Parse the next escape code in the JSON.
    /// Returns: The escaped character.
    private wchar getEscape() {

        assert(!input.empty, "getEscape called with empty input");
        assert(input.front == '\\', "getEscape called, but no escape code was found");

        // Pop the backslash
        input.popFront();

        // Message to throw in case of failure
        string eofError() { return failMsg("Reached end of file in the middle of an escape code"); }

        enforce!JSONException(!input.empty, eofError);

        // Match the first character of the escape code
        const ch = input.front;
        input.popFront();

        switch (ch) {

            // Obvious escape codes
            case '"', '\\', '/': return cast(wchar) ch;

            // Special
            case 'b': return '\b';
            case 'f': return '\f';
            case 'n': return '\n';
            case 'r': return '\r';
            case 't': return '\t';
            case 'u':

                import std.conv : to;

                // Take next 4 characters
                auto code = input.take(4).to!string;

                // Must be 4 characters
                enforce!JSONException(code.length == 4, eofError);

                // Now, create the character
                return code.to!ushort(16);

            default:

                throw new JSONException(
                    failMsg(ch.format!"Unknown escape code '\\%s'")
                );


        }

    }

    /// Skips over line breaks and advances line count.
    /// Returns: Matched line breaks.
    private string getLineBreaks() {

        import std.stdio : writeln;

        string match = "";

        /// Last matched separator
        dchar lineSep;

        loop: while (!input.empty)
        switch (input.front) {

            case '\n', '\r':

                // Match the next character
                match ~= input.front;

                // Using the same separator, or this is the first one
                if (lineSep == input.front || lineSep == dchar.init) {

                    // Advance line count
                    lineNumber++;

                }

                // Encountered a different one? Most likely CRLF, so we shouldn't count the LF.

                // Update the lineSep char
                lineSep = input.front;
                input.popFront();

                // Continue parsing
                continue;

            default: break loop;

        }

        // Return the match
        return match;

    }

    /// Skip whitespace in the document.
    private void skipSpace() {

        // RFC: See section 2.

        // Skip an indefinite amount
        while (!input.empty)
        switch (input.front) {

            // Line feed
            case '\n', '\r':

                // Skip over
                getLineBreaks();
                continue;

            // Remove whitespace
            case ' ', '\t':
                input.popFront();
                continue;

            // Stop on anything else
            default:
                return;

        }

    }

    /// Fail with given message and include a line number.
    private string failMsg(string msg) {

        return msg.format!"%s on line %s"(lineNumber);

    }

    /// Fail with the given message and output the given message, including the next word in the input range.
    pragma(inline, true);
    private string failFoundMsg(string msg) {

        skipSpace();

        return failMsg(msg.format!"%s, found %s"(peekType));

    }

}

unittest {

    auto json = JSONParser(q{
        [
            "hello",
            "world",
            true,
            123,
            {
                "undefined": null,
                "int": 123,
                "negative": -123,
                "float": 123.213
            }
        ]
    });

    // Type validation
    assert(json.getBoolean.collectExceptionMsg == "Expected boolean, found array on line 2");

    // Checking types early
    assert(json.peekType == JSONParser.Type.array);

    // Now, let's get into the contents of the array
    foreach (index; json.getArray) {

        with (JSONParser.Type)
        switch (json.peekType) {

            case string:

                // We have two strings, at indexes 0 and 1
                if (index == 0) assert(json.getString == "hello");
                if (index == 1) assert(json.getString == "world");
                break;

            case boolean:

                // The only boolean in our array is "true"
                assert(json.getBoolean);
                break;

            case number:

                // Now we've got a number
                assert(json.getNumber!int == 123);
                break;

            case object:

                wstring[] keys;

                // Iterate over object items
                foreach (key; json.getObject) {

                    if (key == "undefined") json.getNull();
                    else if (key == "int") assert(json.getNumber!int == 123);
                    else json.skipValue();

                    keys ~= key;

                }

                // Checked the keys, all in order
                assert(keys == ["undefined"w, "int"w, "negative"w, "float"w]);

                break;

            default:

                assert(0);

        }

    }

}

///
unittest {

    auto json = JSONParser(q{
        [
            {
                "name": "John",
                "surname": "Doe",
                "age": 42
            },
            {
                "name": "Jane",
                "surname": "Doe",
                "age": 46
            }
        ]
    });

    // Check each array item
    foreach (index; json.getArray) {

        // Read the object
        auto keys = json.getObject;

        // Check the name
        assert(keys.front == "name");
        json.skipValue();
        keys.popFront();

        // Surname
        assert(keys.front == "surname");
        assert(json.getString == "Doe");
        keys.popFront();

        // Age
        assert(keys.front == "age");
        assert(json.getNumber!uint > 40);
        keys.popFront();

        // Done
        assert(keys.empty);

    }

}

/// Moving to struct with a helper
unittest {

    struct Person {

        string name;
        string surname;
        uint age;

    }

    auto json = JSONParser(q{
        [
            {
                "name": "John",
                "surname": "Doe",
                "age": 42
            },
            {
                "name": "Jane",
                "surname": "Doe",
                "age": 46
            }
        ]
    });

    auto people = json.getArray!Person;

    assert(people[0].name == "John");
    assert(people[1].name == "Jane");
    assert(people[0].age == 42);
    assert(people[1].age == 46);

}

unittest {

    foreach (num; [
        "0",
        "123",
        "123.123",
        "-123",
        "-3",
        "-3.123",
        "0.123e2",
        "0.123e-2",
        "0.123e-2",
        "0.0123e-2",
    ]) {

        import std.conv : to;
        import std.string : toLower;

        auto res1 = JSONParser(num).getNumber!real;
        assert(res1 == num.to!real, format!`Number "%s" is parsed into a wrong number value, "%s"`(num, res1));

        auto res2 = JSONParser(num).getNumber!string;
        assert(res2 == num.toLower, format!`Number "%s" changes string value to "%s"`(num, res2));

    }

    // Invalid cases
    foreach (num; [
        "0123",
        "+123",
        "- 123",
        // Those will not fail instantly, requie checking next value
        // "123e123.123"
        // "123 123"
    ]) {

        assertThrown(JSONParser(num).getNumber!string,
            num.format!`Number "%s" is invalid, but doesn't throw when parsed`);

    }

}

unittest {

    import std.array : array;

    auto json = JSONParser("[false true]");

    assert(
        json.getArray.map!(i => json.getBoolean).array.collectExceptionMsg
        == "Expected a comma between array elements, found boolean on line 1"
    );

}

unittest {

    auto text = q{
        123 "hello, world" 123.124
    };

    auto jsonA = JSONParser(text);
    auto jsonB = JSONParser(text);
    assert(jsonA.get!int == jsonB.getNumber!int);
    assert(jsonA.get!wstring == jsonB.getString);
    assert(jsonA.get!float == jsonB.getNumber!float);



}

unittest {

    auto json1 = JSONParser(`"\uD834\uDD1E"`);
    assert(json1.getString == "\U0001D11E");

    import std.stdio : writefln;
    auto json2 = JSONParser(`"\u0020\u000A\n\t"`);
    assert(json2.getString == " \n\n\t");

}

unittest {

    struct A {

        struct B {

            string foo;
            string bar;

        }

        string name;
        int number = 1;
        B sampleTexts;
        string[] notes;

    }

    auto json = JSONParser(q{
        {
            "name": "library",
            "sampleTexts": {
                "foo": "Dolorem ipsum, quia dolor sit",
                "bar": "amet, consectetur, adipisci velit"
            },
            "notes": [
                "hello,",
                "world!"
            ]
        }
    });

    import std.string : startsWith;

    auto a = json.getStruct!A;

    assert(a.name == "library");
    assert(a.number == 1);
    assert(a.sampleTexts.foo.startsWith("Dolorem ipsum"));
    assert(a.sampleTexts.bar.startsWith("amet"));
    assert(a.notes == ["hello,", "world!"]);

}

unittest {

    struct Test {

        int[3] test;

    }

    auto text = q{ { "test": [1, 2, 3] } };
    auto json = JSONParser(text);

    auto obj = json.getStruct!Test;
    assert(obj.test == [1, 2, 3]);

}

/// UDA used to exclude struct fields from serialization.
enum JSONExclude;

///
unittest {

    struct Product {

        string name;
        float price;

        @JSONExclude {
            float weight;
            string weightUnit;
        }

    }

    auto json = JSONParser(q{
        {
            "name": "foo",
            "price": 123,
            "weight": "500g"
        }
    });

    auto product = json.getStruct!Product((ref Product obj, wstring key) {

        import std.conv : to ;
        import std.uni : isAlpha;
        import std.algorithm : countUntil;

        if (key == "weight") {

            const value = json.getString;
            const splitIndex = value.countUntil!isAlpha;

            // Extract the unit
            obj.weight = value[0..splitIndex].to!float;
            obj.weightUnit = value[splitIndex..$].to!string;
        }

    });

    assert(product.name == "foo");
    assert(product.price == 123);
    assert(product.weight == 500f);
    assert(product.weightUnit == "g");

}

/// Thrown if JSON parsing fails.
class JSONException : Exception {

    mixin basicExceptionCtors;

}
