import parser.base;
import rcdata.parser;

struct Foo {

    mixin makeParser!(string, consume);

}

unittest {

    int count = 3;

    auto result = Foo.match!(
        Foo.matchAny!(a => count-- > 0)
    )("hello, world!");

    assert(result.tokens[0] == Token(1, "h"));

}
