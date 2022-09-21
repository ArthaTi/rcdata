import tests.parser.base;
import rcdata.parser;

struct Foo {

    mixin makeParser!(string, supply);

}

unittest {

    int count = 3;

    auto result = Foo.match!(
        Foo.matchAny!(a => count-- > 0)
    )("hello, world!");

    assert(result.data[0] == Token(TokenType.none, "h"));

}
