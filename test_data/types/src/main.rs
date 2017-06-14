struct Foo {
    f: u32,
}

fn main() {
    let x = Foo { f: 42 };
    let _: Foo = x;
}

fn foo(x: Foo) -> Foo {
    panic!();
}

impl PartialEq for Foo {
    fn eq(&self, other: &Foo) -> bool {
        true
    }
}