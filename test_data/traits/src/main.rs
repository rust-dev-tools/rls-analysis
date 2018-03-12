use mymod::MyTrait;

pub fn main() {
    let _ignored = mymod::MyStruct.trait_fn();
}

mod mymod {
    pub struct MyStruct;

    pub trait MyTrait {
        fn trait_fn(&self);
    }

    impl MyTrait for MyStruct {
        fn trait_fn(&self) {
        }
    }
}
