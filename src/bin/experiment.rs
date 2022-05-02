use std::{
    cell::RefCell,
    ops::Deref,
    rc::{Rc, Weak},
};

#[derive(Debug)]
enum Object {
    Invalid,
    Struct(Struct),
}

#[derive(Debug)]
struct Struct {
    name: String,
    elem: Weak<RefCell<Object>>,
}

fn main() {
    let mut objects: Vec<Rc<RefCell<Object>>> = Vec::new();

    objects.push(Rc::new(RefCell::new(Object::Invalid)));
    objects.push(Rc::new(RefCell::new(Object::Invalid)));

    let object1 = objects.first().unwrap();
    let object2 = objects.last().unwrap();

    object1.replace(Object::Struct(Struct {
        name: "object1".to_string(),
        elem: Rc::downgrade(object2),
    }));
    object2.as_ref().replace(Object::Struct(Struct {
        name: "object2".to_string(),
        elem: Rc::downgrade(object1),
    }));

    let o = Rc::clone(objects.first().unwrap());
    if let Object::Struct(s) = o.borrow().deref() {
        println!("{}", s.name);
        println!("{:?}", s.elem.upgrade());
    }

    println!("{:?}", objects);
}
