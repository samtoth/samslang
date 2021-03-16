#![warn(
    clippy::all,
    clippy::restriction,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo
)]

use std::collections::HashMap;

trait Type {}

struct Unit();

impl Type for Unit {}

struct Ident(String);

struct Expression(Box<dyn Type>);

struct Scope(HashMap<Ident, Expression>);
