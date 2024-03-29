#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;

use rocket_contrib::json::Json;

#[get("/")]
fn index() -> &'static str {
    "Hello, world!"
}

#[get("/parse")]
fn parse() -> Json<Parse> {
    Json(Parse {

    })
}

fn main() {
    rocket::ignite().mount("/", routes![index]).launch();
}
