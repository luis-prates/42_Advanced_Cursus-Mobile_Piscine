use dioxus::{logger::tracing::info, prelude::*};

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const HEADER_SVG: Asset = asset!("/assets/header.svg");

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Stylesheet { href: MAIN_CSS }
        AppBar {}
        Calculator {}
    }
}

#[component]
pub fn Calculator() -> Element {
    let calc_pad = [
        "7", "8", "9", "C", "AC", "4", "5", "6", "+", "-", "1", "2", "3", "x", "/", "0", ".", "00",
        "=",
    ];

    rsx! {
        div { class: "calculator",
            div { class: "text-field-container",
                DisplayField {}
                DisplayField {}
            }
            div { id: "buttons",
                for value in calc_pad {
                    match value {
                        v if v.parse::<i32>().is_ok() || v == "." => rsx! {
                        button {
                            class: "grid-item number",
                            onclick: move |_| info!("Button pressed: {value}"),
                            "{value}"
                        }
                    },
                        "C" | "AC" => rsx! {
                        button {
                            class: "grid-item reset",
                            onclick: move |_| info!("Button pressed: {value}"),
                            "{value}"
                        }
                    },
                        _ => rsx! {
                        button {
                            class: "grid-item operator",
                            onclick: move |_| info!("Button pressed: {value}"),
                            "{value}"
                        }
                    },
                    }
                }
                        // button { class: "skip", "skip" }
            // button { class: "save", "save!" }
            }
        }
    }
}

#[component]
pub fn AppBar() -> Element {
    rsx! {
        nav { class: "app-bar",
            div { class: "app-bar__title", "My App" }
        }
    }
}

#[component]
pub fn DisplayField() -> Element {
    rsx! {
        div { class: "text-display", "0" }
    }
}
