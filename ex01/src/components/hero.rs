use dioxus::{html::div, logger::tracing::info, prelude::*};

const HEADER_SVG: Asset = asset!("/assets/header.svg");

#[component]
pub fn Hero() -> Element {
    let mut value = use_signal(|| true);
    rsx! {
        // We can create elements inside the rsx macro with the element name followed by a block of attributes and children.
        div {
            // Attributes should be defined in the element before any children
            id: "hero",
            // After all attributes are defined, we can define child elements and components
            div { id: "links",
                p {
                    if value() {
                        "A simple text"
                    } else {
                        "Hello World!"
                    }
                }
                button { onclick: move |_| value.set(!value()), "Click me!" }
            }
        }
    }
}
