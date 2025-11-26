use dioxus::prelude::*;
use dioxus_free_icons::icons::md_action_icons::{MdDateRange, MdHistory, MdToday};
use dioxus_free_icons::{Icon, IconShape};
use std::rc::Rc;

/// Define a components module that contains all shared components for our app.
mod components;
const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/styling/main.css");

trait TabIcon {
    fn render_icon(&self, color: String) -> Element;
}

impl<T: IconShape + Clone + PartialEq + 'static> TabIcon for T {
    fn render_icon(&self, color: String) -> Element {
        rsx! {
            Icon {
                width: 24,
                height: 24,
                fill: "{color}",
                icon: self.clone(),
            }
        }
    }
}

struct TabItem {
    name: &'static str,
    icon: Rc<dyn TabIcon>,
}

fn main() {
    dioxus::launch(App);
}

/// App is the main component of our app. Components are the building blocks of dioxus apps. Each component is a function
/// that takes some props and returns an Element. In this case, App takes no props because it is the root of our app.
///
/// Components should be annotated with `#[component]` to support props, better error messages, and autocomplete
#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        BottomBar {}
    }
}

#[component]
fn AppBar() -> Element {
    rsx! {
        nav { class: "app-bar",
            div { class: "app-bar__title", "Weather App" }
        }
    }
}

#[component]
fn TopBar() -> Element {
    rsx! {}
}

#[component]
fn BottomBar() -> Element {
    let mut active_index = use_signal(|| 0usize);
    let drag_start_x = use_signal(|| 0.0f64);
    let tabs = [
        TabItem {
            name: "Currently",
            icon: Rc::new(MdDateRange),
        },
        TabItem {
            name: "Today",
            icon: Rc::new(MdToday),
        },
        TabItem {
            name: "Weekly",
            icon: Rc::new(MdHistory),
        },
    ];
    let tabs_len = tabs.len();
    let on_pointer_down = {
        let mut drag_start_x = drag_start_x;
        move |evt: Event<PointerData>| {
            drag_start_x.set(evt.data.client_coordinates().x);
        }
    };
    let on_pointer_up = {
        let mut active_index = active_index;
        move |evt: Event<PointerData>| {
            let end_x = evt.data.client_coordinates().x;
            let start_x = drag_start_x();
            let diff = end_x - start_x;
            let threshold = 50.0;
            if diff.abs() > threshold {
                if diff < 0.0 && active_index() < tabs_len - 1 {
                    active_index.set(active_index() + 1);
                } else if diff > 0.0 && active_index() > 0 {
                    active_index.set(active_index() - 1);
                }
            }
        }
    };
    rsx! {
        div { style: "display: flex; flex-direction: column; height: 100vh; width: 100%; overflow: hidden; font-family: sans-serif; background-color: #f0f0f0;",
            div {
                style: "flex: 1; display: flex; justify-content: center; align-items: center; background-color: #f0f0f0;",
                onpointerdown: on_pointer_down,
                onpointerup: on_pointer_up,
                h1 { style: "color: black", "{tabs[active_index()].name}" }
            }
            div { style: "height: 60px; display: flex; border-top: 1px solid #ddd; background: white;",
                {
                    tabs.iter()
                        .enumerate()
                        .map(|(idx, item)| {
                            let is_active = active_index() == idx;
                            let color = if is_active {
                                "blue".to_string()
                            } else {
                                "gray".to_string()
                            };
                            rsx! {
                                div {
                                    key: "{item.name}",
                                    style: "flex: 1; display: flex; flex-direction: column; align-items: center; justify-content: center; cursor: pointer; transition: all 0.2s;",
                                    onclick: move |_| active_index.set(idx),
                                    {item.icon.render_icon(color.clone())}
                                    span { style: "font-size: 12px; font-weight: 500; margin-top: 4px; color: {color};",
                                        "{item.name}"
                                    }
                                }
                            }
                        })
                }
            }
        }
    }
}
