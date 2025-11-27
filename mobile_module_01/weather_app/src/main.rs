use dioxus::prelude::*;
use dioxus_free_icons::icons::ld_icons::LdSun;
use dioxus_free_icons::icons::md_action_icons::{
    MdDateRange, MdHistory, MdSettingsBrightness, MdToday,
};
use dioxus_free_icons::{Icon, IconShape};
use serde::{Deserialize, Serialize};
use std::rc::Rc;
use std::thread::sleep;
use std::time::Duration;

/// Define a components module that contains all shared components for our app.
mod components;
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

#[derive(Clone)]
struct TabItem {
    name: &'static str,
    icon: Rc<dyn TabIcon>,
}

/// Represents the current display mode - either search text or geolocation
#[derive(Clone, PartialEq)]
enum DisplayMode {
    Search(String),
    None,
}

impl std::fmt::Display for DisplayMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let active_tab = use_context::<Signal<String>>();
        match self {
            DisplayMode::Search(location) => write!(f, "{}\n{}", active_tab(), location),
            DisplayMode::None => write!(f, "{}", active_tab()),
        }
    }
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
    // Shared state for the display mode
    let display_mode = use_signal(|| DisplayMode::None);
    let active_tab_name = use_signal(|| "Currently".to_string());
    let active_index = use_signal(|| 0usize);
    let tabs = use_signal(|| {
        [
            TabItem {
                name: "Currently",
                icon: Rc::new(LdSun),
            },
            TabItem {
                name: "Today",
                icon: Rc::new(MdToday),
            },
            TabItem {
                name: "Weekly",
                icon: Rc::new(MdDateRange),
            },
        ]
        .to_vec()
    });
    use_context_provider(|| display_mode);
    use_context_provider(|| active_tab_name);
    use_context_provider(|| active_index);
    use_context_provider(|| tabs);

    rsx! {
        document::Link { rel: "stylesheet", href: MAIN_CSS }

        div { class: "app",
            TopBar {}
            MainDisplay {}
            BottomBar {}
        }
    }
}

#[component]
fn MainDisplay() -> Element {
    let mut display_mode = use_context::<Signal<DisplayMode>>();
    let tabs = use_context::<Signal<Vec<TabItem>>>();
    let mut active_index = use_context::<Signal<usize>>();
    let drag_start_x = use_signal(|| 0.0f64);

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
        div {
            class: "main-display",
            onpointerdown: on_pointer_down,
            onpointerup: on_pointer_up,
            h1 { "{display_mode()}" }
        }
    }
}

#[component]
fn BottomBar() -> Element {
    let mut active_tab_name = use_context::<Signal<String>>();
    let mut display_mode = use_context::<Signal<DisplayMode>>();
    let tabs = use_context::<Signal<Vec<TabItem>>>();
    let mut active_index = use_context::<Signal<usize>>();

    active_tab_name.set(tabs.get(active_index()).unwrap().name.to_string());
    rsx! {
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
                                onclick: move |_| {
                                    display_mode.read();
                                    active_index.set(idx);
                                },
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

#[derive(Serialize, Deserialize)]
struct LocationForm {
    location: String,
}

#[component]
fn TopBar() -> Element {
    let mut display_mode = use_context::<Signal<DisplayMode>>();
    let active_tab_name = use_context::<Signal<String>>();
    let mut search_text = use_signal(String::new);
    let mut loading_geo = use_signal(|| false);

    // Handle geolocation click
    let get_geolocation = move |_| {
        loading_geo.set(true);
        spawn(async move {
            // Simulate a delay for geolocation fetching async operation
            tokio::time::sleep(Duration::from_secs(2)).await;
            display_mode.set(DisplayMode::Search("Geolocation".to_string()));
            loading_geo.set(false);
        });
        /* loading_geo.set(true);

        spawn(async move {
            // Use eval to access browser's Geolocation API
            let result = eval(
                r#"
                return await new Promise((resolve, reject) => {
                    if (!navigator.geolocation) {
                        reject("Geolocation not supported");
                        return;
                    }
                    navigator.geolocation.getCurrentPosition(
                        (position) => {
                            resolve({
                                lat: position.coords.latitude,
                                lon: position.coords.longitude
                            });
                        },
                        (error) => {
                            reject(error.message);
                        },
                        { enableHighAccuracy: true, timeout: 10000 }
                    );
                });
            "#,
            )
            .await;

            loading_geo.set(false);

            if let Ok(coords) = result {
                if let (Some(lat), Some(lon)) = (
                    coords.get("lat").and_then(|v| v.as_float()),
                    coords.get("lon").and_then(|v| v.as_float()),
                ) {
                    display_mode.set(DisplayMode::Geolocation { lat, lon });
                }
            }
        }); */
    };

    rsx! {
        header { class: "top-bar",
            div { class: "search-container",
                form {
                    class: "search-form",
                    onsubmit: move |e| {
                        e.prevent_default();
                        println!("Form submitted with value: {:?}", e);
                        let value: LocationForm = e.parsed_values().unwrap();
                        println!("Parsed value: {}", value.location);
                        search_text.set(value.location.clone());
                        if !value.location.is_empty() {
                            display_mode.set(DisplayMode::Search(value.location));
                        } else {
                            display_mode.set(DisplayMode::None);
                        }
                    },
                    input {
                        r#type: "text",
                        id: "search-input",
                        class: "search-input",
                        placeholder: "Search...",
                        value: "{search_text}",
                        oninput: move |e| {
                            let value = e.value();
                            search_text.set(value.clone());
                        },
                        name: "location",
                    }
                }

                button {
                    class: "geo-button",
                    onclick: get_geolocation,
                    disabled: loading_geo(),
                    if loading_geo() {
                        "Loading..."
                    } else {
                        "Get Location"
                    }
                }
            }
        }
    }
}
