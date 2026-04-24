// From iced_aw, license MIT

//! Change the appearance of menu bars and their menus.
use std::sync::Arc;

use crate::Theme;
use iced_widget::core::Color;
use iced_widget::core::Shadow;

/// The appearance of a menu bar and its menus.
#[derive(Debug, Clone)]
pub struct Appearance {
    /// The background color of the menu bar and its menus.
    pub background: Color,
    /// The border width of the menu bar and its menus.
    pub border_width: f32,
    /// The border radius of the menu bar.
    pub bar_border_radius: [f32; 4],
    /// The border radius of the menus.
    pub menu_border_radius: [f32; 4],
    /// The border [`Color`] of the menu bar and its menus.
    pub border_color: Color,
    /// The expand value of the menus' background
    pub background_expand: [u16; 4],
    // /// The highlighted path [`Color`] of the the menu bar and its menus.
    pub path: Color,
    /// The shadow layers of the menus.
    pub shadow: Vec<Shadow>,
}

/// The style sheet of a menu bar and its menus.
pub trait StyleSheet {
    /// The supported style of the [`StyleSheet`].
    type Style: Default;

    /// Produces the [`Appearance`] of a menu bar and its menus.
    fn appearance(&self, style: &Self::Style) -> Appearance;
}

/// The style of a menu bar and its menus
#[derive(Default, Clone)]
#[allow(missing_debug_implementations)]
pub enum MenuBarStyle {
    /// The default style.
    #[default]
    Default,
    /// A [`Theme`] that uses a `Custom` palette.
    Custom(Arc<dyn StyleSheet<Style = Theme> + Send + Sync>),
}

impl From<fn(&Theme) -> Appearance> for MenuBarStyle {
    fn from(f: fn(&Theme) -> Appearance) -> Self {
        Self::Custom(Arc::new(f))
    }
}

impl StyleSheet for fn(&Theme) -> Appearance {
    type Style = Theme;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        (self)(style)
    }
}

impl StyleSheet for Theme {
    type Style = MenuBarStyle;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        let cosmic = self.cosmic();
        let component = &cosmic.background.component;

        match style {
            MenuBarStyle::Default => {
                Appearance {
                    background: Color::WHITE,
                    border_width: 1.0,
                    bar_border_radius: cosmic.corner_radii.radius_xl,
                    menu_border_radius: [8.0; 4],
                    border_color: Color::from_rgba(0.0, 0.0, 0.0, 13.0 / 255.0),
                    // 4px top/bottom creates visual padding between items and
                    // container edge. 1px left/right insets items from the border.
                    // Popup surface is sized to accommodate the expand.
                    background_expand: [4, 1, 4, 1],
                    // Path highlight for active folder item (8% black, matches hover bg)
                    path: Color::from_rgba(0.0, 0.0, 0.0, 20.0 / 255.0),
                    // No shadow — Wayland popup surfaces clip shadows at their bounds
                    shadow: vec![],
                }
            }
            MenuBarStyle::Custom(c) => c.appearance(self),
        }
    }
}
