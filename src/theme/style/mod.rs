// Copyright 2022 System76 <info@system76.com>
// SPDX-License-Identifier: MPL-2.0

//! Stylesheet implements for [`crate::Theme`]

use iced_core::Color;

/// Mode-aware "ink" color matching icetron's opacity-based tokens: white at
/// `alpha`/255 in dark mode, black at `alpha`/255 in light mode. Used for text,
/// borders, dividers, and neutral hover/pressed states so menu and header
/// chrome adapt to dark/light like icetron-theme-humain does.
#[must_use]
pub(crate) fn ink(is_dark: bool, alpha: u8) -> Color {
    let c = if is_dark { 1.0 } else { 0.0 };
    Color::from_rgba(c, c, c, f32::from(alpha) / 255.0)
}

/// Mode-aware elevated surface for menus and header chrome — icetron
/// `surface-elevated`/`card`: white in light mode, `#1e1e1e` in dark mode.
#[must_use]
pub(crate) fn elevated_surface(is_dark: bool) -> Color {
    if is_dark {
        Color::from_rgb8(30, 30, 30)
    } else {
        Color::WHITE
    }
}

mod button;
pub use self::button::Button;

mod dropdown;

pub mod iced;
#[doc(inline)]
pub use self::iced::Checkbox;
#[doc(inline)]
pub use self::iced::Container;
#[doc(inline)]
pub use self::iced::ProgressBar;
#[doc(inline)]
pub use self::iced::Rule;
#[doc(inline)]
pub use self::iced::Svg;
#[doc(inline)]
pub use self::iced::Text;

pub mod menu_bar;

mod segmented_button;
#[doc(inline)]
pub use self::segmented_button::SegmentedButton;

mod text_input;
#[doc(inline)]
pub use self::text_input::TextInput;

#[cfg(all(feature = "wayland", feature = "winit"))]
pub mod tooltip;
#[cfg(all(feature = "wayland", feature = "winit"))]
pub use tooltip::Tooltip;
