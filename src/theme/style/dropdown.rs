// Copyright 2023 System76 <info@system76.com>
// SPDX-License-Identifier: MPL-2.0

use crate::Theme;
use crate::widget::dropdown;
use iced::Background;

impl dropdown::menu::StyleSheet for Theme {
    type Style = ();

    fn appearance(&self, _style: &Self::Style) -> dropdown::menu::Appearance {
        let cosmic = self.cosmic();
        let text_color = cosmic.on_bg_color().into();

        dropdown::menu::Appearance {
            text_color,
            background: Background::Color(cosmic.background.component.base.into()),
            border_width: 0.0,
            border_radius: cosmic.corner_radii.radius_m.into(),
            border_color: iced::Color::TRANSPARENT,

            hovered_text_color: text_color,
            hovered_background: Background::Color(iced::Color::from_rgb8(230, 230, 230)),

            selected_text_color: text_color,
            selected_background: Background::Color(iced::Color::TRANSPARENT),

            description_color: cosmic.primary.component.on_disabled.into(),
        }
    }
}
