// Copyright 2022 System76 <info@system76.com>
// SPDX-License-Identifier: MPL-2.0

use crate::cosmic_theme::{Density, Spacing};
use crate::{Element, theme, widget};
use apply::Apply;
use derive_setters::Setters;
use iced::{Border, Color, Length};
use iced_core::{Vector, Widget, widget::tree};
use std::borrow::Cow;

#[must_use]
pub fn header_bar<'a, Message>() -> HeaderBar<'a, Message> {
    HeaderBar {
        title: Cow::Borrowed(""),
        app_icon: None,
        on_close: None,
        on_drag: None,
        on_maximize: None,
        on_minimize: None,
        on_right_click: None,
        start: Vec::new(),
        center: Vec::new(),
        end: Vec::new(),
        density: None,
        focused: false,
        maximized: false,
        sharp_corners: false,
        is_ssd: false,
        on_double_click: None,
        is_condensed: false,
        transparent: false,
        corner_radius: None,
    }
}

#[derive(Setters)]
pub struct HeaderBar<'a, Message> {
    /// Defines the title of the window
    #[setters(skip)]
    title: Cow<'a, str>,

    /// Optional app icon displayed before the title
    #[setters(skip)]
    app_icon: Option<widget::icon::Handle>,

    /// A message emitted when the close button is pressed.
    #[setters(strip_option)]
    on_close: Option<Message>,

    /// A message emitted when dragged.
    #[setters(strip_option)]
    on_drag: Option<Message>,

    /// A message emitted when the maximize button is pressed.
    #[setters(strip_option)]
    on_maximize: Option<Message>,

    /// A message emitted when the minimize button is pressed.
    #[setters(strip_option)]
    on_minimize: Option<Message>,

    /// A message emitted when the header is double clicked,
    /// usually used to maximize the window.
    #[setters(strip_option)]
    on_double_click: Option<Message>,

    /// A message emitted when the header is right clicked.
    #[setters(strip_option)]
    on_right_click: Option<Message>,

    /// Elements packed at the start of the headerbar.
    #[setters(skip)]
    start: Vec<Element<'a, Message>>,

    /// Elements packed in the center of the headerbar.
    #[setters(skip)]
    center: Vec<Element<'a, Message>>,

    /// Elements packed at the end of the headerbar.
    #[setters(skip)]
    end: Vec<Element<'a, Message>>,

    /// Controls the density of the headerbar.
    #[setters(strip_option)]
    density: Option<Density>,

    /// Focused state of the window
    focused: bool,

    /// Maximized state of the window
    maximized: bool,

    /// Whether the corners of the window should be sharp
    sharp_corners: bool,

    /// HeaderBar used for server-side decorations
    is_ssd: bool,

    /// Whether the headerbar should be compact
    is_condensed: bool,

    /// Whether the headerbar should be transparent
    transparent: bool,

    /// Explicit corner radius [TL, TR, BR, BL] from the compositor.
    /// When set, overrides the theme's radius_window() for SSD headers.
    #[setters(strip_option)]
    corner_radius: Option<[f32; 4]>,
}

impl<'a, Message: Clone + 'static> HeaderBar<'a, Message> {
    /// Defines the title of the window
    #[must_use]
    pub fn title(mut self, title: impl Into<Cow<'a, str>> + 'a) -> Self {
        self.title = title.into();
        self
    }

    /// Sets the app icon displayed before the title
    #[must_use]
    pub fn app_icon(mut self, icon: widget::icon::Handle) -> Self {
        self.app_icon = Some(icon);
        self
    }

    /// Pushes an element to the start region.
    #[must_use]
    pub fn start(mut self, widget: impl Into<Element<'a, Message>> + 'a) -> Self {
        self.start.push(widget.into());
        self
    }

    /// Pushes an element to the center region.
    #[must_use]
    pub fn center(mut self, widget: impl Into<Element<'a, Message>> + 'a) -> Self {
        self.center.push(widget.into());
        self
    }

    /// Pushes an element to the end region.
    #[must_use]
    pub fn end(mut self, widget: impl Into<Element<'a, Message>> + 'a) -> Self {
        self.end.push(widget.into());
        self
    }

    /// Build the widget
    #[must_use]
    #[inline]
    pub fn build(self) -> HeaderBarWidget<'a, Message> {
        HeaderBarWidget {
            header_bar_inner: self.view(),
        }
    }
}

pub struct HeaderBarWidget<'a, Message> {
    header_bar_inner: Element<'a, Message>,
}

impl<Message: Clone + 'static> Widget<Message, crate::Theme, crate::Renderer>
    for HeaderBarWidget<'_, Message>
{
    fn diff(&mut self, tree: &mut tree::Tree) {
        tree.diff_children(&mut [&mut self.header_bar_inner]);
    }

    fn children(&self) -> Vec<tree::Tree> {
        vec![tree::Tree::new(&self.header_bar_inner)]
    }

    fn size(&self) -> iced_core::Size<Length> {
        self.header_bar_inner.as_widget().size()
    }

    fn layout(
        &self,
        tree: &mut tree::Tree,
        renderer: &crate::Renderer,
        limits: &iced_core::layout::Limits,
    ) -> iced_core::layout::Node {
        let child_tree = &mut tree.children[0];
        let child = self
            .header_bar_inner
            .as_widget()
            .layout(child_tree, renderer, limits);
        iced_core::layout::Node::with_children(child.size(), vec![child])
    }

    fn draw(
        &self,
        tree: &tree::Tree,
        renderer: &mut crate::Renderer,
        theme: &crate::Theme,
        style: &iced_core::renderer::Style,
        layout: iced_core::Layout<'_>,
        cursor: iced_core::mouse::Cursor,
        viewport: &iced_core::Rectangle,
    ) {
        let layout_children = layout.children().next().unwrap();
        let state_children = &tree.children[0];
        self.header_bar_inner.as_widget().draw(
            state_children,
            renderer,
            theme,
            style,
            layout_children,
            cursor,
            viewport,
        );
    }

    fn on_event(
        &mut self,
        state: &mut tree::Tree,
        event: iced_core::Event,
        layout: iced_core::Layout<'_>,
        cursor: iced_core::mouse::Cursor,
        renderer: &crate::Renderer,
        clipboard: &mut dyn iced_core::Clipboard,
        shell: &mut iced_core::Shell<'_, Message>,
        viewport: &iced_core::Rectangle,
    ) -> iced_core::event::Status {
        let child_state = &mut state.children[0];
        let child_layout = layout.children().next().unwrap();
        self.header_bar_inner.as_widget_mut().on_event(
            child_state,
            event,
            child_layout,
            cursor,
            renderer,
            clipboard,
            shell,
            viewport,
        )
    }

    fn mouse_interaction(
        &self,
        state: &tree::Tree,
        layout: iced_core::Layout<'_>,
        cursor: iced_core::mouse::Cursor,
        viewport: &iced_core::Rectangle,
        renderer: &crate::Renderer,
    ) -> iced_core::mouse::Interaction {
        let child_tree = &state.children[0];
        let child_layout = layout.children().next().unwrap();
        self.header_bar_inner.as_widget().mouse_interaction(
            child_tree,
            child_layout,
            cursor,
            viewport,
            renderer,
        )
    }

    fn operate(
        &self,
        state: &mut tree::Tree,
        layout: iced_core::Layout<'_>,
        renderer: &crate::Renderer,
        operation: &mut dyn iced_core::widget::Operation<()>,
    ) {
        let child_tree = &mut state.children[0];
        let child_layout = layout.children().next().unwrap();
        self.header_bar_inner
            .as_widget()
            .operate(child_tree, child_layout, renderer, operation);
    }

    fn overlay<'b>(
        &'b mut self,
        state: &'b mut tree::Tree,
        layout: iced_core::Layout<'_>,
        renderer: &crate::Renderer,
        translation: Vector,
    ) -> Option<iced_core::overlay::Element<'b, Message, crate::Theme, crate::Renderer>> {
        let child_tree = &mut state.children[0];
        let child_layout = layout.children().next().unwrap();
        self.header_bar_inner.as_widget_mut().overlay(
            child_tree,
            child_layout,
            renderer,
            translation,
        )
    }

    fn drag_destinations(
        &self,
        state: &tree::Tree,
        layout: iced_core::Layout<'_>,
        renderer: &crate::Renderer,
        dnd_rectangles: &mut iced_core::clipboard::DndDestinationRectangles,
    ) {
        if let Some((child_tree, child_layout)) =
            state.children.iter().zip(layout.children()).next()
        {
            self.header_bar_inner.as_widget().drag_destinations(
                child_tree,
                child_layout,
                renderer,
                dnd_rectangles,
            );
        }
    }

    #[cfg(feature = "a11y")]
    /// get the a11y nodes for the widget
    fn a11y_nodes(
        &self,
        layout: iced_core::Layout<'_>,
        state: &tree::Tree,
        p: iced::mouse::Cursor,
    ) -> iced_accessibility::A11yTree {
        let c_layout = layout.children().next().unwrap();
        let c_state = &state.children[0];
        self.header_bar_inner
            .as_widget()
            .a11y_nodes(c_layout, c_state, p)
    }
}

impl<'a, Message: Clone + 'static> HeaderBar<'a, Message> {
    #[allow(clippy::too_many_lines)]
    /// Converts the headerbar builder into an Iced element.
    pub fn view(mut self) -> Element<'a, Message> {
        let Spacing { space_xxxs, .. } = theme::spacing();

        // Take ownership of the regions to be packed.
        let start = std::mem::take(&mut self.start);
        let center = std::mem::take(&mut self.center);
        let mut end = std::mem::take(&mut self.end);

        // Also packs the window controls at the very end.
        end.push(self.window_controls());

        // Build the title element (with optional app icon) as a separate draggable element.
        let title_element: Option<Element<'a, Message>> = if !self.title.is_empty()
            && !self.is_condensed
        {
            let mut title = Cow::default();
            std::mem::swap(&mut title, &mut self.title);

            let title_text: Element<'a, Message> = widget::text(title)
                .size(15.0)
                .line_height(iced::widget::text::LineHeight::Absolute(iced::Pixels(20.0)))
                .font(crate::font::medium())
                .class(Color::from_rgb8(0x1B, 0x1B, 0x1B))
                .into();

            let title_el: Element<'a, Message> = if let Some(icon_handle) = self.app_icon.take() {
                let (icon_size, icon_gap) = (18, 8);
                let icon_widget = widget::icon::icon(icon_handle)
                    .size(icon_size)
                    .content_fit(iced::ContentFit::Contain);
                widget::row::with_capacity(2)
                    .push(icon_widget)
                    .push(title_text)
                    .spacing(icon_gap)
                    .align_y(iced::Alignment::Center)
                    .into()
            } else {
                title_text
            };

            // Wrap title in a drag-enabled mouse area
            let mut title_area = widget::container(title_el)
                .center_y(Length::Fill)
                .width(Length::Fill)
                .apply(widget::mouse_area)
                .interaction(iced_core::mouse::Interaction::Grab);

            if let Some(message) = self.on_drag.clone() {
                title_area = title_area.on_drag(message);
            }
            if let Some(message) = self.on_double_click.clone() {
                title_area = title_area.on_double_press(message);
            }
            if let Some(message) = self.on_right_click.clone() {
                title_area = title_area.on_right_press(message);
            }

            Some(title_area.into())
        } else {
            // No title: create a draggable spacer to fill remaining space
            let mut spacer =
                widget::container(iced::widget::Space::new(Length::Fill, Length::Fill))
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .apply(widget::mouse_area)
                    .interaction(iced_core::mouse::Interaction::Grab);

            if let Some(message) = self.on_drag.clone() {
                spacer = spacer.on_drag(message);
            }
            if let Some(message) = self.on_double_click.clone() {
                spacer = spacer.on_double_press(message);
            }
            if let Some(message) = self.on_right_click.clone() {
                spacer = spacer.on_right_press(message);
            }

            Some(spacer.into())
        };

        let (header_height, header_padding) = (Length::Fixed(48.0), [8, 12, 8, 12]);

        // Creates the headerbar widget.
        let widget = widget::row::with_capacity(4)
            // Start region: interactive elements (menu bar, nav toggle).
            .push(
                widget::row::with_children(start)
                    .spacing(space_xxxs)
                    .align_y(iced::Alignment::Center)
                    .apply(widget::container)
                    .align_x(iced::Alignment::Start)
                    .width(Length::Shrink),
            )
            // Title region: draggable area with app icon + title.
            .push_maybe(title_element)
            // Center region: only explicit center elements.
            .push_maybe(if !center.is_empty() {
                Some(
                    widget::row::with_children(center)
                        .spacing(space_xxxs)
                        .align_y(iced::Alignment::Center)
                        .apply(widget::container)
                        .center_x(Length::Fill)
                        .into(),
                )
            } else {
                None::<Element<'a, Message>>
            })
            .push(
                widget::row::with_children(end)
                    .spacing(2)
                    .align_y(iced::Alignment::Center)
                    .apply(widget::container)
                    .align_x(iced::Alignment::End)
                    .width(Length::Shrink),
            )
            .align_y(iced::Alignment::Center)
            .height(header_height)
            .padding(header_padding)
            .spacing(8)
            .apply(widget::container)
            .class({
                let sharp = self.sharp_corners;
                let explicit_radius = self.corner_radius;
                crate::theme::Container::custom(move |theme| {
                    let cosmic = theme.cosmic();
                    let window_radius = explicit_radius.unwrap_or_else(|| cosmic.radius_window());
                    iced_widget::container::Style {
                        icon_color: Some(Color::from_rgb8(0x1B, 0x1B, 0x1B)),
                        text_color: Some(Color::from_rgb8(0x1B, 0x1B, 0x1B)),
                        background: Some(iced::Background::Color(Color::from_rgba8(
                            255, 255, 255, 0.99,
                        ))),
                        border: Border {
                            radius: [
                                if sharp { 0.0 } else { window_radius[0] },
                                if sharp { 0.0 } else { window_radius[1] },
                                0.0,
                                0.0,
                            ]
                            .into(),
                            ..Default::default()
                        },
                        shadow: Default::default(),
                    }
                })
            })
            .center_y(Length::Shrink);

        let widget = {
            use iced::widget::{horizontal_rule, rule};
            widget::column::with_capacity(2)
                .push(widget)
                .push(
                    horizontal_rule(1).class(crate::theme::Rule::Custom(Box::new(
                        |_: &crate::Theme| rule::Style {
                            color: Color::from_rgba8(240, 240, 241, 1.0),
                            width: 1,
                            radius: 0.0.into(),
                            fill_mode: rule::FillMode::Full,
                        },
                    ))),
                )
                .apply(widget::mouse_area)
        };

        let mut widget = widget;

        // Right-click menu on overall header area (not drag — drag is on title only)
        if let Some(message) = self.on_right_click.clone() {
            widget = widget.on_right_press(message);
        }

        widget.into()
    }

    /// Creates the widget for window controls.
    fn window_controls(&mut self) -> Element<'a, Message> {
        const ICON_MINIMIZE: &[u8] = include_bytes!("../../res/icons/window-minimize.svg");
        const ICON_MAXIMIZE: &[u8] = include_bytes!("../../res/icons/window-maximize.svg");
        const ICON_RESTORE: &[u8] = include_bytes!("../../res/icons/window-restore.svg");
        const ICON_CLOSE: &[u8] = include_bytes!("../../res/icons/window-close.svg");

        macro_rules! icon {
            ($svg_bytes:expr, $size:expr, $on_press:expr) => {{
                let padding = [6, 6];
                let icon_w = widget::icon::icon(widget::icon::from_svg_bytes($svg_bytes))
                    .size($size)
                    .class(crate::theme::Svg::custom(|_| iced::widget::svg::Style {
                        color: Some(Color::from_rgb8(0x1B, 0x1B, 0x1B)),
                    }));
                let result: Element<'a, Message> = widget::button::custom(icon_w)
                    .padding(padding)
                    .class(crate::theme::Button::HeaderBar)
                    .selected(self.focused)
                    .on_press($on_press)
                    .into();
                result
            }};
        }

        let icon_spacing = 2;

        widget::row::with_capacity(3)
            .push_maybe(
                self.on_minimize
                    .take()
                    .map(|m: Message| icon!(ICON_MINIMIZE, 16, m)),
            )
            .push_maybe(self.on_maximize.take().map(|m| {
                if self.maximized {
                    icon!(ICON_RESTORE, 16, m)
                } else {
                    icon!(ICON_MAXIMIZE, 16, m)
                }
            }))
            .push_maybe(self.on_close.take().map(|m| icon!(ICON_CLOSE, 16, m)))
            .spacing(icon_spacing)
            .apply(widget::container)
            .class(crate::theme::Container::custom(move |theme| {
                let cosmic = theme.cosmic();
                let background = Color::from_rgba8(0xFF, 0xFF, 0xFF, 0.80);
                iced_widget::container::Style {
                    background: Some(iced::Background::Color(background)),
                    border: Border {
                        radius: cosmic.corner_radii.radius_xl.into(),
                        ..Default::default()
                    },
                    ..Default::default()
                }
            }))
            .padding([2, 8])
            .center_y(Length::Fill)
            .apply(widget::container)
            .padding(iced::Padding::from([0, 0, 0, 18]))
            .center_y(Length::Fill)
            .into()
    }
}

impl<'a, Message: Clone + 'static> From<HeaderBar<'a, Message>> for Element<'a, Message> {
    fn from(headerbar: HeaderBar<'a, Message>) -> Self {
        Element::new(headerbar.build())
    }
}

impl<'a, Message: Clone + 'static> From<HeaderBarWidget<'a, Message>> for Element<'a, Message> {
    fn from(headerbar: HeaderBarWidget<'a, Message>) -> Self {
        Element::new(headerbar)
    }
}
