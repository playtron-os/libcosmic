// Copyright 2022 System76 <info@system76.com>
// SPDX-License-Identifier: MPL-2.0

use crate::cosmic_theme::{Density, Spacing};
use crate::{Element, theme, widget};
use apply::Apply;
use derive_setters::Setters;
use iced::{Border, Color, Length};
use iced_core::event;
use iced_core::layout::{self, Layout};
use iced_core::mouse::{self, Cursor};
use iced_core::overlay;
use iced_core::renderer;
use iced_core::widget::{self as core_widget, Operation, Tree, tree};
use iced_core::{Background, Clipboard, Event, Padding, Rectangle, Shell, Size, Vector, Widget};
use std::borrow::Cow;
use std::time::{Duration, Instant};

// ── Animated container background ──────────────────────────────────────────────
//
// Minimal widget that wraps content and smoothly animates its background color
// and border radius when they change. Used for the action bar pill in the header.

const ACB_ANIMATION_DURATION: Duration = Duration::from_millis(150);
const ACB_THRESHOLD: f32 = 0.001;
const ACB_PROP_COUNT: usize = 5; // bg_r, bg_g, bg_b, bg_a, border_radius

fn acb_pack(bg: Color, radius: f32) -> [f32; ACB_PROP_COUNT] {
    [bg.r, bg.g, bg.b, bg.a, radius]
}

fn acb_differs(a: &[f32; ACB_PROP_COUNT], b: &[f32; ACB_PROP_COUNT]) -> bool {
    a.iter()
        .zip(b.iter())
        .any(|(x, y)| (x - y).abs() > ACB_THRESHOLD)
}

fn acb_lerp(
    from: &[f32; ACB_PROP_COUNT],
    to: &[f32; ACB_PROP_COUNT],
    t: f32,
) -> [f32; ACB_PROP_COUNT] {
    let mut out = [0.0; ACB_PROP_COUNT];
    for i in 0..ACB_PROP_COUNT {
        out[i] = from[i] + (to[i] - from[i]) * t;
    }
    out
}

/// Easeout cubic: 1 - (1 - t)^3
fn ease_out(t: f32) -> f32 {
    let inv = 1.0 - t;
    1.0 - inv * inv * inv
}

#[derive(Default)]
struct AcbState {
    animation_start: Option<Instant>,
    start: [f32; ACB_PROP_COUNT],
    target: [f32; ACB_PROP_COUNT],
    current: [f32; ACB_PROP_COUNT],
    initialized: bool,
}

/// Create an animated container that transitions its background color.
fn animated_container_bg<'a, Message: 'a>(
    content: impl Into<Element<'a, Message>>,
) -> AnimatedContainerBg<'a, Message> {
    AnimatedContainerBg {
        content: content.into(),
        target_bg: Color::TRANSPARENT,
        target_border_radius: 0.0,
        width: Length::Shrink,
        height: Length::Shrink,
        padding: Padding::ZERO,
    }
}

struct AnimatedContainerBg<'a, Message> {
    content: Element<'a, Message>,
    target_bg: Color,
    target_border_radius: f32,
    width: Length,
    height: Length,
    padding: Padding,
}

impl<Message> AnimatedContainerBg<'_, Message> {
    fn background(mut self, color: Color) -> Self {
        self.target_bg = color;
        self
    }

    fn border_radius(mut self, radius: f32) -> Self {
        self.target_border_radius = radius;
        self
    }

    fn padding(mut self, padding: impl Into<Padding>) -> Self {
        self.padding = padding.into();
        self
    }
}

impl<Message: 'static> Widget<Message, crate::Theme, crate::Renderer>
    for AnimatedContainerBg<'_, Message>
{
    fn tag(&self) -> core_widget::tree::Tag {
        core_widget::tree::Tag::of::<AcbState>()
    }

    fn state(&self) -> core_widget::tree::State {
        core_widget::tree::State::new(AcbState::default())
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.content)]
    }

    fn diff(&mut self, tree: &mut Tree) {
        tree.diff_children(std::slice::from_mut(&mut self.content));
    }

    fn size(&self) -> Size<Length> {
        Size::new(self.width, self.height)
    }

    fn layout(
        &self,
        tree: &mut Tree,
        renderer: &crate::Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        let limits = limits.width(self.width).height(self.height);
        let padding = self.padding;
        let inner_limits = limits.shrink(padding);

        let child = self
            .content
            .as_widget()
            .layout(&mut tree.children[0], renderer, &inner_limits);

        let child_size = child.size();
        let size = limits.resolve(
            self.width,
            self.height,
            Size::new(
                child_size.width + padding.left + padding.right,
                child_size.height + padding.top + padding.bottom,
            ),
        );

        layout::Node::with_children(
            size,
            vec![child.move_to(iced::Point::new(padding.left, padding.top))],
        )
    }

    fn operate(
        &self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &crate::Renderer,
        operation: &mut dyn Operation<()>,
    ) {
        if let Some(child_layout) = layout.children().next() {
            self.content.as_widget().operate(
                &mut tree.children[0],
                child_layout,
                renderer,
                operation,
            );
        }
    }

    fn on_event(
        &mut self,
        tree: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor: Cursor,
        renderer: &crate::Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
        viewport: &Rectangle,
    ) -> event::Status {
        let state = tree.state.downcast_mut::<AcbState>();

        let new_target = acb_pack(self.target_bg, self.target_border_radius);

        if !state.initialized {
            state.initialized = true;
            state.current = new_target;
            state.target = new_target;
            state.start = new_target;
        }

        if acb_differs(&state.target, &new_target) {
            state.start = state.current;
            state.target = new_target;
            state.animation_start = Some(Instant::now());
        }

        if let Some(start) = state.animation_start {
            let elapsed = start.elapsed();
            if elapsed < ACB_ANIMATION_DURATION {
                let progress =
                    (elapsed.as_secs_f32() / ACB_ANIMATION_DURATION.as_secs_f32()).min(1.0);
                let eased = ease_out(progress);
                state.current = acb_lerp(&state.start, &state.target, eased);
                shell.request_redraw(iced_core::window::RedrawRequest::NextFrame);
            } else {
                state.current = state.target;
                state.animation_start = None;
            }
        }

        if let Some(child_layout) = layout.children().next() {
            self.content.as_widget_mut().on_event(
                &mut tree.children[0],
                event,
                child_layout,
                cursor,
                renderer,
                clipboard,
                shell,
                viewport,
            )
        } else {
            event::Status::Ignored
        }
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: Cursor,
        viewport: &Rectangle,
        renderer: &crate::Renderer,
    ) -> mouse::Interaction {
        if let Some(child_layout) = layout.children().next() {
            self.content.as_widget().mouse_interaction(
                &tree.children[0],
                child_layout,
                cursor,
                viewport,
                renderer,
            )
        } else {
            mouse::Interaction::default()
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut crate::Renderer,
        theme: &crate::Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: Cursor,
        viewport: &Rectangle,
    ) {
        let c = &tree.state.downcast_ref::<AcbState>().current;

        let bg = Color::from_rgba(c[0], c[1], c[2], c[3]);
        let border_radius = c[4];

        iced_core::Renderer::fill_quad(
            renderer,
            renderer::Quad {
                bounds: layout.bounds(),
                border: Border {
                    radius: border_radius.into(),
                    ..Default::default()
                },
                ..Default::default()
            },
            Background::Color(bg),
        );

        if let Some(child_layout) = layout.children().next() {
            self.content.as_widget().draw(
                &tree.children[0],
                renderer,
                theme,
                style,
                child_layout,
                cursor,
                viewport,
            );
        }
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'_>,
        renderer: &crate::Renderer,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Message, crate::Theme, crate::Renderer>> {
        if let Some(child_layout) = layout.children().next() {
            self.content.as_widget_mut().overlay(
                &mut tree.children[0],
                child_layout,
                renderer,
                translation,
            )
        } else {
            None
        }
    }
}

impl<'a, Message: 'a + 'static> From<AnimatedContainerBg<'a, Message>> for Element<'a, Message> {
    fn from(widget: AnimatedContainerBg<'a, Message>) -> Self {
        Element::new(widget)
    }
}

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
        hovered: false,
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

    /// Hovered state of the window
    hovered: bool,

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
                .size(14.0)
                .line_height(iced::widget::text::LineHeight::Absolute(iced::Pixels(20.0)))
                .font(crate::font::medium())
                .class(Color::from_rgba8(0x1B, 0x1B, 0x1B, 1.0))
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

        let header_height = Length::Fixed(47.0);
        let header_padding: Padding = Padding {
            top: 0.0,
            right: 16.0,
            bottom: 0.0,
            left: 16.0,
        };

        // Creates the headerbar widget.
        let header_row = widget::row::with_capacity(4)
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
            .spacing(8);

        // Apply opacity to all header content when unfocused and not hovered
        let content_opacity = if self.focused || self.hovered {
            1.0
        } else {
            0.8
        };
        let header_row: Element<'a, Message> =
            iced::widget::opacity(content_opacity, header_row).into();

        // Bottom border line
        let border_line: Element<'a, Message> =
            widget::container(iced::widget::Space::new(Length::Fill, Length::Fixed(0.0)))
                .width(Length::Fill)
                .height(Length::Fixed(1.0))
                .class(crate::theme::Container::custom(|_theme| {
                    iced_widget::container::Style {
                        background: Some(iced::Background::Color(Color::from_rgba8(
                            240, 240, 241, 1.0,
                        ))),
                        ..Default::default()
                    }
                }))
                .into();

        // Background: translucent gradient (matching icetron style)
        let sharp = self.sharp_corners;
        let explicit_radius = self.corner_radius;
        let header_with_bg = widget::column::with_capacity(2)
            .push(header_row)
            .push(border_line)
            .apply(widget::container)
            .class({
                crate::theme::Container::custom(move |theme| {
                    let cosmic = theme.cosmic();
                    let window_radius = explicit_radius.unwrap_or_else(|| cosmic.radius_window());

                    iced_widget::container::Style {
                        icon_color: Some(Color::from_rgb8(0x1B, 0x1B, 0x1B)),
                        text_color: Some(Color::from_rgb8(0x1B, 0x1B, 0x1B)),
                        background: Some(iced::Background::Color(Color::WHITE)),
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

        let widget = header_with_bg.apply(widget::mouse_area);

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

        // Matches icetron: fill_skim() = rgba(0, 0, 0, 102) = 40% black
        let icon_color = Color::from_rgba(0.0, 0.0, 0.0, 0.4);
        // Matches icetron: ui_size_icon_2xs = 14, ui_size_icon_2md = 28
        let icon_size: u16 = 14;
        let button_size = 28.0;
        let radius = 9999.0_f32; // radii_max

        // Ghost button style for minimize/maximize
        let ghost_button_style = || {
            let hover_bg = Color::from_rgba(0.0, 0.0, 0.0, 0.08);
            let pressed_bg = Color::from_rgba(0.0, 0.0, 0.0, 0.12);
            crate::theme::Button::Custom {
                active: Box::new(move |_focused, _theme| crate::widget::button::Style {
                    background: None,
                    icon_color: Some(icon_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
                disabled: Box::new(move |_theme| crate::widget::button::Style {
                    background: None,
                    icon_color: Some(icon_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
                hovered: Box::new(move |_focused, _theme| crate::widget::button::Style {
                    background: Some(iced::Background::Color(hover_bg)),
                    icon_color: Some(icon_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
                pressed: Box::new(move |_focused, _theme| crate::widget::button::Style {
                    background: Some(iced::Background::Color(pressed_bg)),
                    icon_color: Some(icon_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
            }
        };

        // Destructive close button: red icon + red bg on hover
        let close_button_style = || {
            // Matches icetron: feedback_error_primary = #FF3B30
            let error_color = Color::from_rgb8(0xFF, 0x3B, 0x30);
            // Matches icetron: feedback_error_tertiary = rgba(255, 59, 48, 26) ≈ 10% red
            let error_bg = Color::from_rgba8(0xFF, 0x3B, 0x30, 26.0 / 255.0);
            crate::theme::Button::Custom {
                active: Box::new(move |_focused, _theme| crate::widget::button::Style {
                    background: None,
                    icon_color: Some(icon_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
                disabled: Box::new(move |_theme| crate::widget::button::Style {
                    background: None,
                    icon_color: Some(icon_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
                hovered: Box::new(move |_focused, _theme| crate::widget::button::Style {
                    background: Some(iced::Background::Color(error_bg)),
                    icon_color: Some(error_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
                pressed: Box::new(move |_focused, _theme| crate::widget::button::Style {
                    background: Some(iced::Background::Color(error_bg)),
                    icon_color: Some(error_color),
                    border_radius: radius.into(),
                    ..crate::widget::button::Style::new()
                }),
            }
        };

        macro_rules! ghost_icon {
            ($svg_bytes:expr, $on_press:expr, $style:expr) => {{
                let handle = widget::icon::from_svg_bytes($svg_bytes).symbolic(true);
                let icon_w = widget::icon::icon(handle)
                    .size(icon_size)
                    .content_fit(iced::ContentFit::Contain);
                let result: Element<'a, Message> = widget::button::custom(
                    widget::container(icon_w)
                        .center_x(Length::Fixed(button_size))
                        .center_y(Length::Fixed(button_size)),
                )
                .padding(0)
                .width(Length::Fixed(button_size))
                .height(Length::Fixed(button_size))
                .class($style)
                .on_press($on_press)
                .into();
                result
            }};
        }

        // Action bar pill: rounded container around buttons
        let buttons = widget::row::with_capacity(3)
            .push_maybe(
                self.on_minimize
                    .take()
                    .map(|m: Message| ghost_icon!(ICON_MINIMIZE, m, ghost_button_style())),
            )
            .push_maybe(self.on_maximize.take().map(|m| {
                if self.maximized {
                    ghost_icon!(ICON_RESTORE, m, ghost_button_style())
                } else {
                    ghost_icon!(ICON_MAXIMIZE, m, ghost_button_style())
                }
            }))
            .push_maybe(
                self.on_close
                    .take()
                    .map(|m| ghost_icon!(ICON_CLOSE, m, close_button_style())),
            )
            .spacing(2)
            .align_y(iced::Alignment::Center);

        // Wrap in animated_container_bg for smooth pill bg transitions
        let bar_bg = if self.focused || self.hovered {
            Color::from_rgba(0.0, 0.0, 0.0, 0.03)
        } else {
            Color::TRANSPARENT
        };
        animated_container_bg(buttons)
            .background(bar_bg)
            .border_radius(radius)
            .padding(4)
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
