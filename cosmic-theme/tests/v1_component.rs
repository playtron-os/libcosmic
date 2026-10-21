//! Theme v2 reads a key from the v1 defaults when the v2 defaults lack it, as
//! the installed defaults do for `list_button`. The v1 colors are RGBA structs.

use cosmic_theme::Component;

#[test]
fn a_v1_component_reads_as_v2() {
    let component: Component =
        ron::from_str(include_str!("fixtures/v1_list_button.ron")).expect("v1 list_button");
    assert!((component.pressed.alpha - 0.5).abs() < f32::EPSILON);
}
