// SPDX-License-Identifier: MPL-2.0

//! An icon is tinted only when the file found for it is symbolic.

#![cfg(all(unix, not(target_os = "macos")))]

use cosmic::widget::icon;
use std::fs;

const INDEX: &str = "[Icon Theme]
Name=Cosmic
Directories=scalable/apps

[scalable/apps]
Size=16
MinSize=8
MaxSize=512
Type=Scalable
";

const SVG: &str = r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><rect width="16" height="16" fill="teal"/></svg>"#;

// The only test in this binary, since it sets the environment the lookup reads.
#[test]
fn only_a_symbolic_file_is_tinted() {
    let data = tempfile::tempdir().unwrap();
    let apps = data.path().join("icons/Cosmic/scalable/apps");
    fs::create_dir_all(&apps).unwrap();
    fs::write(data.path().join("icons/Cosmic/index.theme"), INDEX).unwrap();
    let colour = apps.join("libcosmic-test-screenshooter.svg");
    let symbolic = apps.join("libcosmic-test-camera-symbolic.svg");
    fs::write(&colour, SVG).unwrap();
    fs::write(&symbolic, SVG).unwrap();
    // SAFETY: no other thread reads the environment, and the lookup reads it on first use.
    unsafe {
        std::env::set_var("XDG_DATA_DIRS", data.path());
        std::env::set_var("XDG_DATA_HOME", data.path().join("home"));
        std::env::set_var("HOME", data.path().join("home"));
    }

    // The screenshot portal asks for `applets-screenshooter-symbolic`; a theme
    // without it falls back to the colour `applets-screenshooter`.
    let fallback = icon::from_name("libcosmic-test-screenshooter-symbolic");
    assert_eq!(fallback.clone().path(), Some(colour));
    assert!(!fallback.handle().symbolic, "a colour fallback is tinted");

    let named = icon::from_name("libcosmic-test-camera-symbolic");
    assert_eq!(named.clone().path(), Some(symbolic));
    assert!(named.clone().handle().symbolic, "a symbolic icon is not tinted");
    assert!(!named.symbolic(false).handle().symbolic);
}
