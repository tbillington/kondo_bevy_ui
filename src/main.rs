use std::sync::mpsc::{Receiver, Sender};

use bevy::{
    ecs::system::{RunSystemOnce, SystemId},
    feathers::{
        FeathersPlugin,
        controls::{ButtonProps, ButtonVariant, button},
        dark_theme::create_dark_theme,
        theme::{ThemeBackgroundColor, ThemedText, UiTheme},
        tokens,
    },
    input_focus::{
        InputDispatchPlugin,
        tab_navigation::{TabGroup, TabNavigationPlugin},
    },
    prelude::*,
    ui_widgets::{Activate, Callback, UiWidgetsPlugins},
};

fn main() {
    let mut app = App::new();

    app.set_error_handler(bevy::ecs::error::error);

    app.add_plugins(DefaultPlugins);

    app.add_plugins((
        UiWidgetsPlugins,
        InputDispatchPlugin,
        TabNavigationPlugin,
        FeathersPlugin,
    ));

    app.insert_resource(UiTheme(create_dark_theme()));

    app.insert_resource(bevy::winit::WinitSettings::desktop_app());

    // app.add_plugins(bevy::diagnostic::FrameTimeDiagnosticsPlugin::default());
    // app.add_plugins(bevy::dev_tools::frame_time_graph::FrameTimeGraphPlugin);

    app.add_systems(Startup, (setup, spawn_project_list).chain());

    app.add_systems(
        Update,
        (
            process_new_projects,
            update_project_list_ui,
            select_project_update,
        )
            .chain(),
    );

    app.add_systems(Update, send_scroll_events);

    app.add_observer(on_scroll_handler);

    app.insert_resource(SelectedProject(None));

    app.insert_non_send_resource(BackgroundThreadCommunication::default());

    app.run();
}

struct BackgroundThreadCommunication {
    send: Sender<ProjectListEntry>,
    recv: Receiver<ProjectListEntry>,
}

impl Default for BackgroundThreadCommunication {
    fn default() -> Self {
        let (send, recv) = std::sync::mpsc::channel();

        Self { send, recv }
    }
}

const SCAN_OPTIONS: &kondo_lib::ScanOptions = &kondo_lib::ScanOptions {
    follow_symlinks: false,
    same_file_system: false,
};

fn hot_test(mut _q: Query<&mut Children, With<ProjectListTag>>) {
    // for mut c in q.iter_mut() {
    //     c.swap(0, 1);
    // }
    // println!("Wow!");
}

fn setup(mut c: Commands) {
    c.spawn(Camera2d);

    // c.spawn(Text::new("Hello!"));

    let root = spawn_root(&mut c);

    let _root_id = c.spawn(root).id();

    // c.spawn((
    //     ProjectList,
    //     ChildOf(root_id),
    //     Node {
    //         flex_direction: FlexDirection::Column,
    //         ..default()
    //     },
    //     children![
    //         build_project_list_entry("bevy (Cargo) 7.8GiB"),
    //         build_project_list_entry("app (Cargo) 2.1GiB"),
    //     ],
    // ));
}

#[derive(Resource, Deref, DerefMut)]
struct ProjectList(Vec<ProjectListEntry>);

#[derive(Resource)]
struct Callbacks {
    project_list_entry_clicked: SystemId<In<Activate>>,
}

fn spawn_project_list(
    root_ui: Single<Entity, With<RootUITag>>,
    pl: Query<Entity, With<ProjectListTag>>,
    mut c: Commands,
) {
    for pl in pl.iter() {
        c.entity(pl).despawn();
    }

    let ples = vec![];

    c.insert_resource(ProjectList(ples.clone()));

    // let callback_sys = c.register_system(|In(activate): In<Activate>, mut c: Commands| {
    //     c.queue(ProjectListEntryClicked(activate.0));
    // });

    let callback_sys = c.register_system(project_list_entry_clicked);

    c.insert_resource(Callbacks {
        project_list_entry_clicked: callback_sys.clone(),
    });

    let central_area_ui = c
        .spawn((
            CentralUIAreaTag,
            ChildOf(*root_ui),
            Node {
                flex_direction: FlexDirection::Row,
                // row_gap: Val::Px(8.),
                // padding: Val::Px(8.).into(),
                // overflow: Overflow::scroll_y(),
                // height: Val::Percent(100.),
                width: Val::Percent(100.),
                min_height: Val::Px(0.),
                // flex_shrink: 0.,
                ..default()
            },
        ))
        .id();

    c.spawn((
        ProjectListTag,
        ChildOf(central_area_ui),
        Node {
            flex_direction: FlexDirection::Column,
            row_gap: Val::Px(8.),
            padding: Val::Px(8.).into(),
            overflow: Overflow::scroll_y(),
            width: Val::Percent(50.),
            // height: Val::Percent(100.),
            // flex_shrink: 0.,
            ..default()
        },
    ))
    .with_children(|c| {
        for ple in ples {
            c.spawn(build_project_list_entry(
                Callback::System(callback_sys.clone()),
                ple,
            ));
        }
    });

    c.spawn((
        SelectedProjectTag,
        ChildOf(central_area_ui),
        Node {
            flex_direction: FlexDirection::Column,
            row_gap: Val::Px(8.),
            padding: Val::Px(8.).into(),
            overflow: Overflow::scroll_y(),
            width: Val::Percent(50.),
            // flex_shrink: 0.,
            ..default()
        },
    ))
    .with_children(|c| {
        c.spawn((Text::new("Selected Project"), ThemedText));
    });
}

fn select_project_update(
    root: Query<Entity, With<SelectedProjectTag>>,
    sp: Res<SelectedProject>,
    mut c: Commands,
) {
    let Ok(root) = root.single() else {
        return;
    };

    if !sp.is_changed() {
        return;
    }

    c.entity(root).despawn_children();

    let Some(ple) = &sp.0 else {
        return;
    };

    let display_name = ple
        .kproj
        .path
        .file_name()
        .map(|n| n.to_string_lossy())
        .unwrap_or_else(|| ple.kproj.name());

    let mut dir_sizes = ple.kproj.size_dirs(SCAN_OPTIONS);

    dir_sizes
        .dirs
        .sort_unstable_by_key(|d| std::cmp::Reverse(d.1));

    let delete_artifacts_callback = c.register_system(|_: In<Activate>| {
        println!("Clean artifacts");
    });

    c.spawn((
        ChildOf(root),
        Node {
            width: Val::Percent(100.),
            flex_direction: FlexDirection::Column,
            row_gap: Val::Px(8.),
            ..default()
        },
        Children::spawn((
            Spawn((
                Node {
                    justify_content: JustifyContent::Center,
                    ..default()
                },
                Children::spawn_one((Text::new(display_name), ThemedText)),
            )),
            Spawn((Text::new(ple.kproj.path.to_string_lossy()), ThemedText)),
            Spawn((
                Text::new(format!("{} project", ple.kproj.type_name())),
                ThemedText,
            )),
            Spawn((
                Text::new(format!(
                    "{} Total Size",
                    kondo_lib::pretty_size(dir_sizes.artifact_size + dir_sizes.non_artifact_size)
                )),
                ThemedText,
            )),
            Spawn((
                Text::new(format!(
                    "{} Artifact Size",
                    kondo_lib::pretty_size(dir_sizes.non_artifact_size)
                )),
                ThemedText,
            )),
            SpawnIter(dir_sizes.dirs.into_iter().map(|(name, size, artifacts)| {
                (
                    Node {
                        padding: UiRect::left(Val::Px(32.0)),
                        ..default()
                    },
                    Children::spawn_one((
                        Text::new(format!(
                            "{} {} {}",
                            name,
                            if artifacts { "🗑️" } else { "" },
                            kondo_lib::pretty_size(size)
                        )),
                        ThemedText,
                    )),
                )
            })),
            Spawn(button(
                ButtonProps {
                    on_click: Callback::System(delete_artifacts_callback),
                    variant: ButtonVariant::Primary,
                    ..default()
                },
                (),
                Spawn((Text::new("Delete Artifacts"), ThemedText)),
            )),
        )),
    ));
}

fn update_project_list_ui(
    q: Query<Entity, With<ProjectListTag>>,
    pl: Res<ProjectList>,
    callbacks: Res<Callbacks>,
    mut c: Commands,
) {
    if !pl.is_changed() {
        return;
    }

    for pl_ui in q.iter() {
        c.entity(pl_ui).despawn_children().with_children(|c| {
            for p in pl.iter() {
                c.spawn(build_project_list_entry(
                    Callback::System(callbacks.project_list_entry_clicked.clone()),
                    p.clone(),
                ));
            }
        });
    }

    c.queue(SortProjectList::Size);
}

#[derive(Component)]
struct RootUITag;

fn scan_dirs(dirs: Option<Vec<std::path::PathBuf>>) {
    let Some(dirs) = dirs else {
        return;
    };

    std::thread::spawn(move || {
        dirs.into_iter()
            .flat_map(|dir| kondo_lib::scan(&dir, SCAN_OPTIONS))
            .filter_map(Result::ok)
            .for_each(|proj| {
                println!("Found {:?}: {}", proj.project_type, proj.name());
            });
    });
}

fn collect_dirs<'a>(
    dirs: &'a Option<Vec<std::path::PathBuf>>,
) -> impl Iterator<Item = ProjectListEntry> + 'a {
    dirs.iter()
        .flatten()
        .into_iter()
        .flat_map(|dir| kondo_lib::scan(&dir, SCAN_OPTIONS))
        .filter_map(Result::ok)
        .map(|p| ProjectListEntry {
            size: p.size(SCAN_OPTIONS),
            kproj: p,
        })
}

fn process_new_projects(tc: NonSend<BackgroundThreadCommunication>, mut pl: ResMut<ProjectList>) {
    while let Ok(proj) = tc.recv.try_recv() {
        pl.push(proj);
    }
}

fn spawn_root(c: &mut Commands) -> impl Bundle {
    let primary_sys = |_: In<Activate>, tc: NonSend<BackgroundThreadCommunication>| {
        use rfd::FileDialog;

        let directories = FileDialog::new().pick_folders();

        println!("Picked dirs: {:?}", directories);

        // scan_dirs(directories);

        let send = tc.send.clone();

        std::thread::spawn(move || {
            for dir in collect_dirs(&directories) {
                send.send(dir).unwrap();
            }
        });

        // c.insert_resource(ProjectList());
    };

    (
        RootUITag,
        Node {
            width: Val::Percent(100.0),
            height: Val::Percent(100.0),
            align_items: AlignItems::Start,
            justify_content: JustifyContent::Start,
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            row_gap: Val::Px(10.0),
            ..default()
        },
        TabGroup::default(),
        ThemeBackgroundColor(tokens::WINDOW_BG),
        Children::spawn_one((
            Node {
                display: Display::Flex,
                flex_direction: FlexDirection::Row,
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Start,
                column_gap: Val::Px(8.0),
                padding: UiRect::all(Val::Px(8.)),
                ..default()
            },
            Children::spawn((
                Spawn(button(
                    ButtonProps {
                        on_click: Callback::System(c.register_system(
                            |_: In<Activate>, mut c: Commands| c.queue(SortProjectList::Name),
                        )),
                        ..default()
                    },
                    (),
                    Spawn((Text::new("Sort by Name"), ThemedText)),
                )),
                Spawn(button(
                    ButtonProps {
                        on_click: Callback::System(c.register_system(
                            |_: In<Activate>, mut c: Commands| c.queue(SortProjectList::Size),
                        )),
                        ..default()
                    },
                    (),
                    Spawn((Text::new("Sort by Size"), ThemedText)),
                )),
                Spawn(button(
                    ButtonProps {
                        on_click: Callback::System(c.register_system(primary_sys)),
                        variant: ButtonVariant::Primary,
                        ..default()
                    },
                    (),
                    Spawn((Text::new("Select Directory"), ThemedText)),
                )),
            )),
            // children![
            //     button(
            //         ButtonProps {
            //             on_click: Callback::System(c.register_system(
            //                 |_: In<Activate>, mut c: Commands| c.queue(SortProjectList::Name)
            //             )),
            //             ..default()
            //         },
            //         (),
            //         Spawn((Text::new("Sort by Name"), ThemedText))
            //     ),
            //     button(
            //         ButtonProps {
            //             on_click: Callback::System(c.register_system(
            //                 |_: In<Activate>, mut c: Commands| c.queue(SortProjectList::Size)
            //             )),
            //             ..default()
            //         },
            //         (),
            //         Spawn((Text::new("Sort by Size"), ThemedText))
            //     ),
            //     button(
            //         ButtonProps {
            //             on_click: Callback::System(c.register_system(|_: In<Activate>| {
            //                 println!("Disabled button clicked!");
            //             })),
            //             ..default()
            //         },
            //         InteractionDisabled,
            //         Spawn((Text::new("Disabled"), ThemedText))
            //     ),
            //     button(
            //         ButtonProps {
            //             on_click: Callback::System(c.register_system(primary_sys)),
            //             variant: ButtonVariant::Primary,
            //             ..default()
            //         },
            //         (),
            //         Spawn((Text::new("Select Directory"), ThemedText))
            //     ),
            // ],
        )),
    )
}

#[derive(Component)]
struct ProjectListTag;

#[derive(Component)]
struct SelectedProjectTag;

#[derive(Component)]
struct CentralUIAreaTag;

#[derive(Component, Clone)]
struct ProjectListEntry {
    // name: String,
    // kind: String,
    // size: String,
    kproj: kondo_lib::Project,
    size: u64,
}

fn build_project_list_entry(
    on_click: Callback<In<Activate>>,
    ple: ProjectListEntry,
) -> impl Bundle {
    // let text = format!("{} ({}) {}", ple.name, ple.kind, ple.size);
    // let text = String::new();
    let proj = &ple.kproj;
    let display_name = proj
        .path
        .file_name()
        .map(|n| n.to_string_lossy())
        .unwrap_or_else(|| proj.name());
    let text = format!(
        "{} ({}) {}",
        display_name,
        proj.type_name(),
        kondo_lib::pretty_size(ple.size),
    );

    (
        Node {
            // padding: UiRect::vertical(Val::Px(8.0)),
            ..default()
        },
        ple.clone(),
        Children::spawn_one(button_left(
            ButtonProps {
                on_click,
                ..default()
            },
            ple,
            Spawn((Text::new(text), ThemedText)),
            // Spawn((
            //     Node {
            //         width: Val::Percent(100.),
            //         ..default()
            //     },
            //     Children::spawn_one((
            //         Text::new(text),
            //         ThemedText,
            //         // Outline::new(
            //         //     Val::Px(1.),
            //         //     Val::ZERO,
            //         //     bevy::color::palettes::css::RED.into(),
            //         // ),
            //     )),
            // )),
        )),
    )
}

pub fn button_left<
    C: bevy::ecs::spawn::SpawnableList<ChildOf> + Send + Sync + 'static,
    B: Bundle,
>(
    props: ButtonProps,
    overrides: B,
    children: C,
) -> impl Bundle {
    (
        Node {
            height: bevy::feathers::constants::size::ROW_HEIGHT,
            justify_content: JustifyContent::Start,
            align_items: AlignItems::Center,
            padding: UiRect::axes(Val::Px(8.0), Val::Px(8.)),
            flex_grow: 1.0,
            ..Default::default()
        },
        bevy::ui_widgets::Button {
            on_activate: props.on_click,
        },
        props.variant,
        bevy::picking::hover::Hovered::default(),
        bevy::feathers::cursor::EntityCursor::System(bevy::window::SystemCursorIcon::Pointer),
        bevy::input_focus::tab_navigation::TabIndex(0),
        props.corners.to_border_radius(4.0),
        ThemeBackgroundColor(tokens::BUTTON_BG),
        bevy::feathers::theme::ThemeFontColor(tokens::BUTTON_TEXT),
        bevy::feathers::font_styles::InheritableFont {
            font: bevy::feathers::handle_or_path::HandleOrPath::Path(
                bevy::feathers::constants::fonts::REGULAR.to_owned(),
            ),
            font_size: 14.0,
        },
        overrides,
        Children::spawn(children),
    )
}

// struct ProjectListEntryClicked(Entity);

// impl Command for ProjectListEntryClicked {
//     fn apply(self, world: &mut World) {
//         world
//             .run_system_once_with(project_list_entry_clicked, self.0)
//             .unwrap();
//     }
// }

#[derive(Resource)]
struct SelectedProject(Option<ProjectListEntry>);

fn project_list_entry_clicked(
    In(Activate(ple_id)): In<Activate>,
    ple: Query<&ProjectListEntry>,
    mut sp: ResMut<SelectedProject>,
) {
    if let Ok(ple) = ple.get(ple_id) {
        sp.0 = Some(ple.clone());
    }
}

enum SortProjectList {
    Name,
    Size,
}

impl Command for SortProjectList {
    fn apply(self, world: &mut World) {
        world.run_system_once_with(sort_projectlist, self).unwrap();
    }
}

fn sort_projectlist(
    In(sort): In<SortProjectList>,
    mut pl: Query<&mut Children, With<ProjectListTag>>,
    ple: Query<&ProjectListEntry>,
) {
    for mut pl in pl.iter_mut() {
        match sort {
            SortProjectList::Name => {
                pl.sort_by_key(|k| {
                    let key = ple.get(*k).unwrap().kproj.name();
                    key
                });
            }
            SortProjectList::Size => {
                pl.sort_by_key(|k| {
                    let ple = ple.get(*k).unwrap();
                    let key = std::cmp::Reverse(ple.size);
                    key
                });
            }
        }
    }
}

const LINE_HEIGHT: f32 = 21.;

/// Injects scroll events into the UI hierarchy.
fn send_scroll_events(
    mut mouse_wheel_reader: MessageReader<bevy::input::mouse::MouseWheel>,
    hover_map: Res<bevy::picking::hover::HoverMap>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
    mut commands: Commands,
) {
    for mouse_wheel in mouse_wheel_reader.read() {
        let mut delta = -Vec2::new(mouse_wheel.x, mouse_wheel.y);

        if mouse_wheel.unit == bevy::input::mouse::MouseScrollUnit::Line {
            delta *= LINE_HEIGHT;
        }

        if keyboard_input.any_pressed([KeyCode::ControlLeft, KeyCode::ControlRight]) {
            std::mem::swap(&mut delta.x, &mut delta.y);
        }

        for pointer_map in hover_map.values() {
            for entity in pointer_map.keys().copied() {
                commands.trigger(Scroll { entity, delta });
            }
        }
    }
}

/// UI scrolling event.
#[derive(EntityEvent, Debug)]
#[entity_event(propagate, auto_propagate)]
struct Scroll {
    entity: Entity,
    /// Scroll delta in logical coordinates.
    delta: Vec2,
}

fn on_scroll_handler(
    mut scroll: On<Scroll>,
    mut query: Query<(&mut ScrollPosition, &Node, &ComputedNode)>,
) {
    let Ok((mut scroll_position, node, computed)) = query.get_mut(scroll.entity) else {
        return;
    };

    let max_offset = (computed.content_size() - computed.size()) * computed.inverse_scale_factor();

    let delta = &mut scroll.delta;
    if node.overflow.x == OverflowAxis::Scroll && delta.x != 0. {
        // Is this node already scrolled all the way in the direction of the scroll?
        let max = if delta.x > 0. {
            scroll_position.x >= max_offset.x
        } else {
            scroll_position.x <= 0.
        };

        if !max {
            scroll_position.x += delta.x;
            // Consume the X portion of the scroll delta.
            delta.x = 0.;
        }
    }

    if node.overflow.y == OverflowAxis::Scroll && delta.y != 0. {
        // Is this node already scrolled all the way in the direction of the scroll?
        let max = if delta.y > 0. {
            scroll_position.y >= max_offset.y
        } else {
            scroll_position.y <= 0.
        };

        if !max {
            scroll_position.y += delta.y;
            // Consume the Y portion of the scroll delta.
            delta.y = 0.;
        }
    }

    // Stop propagating when the delta is fully consumed.
    if *delta == Vec2::ZERO {
        scroll.propagate(false);
    }
}
