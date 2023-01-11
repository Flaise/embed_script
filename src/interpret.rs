
fn whatever() {
    let mut actor = Actor::new();
    actor.set_commands(&[
        ("set", ????????? ),
    ]);
    actor.execute_script("set r <- 10");
    assert_eq!(actor.variable("r"), 10);

    let mut actor = Actor::new();
    actor.set_commands(&[
        ("set", ????????? ),
        ("event", ??????? ),
        ("end event", ??? ),
    ]);
    actor.prepare_script("
        event testing
            set r <- 10
        end event
    ");
    assert!(actor.has_event("testing"));
    actor.run_event("testing");
}
