
pub struct Actor {

}

impl Actor {
    pub fn new() -> Actor {
        Actor {}
    }

    pub fn set_commands(&mut self, commands: &[&str]) {

    }

    pub fn prepare_script(&mut self, script: &str) {

    }

    pub fn run_event(&mut self, event: &str) {

    }

    pub fn has_event(&self, event: &str) -> bool {
        false
    }

    pub fn get_variable(&self, name: &str) -> () {

    }

    pub fn set_variable(&self, name: &str, value: ()) {

    }
}


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
