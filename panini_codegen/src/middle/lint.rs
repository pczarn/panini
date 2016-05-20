pub use self::Level::*;
pub use self::Lint::*;

/// Setting for how to handle a lint.
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub enum Level {
    Allow, Warn, Deny, Forbid
}

impl Level {
    /// Convert a level to a lower-case string.
    pub fn as_str(self) -> &'static str {
        match self {
            Allow => "allow",
            Warn => "warn",
            Deny => "deny",
            Forbid => "forbid",
        }
    }

    /// Convert a lower-case string to a level.
    pub fn from_str(x: &str) -> Option<Level> {
        match x {
            "allow" => Some(Allow),
            "warn" => Some(Warn),
            "deny" => Some(Deny),
            "forbid" => Some(Forbid),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
pub enum Lint {
    Unproductive,
    DeadCode,
    Cycles,
}

impl Lint {
    /// Get a lint's name.
    pub fn name(self) -> &'static str {
        match self {
            Unproductive => "unproductive_rules",
            DeadCode => "dead_code",
            Cycles => "cycles",
        }
    }

    /// Convert a lint to a lower-case string.
    pub fn default_level(self) -> Level {
        Warn
    }
}
