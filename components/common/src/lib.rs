// Copyright (c) 2016-2017 Chef Software Inc. and/or applicable contributors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

extern crate ansi_term;
extern crate bimap;
extern crate glob;
extern crate habitat_api_client as api_client;
#[macro_use]
extern crate habitat_core as hcore;
extern crate handlebars;
extern crate hyper;
extern crate json;
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
extern crate pbr;
extern crate petgraph;
extern crate regex;
extern crate retry;
extern crate serde;
#[macro_use]
extern crate serde_derive;

// This is a little gross, but we only need the macros in tests right
// now.
#[cfg(test)]
#[macro_use]
extern crate serde_json;
#[cfg(not(test))]
extern crate serde_json;

extern crate serde_transcode;
extern crate serde_yaml;
extern crate tempfile;
extern crate term;
extern crate time;
extern crate toml;
extern crate uuid;
extern crate valico;

#[cfg(windows)]
extern crate winapi;

pub use self::error::{Error, Result};

pub mod command;
pub mod defaults;
pub mod error;
pub mod locked_env_var;
pub mod package_graph;
pub mod templating;
pub mod ui;
pub mod util;
