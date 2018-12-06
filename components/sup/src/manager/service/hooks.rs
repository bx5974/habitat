// Copyright (c) 2016 Chef Software Inc. and/or applicable contributors
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

use std;
use std::ffi::OsStr;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
#[cfg(unix)]
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::path::{Path, PathBuf};
#[cfg(not(windows))]
use std::process::{Child, Command, ExitStatus, Stdio};
use std::result;

use serde::{Serialize, Serializer};

use hcore::templating::package::Pkg;
use hcore::templating::hooks::{self, ExitCode, Hook, HookOutput, RenderPair};
use hcore::templating::TemplateRenderer;
use super::health;
use hcore::crypto;
use error::{Error, Result};
use hcore::fs;
#[cfg(windows)]
use hcore::os::process::windows_child::{Child, ExitStatus};
use hcore::package::PackageInstall;

static LOGKEY: &'static str = "HK";

#[derive(Debug, Serialize)]
pub struct FileUpdatedHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for FileUpdatedHook {
    type ExitValue = bool;

    fn file_name() -> &'static str {
        "file-updated"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        FileUpdatedHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(&self, _: &str, _: &'a HookOutput, status: &ExitStatus) -> Self::ExitValue {
        status.success()
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct HealthCheckHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for HealthCheckHook {
    type ExitValue = health::HealthCheck;

    fn file_name() -> &'static str {
        "health-check"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        HealthCheckHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(0) => health::HealthCheck::Ok,
            Some(1) => health::HealthCheck::Warning,
            Some(2) => health::HealthCheck::Critical,
            Some(3) => health::HealthCheck::Unknown,
            Some(code) => {
                outputln!(preamble service_group,
                    "Health check exited with an unknown status code, {}", code);
                health::HealthCheck::default()
            }
            None => {
                Self::output_termination_message(service_group, status);
                health::HealthCheck::default()
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct InitHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for InitHook {
    type ExitValue = bool;

    fn file_name() -> &'static str {
        "init"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        InitHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(0) => true,
            Some(code) => {
                outputln!(preamble service_group, "Initialization failed! '{}' exited with \
                    status code {}", Self::file_name(), code);
                false
            }
            None => {
                outputln!(preamble service_group, "Initialization failed! '{}' exited without a \
                    status code", Self::file_name());
                false
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct RunHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for RunHook {
    type ExitValue = ExitCode;

    fn file_name() -> &'static str {
        "run"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        RunHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn run<T>(&self, _: &str, _: &Pkg, _: Option<T>) -> Self::ExitValue
    where
        T: ToString,
    {
        panic!(
            "The run hook is a an exception to the lifetime of a service. It should only be \
             run by the Supervisor module!"
        );
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(code) => ExitCode(code),
            None => {
                Self::output_termination_message(service_group, status);
                ExitCode::default()
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct PostRunHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for PostRunHook {
    type ExitValue = ExitCode;

    fn file_name() -> &'static str {
        "post-run"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        PostRunHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(code) => ExitCode(code),
            None => {
                Self::output_termination_message(service_group, status);
                ExitCode::default()
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct ReloadHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for ReloadHook {
    type ExitValue = ExitCode;

    fn file_name() -> &'static str {
        "reload"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        ReloadHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(0) => ExitCode(0),
            Some(code) => {
                outputln!(preamble service_group, "Reload failed! '{}' exited with \
                    status code {}", Self::file_name(), code);
                ExitCode(code)
            }
            None => {
                Self::output_termination_message(service_group, status);
                ExitCode::default()
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct ReconfigureHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for ReconfigureHook {
    type ExitValue = ExitCode;

    fn file_name() -> &'static str {
        "reconfigure"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        ReconfigureHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(code) => ExitCode(code),
            None => {
                Self::output_termination_message(service_group, status);
                ExitCode::default()
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct SmokeTestHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for SmokeTestHook {
    type ExitValue = health::SmokeCheck;

    fn file_name() -> &'static str {
        "smoke-test"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        SmokeTestHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(0) => health::SmokeCheck::Ok,
            Some(code) => health::SmokeCheck::Failed(code),
            None => {
                Self::output_termination_message(service_group, status);
                health::SmokeCheck::Failed(-1)
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct SuitabilityHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for SuitabilityHook {
    type ExitValue = Option<u64>;

    fn file_name() -> &'static str {
        "suitability"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        SuitabilityHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        hook_output: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(0) => {
                if let Some(reader) = hook_output.stdout() {
                    if let Some(line_reader) = reader.lines().last() {
                        match line_reader {
                            Ok(line) => {
                                match line.trim().parse::<u64>() {
                                    Ok(suitability) => {
                                        outputln!(preamble service_group,
                                                  "Reporting suitability of: {}", suitability);
                                        return Some(suitability);
                                    }
                                    Err(err) => {
                                        outputln!(preamble service_group,
                                            "Parsing suitability failed: {}", err);
                                    }
                                };
                            }
                            Err(err) => {
                                outputln!(preamble service_group,
                                    "Failed to read last line of stdout: {}", err);
                            }
                        };
                    } else {
                        outputln!(preamble service_group,
                                  "{} did not print anything to stdout", Self::file_name());
                    }
                }
            }
            Some(code) => {
                outputln!(preamble service_group,
                    "{} exited with status code {}", Self::file_name(), code);
            }
            None => {
                Self::output_termination_message(service_group, status);
            }
        }
        None
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Serialize)]
pub struct PostStopHook {
    render_pair: RenderPair,
    stdout_log_path: PathBuf,
    stderr_log_path: PathBuf,
}

impl Hook for PostStopHook {
    type ExitValue = bool;

    fn file_name() -> &'static str {
        "post-stop"
    }

    fn new(package_name: &str, pair: RenderPair) -> Self {
        PostStopHook {
            render_pair: pair,
            stdout_log_path: hooks::stdout_log_path::<Self>(package_name),
            stderr_log_path: hooks::stderr_log_path::<Self>(package_name),
        }
    }

    fn handle_exit<'a>(
        &self,
        service_group: &str,
        _: &'a HookOutput,
        status: &ExitStatus,
    ) -> Self::ExitValue {
        match status.code() {
            Some(0) => true,
            Some(code) => {
                outputln!(preamble service_group, "Post stop failed! '{}' exited with \
                    status code {}", Self::file_name(), code);
                false
            }
            None => {
                Self::output_termination_message(service_group, status);
                false
            }
        }
    }

    fn path(&self) -> &Path {
        &self.render_pair.path
    }

    fn renderer(&self) -> &TemplateRenderer {
        &self.render_pair.renderer
    }

    fn stdout_log_path(&self) -> &Path {
        &self.stdout_log_path
    }

    fn stderr_log_path(&self) -> &Path {
        &self.stderr_log_path
    }
}

#[derive(Debug, Default, Serialize)]
pub struct HookTable {
    pub health_check: Option<HealthCheckHook>,
    pub init: Option<InitHook>,
    pub file_updated: Option<FileUpdatedHook>,
    pub reload: Option<ReloadHook>,
    pub reconfigure: Option<ReconfigureHook>,
    pub suitability: Option<SuitabilityHook>,
    pub run: Option<RunHook>,
    pub post_run: Option<PostRunHook>,
    pub smoke_test: Option<SmokeTestHook>,
    pub post_stop: Option<PostStopHook>,
}

impl HookTable {
    /// Read all available hook templates from the table's package directory into the table.
    pub fn load<P, T>(package_name: &str, templates: T, hooks_path: P) -> Self
    where
        P: AsRef<Path>,
        T: AsRef<Path>,
    {
        let mut table = HookTable::default();
        if let Some(meta) = std::fs::metadata(templates.as_ref()).ok() {
            if meta.is_dir() {
                table.file_updated = FileUpdatedHook::load(package_name, &hooks_path, &templates);
                table.health_check = HealthCheckHook::load(package_name, &hooks_path, &templates);
                table.suitability = SuitabilityHook::load(package_name, &hooks_path, &templates);
                table.init = InitHook::load(package_name, &hooks_path, &templates);
                table.reload = ReloadHook::load(package_name, &hooks_path, &templates);
                table.reconfigure = ReconfigureHook::load(package_name, &hooks_path, &templates);
                table.run = RunHook::load(package_name, &hooks_path, &templates);
                table.post_run = PostRunHook::load(package_name, &hooks_path, &templates);
                table.smoke_test = SmokeTestHook::load(package_name, &hooks_path, &templates);
                table.post_stop = PostStopHook::load(package_name, &hooks_path, &templates);
            }
        }
        debug!(
            "{}, Hooks loaded, destination={}, templates={}",
            package_name,
            hooks_path.as_ref().display(),
            templates.as_ref().display()
        );
        table
    }

    /// Compile all loaded hooks from the table into their destination service directory.
    ///
    /// Returns `true` if compiling any of the hooks resulted in new
    /// content being written to the hook scripts on disk.
    pub fn compile<T>(&self, service_group: &str, ctx: &T) -> bool
    where
        T: Serialize,
    {
        debug!("{:?}", self);
        let mut changed = false;
        if let Some(ref hook) = self.file_updated {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.health_check {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.init {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.reload {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.reconfigure {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.suitability {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.run {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.post_run {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.smoke_test {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        if let Some(ref hook) = self.post_stop {
            changed = self.compile_one(hook, service_group, ctx) || changed;
        }
        changed
    }

    fn compile_one<H, T>(&self, hook: &H, service_group: &str, ctx: &T) -> bool
    where
        H: Hook,
        T: Serialize,
    {
        match hook.compile(service_group, ctx) {
            Ok(status) => status,
            Err(e) => {
                outputln!(preamble service_group,
                          "Failed to compile {} hook: {}", H::file_name(), e);
                false
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use tempfile::TempDir;

    use super::*;
    use package::{PackageIdent, PackageInstall};
    use service::ServiceGroup;
    use templating::config::Cfg;
    use templating::context::RenderContext;
    use templating::package::Pkg;
    use templating::test_helpers::*;

    // Turns out it's useful for Hooks to implement AsRef<Path>, at
    // least for these tests. Ideally, this would be useful to use
    // outside of the tests as well, but some additional refactoring
    // will be necessary.
    macro_rules! as_ref_path_impl {
        ($($t:ty)*) => ($(
            impl AsRef<Path> for $t {
                fn as_ref(&self) -> &Path {
                    &self.render_pair.path
                }
            }
        )*)
    }

    as_ref_path_impl!(FileUpdatedHook
                      HealthCheckHook
                      InitHook
                      PostRunHook
                      ReconfigureHook
                      ReloadHook
                      RunHook
                      SmokeTestHook
                      SuitabilityHook
                      PostStopHook);

    fn hook_fixtures_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("fixtures")
            .join("hooks")
    }

    fn hook_templates_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("fixtures")
            .join("hooks")
            .join("hook_templates")
    }

    fn rendered_hooks_path() -> TempDir {
        TempDir::new().expect("create temp dir")
    }

    fn service_group() -> ServiceGroup {
        ServiceGroup::new(None, "test_service", "test_group", None)
            .expect("couldn't create ServiceGroup")
    }

    ////////////////////////////////////////////////////////////////////////

    #[test]
    fn hashing_a_hook_that_already_exists_returns_a_hash_of_the_file() {
        let service_group = service_group();
        let concrete_path = rendered_hooks_path();
        let template_path = hook_templates_path();

        let hook = InitHook::load(&service_group, &concrete_path, &template_path)
            .expect("Could not create testing init hook");

        let content = r#"
#!/bin/bash

echo "The message is Hello World"
"#;
        create_with_content(&hook, content);

        assert_eq!(
            hash_content(hook.path()).unwrap(),
            "1cece41b2f4d5fddc643fc809d80c17d6658634b28ec1c5ceb80e512e20d2e72"
        );
    }

    #[test]
    fn hashing_a_hook_that_does_not_already_exist_returns_an_empty_string() {
        let service_group = service_group();
        let concrete_path = rendered_hooks_path();
        let template_path = hook_templates_path();
        let hook = InitHook::load(&service_group, &concrete_path, &template_path)
            .expect("Could not create testing init hook");

        assert_eq!(hash_content(hook.path()).unwrap(), "");
    }

    #[test]
    fn updating_a_hook_with_the_same_content_is_a_noop() {
        let service_group = service_group();
        let concrete_path = rendered_hooks_path();
        let template_path = hook_templates_path();

        let hook = InitHook::load(&service_group, &concrete_path, &template_path)
            .expect("Could not create testing init hook");

        // Since we're trying to update a file that should already
        // exist, we need to actually create it :P
        let content = r#"
#!/bin/bash

echo "The message is Hello World"
"#;
        create_with_content(&hook, content);

        let pre_change_content = file_content(&hook);

        // In the real world, we'd be templating something with this
        // content, but for the purposes of detecting changes, feeding
        // it the final text works well enough, and doesn't tie this
        // test to the templating machinery.
        assert_eq!(write_hook(&content, hook.path()).unwrap(), false);

        let post_change_content = file_content(&hook);
        assert_eq!(post_change_content, pre_change_content);
    }

    #[test]
    fn updating_a_hook_that_creates_the_file_works() {
        let service_group = service_group();
        let concrete_path = rendered_hooks_path();
        let template_path = hook_templates_path();

        let hook = InitHook::load(&service_group, &concrete_path, &template_path)
            .expect("Could not create testing init hook");

        // In this test, we'll start with *no* rendered content.
        assert_eq!(hook.as_ref().exists(), false);

        let updated_content = r#"
#!/bin/bash

echo "The message is Hello World"
"#;
        // Since there was no compiled hook file before, this should
        // create it, returning `true` to reflect that
        assert_eq!(write_hook(&updated_content, hook.path()).unwrap(), true);

        // The content of the file should now be what we just changed
        // it to.
        let post_change_content = file_content(&hook);
        assert_eq!(post_change_content, updated_content);
    }

    #[test]
    fn truly_updating_a_hook_works() {
        let service_group = service_group();
        let concrete_path = rendered_hooks_path();
        let template_path = hook_templates_path();

        let hook = InitHook::load(&service_group, &concrete_path, &template_path)
            .expect("Could not create testing init hook");

        let initial_content = r#"
#!/bin/bash

echo "The message is Hello World"
"#;
        create_with_content(&hook, initial_content);

        // Again, we're not templating anything here (as would happen
        // in the real world), but just passing the final content that
        // we'd like to update the hook with.
        let updated_content = r#"
#!/bin/bash

echo "The message is Hola Mundo"
"#;
        assert_eq!(write_hook(&updated_content, hook.path()).unwrap(), true);

        let post_change_content = file_content(&hook);
        assert_ne!(post_change_content, initial_content);
        assert_eq!(post_change_content, updated_content);
    }

    /// Avert your eyes, children; avert your eyes!
    ///
    /// All I wanted was a simple RenderContext so I could compile a
    /// hook. With the type signatures as they are, though, I don't
    /// know if that's possible. So, in the functions that follow, a
    /// minimal fake RenderContext is created within this function,
    /// and we pass it into the relevant compilation functions to test
    ///
    /// A `RenderContext` could _almost_ be anything that's
    /// JSON-serializable, in which case we wouldn't have to jump
    /// through _nearly_ as many hoops as we do here. Unfortunately,
    /// the compilation call also pulls things out of the context's
    /// package struct, which is more than just a blob of JSON
    /// data. We can probably do something about that, though.
    ///
    /// The context that these functions ends up making has a lot of
    /// fake data around the ring membership, the package, etc. We
    /// don't really need all that just to make compilation actually
    /// change a file or not.
    ///
    /// Due to how a RenderContext is currently set up, though, I
    /// couldn't sort out the relevant Rust lifetimes and type
    /// signatures needed to have a helper function that just handed
    /// back a RenderContext. It may be possible, or we may want to
    /// refactor that code to make it possible. In the meantime, copy
    /// and paste of the code is how we're going to do it :(
    #[test]
    fn compile_a_hook() {
        let service_group = service_group();
        let concrete_path = rendered_hooks_path();
        let template_path = hook_templates_path();

        let hook = InitHook::load(&service_group, &concrete_path, &template_path)
            .expect("Could not create testing init hook");

        ////////////////////////////////////////////////////////////////////////
        // BEGIN RENDER CONTEXT SETUP
        // (See comment above)

        let pg_id = PackageIdent::new(
            "testing",
            &service_group.service(),
            Some("1.0.0"),
            Some("20170712000000"),
        );

        let pkg_install = PackageInstall::new_from_parts(
            pg_id.clone(),
            PathBuf::from("/tmp"),
            PathBuf::from("/tmp"),
            PathBuf::from("/tmp"),
        );
        let pkg = Pkg::from_install(pkg_install).expect("Could not create package!");

        // This is gross, but it actually works
        let cfg_path = concrete_path.as_ref().join("default.toml");
        create_with_content(cfg_path, &String::from("message = \"Hello\""));

        let cfg = Cfg::new(&pkg, Some(&concrete_path.as_ref().to_path_buf()))
            .expect("Could not create config");

        let ctx = RenderContext::new(&pkg, &cfg);

        // END RENDER CONTEXT SETUP
        ////////////////////////////////////////////////////////////////////////

        assert_eq!(hook.compile(&service_group, &ctx).unwrap(), true);

        let post_change_content = file_content(&hook);
        let expected = r#"#!/bin/bash

echo "The message is Hello"
"#;
        assert_eq!(post_change_content, expected);

        // Compiling again should result in no changes
        assert_eq!(hook.compile(&service_group, &ctx).unwrap(), false);
        let post_second_change_content = file_content(&hook);
        assert_eq!(post_second_change_content, post_change_content);
    }

    #[test]
    fn compile_hook_table() {
        let tmp_root = rendered_hooks_path();
        let hooks_path = tmp_root.path().join("hooks");
        fs::create_dir_all(&hooks_path).unwrap();

        let service_group = service_group();

        let concrete_path = hooks_path.clone(); //rendered_hooks_path();
        let template_path = hook_templates_path();

        ////////////////////////////////////////////////////////////////////////
        // BEGIN RENDER CONTEXT SETUP
        // (See comment above)

        let pg_id = PackageIdent::new(
            "testing",
            &service_group.service(),
            Some("1.0.0"),
            Some("20170712000000"),
        );

        let pkg_install = PackageInstall::new_from_parts(
            pg_id.clone(),
            PathBuf::from("/tmp"),
            PathBuf::from("/tmp"),
            PathBuf::from("/tmp"),
        );
        let pkg = Pkg::from_install(pkg_install).expect("Could not create package!");

        // This is gross, but it actually works
        let cfg_path = &concrete_path.as_path().join("default.toml");
        create_with_content(cfg_path, &String::from("message = \"Hello\""));

        let cfg = Cfg::new(&pkg, Some(&concrete_path.as_path().to_path_buf()))
            .expect("Could not create config");

        let ctx = RenderContext::new(&pkg, &cfg);

        // END RENDER CONTEXT SETUP
        ////////////////////////////////////////////////////////////////////////

        let hook_table = HookTable::load(&service_group, &template_path, &hooks_path);
        assert_eq!(hook_table.compile(&service_group, &ctx), true);

        // Verify init hook
        let init_hook_content = file_content(&hook_table.init.as_ref().expect("no init hook??"));
        assert_eq!(
            init_hook_content,
            "#!/bin/bash\n\necho \"The message is Hello\"\n"
        );
        // Verify run hook
        let run_hook_content = file_content(&hook_table.run.as_ref().expect("no run hook??"));
        assert_eq!(
            run_hook_content,
            "#!/bin/bash\n\necho \"Running a program\"\n"
        );

        // Recompiling again results in no changes
        assert_eq!(hook_table.compile(&service_group, &ctx), false);

        // Re-Verify init hook
        let init_hook_content = file_content(&hook_table.init.as_ref().expect("no init hook??"));
        assert_eq!(
            init_hook_content,
            "#!/bin/bash\n\necho \"The message is Hello\"\n"
        );
        // Re-Verify run hook
        let run_hook_content = file_content(&hook_table.run.as_ref().expect("no run hook??"));
        assert_eq!(
            run_hook_content,
            "#!/bin/bash\n\necho \"Running a program\"\n"
        );
    }

    ////////////////////////////////////////////////////////////////////////

    #[test]
    #[cfg(not(windows))]
    fn hook_output() {
        use std::fs::DirBuilder;
        use std::process::{Command, Stdio};

        let tmp_dir = TempDir::new().expect("create temp dir");
        let logs_dir = tmp_dir.path().join("logs");
        DirBuilder::new()
            .recursive(true)
            .create(logs_dir)
            .expect("couldn't create logs dir");
        let mut cmd = Command::new(hook_fixtures_path().join(InitHook::file_name()));
        cmd.stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let mut child = cmd.spawn().expect("couldn't run hook");
        let stdout_log = tmp_dir
            .path()
            .join("logs")
            .join(format!("{}.stdout.log", InitHook::file_name()));
        let stderr_log = tmp_dir
            .path()
            .join("logs")
            .join(format!("{}.stderr.log", InitHook::file_name()));
        let mut hook_output = HookOutput::new(&stdout_log, &stderr_log);
        let service_group = ServiceGroup::new(None, "dummy", "service", None)
            .expect("couldn't create ServiceGroup");

        hook_output.stream_output::<InitHook>(&service_group, &mut child);

        let mut stdout = String::new();
        hook_output
            .stdout()
            .unwrap()
            .read_to_string(&mut stdout)
            .expect("couldn't read stdout");
        assert_eq!(stdout, "This is stdout\n");

        let mut stderr = String::new();
        hook_output
            .stderr()
            .unwrap()
            .read_to_string(&mut stderr)
            .expect("couldn't read stderr");
        assert_eq!(stderr, "This is stderr\n");

        fs::remove_dir_all(tmp_dir).expect("remove temp dir");
    }
}
