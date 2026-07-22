# RULES FOR AGENTS TO FOLLOW

* If I ask a question (ie. my prompt ends with a ?), never
  make modifications to any files or carry out destructive actions.
  Just answer the question as accurately as you can.
* Never add documentation and other non-source files to a commit unless
  asked to do so.
* When making changes to a branch or API that hasn't yet been merged into
  trunk, never worry about compatibility or breaking changes.
* Never frame a change around what was removed when the user asked for a clean
  cutover. State only the supported behavior and the files changed.
* Always do things the cleanest way possible. No stopgaps, no hacks,
  no workarounds. Only changes that are safe to commit and publish in a
  production setting.
* Never change API contracts, eg. URLs and response payload
  formats without my explicit authorization.
* If I ask you to remove functionality, also remove the tests for it, don't
  change the tests to assert lack of functionality.
* NEVER remove comments when asked to simplify or cleanup code.
* ALWAYS document public types, functions and fields.
* ALWAYS document private types, functions and fields if they aren't obvious.
* NEVER use unicode characters in source code comments, such as m-dashes.
* NEVER make unrelated changes without asking first.
* ALWAYS start by explaining how you understand an issue and propose a fix when
  I identify something wrong, DO NOT just fix it.
* When I ask you to fix something that was recently commited to a feature
  branch, ALWAYS amend the commit that introduced the bad code.
* NEVER write comments that narrate a migration, compare against previous
  behavior, defend a design choice, mention rejected alternatives, explain what
  the code does not do, or reference plan/review history. Comments should
  describe only the current code’s stable purpose, invariants, or non-obvious
  constraints.
* When writing technical documentation, comments, etc. ALWAYS adhere to
  ADS-STE100 Simplified Technical English, unless asked to adhere to another
  style explicitly.
