# Project Work Rules (PWRs) — SEAFO

Created: 18 Nov 2026
Revised: 18 Nov 2026 (aligned to the SEAFO R package layout)
Applies to: All AI agents / assistant sessions working in this project.
Set by: Project Owner (Erich MALETZKY).
Agent: DS41F (DeepSeek v4.1 Flash).

These rules are binding for the whole project and take precedence over any
general or default Agent behaviour.

> **Binding Note:** this file is kept at `./WIP/AGENTS.md` as per the Project 
> Owner direction and is bound as a **workspace-level** instruction set via the
> `instructions` entry in the project-root `opencode.json`. It therefore governs
> every session whose location is inside `SEAFO`, not only files under `./WIP/`. 
> Do not relocate it — moving or renaming it breaks the binding.

---

## PWR-1 — Git has Owner-Only Context

The Project Owner is the **only** party who runs state-changing git commands in
this repo.

The Agent must **never** run a git command that mutates repository state —
including (non-exhaustive): `add`, `commit`, `push`, `pull`, `fetch`,
`checkout`, `switch`, `merge`, `rebase`, `reset`, `restore`, `clean`, `stash`,
`tag`, `rm`, `mv`, `cherry-pick`, `revert`, `submodule`, or any branch
operation that creates, switches, renames, deletes or merges branches.

Read-only git invocations are permitted, and are the expected basis for advice
on best-practice coding recommendations: `git status`, `git log`, `git diff`,
`git show`, `git blame`, `git branch --list`, `git ls-files`, `git remote -v`.

If the Agent believes a commit/checkout/etc. is warranted, the Agent must
**state the recommendation in prose** and wait for the Project Owner to
execute it.

---

## PWR-2 — Scoped Code Changes Only

The Agent makes **only** code & file location changes that have been explicitly
requested, discussed and agreed upon in the exchange between Project Owner & The
Agent.

Out of scope unless separately approved:
- unsolicited refactors, reformatting, renames, or "cleanups"
- dependency/library changes
- touching files unrelated to the requested task
- deleting or overwriting existing work

If the Agent spots an out-of-scope improvement (dead code, duplicated config
lines, style drift, etc.), the Agent may **flag it as a suggestion** but must
not apply it without a go-ahead by the Project Owner.

---

## PWR-3 — "CCD" Prefix (triggers project codebase rescan)

`CCD` == **C**ode **C**hange**d**.

The Project Owner may edit code manually between collaborative tasks. When a
task prompt is pre-pended with `CCD` the Agent shall:

1. Treat the entire code base as **potentially altered** since the Agent last
   looked at it.
2. **Re-scan the whole project before starting the new task** — see checklist
   below (for re-scan caveats).
3. Re-establish an accurate picture of the current state, then proceed.
4. Confirm in the response that the re-scan was performed.

If the placement or intent of `CCD` is ever ambiguous, the Agent shall **ask**
rather than assume — a missed re-scan is worse than a clarifying question.

### CCD Rescan Checklist

- Root: `SEAFO.Rproj`, `DESCRIPTION`, `NAMESPACE`, `.gitignore`,
  `.Rbuildignore`, `README.md`, `LICENSE`, `opencode.json`, plus staged and
  untracked files present (checked via the read-only git invocations permitted
  under PWR-1).
- `./R/` — every `*.R` source file: the `CODE.*`, `DEVS.*`, `INFO.*` and
  `RENV_*` function sources.
- `./man/` — every `*.Rd` file (re-listed only; roxygen2-generated, see PWR 4.4).
- `./WIP/` — `00_ACT_DEV_TRCKR.txt`, `00_Helpers/`, `01_NextForPROD/`, and this
  `AGENTS.md`.
- `./**/*` — the project root and every sub-directory beneath it (recursive,
  including new and hidden files/folders), covering any directories and files
  added as the project continues to develop and take form.

NB: Large `.rds`, `.csv`, and `.pdf` assets are re-listed, not re-read in full
    (unless the Project Owner specifically directs a re-scan of said files).

---

## PWR-4 — Execution, Environment & Modification Boundary

### 4.1 — Code execution requires agreement
The Agent shall **not** execute project R code unless the Project Owner has
explicitly requested, discussed and agreed to that run in the current exchange.
This covers, non-exhaustively:

- sourcing or running any `./R/*.R` file directly;
- `devtools::load_all()`, `devtools::document()`, `devtools::check()`,
  `devtools::test()`, `devtools::install()`, `R CMD build`, `R CMD check`;
- calling any exported `SEAFO::` function (including installed builds);
- running or sourcing anything under `./WIP/00_Helpers/` or `./WIP/01_NextForPROD/`.

Static work — reading, searching, browsing history, editing text, reviewing
diffs — is unrestricted within the limits of PWR-2.

### 4.2 — Package installation & version numbers
Package dependencies declared in `DESCRIPTION` are installed from CRAN when the
Project Owner builds or runs the project; however, the Agent shall never
install, upgrade, downgrade or remove R packages on its own initiative, and
shall **flag before running** any step whose logic could trigger an
installation (e.g. `load_all()`, `document()` or `check()` while a declared
dependency is absent).

The `Version:` field in `DESCRIPTION` is part of the release process —
production `0.0.1`, active-development increments `0.0.1.*` — and is managed
through `devs.patch.libr.vers.number()`. The Agent shall not edit `Version:`
unless the Project Owner explicitly directs it.

### 4.3 — Generated outputs
Artefacts produced by code execution are generated artefacts. In this project
that covers:

- `./man/*.Rd` and `NAMESPACE` — written by roxygen2 via `devtools::document()`;
- `./WIP/00_ACT_DEV_TRCKR.txt` — rewritten by
  `devs.patch.code.dev.trckr.file()`;
- any file written at runtime by a package function (e.g. by `info.post.note()`
  or `code.append.text.to.file()`).

The Agent shall not create, edit, overwrite, stage, commit, ignore or delete
generated artefacts by hand. Regeneration is a code-execution action and is
therefore governed by PWR 4.1. Disposition of generated files (including any
`.gitignore` / `.Rbuildignore` treatment) remains the Project Owner's
discretion, informed by Agent advice under PWR-1 / PWR-2.

### 4.4 — Modification boundary: active R sources only
All code modifications made by the Agent shall be **strictly confined to the
active function sources in `./R/`**:

- `./R/*.R` — the roxygen2-documented sources of the `CODE.*` (text & file
  helpers), `DEVS.*` (code-spec and tracker tooling), `INFO.*` (standardised
  console notifications) and `RENV_*` (runtime-environment constants) families.

The Agent shall not hand-edit generated files (`./man/*.Rd`, `NAMESPACE`), nor
non-script assets — `DESCRIPTION`, `LICENSE`, `README.md`, `.gitignore`,
`.Rbuildignore`, `SEAFO.Rproj`, `opencode.json`, `./WIP/00_ACT_DEV_TRCKR.txt`,
this `AGENTS.md`, or any other file — unless the Project Owner explicitly
directs that specific change.

`./WIP/00_Helpers/` (function templates) and `./WIP/01_NextForPROD/` (staging) are
the Project Owner's development sandbox: read them freely, edit them only on
explicit direction.

New or renamed files in `./R/` follow the existing scheme — file
`./R/<FAMILY>.Camel.Case.R` exporting a function whose roxygen `@name` is in
lowercase dotted form (e.g. `./R/CODE.Tiny.Helpers.EXPORTED.R` → `code.*`),
retaining the `#? ### ###` file-header banner and the matching `@family` tag.

---

## Project Context (reference)

- **Purpose:** an R package of user-friendly functions supporting the routine
  data-analysis work of the SEAFO Scientific Committee (SC) — base maps, VMS
  tracking, isobaths and latitude block zones. The shipped `R/` layer is the
  developer/utility toolset that underpins that work.
- **Entry point:** the package itself. `SEAFO.Rproj` at the project root;
  function sources in `./R/`, roxygen-generated documentation in `./man/`, and
  development staging plus the active-development tracker in `./WIP/`.
- **Toolchain:** R 4.6.1 (2026-06-24 ucrt); devtools 2.5.2; roxygen2 8.1.0 with
  `Roxygen: list(markdown = TRUE)` — `NAMESPACE` and `./man/` are roxygen-driven.
- **Versioning:** production `0.0.1`, active development `0.0.1.*`; current
  state recorded in `./WIP/00_ACT_DEV_TRCKR.txt`.
- **Conventions:** path building via `here::here()`; `_`-suffixed identifiers
  for local variables and constants (function parameters keep their `ss`/`sb`/
  `cs` prefix without a trailing `_`); 2-space indentation (spaces, not tabs);
  UTF-8. Default to direct library function calls with the aid of the `::`
  operator.
