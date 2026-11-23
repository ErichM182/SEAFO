<div align="center">

<img src="man/figures/logo.png" width="103" alt="SEAFO logo" />

# SEAFO

**The SEAFO Scientific Committee Data-Analysis & Mapping R Library**

<sub>Convention-Area base maps, spatial layers and developer tooling for the Scientific Committee (SC) of the
South East Atlantic Fisheries Organisation (SEAFO)</sub>

<br/>

[![R](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Active%20Development-orange)](https://github.com/ErichM182/SEAFO)
[![Exports](https://img.shields.io/badge/Exports-21%20Objects-blueviolet)](man/)
[![Docs](https://img.shields.io/badge/Roxygen2-8.1.0-informational)](https://roxygen2.r-lib.org/)

<br/>

**Current Release (STABLE): `0.0.3`** &nbsp;•&nbsp; **Active Development (ActDev): `0.0.3.07`**

</div>

---

## 📖 What is SEAFO?

`SEAFO` is an R package comprised of user-friendly functions that assist with the routine data-analysis 
work conducted by the Scientific Committee (SC) of the **South East Atlantic Fisheries Organisation**.
It ships three main tool sets in one library:

| | Component | What it gives you |
|:--|:--|:--|
| 🗺️ | **Mapping** | A one-call interactive Leaflet base map of the SEAFO Convention Area, with EEZ, VME-closure and area-label overlays |
| 🧰 | **Utilities** | Code/file helpers, null-coalescing operators, immutable constant lists and standardised console notifications |
| 🔁 | **Release tooling** | A self-tracking development workflow that ties `R CMD check`, package versioning and an md5 code-base fingerprint together |

Everything is `roxygen2`-documented, and the package currently passes `R CMD check --as-cran` with
**0 ERRORs, 0 WARNINGs and 0 NOTEs**.

---

## ✨ Highlights

- **`maps_dynamic.compile.basemap.seafo()`** — one call returns a fully assembled Leaflet widget:
  ESRI Ocean or ESRI NatGeo tiles, SEAFO Convention Area (CA), African EEZ v11, SEAFO VME closures,
  CA area labels, a measuring tool, a scale bar and a layer-control box.
- **`%??%`** — a null-coalescing operator that also treats zero-length and `NA` values as "missing".
- **Environment-locked constants** — `RENV_COLOURS`, `RENV_FORMATS`, `RENV_ICONS`, `RENV_FSID`
  and `RENV_DEVS` are immutable once loaded, so they cannot be clobbered mid-session.
- **Self-describing output** — `SEAFO` functions can report their own function ID, caller and source
  line number (`code_editor.fetch.celn()` + `info.post.func.self.id()`).
- **Delta-gated versioning** — the version only moves when something actually changed.

---

## 🚀 Installation from scratch

### 1. Prerequisites

| Requirement | Version | Notes |
|:--|:--|:--|
| **R** | `≥ 4.1.0` | Developed and tested on **R 4.6.1** (2026-06-24 ucrt) |
| **RStudio** / Positron | any recent | Optional, but recommended for the map viewer pane |
| **Rtools** | *not required* | `SEAFO` is pure R — there is no `src/` directory to compile |

### 2. Install the package

```r
# 1) a GitHub installer (skip if you already have remotes or devtools)
install.packages("remotes")

# 2) install SEAFO together with all of its CRAN dependencies
remotes::install_github("ErichM182/SEAFO")

# 3) verify
library(SEAFO)
packageVersion("SEAFO")
#> [1] '0.0.3'
```

<details>
<summary><b>🔒 If the repository is private</b></summary>

<br/>

```r
# Option A - GitHub Personal Access Token (recommended)
Sys.setenv(GITHUB_PAT = "ghp_your_token_here")
remotes::install_github("ErichM182/SEAFO", auth_token = Sys.getenv("GITHUB_PAT"))

# Option B - SSH access (the canonical remote for this repository)
remotes::install_git("git@github.com:ErichM182/SEAFO.git")
```

Store the token permanently in `~/.Renviron` (e.g. with `usethis::edit_r_environ()`) instead of
hard-coding it into a script.

</details>

### 3. Alternative installation routes

```r
# From a local clone (development mode, no installation needed)
devtools::load_all("path/to/SEAFO")

# From a local clone (installed copy)
devtools::install("path/to/SEAFO")

# From a built source tarball
install.packages("SEAFO_0.0.3.tar.gz", repos = NULL, type = "source")
```

### 4. Dependencies

`SEAFO` imports the following CRAN packages (installed automatically by `install_github()`):

| Package | Tested with | Used for |
|:--|:--|:--|
| `beepr` | 2.0 | audible run-time notifications |
| `desc` | 1.4.3 | reading/patching `DESCRIPTION` |
| `devtools` | 2.5.2 | documentation + `R CMD check` orchestration |
| `here` | 1.0.2 | project-root-relative file paths |
| `htmlwidgets` | 1.6.4 | `onRender()` callbacks on the Leaflet widget |
| `jsonlite` | 2.0.0 | JSON parsing |
| `leaflet` | 2.2.3 | interactive map canvas and controls |
| `leaflet.providers` | 3.0.0 | ESRI tile-provider definitions |
| `sfheaders` | 0.4.5 | data frame → `sf` polygon conversion |
| `stringr` | 1.6.0 | regular-expression extraction |

---

## ⚡ Quick start

```r
library(SEAFO)

### 1. Null-coalescing operator ------------------------------------------------
NULL      %??% "fallback"      #> "fallback"
"actual"  %??% "fallback"      #> "actual"
NA        %??% "fallback"      #> "fallback"   (NA counts as missing too)

### 2. Poll the project run-time mode -----------------------------------------
SEAFO::code.poll.r.runtime.mode()
#> $IS_DEBUG
#> [1] FALSE
#>
#> $IS_VERBOSE
#> [1] FALSE

### 3. Environment-locked constants -------------------------------------------
SEAFO::RENV_DEVS$PATH_TO_FOLDER_WIP_PROD
#> [1] "./WIP/01_NextForPROD"

SEAFO::RENV_FORMATS$ANSI_BOLD       #> "\033[1m"   (bold text; see ?RENV_FORMATS)

### 4. SEAFO Convention Area base map -----------------------------------------
coMap <- SEAFO::maps_dynamic.compile.basemap.seafo(
  snMapSRC          = 1,       # 1 = ESRI Ocean, anything else = ESRI NatGeo World Map
  sbShowEEZs        = TRUE,    # African EEZ v11 overlay
  sbShowClosedAreas = TRUE     # SEAFO VME closures overlay
)

coMap                          # renders in the RStudio Viewer pane
```

> **ℹ️ Base-map data path.** `maps_dynamic.compile.basemap.seafo()` reads its layers from
> `here::here("data", ...)`, so it expects the SEAFO project root (or any project that contains a
> `data/` folder with the three `.rda` files) to be `here()`'s root. All other functions work from
> anywhere.

---

## 🆕 What's new in `0.0.3`

| | Change | Detail |
|:--|:--|:--|
| 🗺️ **New** | `maps_dynamic.compile.basemap.seafo()` | Full Leaflet basemap: tile-provider selection, CA + EEZ + VME-closure overlays, CA area labels (`A`, `A1` … `D1`), measuring tool, scale bar, captioned layer control |
| 📦 **New data** | 3 documented spatial datasets | `dfSEAFO_CA`, `dfSEAFO_ClosedAreas`, `dfSEAFO_EEZv11` — lazily loaded (`xz`-compressed) |
| 🔁 **Release tooling** | Delta-gated version bumping | `devs.check.code.specs()` now bumps on a **documentation delta** *or* a **code-base delta**, and writes a "Code Base Fingerprint" (md5 over `R/`, `src/`, `data/`, `tests/` and `inst/`) into the tracker |
| 🔧 **Refactor** | `sbPatchVers` argument | `devs.patch.libr.vers.number()` can suppress its own increments so the caller decides when to bump |
| 📚 **Docs** | Complete basemap reference | Title, all 12 parameters, layer-by-layer `@details`, runnable examples |
| 🧹 **Housekeeping** | Clean CRAN result | Imports pruned (`here` added, `leafem`/`usethis` removed); `utils::globalVariables()` registered for lazily `load()`ed data; Leaflet measure tool now uses the **valid** locale key `"en"` (the previous `"na"` was silently ignored by the plugin) |

### Recent commits

```
5d69972  TWEAKS   -> SEAFO BaseMap Data Declarations
41bdd2e  PROGRESS -> SEAFO BaseMap COMPLETED
207cd09  PROGRESS -> SEAFO BaseMap v01 COMPLETED
2210d95  CHANGED  -> Versioning Bump System
6cb3f13  CLEAN UP -> Code base (multiple funcs)
```

---

## 🔢 Versioning & release workflow

`SEAFO` uses a two-level version scheme, both recorded in `WIP/00_ACT_DEV_TRCKR.txt`:

| Level | Format | Example | Meaning |
|:--|:--|:--|:--|
| **Production** | `0.0.X` | `0.0.3` | public release cut |
| **Active development** | `0.0.X.YY` | `0.0.3.00` | increments `.01`, `.02`, … per accepted change |

Both `DESCRIPTION` and the tracker are only written **when something actually changed**:

- the regenerated `man/*.Rd` + `NAMESPACE` differ from the previous run (documentation delta), or
- any file inside `R/`, `src/`, `data/`, `tests/` or `inst/` differs from the stored md5
  fingerprint (code-base delta), or
- a production release is explicitly requested.

A run with no delta is a true no-op — no version churn, no tracker rewrite.

---

## 📚 Function reference

### 🗺️ MAPS — mapping

| Function | Description |
|:--|:--|
| `maps_dynamic.compile.basemap.seafo()` | Compile an interactive SEAFO Leaflet base map (CA, EEZ, VME closures, labels, controls) |

### 🧰 CODE — code & file helpers

| Function | Description |
|:--|:--|
| `%??%` | Null-coalescing operator (also catches zero-length and `NA` values) |
| `code.append.text.to.file()` | Append text blocks or line stubs to a file |
| `code.classify.func()` | Classify R functions on the basis of code-base weights (CBWs) |
| `code.coalesce.dots.args()` | Coalesce two "DotsArgs" (`...`) lists into one |
| `code.poll.r.runtime.mode()` | Poll the `RCT_IS_DEBUG_RT_MODE_` / `RCT_IS_VERBOSE_RT_MODE_` flags |
| `code.return.renv.list()` | Build R environment-locked constant lists |
| `code_editor.fetch.celn()` | Extract the code-editor line number (CELN) of a call |
| `code_enums.create.renv.enum()` | Create immutable, environment-safe enumerated objects |

### 🛠️ DEVS — development & release tooling

| Function | Description |
|:--|:--|
| `devs.check.code.specs()` | Regenerate docs, bump only on a delta, then run CRAN validation |
| `devs.find.code.instances()` | Locate text or regex strings across R code |
| `devs.patch.code.dev.trckr.file()` | Update the development tracker (versions + code fingerprint) |
| `devs.patch.libr.vers.number()` | Increment the production / active-development version numbers |
| `devs.pull.libr.info()` | Extract R package meta-information |

### 🖨️ INFO — standardised console output

| Function | Description |
|:--|:--|
| `info.post.note()` | Post standardised, optionally boxed project notifications |
| `info.post.func.self.id()` | Print function Self-ID information (entry/exit, CELN, run-time) |

### 🔒 CONSTANTS — environment-locked lists

| Object | Description |
|:--|:--|
| `RENV_COLOURS` | ANSI console colour selections (foreground and background) |
| `RENV_FORMATS` | ANSI text formats (bold, italics, underline, reverse, reset) |
| `RENV_ICONS` | Console icon/decoration glyphs |
| `RENV_FSID` | Function Self-ID argument tags (`F_ARGS_*`, `CONSTS_*`) |
| `RENV_DEVS` | Development paths, file locations and date/time log formats (`FORMAT_TIME_DEV_LOG_*`) |

---

## 🗃️ Datasets

| Dataset | Description |
|:--|:--|
| `dfSEAFO_CA` | SEAFO Convention Area polygons (WGS84 / EPSG:4326) |
| `dfSEAFO_ClosedAreas` | SEAFO VME closures / closed areas polygons (WGS84) |
| `dfSEAFO_EEZv11` | African Exclusive Economic Zones, MarineRegions EEZ v11 (2019), WGS84 |

All three are `LazyData: true` and `xz`-compressed; they load on first access.

```r
data(dfSEAFO_CA)          # or simply reference it after library(SEAFO)
```

---

## 🗂️ Project layout

```
SEAFO/
├── R/                      # 21 roxygen-documented source files
│   ├── CODE*.R             #   code & file helpers
│   ├── DEVS.*.R            #   development & release tooling
│   ├── INFO.*.R            #   standardised console notifications
│   ├── MAPS_*.R            #   mapping functions
│   ├── RENV_*.R            #   environment-locked constant lists
│   └── data.R              #   dataset documentation
├── data/                   # 3 lazily-loaded spatial datasets (.rda)
├── man/                    # 24 help pages (generated by roxygen2)
│   └── figures/logo.png    #   package logo (README header image)
├── WIP/                    # development staging, tracker & workflow rules
│   ├── 00_ACT_DEV_TRCKR.txt
│   ├── 00_Helpers/
│   └── 01_NextForPROD/
├── DESCRIPTION             # package metadata (version lives here)
├── NAMESPACE               # generated by roxygen2
└── SEAFO.Rproj             # RStudio project file
```

---

## 🧑‍💻 Development workflow

```r
devtools::load_all()                  # work against the source tree (no install)
devtools::document()                  # regenerate man/ + NAMESPACE
devtools::check()                     # R CMD check --as-cran

SEAFO::devs.check.code.specs()                    # docs + CRAN check, bumps on a delta only
SEAFO::devs.check.code.specs(sbIsProdRel = TRUE)  # cut a production release
```

Conventions used throughout the source:

- `here::here()` for path building and `pkg::fun()` for every external call.
- Local aliases (`rasBase*`, `rasLeaflet*`, `rasSeafo*`) keep call sites greppable and shield them
  from search-path masking.
- `_`-suffixed identifiers for local variables and constants.
- 2-space indentation, UTF-8, `roxygen2` with `markdown = TRUE`.

---

## 🗺️ Roadmap

- 🐟 Fishing-footprint (VMS) overlay — `sbShowFishFootPrint` is already plumbed through and reserved.
- 🧭 Layer-control ordering and a naming pass over the `sbHideWCL` switch.
- 🧪 `testthat` suite under `tests/testthat/` (the fingerprint already tracks that tree).
- 📦 Possible CRAN submission.

---

## 📝 Citation

```bibtex
@Manual{SEAFO2026,
  title  = {SEAFO: SEAFO Scientific Committee R Package},
  author = {MALETZKY, Erich},
  year   = {2026},
  note   = {R package version 0.0.3},
  url    = {https://github.com/ErichM182/SEAFO}
}
```

---

## 📄 License & contact

Released under the **MIT License** — see [LICENSE](LICENSE). © 2026 SEAFO authors.

- **Maintainer:** Erich MALETZKY — [`esmaletzky@gmail.com`](mailto:esmaletzky@gmail.com)
- **Issues & bug reports:** <https://github.com/ErichM182/SEAFO/issues>
- **Organisation:** South East Atlantic Fisheries Organisation (SEAFO) — Scientific Committee

<div align="center">

<sub>Built for the SEAFO Scientific Committee with ❤️, roxygen2 and a healthy respect for `R CMD check`.</sub>

</div>
