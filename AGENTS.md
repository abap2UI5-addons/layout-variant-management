# AGENTS.md — AI Assistant Guide for abap2UI5 layout-management

> This file follows the cross-tool AGENTS.md convention and is the single
> agent instruction file of this repository — Claude Code reads `AGENTS.md`
> natively, there is no separate `CLAUDE.md`.

## Project Overview

Customizable table and form layouts with persistent variant management for
[abap2UI5](https://github.com/abap2UI5/abap2UI5) (`z2ui5_cl_layo_*`). Layout
variants are persisted to this addon's own database tables (`z2ui5_t_11`,
`z2ui5_t_12`).

**Language:** English — all code, comments, commit messages, PRs, issues and
documentation must be in English.

## Package Structure

| Package | Content |
|---|---|
| `src/01/`, `src/02/`, `src/03/` | Layout classes and samples (`z2ui5_cl_layo_*`) |

## Utility Access

The addon uses the abap2UI5 core utility class `z2ui5_cl_util` directly
(`z2ui5_cl_util=>…`). abap2UI5 is a hard dependency of this addon (installed
alongside via abapGit and declared in the abaplint configs), so no vendored
utility copy is kept.

## Dependencies

Installed alongside via abapGit; declared in the abaplint configs:

* [abap2UI5](https://github.com/abap2UI5/abap2UI5)

## Coding Style

Follows the abap2UI5 core conventions (see its
[AGENTS.md](https://github.com/abap2UI5/abap2UI5/blob/main/AGENTS.md)): Clean
ABAP with Hungarian prefixes, backtick string literals, `xsdbool()`. After a
functional method call, do not read `sy-subrc` (it is undefined) — check the
returned value instead.

## Validation

Run `npx abaplint` before considering changes complete (config `abaplint.jsonc`,
0 issues expected). CI:

* `ABAP_STANDARD` / `ABAP_CLOUD` — lint against Standard ABAP and ABAP Cloud
* `ABAP_702` — lint the downported `702` branch; `npm run downport` /
  `auto_downport` produce it (`abaplint --fix` against `.github/abaplint/abap_702.jsonc`)
* `renaming` (`rename_test.yaml`) — namespace-rename check
* `build_rename` — manual workflow that pushes a namespace-renamed branch
  `rename_<name>` for a parallel install

All `.abap`/`.xml`/config files are LF-only (`.gitattributes` enforces it).
