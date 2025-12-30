# Agent Guide for omarchy-emacs-themes

This document contains everything an AI agent needs to effectively work in this codebase.

## Project Overview

**omarchy-emacs-themes** is an integration layer that synchronizes Emacs themes with [Omarchy](https://github.com/basecamp/omarchy), a Hyprland-based desktop environment. When users switch themes in Omarchy, their running Emacs sessions automatically update to match.

**Language**: Bash scripts + Emacs Lisp
**License**: MIT
**Key Feature**: Live theme synchronization without restarting Emacs

## Architecture

### Three Main Components

1. **Theme Metadata Files** (`themes/*/emacs.json`)
   - JSON files mapping Omarchy themes to Emacs themes
   - One file per Omarchy theme (14 themes total)
   - Contains: theme name, package name, optional variant settings

2. **Shell Script** (`bin/omarchy-theme-set-emacs`)
   - Bash script that reads theme metadata and updates Emacs
   - Uses `emacsclient --eval` to send Elisp commands to running Emacs
   - Includes graceful error handling and opt-out support

3. **Elisp Library** (`config/emacs/omarchy-theme.el`)
   - Emacs Lisp library for file-watching and theme application
   - Provides `omarchy-theme-apply` and `omarchy-theme-follow` functions
   - Uses `file-notify` to watch for theme changes

## Directory Structure

```
omarchy-emacs-themes/
├── bin/
│   └── omarchy-theme-set-emacs      # Main shell script
├── config/
│   └── emacs/
│       └── omarchy-theme.el         # Elisp library
├── themes/
│   ├── catppuccin/emacs.json        # Theme metadata (14 themes)
│   ├── tokyo-night/emacs.json
│   └── ...
├── install.sh                       # Installation script
├── README.md                        # User-facing documentation
└── LICENSE                          # MIT license
```

### Installation Paths (after `./install.sh`)

- Script: `~/.local/share/omarchy/bin/omarchy-theme-set-emacs`
- Elisp: `~/.local/share/omarchy/config/emacs/omarchy-theme.el`
- Theme metadata: `~/.local/share/omarchy/themes/*/emacs.json`
- Current theme symlink: `~/.config/omarchy/current/theme/emacs.json`

## Essential Commands

### Testing Installation

```bash
# Install locally
./install.sh

# Test the script (requires running Emacs and Omarchy)
~/.local/share/omarchy/bin/omarchy-theme-set-emacs

# Verify files were copied
ls -la ~/.local/share/omarchy/bin/omarchy-theme-set-emacs
ls -la ~/.local/share/omarchy/config/emacs/omarchy-theme.el
ls -la ~/.local/share/omarchy/themes/*/emacs.json
```

### Git Operations

```bash
# Check status
git status

# View recent changes
git log --oneline -10

# See what changed in a file
git log -p -- <file>
```

### No Build System

This project has no build, test, or lint commands. It's pure Bash + Elisp scripts.

## Code Conventions

### Bash Scripts

**Style**:
- Use `#!/bin/bash` shebang
- Set strict error handling: `set -euo pipefail`
- Use double brackets for conditionals: `[[ ... ]]`
- Quote all variables: `"$VAR"`
- Use lowercase for local variables, UPPERCASE for constants

**Error Handling**:
- Fail early with `set -e`
- Use `|| true` for commands that should never break the workflow
- Exit silently (exit 0) when preconditions aren't met (missing deps, opt-out flags)
- Redirect errors to `/dev/null 2>&1` when intentionally ignoring them

**Heredoc Pattern**:
```bash
# Use || true to handle EOF exit codes
read -r -d '' ELISP <<EOF || true
(elisp code here)
EOF
```

**Escaping Elisp Symbols**:
```bash
# DON'T escape Elisp symbols in heredocs
theme='$theme        # Single quotes for symbol - correct
package=$package_elisp   # Variable substitution without quotes
```

### Emacs Lisp

**Style**:
- Use `;;; file.el --- Description -*- lexical-binding: t; -*-` header
- Group related functionality with comments
- Provide interactive commands with `(interactive)`
- End with `(provide 'module-name)`

**Naming Conventions**:
- Public functions: `omarchy-theme-apply`
- Private functions: `omarchy-theme--read-metadata` (double dash)
- Custom variables: `omarchy-theme-metadata-file`
- Custom group: `omarchy-theme`

**Error Handling**:
- Use `condition-case` for error recovery
- Use `nil 'noerror` for optional requires
- Message the user on errors: `(message "[omarchy] ...")`
- Fail gracefully - don't break Emacs

**Key Patterns**:
- Disable all themes before loading: `(mapc #'disable-theme custom-enabled-themes)`
- Load themes non-interactively: `(load-theme theme t)` (t = no confirmation)
- Use file-notify for watching: `(file-notify-add-watch ...)`
- Debounce rapid changes with timers: `(run-with-timer delay nil #'function)`

### JSON Theme Metadata

**Required Fields**:
```json
{
  "theme": "doom-tokyo-night",
  "package": "doom-themes"
}
```

**Optional Fields**:
```json
{
  "theme": "catppuccin",
  "package": "catppuccin-theme",
  "variant": "mocha",
  "variantVariable": "catppuccin-flavor",
  "loadPath": "~/.emacs.d/themes",
  "note": "Human-readable comment (not used by code)"
}
```

**Field Semantics**:
- `theme`: The Emacs theme name (symbol) to load
- `package`: The Emacs package name (symbol) to require
- `variant`: Value to set before loading theme (e.g., "mocha", "latte")
- `variantVariable`: Variable name to set variant on (e.g., `catppuccin-flavor`)
- `loadPath`: Additional directory to add to `custom-theme-load-path`
- `note`: Documentation only, not parsed by code

## Theme Mappings

All 14 Omarchy themes are supported:

| Omarchy Theme | Emacs Theme | Package | Notes |
|---------------|-------------|---------|-------|
| catppuccin | catppuccin (mocha) | catppuccin-theme | Variant: mocha |
| catppuccin-latte | catppuccin (latte) | catppuccin-theme | Variant: latte |
| everforest | doom-nova | doom-themes | |
| flexoki-light | doom-solarized-light | doom-themes | |
| gruvbox | doom-gruvbox | doom-themes | |
| kanagawa | doom-henna | doom-themes | |
| nord | doom-nord | doom-themes | |
| rose-pine | doom-earl-grey | doom-themes | |
| tokyo-night | doom-tokyo-night | doom-themes | |
| ethereal | deeper-blue | (built-in) | No direct equivalent |
| hackerman | doom-one | doom-themes | |
| matte-black | doom-ir-black | doom-themes | |
| osaka-jade | doom-pine | doom-themes | |
| ristretto | doom-monokai-ristretto | doom-themes | |

**Package Dependencies**:
- Most themes use [doom-themes](https://github.com/doomemacs/themes)
- Catppuccin variants require [catppuccin-theme](https://github.com/catppuccin/emacs)
- One theme (ethereal) uses built-in `deeper-blue`

## Important Gotchas

### 1. Heredoc EOF Handling

**Problem**: `read -r -d ''` returns exit code 1 on EOF, which breaks `set -e`

**Solution**: Always append `|| true`
```bash
read -r -d '' ELISP <<EOF || true
(elisp code)
EOF
```

**Recent Fix**: Commit c4302a6 added `|| true` to prevent script failure

### 2. Escaping Elisp Symbols in Bash

**Problem**: Elisp symbols must be properly quoted when embedded in bash heredocs

**Correct Pattern** (from commit 58da94b):
```bash
# Prepare elisp values
package_elisp="nil"
[[ -n "$package" ]] && package_elisp="'$package"  # Note single quote inside string

# Then use in heredoc WITHOUT additional quotes
read -r -d '' ELISP <<EOF || true
(let ((package $package_elisp))  ; Variable contains the quote
  ...)
EOF
```

**Wrong Pattern**:
```bash
package_elisp="$package"  # Missing quote
# Or:
(let ((package '$package_elisp))  # Double-quoting the symbol
```

### 3. Silent Failure Design

The script intentionally fails silently (exits 0) when:
- `emacs.json` doesn't exist
- Skip flag is present: `~/.local/state/omarchy/toggles/skip-emacs-theme-changes`
- `jq` command not found
- `emacsclient` command not found
- Emacs is not running

**Rationale**: Theme switching should never break the overall Omarchy workflow. If Emacs isn't available, just skip it.

### 4. Emacs Symbol Normalization

**Problem**: JSON parsing can return strings or symbols depending on the parser

**Solution**: Always normalize to symbols in Elisp:
```elisp
(defun omarchy-theme--normalize-symbol (value)
  "Convert VALUE to a symbol when VALUE is a string; otherwise return VALUE."
  (cond
   ((symbolp value) value)
   ((stringp value) (intern value))
   (t value)))
```

Use this for all theme/package/variant names read from JSON.

### 5. File Watching Edge Cases

The Elisp watcher monitors the parent directory, not the symlink itself, because:
- Symlinks can be replaced atomically (common pattern in Omarchy)
- Watching the symlink target doesn't catch updates
- Watching the directory catches all changes including symlink updates

### 6. Theme Variants

Some themes (like Catppuccin) have variants (mocha, latte, frappe, macchiato).

**Variant Application Order**:
1. Set variant variable BEFORE loading theme
2. Load theme
3. Don't set variant after - theme may not pick it up

```elisp
(when (and variant-var variant)
  (set variant-var variant))  ; Must happen first
(load-theme theme t)          ; Then load
```

## Testing Considerations

### Manual Testing Workflow

This project has no automated tests. Testing requires a real environment:

1. **Install Omarchy** first (dependency)
2. **Run the installer**: `./install.sh`
3. **Start Emacs** with server: `emacs --daemon` or set `(server-start)` in init
4. **Install Emacs packages**:
   ```elisp
   (use-package catppuccin-theme :ensure t)
   (use-package doom-themes :ensure t)
   ```
5. **Switch themes**: `omarchy-theme-set <theme-name>`
6. **Verify Emacs updates** automatically

### What to Test When Making Changes

**When modifying `bin/omarchy-theme-set-emacs`**:
- Test with theme that has variants (catppuccin)
- Test with theme that has no package (ethereal/deeper-blue)
- Test with theme that uses doom-themes
- Test when Emacs is not running (should exit cleanly)
- Test with skip flag set (should exit cleanly)
- Verify heredoc escaping doesn't break Elisp evaluation

**When modifying `config/emacs/omarchy-theme.el`**:
- Test `(omarchy-theme-apply)` interactively
- Test `(omarchy-theme-follow)` file watcher
- Test rapid theme switches (debouncing)
- Test with missing package (should message error, not crash)
- Test with malformed JSON (should handle gracefully)

**When adding/modifying theme metadata**:
- Verify JSON is valid: `jq . themes/*/emacs.json`
- Test the theme actually exists in the package
- Test variants are spelled correctly (case-sensitive)
- Document any approximations in `note` field

## Dependencies

### Runtime Requirements

**User's System**:
- Omarchy installed at `~/.local/share/omarchy/`
- `jq` command-line JSON processor
- `emacsclient` (part of Emacs)
- Emacs running with server enabled

**Emacs Packages** (user must install):
- `catppuccin-theme` - for Catppuccin variants
- `doom-themes` - for most other themes
- No packages needed for built-in themes (deeper-blue)

### Development Requirements

None. This is pure shell + Elisp with no build tools.

## File Watching vs Hook-Based Approaches

The project supports two integration methods:

### Hook-Based (Recommended)

**Mechanism**: Omarchy calls `omarchy-theme-set-emacs` script via hook
**Setup**: Create `~/.config/omarchy/hooks/theme-set`
**Pros**: Immediate updates, no polling, works without Emacs integration
**Cons**: Requires hook configuration

### File Watcher (Alternative)

**Mechanism**: Emacs watches `~/.config/omarchy/current/theme/emacs.json`
**Setup**: Call `(omarchy-theme-follow)` in Emacs init
**Pros**: Self-contained in Emacs, no hook needed
**Cons**: Requires Emacs restart to set up, adds file-notify dependency

Both methods work independently or together (redundant but harmless).

## Common Modifications

### Adding a New Theme

1. Create `themes/<theme-name>/emacs.json`:
   ```json
   {
     "theme": "emacs-theme-symbol",
     "package": "emacs-package-name"
   }
   ```

2. Test locally:
   ```bash
   # Copy to Omarchy directory
   mkdir -p ~/.local/share/omarchy/themes/<theme-name>
   cp themes/<theme-name>/emacs.json ~/.local/share/omarchy/themes/<theme-name>/
   
   # Switch to it
   omarchy-theme-set <theme-name>
   ```

3. Update README.md theme table

### Improving a Theme Mapping

If you find a better Emacs theme match for an Omarchy theme:

1. Edit `themes/<theme-name>/emacs.json`
2. Test the new theme works
3. Update README.md theme table
4. Document reasoning in commit message

### Adding Custom Load Paths

For themes not in standard Emacs packages:

```json
{
  "theme": "my-custom-theme",
  "package": "my-theme-pkg",
  "loadPath": "~/.emacs.d/themes"
}
```

The script will add this to `custom-theme-load-path` before loading.

## Project History

- **Initial commit** (1351368): Basic infrastructure
- **c4302a6**: Fixed heredoc EOF exit code issue
- **58da94b**: Fixed Elisp symbol escaping in heredoc
- **3f4debb**: Simplified to use doom-themes for most themes (latest)

Key insight from history: Most complexity came from properly escaping Elisp code in Bash heredocs. The current pattern is stable.

## Related Documentation

- **README.md**: User-facing documentation and installation instructions
- **LICENSE**: MIT license
- **Omarchy project**: https://github.com/basecamp/omarchy
- **doom-themes**: https://github.com/doomemacs/themes
- **catppuccin-theme**: https://github.com/catppuccin/emacs

## Philosophy and Design Decisions

1. **Fail gracefully**: Never break the user's workflow. If Emacs isn't available, just skip it silently.

2. **Minimal dependencies**: Only require what's absolutely needed (jq, emacsclient). No heavy frameworks.

3. **Respect user control**: Provide opt-out flag, don't force theme changes.

4. **Work with running Emacs**: Use `emacsclient --eval` to update live sessions, no restart needed.

5. **Separate from Omarchy core**: This is an extension, not part of Omarchy itself (per maintainer DHH's preference).

6. **Best-effort theme mapping**: Some Omarchy themes don't have exact Emacs equivalents. Use closest match from doom-themes when possible.

7. **Keep it simple**: Pure shell + Elisp, no build system, no test framework. Easy to read and modify.

## When to Update AGENTS.md

Update this file when:
- Adding new theme mappings or features
- Discovering new gotchas or edge cases
- Changing file structure or architecture
- Adding new dependencies or requirements
- Learning better patterns for Bash/Elisp integration
- Users report confusing behavior that should be documented
