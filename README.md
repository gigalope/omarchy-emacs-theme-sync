# Omarchy Emacs Theme Integration

Automatic theme synchronization for Emacs when using [Omarchy](https://github.com/basecamp/omarchy).

When you switch themes in Omarchy, your Emacs session will automatically update to match.

## Features

- ✨ Automatic theme switching via hooks or file watching
- 🎨 Support for all 15 Omarchy themes
- 🔄 Live updates to running Emacs sessions (no restart needed)
- 🎯 Theme variant support (e.g., Catppuccin flavors, Gruvbox variants)
- 🚫 Easy opt-out via skip flag
- 💪 Robust error handling and graceful degradation

## Prerequisites

- [Omarchy](https://github.com/basecamp/omarchy) installed and configured
- Emacs (with `emacsclient` available)
- `jq` command-line tool
- Required Emacs theme packages (see [Theme Coverage](#theme-coverage))

## Installation

```bash
git clone https://github.com/yourusername/omarchy-emacs-themes
cd omarchy-emacs-themes
./install.sh
```

The installer will:
- Copy `omarchy-theme-set-emacs` script to `~/.local/share/omarchy/bin/`
- Copy `omarchy-theme.el` library to `~/.local/share/omarchy/config/emacs/`
- Copy all theme metadata files to `~/.local/share/omarchy/themes/*/emacs.json`

## Usage

### Option 1: Hook-Based (Recommended)

Automatically update Emacs when Omarchy themes change:

```bash
# Create the hook file
cat > ~/.config/omarchy/hooks/theme-set << 'EOF'
#!/bin/bash
omarchy-theme-set-emacs "$1"
EOF

# Make it executable
chmod +x ~/.config/omarchy/hooks/theme-set
```

Now whenever you run `omarchy-theme-set <theme>`, Emacs will update automatically.

**Tip**: To make Emacs start with the current Omarchy theme (instead of your default theme), add this to your init file:
```elisp
(add-to-list 'load-path "~/.local/share/omarchy/config/emacs")
(require 'omarchy-theme)
(omarchy-theme-apply)  ; Apply current Omarchy theme on startup
```
*For Doom Emacs users: Add to `~/.doom.d/config.el` instead of `init.el`*

### Option 2: File Watcher

Have Emacs watch for theme changes and update automatically:

Add to your `~/.emacs.d/init.el` (or `~/.doom.d/config.el` for Doom Emacs):

```elisp
(add-to-list 'load-path "~/.local/share/omarchy/config/emacs")
(require 'omarchy-theme)
(omarchy-theme-apply)   ; Apply current theme on startup
(omarchy-theme-follow)  ; Watch for theme changes
```

**Note**: This method doesn't require the hook from Option 1.

### Option 3: Manual

Run the script manually after changing themes:

```bash
omarchy-theme-set tokyo-night
omarchy-theme-set-emacs
```

## Theme Coverage

All 14 Omarchy themes are supported:

| Omarchy Theme | Emacs Theme | Package |
|---------------|-------------|---------|
| catppuccin | catppuccin (mocha) | catppuccin-theme |
| catppuccin-latte | catppuccin (latte) | catppuccin-theme |
| everforest | doom-nova | doom-themes |
| flexoki-light | doom-solarized-light | doom-themes |
| gruvbox | doom-gruvbox | doom-themes |
| kanagawa | doom-henna | doom-themes |
| nord | doom-nord | doom-themes |
| rose-pine | doom-earl-grey | doom-themes |
| tokyo-night | doom-tokyo-night | doom-themes |
| ethereal | deeper-blue | (built-in) |
| hackerman | doom-one | doom-themes |
| matte-black | doom-ir-black | doom-themes |
| osaka-jade | doom-pine | doom-themes |
| ristretto | doom-monokai-ristretto | doom-themes |

**Note**: Most themes use [doom-themes](https://github.com/doomemacs/themes) which provides 50+ well-crafted themes. Only catppuccin variants require a separate package.

### Installing Theme Packages

**Simple setup** - Only 2 packages needed:

Using `use-package`:

```elisp
(use-package catppuccin-theme :ensure t)
(use-package doom-themes :ensure t)
```

Or install manually:
```elisp
M-x package-install RET catppuccin-theme RET
M-x package-install RET doom-themes RET
```

## How It Works

### Architecture

1. **Theme Metadata**: Each Omarchy theme has an `emacs.json` file with theme information:
   ```json
   {
     "theme": "tokyonight",
     "package": "tokyonight-theme",
     "variant": "night",
     "variantVariable": "tokyonight-variant"
   }
   ```

2. **Script Execution**: `omarchy-theme-set-emacs` reads the metadata and sends Elisp code to Emacs via `emacsclient --eval`

3. **Live Updates**: Emacs disables the old theme and loads the new one without restarting

### File Locations

- Script: `~/.local/share/omarchy/bin/omarchy-theme-set-emacs`
- Elisp Library: `~/.local/share/omarchy/config/emacs/omarchy-theme.el`
- Theme Metadata: `~/.local/share/omarchy/themes/*/emacs.json`
- Current Theme: `~/.config/omarchy/current/theme/emacs.json` (symlink)

## Configuration

### Opting Out

To disable automatic Emacs theme changes:

```bash
mkdir -p ~/.local/state/omarchy/toggles
touch ~/.local/state/omarchy/toggles/skip-emacs-theme-changes
```

To re-enable:

```bash
rm ~/.local/state/omarchy/toggles/skip-emacs-theme-changes
```

### Custom Omarchy Themes

If you've installed custom Omarchy themes (via `omarchy-theme-install`), you can add Emacs support by creating an `emacs.json` file:

```bash
# Example: Adding Emacs support for synthwave84
cat > ~/.config/omarchy/themes/synthwave84/emacs.json << 'EOF'
{
  "theme": "doom-outrun-electric",
  "package": "doom-themes",
  "note": "Synthwave84 theme not available for Emacs. Using doom-outrun-electric as alternative."
}
EOF
```

**JSON fields:**
- `theme` (required): Emacs theme name (e.g., `doom-tokyo-night`)
- `package` (optional): Package to load (e.g., `doom-themes`)
- `variant` (optional): Theme variant value (e.g., `mocha`)
- `variantVariable` (optional): Variable to set variant (e.g., `catppuccin-flavor`)
- `loadPath` (optional): Custom theme directory (e.g., `~/.emacs.d/themes`)
- `note` (optional): Description for documentation

After creating the file, run `omarchy-theme-set <your-theme>` and Emacs will pick it up automatically!

## Troubleshooting

### Theme doesn't change

1. **Check if Emacs is running**: `pgrep -x emacs`
2. **Verify emacsclient works**: `emacsclient --eval "(+ 1 1)"` should return `2`
3. **Check for skip flag**: `ls ~/.local/state/omarchy/toggles/skip-emacs-theme-changes`
4. **Verify theme package is installed**: `M-x list-packages`

### Error messages

- **"jq: command not found"**: Install `jq` via your package manager
- **"emacsclient: command not found"**: Make sure Emacs is installed and in your PATH
- **"Package 'X' is unavailable"**: Install the theme package in Emacs

### Theme package missing

If a theme fails to load, install the required package:

```elisp
M-x package-install RET <package-name> RET
```

Then try switching themes again.

## Contributing

Contributions are welcome! Here's how you can help:

### Adding Support for New Themes

1. Create `themes/<theme-name>/emacs.json`:
   ```json
   {
     "theme": "emacs-theme-name",
     "package": "emacs-package-name"
   }
   ```

2. Test it: `omarchy-theme-set <theme-name>`

3. Submit a pull request

### Improving Theme Mappings

Some themes use generic alternatives (e.g., Doom themes). If you know of better Emacs theme equivalents, please open an issue or PR!

## Why This Exists

From Omarchy maintainer DHH:
> "I don't want to carry theme support for all the optional editors. I could see this existing as a separate package of some sort, though."

This project provides Omarchy theme integration for Emacs as a separate extension, respecting the main project's scope while giving Emacs users a great theming experience.

## License

MIT License - See LICENSE file for details

## Credits

- [Omarchy](https://github.com/basecamp/omarchy) - The beautiful theme system this integrates with
- All the amazing Emacs theme authors

## Related Projects

- [Omarchy](https://github.com/basecamp/omarchy) - Hyprland-based desktop environment
- [doom-themes](https://github.com/doomemacs/themes) - Extensive Emacs theme collection
