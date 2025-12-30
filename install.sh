#!/bin/bash
set -e

OMARCHY_BIN="$HOME/.local/share/omarchy/bin"
OMARCHY_CONFIG="$HOME/.local/share/omarchy/config"
OMARCHY_THEMES="$HOME/.local/share/omarchy/themes"

echo "Installing omarchy-emacs-themes..."
echo ""

# Check if Omarchy is installed
if [ ! -d "$HOME/.local/share/omarchy" ]; then
  echo "Error: Omarchy not found. Please install Omarchy first."
  echo "Visit: https://github.com/basecamp/omarchy"
  exit 1
fi

# Copy script
echo "→ Installing omarchy-theme-set-emacs script..."
mkdir -p "$OMARCHY_BIN"
cp bin/omarchy-theme-set-emacs "$OMARCHY_BIN/"
chmod +x "$OMARCHY_BIN/omarchy-theme-set-emacs"

# Copy Elisp library
echo "→ Installing omarchy-theme.el library..."
mkdir -p "$OMARCHY_CONFIG/emacs"
cp config/emacs/omarchy-theme.el "$OMARCHY_CONFIG/emacs/"

# Copy theme metadata
echo "→ Installing theme metadata for all themes..."
for theme_dir in themes/*/; do
  theme=$(basename "$theme_dir")
  mkdir -p "$OMARCHY_THEMES/$theme"
  if [ -f "$theme_dir/emacs.json" ]; then
    cp "$theme_dir/emacs.json" "$OMARCHY_THEMES/$theme/"
    echo "  ✓ $theme"
  fi
done

echo ""
echo "✓ Installation complete!"
echo ""
echo "═══════════════════════════════════════════════════════"
echo "  Omarchy Emacs Theme Integration"
echo "═══════════════════════════════════════════════════════"
echo ""
echo "To enable automatic theme updates, choose one option:"
echo ""
echo "Option 1: Hook-based (recommended)"
echo "  Create ~/.config/omarchy/hooks/theme-set with:"
echo ""
echo "    #!/bin/bash"
echo "    omarchy-theme-set-emacs \"\$1\""
echo ""
echo "  Then make it executable:"
echo "    chmod +x ~/.config/omarchy/hooks/theme-set"
echo ""
echo "Option 2: File watcher"
echo "  Add to your Emacs init file (~/.emacs.d/init.el):"
echo ""
echo "    (add-to-list 'load-path \"$OMARCHY_CONFIG/emacs\")"
echo "    (require 'omarchy-theme)"
echo "    (omarchy-theme-follow)"
echo ""
echo "Option 3: Manual"
echo "  Run 'omarchy-theme-set-emacs' after changing themes"
echo ""
echo "═══════════════════════════════════════════════════════"
echo ""
echo "Notes:"
echo "  - Make sure required Emacs theme packages are installed"
echo "  - Use 'omarchy-toggle-skip-emacs-theme-changes' to opt-out"
echo "  - See README.md for more information"
echo ""
