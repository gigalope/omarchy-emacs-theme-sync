# TODO

Development roadmap and future improvements for omarchy-emacs-themes.

## Theme Package Development

### High Priority
- [ ] **osaka-jade-emacs-theme** - Create native Emacs theme package (currently using doom-pine)
- [ ] **everforest-emacs** - Better native support (currently using doom-nova)
- [ ] **ristretto-emacs** - Monokai Pro Ristretto variant (currently using doom-monokai-ristretto)

### Medium Priority
- [ ] **kanagawa-emacs** - Native support (currently using doom-henna)
- [ ] **rose-pine-emacs** - Check if rose-pine-theme exists (currently using doom-earl-grey)
- [ ] **ethereal-emacs** - Native support (currently using deeper-blue built-in)

### Research Needed
- [ ] Investigate if any other themes have better native equivalents than current doom-themes mappings

## Features

### User Experience
- [ ] Helper script: `omarchy-theme-add-emacs` for custom themes
- [ ] Auto-detect if theme package is installed before requiring
- [ ] Better error messages when theme packages are missing
- [ ] Check for `emacsclient` availability during install

### Advanced Features
- [ ] Support for `light.mode` awareness (auto-switch light/dark variants)
- [ ] Support for multiple Emacs instances/daemons
- [ ] Integration test suite
- [ ] Uninstall script

## Documentation

- [ ] Add screenshots to README showing theme switching
- [ ] Video/GIF demo of automatic theme switching
- [ ] Contributing guidelines (CONTRIBUTING.md)
- [ ] Troubleshooting flowchart
- [ ] Blog post / announcement

## Upstream Tracking

### MELPA Submissions
- [ ] Submit osaka-jade-emacs-theme to MELPA (after creation)
- [ ] Submit everforest-emacs-theme to MELPA (after creation)

### Omarchy Integration
- [ ] Consider submitting as official Omarchy plugin (when plugin system exists)
- [ ] Track Omarchy theme additions for coverage

## Completed ✅

- [x] Initial repository setup
- [x] All 14 default Omarchy themes supported
- [x] Hook-based integration
- [x] File watcher integration
- [x] Startup theming support
- [x] Custom theme documentation
- [x] Doom Emacs support notes
- [x] flexoki-themes native package support
