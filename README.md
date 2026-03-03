# JimMoen's Emacs Configuration

Personal Emacs configuration for daily development, focused on Erlang/Elixir, Rust, Go, Python and web development.

## Requirements

- Emacs 29+ (built with native-comp recommended)
- Git
- [librime](https://github.com/rime/librime) (for emacs-rime Chinese input)
- ripgrep (`rg`, for counsel-rg)

## Quick Start

```bash
git clone <repo-url> ~/.config/emacs
```

First launch will bootstrap [Elpaca](https://github.com/progfolio/elpaca) and install all packages automatically. Initial startup takes a few minutes for package installation and Rime deployment.

### Daemon Mode (Recommended)

```bash
# Enable systemd user service
systemctl --user enable --now emacs

# Connect
emacsclient -c
```

The systemd service override at `~/.config/systemd/user/emacs.service.d/override.conf` sets `GTK_IM_MODULE=emacs` to prevent Fcitx5 from intercepting Emacs keybindings.

## Directory Structure

```
~/.config/emacs/
├── early-init.el            Coding system, GC, startup timer
├── init.el                  Load path, custom-file, require modules
├── elpaca-lock.el           Package version lock file
├── etc/
│   ├── init-core.el         Elpaca bootstrap, use-package integration
│   ├── init-base.el         Keybindings, ivy/counsel, dired, which-key
│   ├── init-ui.el           Dashboard, doom-modeline, doom-themes
│   ├── init-editing.el      Smartparens, rime, sis, treesit, avy
│   ├── init-utils.el        Youdao dictionary, ssh-config, pdf-tools
│   ├── init-dev-tools.el    Magit, LSP, company, flycheck, copilot
│   ├── init-lib.el          Custom utility functions
│   ├── editor-layouts/      persp-mode workspace management
│   └── init-dev-lang/       Per-language configurations
│       ├── lang-erlang.el
│       ├── lang-elixir.el
│       ├── lang-rust.el
│       ├── lang-go.el
│       ├── lang-python.el
│       ├── lang-vue.el
│       ├── lang-yaml.el
│       ├── lang-c3.el
│       ├── lang-hocon.el
│       └── lang-qml.el
├── var/                     [gitignored] Package data (no-littering)
└── elpaca/                  [gitignored] Installed packages
```

## Key Features

### Package Management

[Elpaca](https://github.com/progfolio/elpaca) with `use-package` integration. Packages are installed asynchronously from MELPA and GitHub.

- `F12` — Open elpaca-manager
- `M-x my/elpaca-write-lock-file` — Write package version lock file
- `M-x elpaca-pull-all` — Update all packages

### Input Method

[emacs-rime](https://github.com/DogLooksGood/emacs-rime) for native Rime input inside Emacs, with [sis](https://github.com/laishulu/emacs-smart-input-source) for smart source switching.

- `C-\` — Toggle Rime input method
- `C-`` — Force enable Rime (override predicates)
- Rime config shared with Fcitx5-Rime via symlinks (`~/.config/rime/`)

### Workspace

[persp-mode](https://github.com/Bad-ptr/persp-mode.el) for project-scoped perspectives.

- `C-x w s` — Switch perspective
- `C-x w P` — Switch to project perspective (ivy)
- `C-x w d` — Go to default (Main) perspective
- `C-c d` / `F9` — Open dashboard

### Navigation & Completion

- **ivy/counsel/swiper** — Minibuffer completion framework
- **company** — In-buffer completion with copilot integration
- **avy** — Jump to visible text (`M-g M-g`, `M-g M-c`, etc.)
- **projectile** — Project management (`C-x p`)

### Development

- **LSP** — Language server protocol via lsp-mode (`C-c l` prefix)
- **Magit** — Git interface (`C-x g`)
- **Flycheck** — On-the-fly syntax checking
- **Treemacs** — File tree sidebar (`C-x t`)
- **Apheleia** — Auto-formatting on save

### Custom Keybindings

| Key     | Action                          |
|---------|---------------------------------|
| `C-h`   | Backward delete char (not help) |
| `C-w`   | Backward kill word              |
| `C-x h` | Help command                    |
| `C-x H` | Mark whole buffer               |
| `M-w`   | Kill ring save                  |
| `M-W`   | Kill region                     |

## Rime Setup

Rime configuration is shared between emacs-rime and Fcitx5-Rime:

```
~/.config/rime/                Shared YAML configs (edit here)
~/.local/share/fcitx5/rime/    Fcitx5 user data (symlinks to shared configs)
~/.config/emacs/var/rime/      emacs-rime user data (symlinks to shared configs)
```

Vocabulary sync between the two frontends via Rime's built-in sync mechanism (`M-x rime-sync`).

## License

GPL-3.0
