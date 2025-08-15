# Modern Emacs Configuration

A clean, performant Emacs configuration optimised for multi-language development with Go, C/C++, Rust, TypeScript, and shell scripting. Built with modern completion frameworks, unified LSP integration, and efficient project management.

## Features

- 🚀 **Fast startup** (~1-2 seconds) with lazy loading
- 🔧 **Modern completion** with Vertico, Consult, and Corfu
- 🌐 **Unified LSP** across all languages
- 📁 **Excellent project management** with Projectile and Magit
- 🎨 **Beautiful UI** with Doom themes and modeline
- 🔍 **Powerful search** with ripgrep integration
- 🏗️ **Modular structure** for easy maintenance
- 🖥️ **Cross-platform** (macOS and Linux)

## Directory Structure

```
~/.emacs.d/
├── early-init.el             # Early initialization (Emacs 27+)
├── init.el                   # Main configuration entry point
├── custom.el                 # Emacs custom variables
├── config/
│   ├── core.el               # Core Emacs settings
│   ├── ui.el                 # UI and appearance
│   ├── completion.el         # Modern completion framework
│   ├── project-management.el # Projectile, Magit, etc.
│   ├── programming.el        # General programming settings
│   ├── keybindings.el        # Custom keybindings
│   ├── platform.el           # Platform-specific settings
│   └── languages/
│       ├── lang-c.el         # C/C++ configuration
│       ├── lang-go.el        # Go configuration
│       ├── lang-rust.el      # Rust configuration
│       ├── lang-typescript.el # TypeScript configuration
│       ├── lang-shell.el     # Shell scripting
│       └── lang-org.el       # Org mode
├── snippets/                 # Custom yasnippet templates
├── backups/                  # Auto-generated backup files
└── auto-saves/               # Auto-generated autosave files
```

## Installation

### Prerequisites

**Emacs Version:** 27.1 or higher (29+ recommended for tree-sitter support)

### Quick Setup

```bash
# Backup existing configuration
mv ~/.emacs.d ~/.emacs.d.backup$(date +%Y%m%d)

# Clone or create the new configuration
git clone https://github.com/yourusername/emacs-config.git
cd emacs-config

# Run installation script
chmod +x scripts/install.sh
./scripts/install.sh

# First startup will install all MELPA/ELPA packages automatically
# This may take 5-10 minutes on first run
emacs
```

### Manual Installation

If you prefer to install manually:

```bash
# Create directory structure
mkdir -p ~/.emacs.d/{config/languages,snippets,backups,auto-saves}

# Copy configuration files
cp configs/* ~/.emacs.d/
cp -r configs/config ~/.emacs.d/

# Start Emacs - packages will install automatically
emacs
```

## Package Management

This configuration uses a bootstrap system that automatically installs all required packages from MELPA/GNU ELPA on first startup.

### Installed Packages

**Core Completion Framework:**
- `vertico` - Vertical completion UI
- `orderless` - Flexible completion styles  
- `marginalia` - Completion annotations
- `consult` - Enhanced search and navigation
- `embark` - Actions on completion candidates
- `corfu` - In-buffer completion popup
- `cape` - Completion at point extensions
- `which-key` - Keybinding help

**Project Management:**
- `projectile` - Project navigation and management
- `magit` - Git interface
- `git-gutter` - Show git changes in fringe
- `treemacs` - File tree sidebar
- `perspective` - Workspace management

**Programming Tools:**
- `lsp-mode` - Language Server Protocol client
- `lsp-ui` - UI improvements for LSP
- `flycheck` - Syntax checking
- `company` - Text completion (fallback)
- `yasnippet` - Snippet system
- `smartparens` - Balanced parentheses
- `multiple-cursors` - Edit multiple locations

**Language-Specific Packages:**

*Go Development:*
- `go-mode` - Go language support
- `go-eldoc` - Documentation in echo area
- `gotest` - Test integration

*Rust Development:*
- `rustic` - Enhanced Rust mode
- `cargo` - Cargo integration
- `toml-mode` - TOML file support

*C/C++ Development:*
- `modern-cpp-font-lock` - Enhanced C++ highlighting
- `cmake-mode` - CMake file support
- `clang-format` - Code formatting

*TypeScript/JavaScript:*
- `typescript-mode` - TypeScript support
- `tide` - TypeScript IDE features
- `web-mode` - JSX/TSX support
- `prettier-js` - Code formatting

**UI and Themes:**
- `doom-themes` - Modern theme collection
- `doom-modeline` - Enhanced modeline
- `all-the-icons` - Icon fonts
- `dashboard` - Startup screen

### Package Management Commands

```elisp
;; Check package status
M-x my/check-package-status

;; Install missing packages
M-x my/install-missing-packages

;; Update all packages  
M-x my/update-all-packages

;; List installed packages
M-x list-packages
```

### Adding New Packages

1. Add the package name to `my/required-packages` in `package-bootstrap.el`
2. Restart Emacs or run `M-x my/install-missing-packages`
3. Add configuration using `use-package` in the appropriate config file

## Dependencies

### Required System Tools

#### Language Servers
```bash
# Go language server
go install golang.org/x/tools/gopls@latest

# Rust language server
rustup component add rust-analyzer

# C/C++ language server (choose one)
# Ubuntu/Debian:
sudo apt install clangd-12
# macOS:
brew install llvm
# Or install via your distribution's package manager

# TypeScript language server
npm install -g typescript-language-server typescript

# Optional: Additional TypeScript tools
npm install -g eslint prettier
```

#### Search and Development Tools
```bash
# Ripgrep (essential for fast searching)
# Ubuntu/Debian:
sudo apt install ripgrep
# macOS:
brew install ripgrep

# Git (required for Magit)
# Usually pre-installed, but ensure it's available

# Shell linting
# Ubuntu/Debian:
sudo apt install shellcheck
# macOS:
brew install shellcheck

# Optional: Additional development tools
# fd (faster find)
brew install fd  # macOS
sudo apt install fd-find  # Ubuntu

# delta (better git diffs)
brew install git-delta  # macOS
sudo apt install git-delta  # Ubuntu
```

#### macOS Specific
```bash
# Terminal notifications
brew install terminal-notifier

# Ensure command line tools are installed
xcode-select --install
```

### Language-Specific Tools

#### Go Development
```bash
# Essential Go tools
go install golang.org/x/tools/cmd/goimports@latest
go install github.com/go-delve/delve/cmd/dlv@latest
go install honnef.co/go/tools/cmd/staticcheck@latest
go install github.com/fatih/gomodifytags@latest
go install github.com/josharian/impl@latest
```

#### Rust Development
```bash
# Rust toolchain (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Essential Rust tools
cargo install cargo-edit       # cargo add, cargo rm
cargo install cargo-watch      # cargo watch
cargo install cargo-expand     # macro expansion
cargo install cargo-tree       # dependency tree
cargo install cargo-outdated   # outdated dependencies
cargo install cargo-audit      # security audit
cargo install cargo-deny       # dependency checking
```

#### C/C++ Development
```bash
# CMake (for CMake projects)
# Ubuntu/Debian:
sudo apt install cmake
# macOS:
brew install cmake

# Clang tools (if using clang)
# Ubuntu/Debian:
sudo apt install clang-tools
# macOS: included with Xcode command line tools

# Optional: clang-format for code formatting
# Usually included with clang-tools
```

#### Tree-sitter Setup (Optional - Emacs 29+)

Tree-sitter provides better syntax highlighting and parsing, but requires language grammars to be installed.

**To enable tree-sitter:**

1. **Install language grammars:**
   ```elisp
   M-x my/install-treesit-grammars
   ```

2. **Enable tree-sitter modes:**
   ```elisp
   M-x my/enable-treesit-modes
   ```

3. **Restart Emacs** for full effect

**Tree-sitter Management:**
- `C-c T i` - Install tree-sitter grammars
- `C-c T e` - Enable tree-sitter modes
- `C-c T s` - Show tree-sitter status

**Note:** Tree-sitter is disabled by default to avoid grammar installation requirements. Traditional syntax highlighting works perfectly well for most use cases.

#### TypeScript/Node.js Development
```bash
# Node.js (via nvm recommended)
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
nvm install node
nvm use node

# Essential Node.js tools
npm install -g typescript
npm install -g eslint
npm install -g prettier
npm install -g @typescript-eslint/parser
npm install -g @typescript-eslint/eslint-plugin
```

## Key Bindings

### Global Navigation
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `M-g` | `goto-line` | Go to line number |
| `M-/` | `hippie-expand` | Smart text expansion |
| `C-x C-b` | `ibuffer` | Enhanced buffer list |
| `M-o` | `ace-window` | Quick window switching |
| `C-=` | `er/expand-region` | Expand selection |

### Project Management
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c p p` | `projectile-switch-project` | Switch between projects |
| `C-c p f` | `projectile-find-file` | Find file in project |
| `C-c p s g` | `projectile-grep` | Search in project |
| `C-c p r` | `projectile-replace` | Replace in project |
| `C-c p c` | `projectile-compile-project` | Compile project |

### Version Control (Magit)
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-x g` | `magit-status` | Git status |
| `C-x M-g` | `magit-dispatch` | Magit dispatch menu |
| `C-c g c` | `magit-clone` | Clone repository |
| `C-c g f` | `magit-fetch` | Fetch from remote |
| `C-c g p` | `magit-push` | Push to remote |
| `C-c g l` | `git-link` | Generate link to current line |
| `C-c g t` | `git-timemachine` | Browse file history |

### LSP (Language Server)
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c l r` | `lsp-rename` | Rename symbol |
| `C-c l f` | `lsp-format-buffer` | Format buffer |
| `C-c l a` | `lsp-execute-code-action` | Execute code action |
| `C-c l d` | `lsp-find-definition` | Go to definition |
| `C-c l R` | `lsp-find-references` | Find references |
| `C-c l i` | `lsp-find-implementation` | Find implementation |
| `C-c l t` | `lsp-find-type-definition` | Go to type definition |
| `C-c l h` | `lsp-describe-thing-at-point` | Show documentation |

### Search and Navigation
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-s` | `consult-line` | Search in current buffer |
| `M-s r` | `consult-ripgrep` | Search in project with ripgrep |
| `M-s g` | `consult-grep` | Grep search |
| `M-s l` | `consult-line` | Line-based search |
| `C-x b` | `consult-buffer` | Enhanced buffer switching |
| `M-y` | `consult-yank-pop` | Smart yank from kill ring |

### Completion and Editing
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `TAB` | `completion-at-point` | Complete at point |
| `M-TAB` | `company-complete` | Manual completion |
| `C-M-i` | `completion-at-point` | Alternative completion |
| `C->` | `mc/mark-next-like-this` | Multiple cursors next |
| `C-<` | `mc/mark-previous-like-this` | Multiple cursors previous |
| `C-S-c C-S-c` | `mc/edit-lines` | Multiple cursors on lines |

### Programming Modes

#### Go Mode
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c C-f` | `gofmt` | Format Go code |
| `C-c C-i` | `go-goto-imports` | Go to imports |
| `C-c C-d` | `godoc-at-point` | Show documentation |
| `C-c C-t` | `go-test-current-test` | Run current test |
| `C-c C-p` | `go-test-current-project` | Run all tests |
| `C-c r` | `my/go-run-main` | Run main function |
| `C-c b` | `my/go-build-project` | Build project |

#### Rust Mode
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c C-c t` | `rustic-cargo-test` | Run tests |
| `C-c C-c b` | `rustic-cargo-build` | Build project |
| `C-c C-c r` | `rustic-cargo-run` | Run project |
| `C-c C-c c` | `rustic-cargo-check` | Check project |
| `C-c C-c l` | `flycheck-list-errors` | List errors |
| `C-c C-c s` | `lsp-rust-analyzer-status` | Rust analyzer status |
| `C-c C-c e` | `lsp-rust-analyzer-expand-macro` | Expand macro |

#### C/C++ Mode
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c c` | `compile` | Compile project |
| `C-c C-c` | `recompile` | Recompile |
| `C-c i` | `clang-format-region` | Format region |
| `C-c u` | `clang-format-buffer` | Format buffer |
| `RET` | `newline-and-indent` | Smart newline |

#### TypeScript Mode
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c d` | `tide-documentation-at-point` | Show documentation |
| `C-c r` | `tide-rename-symbol` | Rename symbol |
| `C-c o` | `tide-organize-imports` | Organize imports |
| `C-c f` | `tide-format` | Format code |
| `C-c t` | `my/typescript-check-types` | Type check |
| `C-c b` | `my/typescript-build` | Build project |

### Custom Functions
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c e` | `my/edit-init-file` | Edit init.el |
| `C-c q` | `my/quit-emacs` | Quit with confirmation (server-aware) |
| `C-c t` | `eshell` | Open terminal |
| `C-c r` | `revert-buffer` | Reload file from disk |
| `C-c w` | `whitespace-mode` | Toggle whitespace visibility |
| `C-c n` | `display-line-numbers-mode` | Toggle line numbers |

### Server Mode Management
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c s s` | `my/start-server` | Start Emacs server |
| `C-c s k` | `my/kill-server` | Kill Emacs server |
| `C-c s r` | `my/restart-server` | Restart Emacs server |
| `C-x C-c` | `delete-frame` | Close frame (in server mode) |

### Debug Mode (DAP)
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `<f12>` | `dap-debug` | Start debugging |
| `<f8>` | `dap-continue` | Continue execution |
| `<f9>` | `dap-next` | Step over |
| `<f11>` | `dap-step-in` | Step into |
| `<f23>` | `dap-step-out` | Step out (Shift+F11) |
| `C-<f11>` | `dap-disconnect` | Disconnect debugger |

### Window Management
| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-x o` | `other-window` | Switch to other window |
| `C-x 0` | `delete-window` | Delete current window |
| `C-x 1` | `delete-other-windows` | Delete other windows |
| `C-x 2` | `split-window-below` | Split window horizontally |
| `C-x 3` | `split-window-right` | Split window vertically |
| `C-c <left>` | `winner-undo` | Undo window configuration |
| `C-c <right>` | `winner-redo` | Redo window configuration |

## Configuration

### Customising Paths

Edit these variables in the configuration files:

```elisp
;; In project-management.el
(setq projectile-project-search-path '("~/Projects" "~/Work"))

;; In ui.el - Font configuration
(set-face-attribute 'default nil :font "Your-Preferred-Font" :height 120)

;; In core.el - User information
(setq user-full-name "Your Name"
      user-mail-address "your.email@example.com")
```

### Adding New Languages

1. Create `config/languages/lang-<language>.el`
2. Follow the pattern from existing language files
3. Add `(require 'lang-<language>)` to `init.el`

### Platform-Specific Settings

Settings for macOS and Linux are automatically detected and applied via `config/platform.el`.

## Performance

### Startup Time
- **First startup:** ~10-15 seconds (package installation)
- **Subsequent startups:** ~1-2 seconds
- **Server mode:** Instant client connections after daemon is running
- **Memory usage:** ~50-80MB after startup

### Server Mode (Recommended)

This configuration automatically starts Emacs in server mode, which provides:

**Benefits:**
- **Instant file opening** from terminal: `emacsclient file.txt`
- **Shared sessions** - all clients share the same Emacs instance
- **Persistent state** - buffers, history, and customizations persist
- **Lower resource usage** - single Emacs process serves multiple clients

**Usage:**
```bash
# Start Emacs daemon (automatic on first launch)
emacs --daemon

# Open files instantly
emacsclient -c file.txt          # Open in new frame
emacsclient -t file.txt          # Open in terminal
emacsclient file.txt             # Open in existing frame

# Create desktop/shell aliases
alias e='emacsclient -c'         # GUI Emacs
alias et='emacsclient -t'        # Terminal Emacs
alias ec='emacsclient'           # Use existing frame

# Stop daemon when needed
emacsclient -e "(kill-emacs)"
```

**Server Mode Keybindings:**
- `C-x C-c` in server mode closes the current frame (not Emacs)
- `M-x kill-emacs` actually stops the server
- `M-x server-start` manually starts server if needed

### Optimisation Tips

1. **Use server mode** for instant startup (automatically configured)
2. **Increase GC threshold** during startup (already configured)
3. **Use native compilation** (Emacs 28+) for better performance
4. **Disable unused packages** by commenting them out
5. **Profile with `emacs --daemon`** for fastest experience

### Profiling

```elisp
;; Profile startup
M-x profiler-start
;; Restart Emacs
M-x profiler-report

;; Profile runtime
M-x profiler-start
;; Do some work
M-x profiler-stop
M-x profiler-report
```

## Troubleshooting

### Common Startup Issues

#### Missing Configuration Files
**Error:** `File is missing: Cannot open load file, No such file or directory, lang-c`

**Solution:**
```bash
# Run the fix script to create missing files
chmod +x scripts/fix-startup.sh
./scripts/fix-startup.sh

# Or create the missing directories manually
mkdir -p ~/.emacs.d/config/languages
```

#### Package Installation Issues
**Error:** `The package embark-consult should be installed if you use both Embark and Consult`

**Solution:**
```bash
# Check package status
emacs --batch --eval "(progn (load-file \"~/.emacs.d/package-bootstrap.el\") (my/check-package-status))"

# Or in Emacs:
M-x my/install-missing-packages
```

#### Projectile Keybinding Conflicts
**Error:** `Key sequence C-c p f starts with non-prefix key C-c p`

**Solution:** This is fixed in the updated configuration. The individual keybindings are removed in favour of the projectile keymap.

#### Package Initialize Warning
**Warning:** `Unnecessary call to 'package-initialize' in init file`

**Solution:** This is fixed by moving package initialization to `early-init.el`.

### Quick Diagnostic

Run the diagnostic script to check your configuration:

```bash
chmod +x scripts/diagnose.sh
./scripts/diagnose.sh
```

This will check:
- ✅ Required files exist
- ✅ Directory structure is correct  
- ✅ Basic configuration loads
- ✅ Package installation status

### Getting Help

- **Emacs built-in help:** `C-h` prefix
- **Describe key:** `C-h k` followed by key combination
- **Describe function:** `C-h f` followed by function name
- **LSP doctor:** `M-x lsp-doctor`
- **Package status:** `M-x list-packages`

## Contributing

This configuration is designed to be modular and easily customisable. To contribute:

1. Keep language-specific settings in their respective files
2. Use `use-package` for all package configurations
3. Add appropriate keybindings and documentation
4. Test across platforms (macOS and Linux)

## License

This configuration is provided as-is under the MIT license. Feel free to modify and distribute.

---

**Happy coding with Emacs!** 🚀
