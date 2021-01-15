<div align="center">

# My Doom Emacs Config

</div>

---

### Table of Contents

# Dependencies

## On macOS
* [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
``` sh
brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
```
* vterm
``` sh
brew install libvterm
```

# Install

``` sh
git clone https://github.com/2770862886/doom.d.git ~/.doom.d

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```
