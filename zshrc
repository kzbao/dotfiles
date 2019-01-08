# Aliases
alias brewall='brew update && brew upgrade && brew cleanup && brew prune'
alias ke="emacsclient -e '(kill-emacs)'"
alias magit="/usr/local/bin/emacs -nw --eval='(progn (magit-status) (delete-other-windows))'"

# Environment Variables
export PATH=/usr/local/bin:$HOME/bin:$PATH
export MANPATH="/usr/local/man:$MANPATH"
export SSH_KEY_PATH="~/.ssh/rsa_id"
export EDITOR="emacs -nw"

# Oh-my-zsh
export ZSH=~/.oh-my-zsh
ZSH_THEME="fishy"
plugins=(emacs git osx pyenv ssh-agent zsh-completions)
source $ZSH/oh-my-zsh.sh

# Options
bindkey -e
setopt AUTO_CD
setopt AUTO_PARAM_SLASH
HYPHEN_INSENSITIVE="true"

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS

# Autocompletion
autoload -U compinit && compinit
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format 'No matches for: %d'
