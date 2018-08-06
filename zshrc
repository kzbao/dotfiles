# Environment Variables
export PATH=/usr/local/bin:$HOME/bin:$PATH
export MANPATH="/usr/local/man:$MANPATH"
export SSH_KEY_PATH="~/.ssh/rsa_id"
export EDITOR="emacs -nw"

# Oh-my-zsh
export ZSH=~/.oh-my-zsh
ZSH_THEME="fishy"
plugins=(aws docker emacs git github osx postgres python rails ruby ssh-agent zsh-completions)
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

# Aliases
alias brewall='brew update && brew upgrade && brew cleanup && brew prune'
alias ke="emacsclient -e '(kill-emacs)'"
alias pg="pg_ctl -D /usr/local/var/postgres start"

# Don't mess with R
disable r

# Completion
autoload -Uz compinit && compinit
setopt NO_list_beep
#zstyle ':completion:*' verbose yes
#zstyle ':completion:*:descriptions' format '%B%d%b'
#zstyle ':completion:*:messages' format '%d'
#zstyle ':completion:*:warnings' format 'No matches for: %d'
#zstyle ':completion:*' group-name ''