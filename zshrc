# Environment Variables
export PATH=/usr/local/bin:$HOME/bin:$PATH

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

# Oh-my-zsh
export ZSH=~/.oh-my-zsh
plugins=(ag docker docker-compose emacs encode64 git macos node nodenv npm postgres pyenv python rbenv ruby)
source $ZSH/oh-my-zsh.sh
autoload -U promptinit && promptinit
prompt pure

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

# Aliases
alias brewall="brew update && brew upgrade && brew cleanup"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias ke="emacsclient -e '(kill-emacs)'"
alias magit="/usr/local/bin/emacs -nw --eval='(progn (magit-status) (delete-other-windows))'"
alias rmds="find . -name '.DS_Store' -type f -delete"
