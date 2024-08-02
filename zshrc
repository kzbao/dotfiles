# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

# Oh-my-zsh
export ZSH="$HOME/.oh-my-zsh"
plugins=(docker docker-compose emacs encode64 git macos node nodenv npm postgres pyenv python rbenv ruby)
source $ZSH/oh-my-zsh.sh

# Prompt
fpath+=("$(brew --prefix)/share/zsh/site-functions")
autoload -U promptinit && promptinit
prompt pure

# Options
bindkey -e
setopt AUTO_CD AUTO_PARAM_SLASH
HYPHEN_INSENSITIVE=true

# History
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=10000
setopt INC_APPEND_HISTORY SHARE_HISTORY HIST_IGNORE_ALL_DUPS

# Autocompletion
autoload -U compinit && compinit
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format 'No matches for: %d'

# Aliases
alias brewall="brew update && brew upgrade && brew cleanup"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias ke="emacsclient -e '(kill-emacs)'"
alias magit="/Applications/Emacs.app/Contents/MacOS/emacs-nw --eval='(progn (magit-status) (delete-other-windows))'"

# Image compression for web
function ic() {
  local max_dimension=2500
  local quality=90

  while getopts ":m:q:" opt; do
    case ${opt} in
      m )
        max_dimension=$OPTARG
        ;;
      q )
        quality=$OPTARG
        ;;
      \? )
        echo "Invalid option: -$OPTARG" 1>&2
        return 1
        ;;
      : )
        echo "Option -$OPTARG requires an argument" 1>&2
        return 1
        ;;
    esac
  done
  shift $((OPTIND -1))

  if [ $# -lt 1 ]; then
    echo "Usage: ic [-m max_dimension] [-q quality] <image_file>"
    return 1
  fi

  local input_file=$1
  local output_file="${input_file%.*}.avif"

  magick "$input_file" -resize "$max_dimension"x"$max_dimension"\> -quality "$quality" -strip "$output_file"

  echo "Converted $input_file to $output_file"
}
