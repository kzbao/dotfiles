#!zsh

echo "=== Install Homebrew ==="
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

echo "=== Install Git ==="
/opt/homebrew/bin/brew install git

echo "=== SSH things ==="
ssh-keygen -t ed25519 -C "kzb@kevinbao.com"
cat <<EOF > ~/.ssh/config
Host github.com
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_ed25519
EOF
eval "$(ssh-agent -s)"
echo "Please copy ~/.ssh/id_ed25519.pub to Github. Press any key to continue..."
read -k 1 -s

echo "=== Install oh-my-zsh ==="
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
rm ~/.zshrc

echo "=== Retrieve dotfiles ==="
mkdir -p ~/Development
cd ~/Development
/opt/homebrew/bin/git clone git@github.com:kzbao/dotfiles.git
cd dotfiles
mkdir -p ~/.emacs.d/lisp
for file in emacs.d/*; do
    ln -s "$PWD/$file" ~/.emacs.d/
for file in emacs.d/lisp/*; do
    ln -s "$PWD/$file" ~/.emacs.d/lisp/
done
ln -s "$PWD/gitconfig" ~/.gitconfig
ln -s "$PWD/gitignore" ~/.gitignore
ln -s "$PWD/zprofile" ~/.zprofile
ln -s "$PWD/zshrc" ~/.zshrc

echo "=== Install remaining Homebrew packages ==="
/opt/homebrew/bin/brew bundle
mkdir -p ~/.docker
cat <<EOF > ~/.docker/config.json
{
  "cliPluginsExtraDirs": ["/opt/homebrew/lib/docker/cli-plugins"]
}
EOF
