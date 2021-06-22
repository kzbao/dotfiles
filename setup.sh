#!zsh

mkdir -p ~/.emacs.d/lisp
ln -s "$PWD/emacs.d/init.el" ~/.emacs.d/init.el
for file in emacs.d/lisp/*; do
    ln -s "$PWD/$file" ~/.emacs.d/lisp/
done

ln -s "$PWD/gitconfig" ~/.gitconfig
ln -s "$PWD/gitignore" ~/.gitignore

ln -s "$PWD/zprofile" ~/.zprofile
ln -s "$PWD/zshrc" ~/.zshrc
