#!/bin/sh

read -p "Enter repository directory name: " reponame

function config {
	/usr/bin/git --git-dir=$HOME/$reponame/ --work-tree=$HOME $@
}

#Clones the remote repository into a local bare repository
git clone --bare https://gitlab.com/vallenite/dotfiles.git $HOME/$reponame/

#Comment out if you want config status to show untracked files
config config status.showUntrackedFiles no

#mkdir -p $HOME/.config/config-bak

#echo "Backing up config files..."
#config checkout 2>&1 | grep "\s+\." | awk {'print $1'} | xargs -I{} mv -v {} ~/$HOME/.config/config-bak/{}

if config checkout -f
then
	echo "Checked out config."
else
	echo "Checkout failed."
	exit
fi

echo "Distributing config files..."
mv -vf $HOME/.config/etc/* /etc/
mv -vf $HOME/.config/home/* $HOME/
echo "Installation complete!

exit 0
