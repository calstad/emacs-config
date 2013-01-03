#!/bin/sh

# Fetch git submodules
echo "Initializing vendored git submodules..."
git submodule init
echo "Updating vendored git submodules..."
git submodule update

# Setup sym link to the dotfiles if its not there

if [ "$EMACS_DIR" = "" ]
then
    EMACS_DIR="$HOME/.emacs.d"
fi

exit_emacs_conf_install () {
    echo "Bye"
    exit 0
}

link_emacs_conf_to_dot () {
    echo "Symlinking $(pwd) to $EMACS_DIR..."
    ln -s $(pwd) $EMACS_DIR
    exit_emacs_conf_install
}

delete_old_emacs_dir () {
    read -p "Are you sure you want to delete the .emacs.d folder (yes/no)? " choice
    if [[ $choice =~ ^(Y|y)es ]]
    then
        echo "Deleting old .emacs.d folder..."
        rm -rf $EMACS_DIR
        link_emacs_conf_to_dot
    else
        choose_emacs_dir_action
    fi
}

backup_old_emacs_dir () {
    emacs_backup_dir="$HOME/emacs-config-backup-$(date +%s)"
    if [ ! -d "$emacs_backup_dir" ]
    then
        echo "Backup up old .emacs.d folder to $emacs_backup_dir"
        mv $EMACS_DIR $emacs_backup_dir
        link_emacs_conf_to_dot
    else
        echo "Backup directory already exists!"
        exit 1
    fi
}

choose_emacs_dir_action () {
    if [ -d "$EMACS_DIR" ]
    then
        read -p "[D]elete, [B]ackup, [E]xit current .emacs.d folder?  " choice
        case "$choice" in
            D|d) delete_old_emacs_dir;;
            B|b) backup_old_emacs_dir;;
            E|e|*) exit_emacs_conf_install;;
        esac
    else
        link_emacs_conf_to_dot
    fi
}

choose_emacs_dir_action
