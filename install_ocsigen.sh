#!/bin/bash
## ########################################################################## ##
## Project: La Vie Est Un Jeu - Installation of Ocsigen projects              ##
## Description: Will install a working environment for Ocsigen developers     ##
## Author: db0 (db0company@gmail.com, http://db0.fr/)                         ##
## Latest Version, copyright: https://github.com/LaVieEstUnJeu/Internal-Tools ##
## ########################################################################## ##

directory='eip'

function	checkandremove() {
    repo=$1
    cd $repo
    if [[ $repo = 'bootstrap' ]]
    then rm -rf $repo
	return;
    fi
    if ! git diff-index --quiet HEAD --
    then
	git status
	echo -n "The repository $i has changes. Are you sure you want to erase everything (y/N)? " && \
	    read r && \
            if [[ $r != 'y' ]]
	    then exit;
	fi;
    fi;
    cd ..
    rm -rf $repo
}

function	clean() {
    directory=$1
    if [ ! -x $directory ]
    then mkdir $directory;
	return;
    fi
    cd $directory && \
    for i in `ls`;
    do checkandremove $i
    done;
    cd ..
}

function	install() {
    directory=$1
    clean $directory && \
    cd $directory && \
    git clone git@github.com:LaVieEstUnJeu/Website.git && \
    git clone git@github.com:LaVieEstUnJeu/API-OCaml.git && \
    git clone git@github.com:LaVieEstUnJeu/Internal-Tools.git && \
    git clone git@github.com:db0company/OcsiTools.git && \
    git clone git@github.com:db0company/Pathname.git && \
    wget "http://twitter.github.com/bootstrap/assets/bootstrap.zip" && \
    unzip "bootstrap.zip" && \
    rm "bootstrap.zip" && \

    for i in `ls`;
    do
	cd $i
	if [ -e "install.sh" ]
	then ./install.sh -link
	fi
	cd ..
    done
    ls -l
}

if [[ $1 = "-clean" ]]
then clean $directory
else install $directory
fi
