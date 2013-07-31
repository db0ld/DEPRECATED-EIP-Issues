#!/bin/bash
## ########################################################################## ##
## Project: La Vie Est Un Jeu - Internal Portal                               ##
## Description: Installation script                                           ##
## Author: db0 (db0company@gmail.com, http://db0.fr/)                         ##
## Latest Version is on GitHub: http://goo.gl/nq7mj                           ##
## ########################################################################## ##

port=80
conf='lavieestunjeu-portal.conf'

function	edit_conf_file() {
    file=$1
    port=$2
    pwd=`pwd | sed 's#\/#\\\/#g'`

    cp $file.template $file && \

    sed -i".tmp" 's/\$PORT/'$port'/' $file && \
    sed -i".tmp" 's/\$PWD/'$pwd'/' $file && \
    echo -n "Your GitHub login? " && \
    read githublogin && \
    sed -i".tmp" 's/\$GITHUBLOGIN/'$githublogin'/' $file
    echo -n "Your GitHub password? " && \
    read githubpassword && \
    sed -i".tmp" 's/\$GITHUBPASSWORD/'$githubpassword'/' $file
    rm *.tmp && \
    return 0
    return 1
}

function        delete_files() {
    echo "Cleaning..."
    rm -vf $conf
    rm -vf "tools.eliom" "tools.eliomi"
    rm -vf "gCal.eliom" "gCal.eliomi"
    rm -vf "github.ml" "github.mli"
    rm -rf "bootstrap"
    echo "Done."
}

function        install_modules() {
    delete_files
    wget "https://raw.github.com/db0company/OcsiTools/master/tools.eliom" && \
    wget "https://raw.github.com/db0company/OcsiTools/master/tools.eliomi" && \

    wget "https://raw.github.com/db0company/GCal/master/gCal.eliom" && \
    wget "https://raw.github.com/db0company/GCal/master/gCal.eliomi" && \

    wget "https://raw.github.com/db0company/GitHub-API-OCaml/master/github.ml" && \
    wget "https://raw.github.com/db0company/GitHub-API-OCaml/master/github.mli" && \

    wget "http://twitter.github.com/bootstrap/assets/bootstrap.zip" && \
    unzip "bootstrap.zip" && \
    rm "bootstrap.zip" && \

    echo "Done." && \
    return 0
    return 1
}

function        install_modules_links() {
    delete_files
    defaultpath=../..
    echo -n "Depositories path ("$defaultpath")? " && \
    read path && \
    if [ -z $path ]
    then path=$defaultpath
    fi && \
    # get the absolute path if the path is relative
    cd $path && path=`pwd` && cd - > /dev/null && \
    ln -s $path"/OcsiTools/tools.eliom" && \
    ln -s $path"/OcsiTools/tools.eliomi" && \
    ln -s $path"/GCal/gCal.eliom" && \
    ln -s $path"/GCal/gCal.eliomi" && \
    ln -s $path"/GitHub-API-OCaml/github.ml" && \
    ln -s $path"/GitHub-API-OCaml/github.mli" && \
    ln -s $path"/bootstrap" && \
    return 0
    return 1
}

if [ $1 = "-clean" ]
then
    delete_files
    exit 0
fi

echo "Install Modules... " && \

    if [ $1 = "-link" ]
    then install_modules_links
    else install_modules
    fi && \
 
    echo "Done." && \

    echo -n "Edit configuration file... " && \
    edit_conf_file $conf $port && \
    echo "Done."

