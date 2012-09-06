#!/bin/bash
## ########################################################################## ##
## Project: La Vie Est Un Jeu - Internal Portal                               ##
## Description: Installation script                                           ##
## Author: db0 (db0company@gmail.com, http://db0.fr/)                         ##
## Latest Version is on GitHub: http://goo.gl/nq7mj                           ##
## ########################################################################## ##

port=80

function	edit_conf_file() {
    file=$1
    port=$2
    pwd=`pwd | sed 's#\/#\\\/#g'`

    if [ -e $file.bak ]
    then mv $file.bak $file
    fi

    cp $file $file.bak && \
	sed -i".tmp" 's/\$PORT/'$port'/' $file && \
	sed -i".tmp" 's/\$PWD/'$pwd'/' $file && \
	rm *.tmp && \
	return 0
    return 1
}

echo -n "Install Modules... " && \

    if [ -e "tools.eliom" ]
     then rm -f "tools.eliom" "tools.eliomi"
    fi && \
    wget "https://raw.github.com/db0company/OcsiTools/master/tools.eliom" && \
    wget "https://raw.github.com/db0company/OcsiTools/master/tools.eliomi" && \

    if [ -e "gCal.eliom" ]
     then rm -f "gCal.eliom" "gCal.eliomi"
    fi && \
    wget "https://raw.github.com/db0company/gCal/master/gCal.eliom" && \
    wget "https://raw.github.com/db0company/gCal/master/gCal.eliomi" && \

    if [ -e "bootstrap" ]
     then rm -rf "bootstrap"
    fi && \
    wget "http://twitter.github.com/bootstrap/assets/bootstrap.zip" && \
    unzip "bootstrap.zip" && \
    rm "bootstrap.zip" && \

    echo "Done." && \

    echo -n "Edit configuration file... " && \
    edit_conf_file 'lavieestunjeu-portal.conf' $port && \
    echo "Done."

