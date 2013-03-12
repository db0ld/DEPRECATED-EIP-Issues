#!/bin/bash

#some ifs and stuff won't work, and I DON'T FUVHING CARE
# copy paste if you want, you can also improve that shi^W script

# that's for debian based distros

if [ `lsb_release -cs` = 'squeeze' ]
then
	echo "You need to update your Debian to install Ocsigen"
	if [ $UID -ne 0 ]
	then
		echo "root privileges are needed"
		exit
	else
		echo "Are you ok to use an instable version of debian?"
		echo "0) No way"
		echo "1) Yup (with a reboot too)"
		read debianupdate
		if [ $debianupdate -ne 1 ]
		then
			echo "Ok, exiting :'("
			exit
		else
			echo "Yeah! Updating!"
		fi
	fi

	# update to debian testing if needed
	echo "deb http://ftp.fr.debian.org/debian/ wheezy main" >> /etc/apt/sources.list
	echo "deb-src http://ftp.fr.debian.org/debian/ wheezy main" >> /etc/apt/sources.list
	echo "deb http://security.debian.org/ wheezy/updates main" >> /etc/apt/sources.list
	echo "deb-src http://security.debian.org/ wheezy/updates main" >> /etc/apt/sources.list
	echo "deb http://ftp.fr.debian.org/debian/ wheezy-updates main" >> /etc/apt/sources.list
	echo "deb-src http://ftp.fr.debian.org/debian/ wheezy-updates main" >> /etc/apt/sources.list

	# yep, debian testing, let's order a coffee now
	apt-get update && apt-get dist-upgrade -y

	reboot
fi

# compilation dependencies
if [ $UID -eq 0 ]
then
	echo "Installing dependencies"
	apt-get install -y  ocaml ocaml-compiler-libs \
	                	camlp4 camlp4-extra \
	                	ocaml-findlib libfindlib-ocaml-dev \
	                	libcalendar-ocaml-dev libcryptokit-ocaml-dev \
	                	libocamlnet-ocaml-dev libpcre-ocaml-dev \
	                	libreact-ocaml-dev libsqlite3-ocaml-dev \
	                	libssl-ocaml-dev libtext-ocaml-dev \
	                	libev-dev libgdbm-dev libyojson-ocaml \
	                	libextlib-ocaml-dev libcsv-ocaml-dev \
	                	libglib2.0-dev libpgocaml-ocaml-dev libyojson-ocaml-dev \
	                	git make unzip postgresql ocamldsort darcs
	echo "Rerun as user to install ocsigen and glife"
	exit
fi



# get ocsigen bundle
wget http://ocsigen.org/download/ocsigen-bundle-2.2.2.tar.gz

tar -xf ocsigen-bundle-2.2.2.tar.gz
cd ocsigen-bundle-2.2.2
./configure --enable-oclosure --enable-macaque --enable-lwt-all
make pull
make

if [ `lsb_release -si` = 'Ubuntu' ]
then
	sudo make install
else
	su -c 'make install'
fi

cd
mkdir tmp
mkdir glife
cd glife
git clone https://github.com/LaVieEstUnJeu/Website.git
git clone https://github.com/LaVieEstUnJeu/API.git
cd Website
./install.sh
make
ocsigenserver -c lavieestunjeu.conf &
cd ../API/ws
./install.sh
make
ocsigenserver -c api.conf &