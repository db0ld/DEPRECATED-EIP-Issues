
LaVieEstUnJeu Portal
====================

Simple portal to have all our organization tools together on one web page at http://life.paysdu42.fr/

## Install, compile, launch

    ./install.sh
    make
    ocsigenserver -c lavieestunjeu-portal.conf

## F.A.Q.

    ocsigenserver: main: Fatal - You are not allowed to use port 80.

You must be root to launch the server on port 80. If you are not root and you want to launch it to test, you can edit the configuration file to use another available port (> 1024).

    ocsigenserver: main: Fatal - Error in configuration file: can't open log file /tmp/access.log: Permission denied

This message means that an ocsigenserver as already been launch by another user and you can't overwrite its log files. Launch the server as root to overwrite them anyway or edit the configuration file to change /tmp/ pathes.
