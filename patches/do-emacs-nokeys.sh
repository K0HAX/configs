#!/bin/bash
if [ $0 = './do-emacs-nokeys.sh' ]; then
parentdir=`dirname $PWD`
patch -d$parentdir ./emacs/config.org ./patches/emacs-nokeys-config.diff
else
    echo "This script must be run from the patches directory"
fi

