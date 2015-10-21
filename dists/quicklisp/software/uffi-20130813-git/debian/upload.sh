#!/bin/bash -e

dup uffi -Ufiles.b9.com -D/home/ftp/uffi -su \
    -C"(umask 022; cd /srv/www/html/uffi; make install; find . -type d |xargs chmod go+rx; find . -type f | xargs chmod go+r)" $*
