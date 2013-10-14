#!/bin/bash

cabal install \
 && scp -C .cabal-sandbox/bin/rnfssp functor:/srv/sites/rnfssp/rnfssp-new \
 && ssh -t functor 'bash -c "sudo systemctl stop rnfssp \
   && mv /srv/sites/rnfssp/{rnfssp,rnfssp-old} \
   && mv /srv/sites/rnfssp/{rnfssp-new,rnfssp} \
   && sudo systemctl start rnfssp"'
