#!/bin/bash

## Check mbsync email 3 times per minute based on a cronjob firing this script once
killall mbsync &>/dev/null
mbsync -a -q
sleep 20
killall mbsync &>/dev/null
mbsync -a -q
sleep 20
killall mbsync &>/dev/null
mbsync -a -q`

