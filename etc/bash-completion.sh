#!/bin/bash

# read with: source <(etc/bash-completion.sh)

PROG=${PROG:-progcon}

$PROG --bash-completion-script `which $PROG`
