#!/bin/bash

eval $(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)

export GOPATH=~
