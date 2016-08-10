#!/bin/sh

eval $(keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)

