#!/usr/bin/env bash
cd "${0%/*}/.."

REPOSITORY=firebase-model
git clone https://github.com/thSoft/$REPOSITORY.git
$REPOSITORY/scripts/setup.sh
$REPOSITORY/scripts/build.sh
rm -rf $REPOSITORY

sbt eclipse
