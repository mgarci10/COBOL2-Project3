#!/bin/bash
set -e -x
cobc -x GARCIA-P03-PAY-SCREEN.cob
./GARCIA-P03-PAY-SCREEN
