#!/bin/sh
#
# ROSE installation fixups.
# These are necessary as currently, ROSE includes are not installed into
# the correct directory.
#

cd include
ln -s . computation 
ln -s . driver 
ln -s . slicing 

