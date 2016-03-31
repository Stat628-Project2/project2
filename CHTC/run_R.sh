#!/bin/bash

# untar your R installation
tar -xzvf R.tar.gz

# make sure the script will use your R installation
export PATH=$(pwd)/R/bin:$PATH

# run R, with the R script name as an argument to this bash script
R CMD BATCH $@