#!/bin/bash

# Query github PAT
echo "Enter github PAT: "
read -s pat

# Clone from github
git clone https://${pat}@github.com/k-bingcai/controlOrdTS.git

# Build within R
source ~/.bashrc
conda activate controlOrdTS
Rscript install_controlOrdTS.R
conda deactivate
