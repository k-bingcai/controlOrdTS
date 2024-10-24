#!/bin/bash

# Get flags 
LOCAL=0
OPTSTRING=":l"
while getopts ${OPTSTRING} opt; do
  case ${opt} in
    l)
      # Trial value
      LOCAL=1
      ;;
    ?)
      echo "Invalid option: -${OPTARG}."
      exit 1
      ;;
  esac
done

# Script directory 
curr_dir="$(dirname "$(readlink -f "$0")")"

# Timestamp 
timestamp=$(date +%Y%m%d_%H%M%S)

# Activate conda environment
if [ "${LOCAL}" -eq "1" ]
then 
  source /home/bing/anaconda3/etc/profile.d/conda.sh
else 
  source ~/.bashrc 
fi

conda activate controlOrdTS || exit 1

# Define JSON file to use
mod_file=${curr_dir}"/configs/models.json"
gen_file=${curr_dir}"/configs/generation.json"


# Toggle between local testing and actual cluster usage
if [ "${LOCAL}" -eq "1" ]
then 
   
    echo "Error: script not configured for local testing"
    exit 1

else

    # Set output location 
    out_loc_str="/proj/gateslab/users/bingcai/projects/controlOrdTS-main/simulations/${timestamp}"
    mkdir -p ${out_loc_str}

    # Create SLURM file 
    Rscript R/step-1-create-data-SLURM.R --models_json_file ${mod_file} \
	      --gen_json_file ${gen_file} \
        --sim_dir ${out_loc_str} \
        --code_dir ${curr_dir}"/R" || exit 1

    # Submit jobs 
    pushd ${out_loc_str}"/slurm/step-1" >> /dev/null 2>&1
    for FILE in SLURM_create_data_*.slurm
    do
	    sbatch ${FILE}
    done
    popd >> /dev/null 2>&1

fi 

# Deactivate conda environment
conda deactivate || exit 1
