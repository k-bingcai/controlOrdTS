#!/bin/bash
# CREATES SLURM FILES AND SUBMITS JOB 

# Searches 'simulation_longleaf_paths.config'
source configs/simulation_longleaf_paths.config 

# Script directory 
curr_dir="$(dirname "$(readlink -f "$0")")"

# Extracts all available time stamps studies
pushd ${SIM_DIRECTORY} >> /dev/null 2>&1
simfiles=$(ls -l | grep ^d | awk '{print $9}')
popd >> /dev/null 2>&1

# Pulls up all available time stamps studies and prompts for response
echo "The following names were found in the simulation folder; select one:"
PS3="Use the numbers to select a file or 'stop' to cancel: "
select filename in ${simfiles}
do 
    # leave the loop if the user says 'stop'
    if [[ "$REPLY" == stop ]]; then break; fi

    # complain if no file was selected, and loop to ask again
    if [[ "$filename" == "" ]]
    then
        echo "'$REPLY' is not a valid number"
        continue
    fi

    echo "${filename} selected."
    break 
done 

# Activate conda environment
source ~/.bashrc || exit 1
conda activate miivmed || exit 1


# Define JSON file to use
mod_file=${curr_dir}"/configs/models.json"
gen_file=${curr_dir}"/configs/generation.json"

# Output location
out_loc_str=${SIM_DIRECTORY}"/"${filename} 

# Clean out slurm folder 
pushd ${out_loc_str}"/slurm/step-2" >> /dev/null 2>&1
mkdir -p archive
find . -maxdepth 1 -type f -print0 | xargs -0 mv -t archive
popd >> /dev/null 2>&1

# Create SLURM file 
Rscript R/step-2-fit-models-SLURM.R --models_json_file ${mod_file} \
    --gen_json_file ${gen_file} \
    --sim_dir ${out_loc_str} \
    --code_dir ${curr_dir}"/R" || exit 1

# Submit jobs 
pushd ${out_loc_str}"/slurm/step-2" >> /dev/null 2>&1
for FILE in SLURM_fit_models_*.slurm
do
    sbatch ${FILE}
done
popd >> /dev/null 2>&1


# Deactivate conda environment
conda deactivate || exit 1
