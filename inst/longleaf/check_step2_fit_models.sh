#!/bin/bash

# Searches 'simulation_longleaf_paths.config'
source configs/simulation_longleaf_paths.config 

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

# Output location
out_loc_str=${SIM_DIRECTORY}"/"${filename} 

# Enter into folder 
pushd ${out_loc_str}/"models" >> /dev/null 2>&1

modfolders=$(ls -l | grep ^d | awk '{print $9}')
for m in ${modfolders}
do 
    # Echo the model name
    echo "Checking for ${m}"
    pushd ${m} >> /dev/null 2>&1

    # Get model numbers
    modnumbers=$(ls -l | grep ^d | awk '{print $9}')
    for m_num in ${modnumbers}
    do 
        m_num_fitted=$(find ${m_num}"/fitted" -name "_fitted.RDS" | wc -l)
        echo "${m_num} -> ${m_num_fitted} files"
    done 
    popd >> /dev/null 2>&1
done 
popd >> /dev/null 2>&1