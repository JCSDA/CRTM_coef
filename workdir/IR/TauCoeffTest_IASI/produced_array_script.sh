#!/bin/bash
# Job step for: ${ID_TAG}
#SBATCH --partition=serial
#SBATCH --export=ALL
#SBATCH --ntasks=1
#SBATCH --nodes=1  # should be equal to ntasks
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4gb  
#SBATCH --time=8:00:00
#SBATCH --output=output/Array_test.%A_%a.out
#SBATCH --error=output/Array_test.%A_%a.err
#SBATCH --job-name=lbl_%A_%a
# #SBATCH --array=1-${N_JOBS}


# --------------------------------------------
# LBLRTM transmittance production run script
# file for the input TAPE5 file:
#   ${TAPE5_FILE}
# and using the TAPE3 spectroscopic file:
#   ${TAPE3_FILE}" >> ${SCRIPT_FILE}
# --------------------------------------------

# ------------------
# The root directory
# ------------------

ROOT_DIR=/scratch/users/pstegmann/TauCoeffTest_IASI

LBLRUN=/data/users/pstegmann/AER/aer_lblrtm_v12.8_lnfl_v3.1/lblrtm/lblrtm_v12.8_linux_gnu_dbl 
LBLRTM_to_netCDF=/data/users/pstegmann/projects/Moradi_stuff/crtm/src/TauProd/Infrared/LBLRTM_to_netCDF/LBLRTM_to_netCDF
TAPE3_DIR=/data/users/pstegmann/projects/TauCoeffTest/lnfl

# -- Create LBLRTM/HITRAN version string
LBLRTM_HITRAN_VERSION="LBLRTM v9.4; HITRAN 2000 + AER updates"
PROFILE_SET_ID=5

# -- Change to the directory containing the
# -- current TAPE5 file


# ----------------------------
#  Change to required directory
# ----------------------------

cd ${ROOT_DIR}
echo ${PWD}
 
# ----------------------------------
# Read in the calculation parameters
# ----------------------------------

ID_TAG=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_idtag_list.txt)
TAPE3_FILE=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_tape3_list.txt)
RESULTS_DIR=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_resultsdir.txt)

# -- Create derived environment variables
ID_ATTRIBUTE="("`echo ${ID_TAG} | tr '_' ':'`")"
ERROR_LOG_FILE="Error_Log.${ID_TAG}"
TAPE5_FILE="tape5.${ID_TAG}.rdk"

# -- Create output file names
UPWELLING_TAPE_FILE="TAPE20"
UPWELLING_TAU_LBLRTM_FILE="upwelling_tau.${ID_TAG}"
UPWELLING_TAU_NETCDF_FILE="${UPWELLING_TAU_LBLRTM_FILE}.nc"



# -- Add time stamp to the error log file


# --------------------------------------
# Add begin time stamp to error log file
# --------------------------------------

echo >> ${RESULTS_DIR}/${ERROR_LOG_FILE}; echo '--------------------------' >> ${RESULTS_DIR}/${ERROR_LOG_FILE}
echo \"Processing run started at:  \`date\`\" >> ${RESULTS_DIR}/${ERROR_LOG_FILE}

# -- Start the LBLRTM run


# ----------------
# Start LBLRTM run
# ----------------

# I changed the following lines to be able to run LBLRTM - Isaac Moradi - May 18, 2016
cd ${RESULTS_DIR}      
\rm -f TAPE5 TAPE3 
ln -s ${ROOT_DIR}/FSCDXS FSCDXS
ln -s ${ROOT_DIR}/xs xs  
ln -s ${TAPE3_DIR}/${TAPE3_FILE} TAPE3  
ln -s ${TAPE5_FILE} TAPE5  
# ${LBLRUN} -d -n -r ${RESULTS_DIR} -s ${TAPE3_FILE} ${TAPE5_FILE}" >> ${SCRIPT_FILE}
${LBLRUN}
# end of changes

# -- Check exit status of lblrun script

# ------------------------
# Check lblrun exit status" >> ${SCRIPT_FILE}
# ------------------------" >> ${SCRIPT_FILE}

if [ $? -ne 0 ]; then
#  echo '${SCRIPT_FILE}:lblrun fail' >> ${RESULTS_DIR}/${ERROR_LOG_FILE}"
   echo '${SCRIPT_FILE}:lblrun fail' >> ${ERROR_LOG_FILE}
   exit 2
fi

# -- Wait for a bit to allow file copying to complete


# -------------------------------------
# Wait for all file copying to complete
# -------------------------------------

sleep 10

# -- Move the generated TAPE5 file to the results directory


# -------------------------------
# Remove the generated TAPE5 file
# -------------------------------

# mv ${TAPE5_FILE} ${RESULTS_DIR} 2>>${RESULTS_DIR}/${ERROR_LOG_FILE}

# -- Rename or remove the data files


# -------------------------------
# Rename or remove the data files
# -------------------------------

# cd ${RESULTS_DIR}


# -- Check that ${UPWELLING_TAPE_FILE} exists" >> ${SCRIPT_FILE}
if [ -f ${UPWELLING_TAPE_FILE} ]; then 
   mv ${UPWELLING_TAPE_FILE} ${UPWELLING_TAU_LBLRTM_FILE}
else
   echo '${SCRIPT_FILE}:${UPWELLING_TAPE_FILE} not found!' >> ${ERROR_LOG_FILE}
   ls >> ${ERROR_LOG_FILE}
   exit 2
fi


mv TAPE6 tape6.${ID_TAG} 2>>${ERROR_LOG_FILE}
mv TAPE7 tape7.${ID_TAG} 2>>${ERROR_LOG_FILE}
rm -f TAPE* 2>>${ERROR_LOG_FILE}
rm -f OD* 2>>${ERROR_LOG_FILE}

# -- Convert the LBLRTM data files to netCDF

# ----------------------------------
# Convert the LBLRTM files to netCDF
# ----------------------------------


# -- Upwelling file
#export TITLE=\"${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET_ID} profile set. Upwelling (Layer->TOA) transmittance.\"
export HISTORY=\"${LBLRTM_HITRAN_VERSION}\"
export COMMENT=\"${PROFILE_SET_ID}\"
# filename, nLayer, doublepanel?, 
${LBLRTM_to_netCDF} ${UPWELLING_TAU_LBLRTM_FILE} ${UPWELLING_TAU_LBLRTM_FILE}.nc.signal 100 FALSE > error.txt
sleep 10

# -- Check that file was created cleanly" >> ${SCRIPT_FILE}
if [ ! -f ${UPWELLING_TAU_LBLRTM_FILE}.nc.signal ]; then
  echo '${SCRIPT_FILE}:${UPWELLING_TAU_LBLRTM_FILE}.nc creation failed' >> ${ERROR_LOG_FILE}
  exit 2
fi

# we don't actually need downwelling file
# -- Downwelling file" >> ${SCRIPT_FILE}
# -- Upwelling file" >> ${SCRIPT_FILE}
#export TITLE=\"${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET_ID} profile set.  Downwelling (Layer->SFC) transmittance.\"
export HISTORY=\"${LBLRTM_HITRAN_VERSION}\"
export COMMENT=\"${PROFILE_SET_ID}\"
# filename, nLayer, doublepanel?, 
# ${LBLRTM_to_netCDF} ${DOWNWELLING_TAU_LBLRTM_FILE} ${DOWNWELLING_TAU_LBLRTM_FILE}.nc.signal 100 FALSE
# sleep 10

# -- Check that file was created cleanly
# if [ ! -f ${DOWNWELLING_TAU_LBLRTM_FILE}.nc.signal ]; then
#  echo '${SCRIPT_FILE}:${DOWNWELLING_TAU_LBLRTM_FILE}.nc creation failed' >> ${ERROR_LOG_FILE}
#  exit 2
# fi


# -- Remove LBLRTM format files
rm -f ${UPWELLING_TAU_LBLRTM_FILE} 2>>${ERROR_LOG_FILE}

# -- Remove the script files


# ----------------------
# Remove the script file
# ----------------------

cd ${ROOT_DIR}
# rm -f ${SCRIPT_FILE} 2>>${RESULTS_DIR}/${ERROR_LOG_FILE}

# -- Add time stamp to the error log file

# ------------------------------------
# Add end time stamp to error log file
# ------------------------------------

echo \"Processing run finished at: \`date\`\" >> ${RESULTS_DIR}/${ERROR_LOG_FILE}









