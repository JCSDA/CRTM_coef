#!/bin/bash
# Job step for: ${ID_TAG}
#SBATCH --partition=serial
#SBATCH --export=ALL
#SBATCH --ntasks=1
#SBATCH --nodes=1  # should be equal to ntasks
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4gb  
#SBATCH --time=1:00:00
#SBATCH --output=output/apod_test_%A_%a.apod.out
#SBATCH --error=output/apod_test_%A_%a.apod.err
#SBATCH --job-name=Apod_%A_%a

# --------------------------------------------
# LBLRTM transmittance apodization run script"
# file for the output netCDF file:"             
# ${UPWELLING_TAU_NETCDF_FILE}"                 
# --------------------------------------------" 


# -- Create LBLRTM/HITRAN version string
LBLRTM_HITRAN_VERSION="LBLRTM v9.4; HITRAN 2000 + AER updates"

# -- Change to the directory containing the
# -- current TAPE5 file
ROOT_DIR=/scratch/users/pstegmann/TauCoeffTest_IASI/
Apodize_TauSpc_with_IRF=/data/users/pstegmann/lib/CRTM/trunk/src/TauProd/Infrared/Apodize_TauSpc_with_IRF/Apodize_TauSpc_with_IRF

# ----------------------------
# Change to required directory
# ----------------------------

cd ${ROOT_DIR}

# ----------------------------
# Variable transfer section
# ----------------------------

ID_TAG=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_idtag_list.txt)
RESULTS_DIR=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_resultsdir_list.txt)
THIS_TAPE3=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_tape3_list.txt)
THIS_PROFILE=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_profile_list.txt)
THIS_ANGLE=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_angle_list.txt)
THIS_BAND=$(sed -n -e ${SLURM_ARRAY_TASK_ID}p crtm_band_list.txt)


# -- Create output file names
UPWELLING_TAPE_FILE="TAPE20"
UPWELLING_TAU_LBLRTM_FILE="upwelling_tau.${ID_TAG}"


# -----------------------------
# Define the error log filename
# -----------------------------

ERROR_LOG_FILE="Error_Log.${ID_TAG}"

# -- Add time stamp to the error log file

# --------------------------------------
# Add begin time stamp to error log file
# --------------------------------------

echo >> ${RESULTS_DIR}/${ERROR_LOG_FILE}; echo '--------------------------' >> ${RESULTS_DIR}/${ERROR_LOG_FILE}
echo \"Processing run started at:  \`date\`\" >> ${RESULTS_DIR}/${ERROR_LOG_FILE}

# -- Start the LBLRTM run

# ---------------------
# Start Apodization run
# ---------------------

cd ${RESULTS_DIR}
echo ${PWD}
ISENSOR=1 # -- iSensor = IASI
IDIR=1    # -- iDir = UPWELLING_DIRECTION (See Tau_Production_Parameters.f90)
PROFILE_SET_NUMBER=5
ulimit -s unlimited
${Apodize_TauSpc_with_IRF} ${UPWELLING_TAU_LBLRTM_FILE}.nc.signal ${PROFILE_SET_NUMBER} ${THIS_TAPE3} ${THIS_PROFILE} ${THIS_ANGLE} ${IDIR} ${ISENSOR} ${THIS_BAND}

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
