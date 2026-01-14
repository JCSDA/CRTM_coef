#!/bin/bash
# Job for submitting process_tape5_files
# because it takes too long.
#SBATCH --partition=serial
#SBATCH --export=ALL
#SBATCH --ntasks=1
#SBATCH --nodes=1  # should be equal to ntasks
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4gb  
#SBATCH --time=9:00:00
#SBATCH --output=output/TauSpcTreiber.out
#SBATCH --error=output/TauSpcTreiber.err
#SBATCH --job-name=TauSpc_Treiber
#module purge
#module load license_intel intel/18.0.3  #whatever version you need
#module load hdf/4.2.14    #and any other modules needed
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CRTM_COEF_ROOT="${CRTM_COEF_ROOT:-$(cd "${SCRIPT_DIR}/../../.." && pwd)}"
${CRTM_COEF_ROOT}/workflows/IR/TauCoeffTest/process_TauSpc_files
