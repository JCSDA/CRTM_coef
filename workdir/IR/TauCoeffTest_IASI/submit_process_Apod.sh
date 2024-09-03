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
#SBATCH --output=output/Treiber_Apod.out
#SBATCH --error=output/Treiber_Apod.err
#SBATCH --job-name=ApodizationTreiber
#module purge
#module load license_intel intel/18.0.3  #whatever version you need
#module load hdf/4.2.14    #and any other modules needed
/scratch/users/pstegmann/TauCoeffTest_IASI/process_apodize_driver
