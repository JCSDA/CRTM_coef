#!/bin/sh

# First step
#EXE_FILE=/data/users/pstegmann/workspace/Fortran_dev/CRTM/CRTM_coef/src/mains/TauRegress/ODPS/ODAS_WLO_Regress/gencoef

#./gen_ODAS_coeff.sh ALLCOM tau_coeff.parameters $EXE_FILE

# Second step
#./ODAS_get_stat.sh tau_coeff.parameters

EXE_file=/data/users/pstegmann/workspace/Fortran_dev/CRTM/CRTM_coef/src/mains/TauRegress/ODAS/Assemble_ODAS/Cat_ODAS
./cat_ODAS_taucoef.sh tau_coeff.parameters $EXE_file

exit
