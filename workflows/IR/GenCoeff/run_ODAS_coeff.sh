#!/bin/sh

# First step
#EXE_FILE=/data/users/pstegmann/workspace/Fortran_dev/CRTM/CRTM_coef/src/apps/TauRegress/ODPS/ODAS_WLO_Regress/gencoef

#./gen_ODAS_coeff.sh ALLCOM tau_coeff.parameters $EXE_FILE

# Second step
#./ODAS_get_stat.sh tau_coeff.parameters

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CRTM_COEF_ROOT="${CRTM_COEF_ROOT:-$(cd "${SCRIPT_DIR}/../../.." && pwd)}"
if [ -z "${CRTM_COEF_BUILD:-}" ]; then
  if [ -d "${CRTM_COEF_ROOT}/build-spack" ]; then
    CRTM_COEF_BUILD="${CRTM_COEF_ROOT}/build-spack"
  else
    CRTM_COEF_BUILD="${CRTM_COEF_ROOT}/build"
  fi
fi
EXE_file="${ASSEMBLE_ODAS_EXE:-${CRTM_COEF_BUILD}/src/apps/TauRegress/ODAS/Assemble_ODAS/Assemble_ODAS}"
./cat_ODAS_taucoef.sh tau_coeff.parameters "$EXE_file"

exit
