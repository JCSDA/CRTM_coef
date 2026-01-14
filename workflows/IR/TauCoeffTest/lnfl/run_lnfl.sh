#!/bin/sh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CRTM_COEF_ROOT="${CRTM_COEF_ROOT:-$(cd "${SCRIPT_DIR}/../../.." && pwd)}"

TAPE3_LIST="${TAPE3_LIST:-1 10 11 12 13 14 15}"
TAPE3_ID="${TAPE3_ID:-aer_v_3.8.1}"
LINE_FILE_ROOT="${LINE_FILE_ROOT:-${CRTM_COEF_ROOT}/external/line_files}"
LINE_FILE="${LINE_FILE:-${LINE_FILE_ROOT}/line_file/${TAPE3_ID}}"
LNFL_EXE="${LNFL_EXE:-${CRTM_COEF_ROOT}/external/LNFL/lnfl_v3.2_linux_gnu_sgl}"
WN_MIN="${WN_MIN:-0}"
WN_MAX="${WN_MAX:-20000}"

if [ ! -x "${LNFL_EXE}" ]; then
  echo "LNFL executable not found: ${LNFL_EXE}"
  exit 1
fi

if [ ! -f "${LINE_FILE}" ]; then
  echo "Line file not found: ${LINE_FILE}"
  exit 1
fi

cd "${SCRIPT_DIR}"

if [ ! -e spd_dep ]; then
  ln -s "${LINE_FILE_ROOT}/spd_dep" spd_dep
fi
if [ ! -e extra_brd_params ]; then
  ln -s "${LINE_FILE_ROOT}/extra_brd_params" extra_brd_params
fi
if [ ! -e lncpl_lines ]; then
  ln -s "${LINE_FILE_ROOT}/lncpl_lines" lncpl_lines
fi

for THIS_TAPE3 in ${TAPE3_LIST}; do
  case "${THIS_TAPE3}" in
    1)  species="H2O";                TAPE3_FILE="tape3.mol1.${TAPE3_ID}" ;;
    2)  species="CO2";                TAPE3_FILE="tape3.mol2.${TAPE3_ID}" ;;
    3)  species="O3";                 TAPE3_FILE="tape3.mol3.${TAPE3_ID}" ;;
    4)  species="N2O";                TAPE3_FILE="tape3.mol4.${TAPE3_ID}" ;;
    5)  species="CO";                 TAPE3_FILE="tape3.mol5.${TAPE3_ID}" ;;
    6)  species="CH4";                TAPE3_FILE="tape3.mol6.${TAPE3_ID}" ;;
    7)  species="O2";                 TAPE3_FILE="tape3.mol7.${TAPE3_ID}" ;;
    8)  species="H2O:CO2:O3:N2O:CO:CH4:O2"; TAPE3_FILE="tape3.${TAPE3_ID}" ;;
    9)  species="NONE";               TAPE3_FILE="tape3.nomol.${TAPE3_ID}" ;;
    10) species="H2O:CO2:O3:N2O:CO:CH4:O2"; TAPE3_FILE="tape3.${TAPE3_ID}" ;;
    11) species="H2O:O3";             TAPE3_FILE="tape3.wvo.${TAPE3_ID}" ;;
    12) species="H2O";                TAPE3_FILE="tape3.mol1.${TAPE3_ID}" ;;
    13) species="CO2:N2O:CO:CH4:O2";  TAPE3_FILE="tape3.dry.${TAPE3_ID}" ;;
    14) species="O3";                 TAPE3_FILE="tape3.mol3.${TAPE3_ID}" ;;
    15) species="NONE";               TAPE3_FILE="tape3.nomol.${TAPE3_ID}" ;;
    17) species="O2";                 TAPE3_FILE="tape3.molc1.${TAPE3_ID}" ;;
    18) species="O2:CH4";             TAPE3_FILE="tape3.molc2.${TAPE3_ID}" ;;
    19) species="O2:CH4:CO";          TAPE3_FILE="tape3.molc3.${TAPE3_ID}" ;;
    20) species="O2:CH4:CO:N2";       TAPE3_FILE="tape3.molc4.${TAPE3_ID}" ;;
    21) species="O2:CH4:CO:N2O:CO2";  TAPE3_FILE="tape3.molc5.${TAPE3_ID}" ;;
    22) species="O2:CH4:CO:N2O:CO2:H2O"; TAPE3_FILE="tape3.molc6.${TAPE3_ID}" ;;
    *) echo "Unsupported TAPE3 index: ${THIS_TAPE3}" ; exit 2 ;;
  esac

  echo "Generating ${TAPE3_FILE} for ${species} (${WN_MIN}-${WN_MAX} cm^-1)"

  rm -f TAPE1 TAPE2 TAPE3 TAPE5 TAPE6* TAPE7* TAPE10*

  python3 create_lnfl_record3.py "${WN_MIN}" "${WN_MAX}" "${species}"
  "${LNFL_EXE}" "${LINE_FILE}"

  mv TAPE3 "${TAPE3_FILE}"
done
