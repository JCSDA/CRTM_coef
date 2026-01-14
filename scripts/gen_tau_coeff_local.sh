#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "Usage: $(basename "$0") <tau_coeff.parameters>" >&2
  exit 1
fi

PARAM_FILE=$1

. ./tau_coeff.func

PARAM_NAME_LIST="MAX_CPUs CH_INT EXE_FILE WORK_DIR PROF_SET SPC_COEFF_DIR TAU_PROFILE_DIR ATM_PROFLE_FILE "\
"GET_SEN_INFO COMPONENTS IR_TYPE COMPONENT_GROUP1 COMPONENT_GROUP2 COMPONENT_GROUP3"

read_parameters

SCRIPT_DIR=${PWD}
SENSOR_LIST_FILENAME=${SCRIPT_DIR}/sensor_list
if [[ ! -f ${SENSOR_LIST_FILENAME} ]]; then
  echo "Missing ${SENSOR_LIST_FILENAME}" >&2
  exit 1
fi

SENSOR_LIST=$(awk '/BEGIN_LIST/ {while(getline){if(match($0,"END_LIST"))exit; print $1}}' "${SENSOR_LIST_FILENAME}")

if [[ ! -f ${EXE_FILE} ]]; then
  echo "Missing ODPS_Regress executable: ${EXE_FILE}" >&2
  exit 1
fi

if [[ ! -f ${GET_SEN_INFO} ]]; then
  echo "Missing GetSenInfo executable: ${GET_SEN_INFO}" >&2
  exit 1
fi

if [[ ! -f ${ATM_PROFLE_FILE} ]]; then
  echo "Missing atmospheric profile file: ${ATM_PROFLE_FILE}" >&2
  exit 1
fi

ChInt=${CH_INT:-1}
ProfSet=${PROF_SET}

for SatSen in ${SENSOR_LIST}; do
  Sensor_DIR=${WORK_DIR}/${ProfSet}/${SatSen}
  mkdir -p "${Sensor_DIR}"

  SpcCoeffFile=${SPC_COEFF_DIR}/${SatSen}.SpcCoeff.nc
  TauProfileFile=${TAU_PROFILE_DIR}/upwelling.${SatSen}.TauProfile.nc

  if [[ ! -f ${TauProfileFile} ]]; then
    echo "Missing TauProfile file: ${TauProfileFile}" >&2
    exit 1
  fi

  if [[ ! -f ${SpcCoeffFile} ]]; then
    echo "Missing SpcCoeff file: ${SpcCoeffFile}" >&2
    exit 1
  fi

  SEN_INFO_LIST=$("${GET_SEN_INFO}" <<EOF
${SpcCoeffFile}
EOF
)

  Sensor_Type=$(echo "${SEN_INFO_LIST}" | awk '{print $1}')
  Nchan=$(echo "${SEN_INFO_LIST}" | awk '{print $2}')

  # Extract n_Angles from TauProfile file
  n_Angles=$(ncdump -h "${TauProfileFile}" | grep "n_Angles =" | awk '{print $3}')
  if [[ -z "${n_Angles}" ]]; then
    echo "Error: Could not extract n_Angles from ${TauProfileFile}" >&2
    exit 1
  fi
  echo "Using ${n_Angles} angles from TauProfile file"

  if [[ "${COMPONENTS}" == "DEFAULT" ]]; then
    if [[ ${Sensor_Type} == 1 ]]; then
      GasName_List=${COMPONENT_GROUP3}
    else
      if [[ ${IR_TYPE} -eq 1 ]]; then
        GasName_List=${COMPONENT_GROUP1}
      else
        GasName_List=${COMPONENT_GROUP2}
      fi
    fi
    GasName_List=$(echo "${GasName_List}" | awk -F, '{for(i=1;i<=NF;i++)print $i}')
  else
    GasName_List=$(echo "${COMPONENTS}" | awk -F, '{for(i=1;i<=NF;i++)print $i}')
  fi

  echo "${GasName_List}" > "${Sensor_DIR}/components.txt"

  if [[ ${Sensor_Type} == 1 ]]; then
    COMPONENT_LIST=${COMPONENT_GROUP3}
    GROUP_ID=3
  else
    if [[ ${IR_TYPE} -eq 1 ]]; then
      COMPONENT_LIST=${COMPONENT_GROUP1}
      GROUP_ID=1
    else
      COMPONENT_LIST=${COMPONENT_GROUP2}
      GROUP_ID=2
    fi
  fi
  COMPONENT_LIST=$(echo "${COMPONENT_LIST}" | awk -F, '{for(i=1;i<=NF;i++)print $i}')

  for GasName in ${GasName_List}; do
    idx=0
    for item in ${COMPONENT_LIST}; do
      idx=$((idx + 1))
      if [[ "${item}" == "${GasName}" ]]; then
        COMP_IDX=${idx}
      fi
    done

    ExecutionDirFile="${Sensor_DIR}/executionDir_${GasName}.txt"
    rm -f "${ExecutionDirFile}" 2>/dev/null

    TopSeqCh=1
    EndChan=$((TopSeqCh + Nchan - 1))

    while [[ ${TopSeqCh} -le ${EndChan} ]]; do
      LastSeqCh=$((TopSeqCh + ChInt - 1))
      if [[ ${LastSeqCh} -gt ${EndChan} ]]; then
        LastSeqCh=${EndChan}
      fi

      printf -v TopSeqChP "%03d" "${TopSeqCh}"
      printf -v LastSeqChP "%03d" "${LastSeqCh}"

      ProcSuffix=${SatSen}.${GasName}.ch${TopSeqChP}to${LastSeqChP}.${ProfSet}
      RunDir=${Sensor_DIR}/${GasName}/${ProcSuffix}
      mkdir -p "${RunDir}"
      cd "${RunDir}"

      TauCoeffFile="TauCoeff_${TopSeqChP}.nc"

      cat <<EOF > Namelist.txt

    &SATSEN
      File_Prefix  = "${SatSen}",
      Ichan_start    =  ${TopSeqChP},
      Ichan_end   =  ${LastSeqChP}
/
    &GENCOEF
      icom = ${COMP_IDX},
      Nangle_regression    = ${n_Angles}
/
    &FILENAMES
      inFilename_spcCoef = "${SpcCoeffFile}",
      inFilename_atmProfile = "${ATM_PROFLE_FILE}",
      inFilename_tauProfile = "${TauProfileFile}"
      inFilename_tauCoeff = "${TauCoeffFile}"
/
EOF

      "${EXE_FILE}" "${GROUP_ID}" > odps_regress.log 2>&1

      echo "${RunDir}" >> "${ExecutionDirFile}"

      TopSeqCh=$((TopSeqCh + ChInt))
    done
  done
done
