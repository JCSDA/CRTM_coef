#!/usr/bin/env bash
set -euo pipefail

SPACK_STACK_DIR="${SPACK_STACK_DIR:-/home/ben/spack-stack}"
SPACK_ENV="${SPACK_ENV:-/home/ben/spack-stack/envs/unified-env.mylinux}"
MODULE_PATH="${MODULE_PATH:-/home/ben/spack-stack/envs/unified-env.mylinux/modules}"
MODULE_NAME="${MODULE_NAME:-openmpi/5.0.8/none/none/jedi-fv3-env}"
BUILD_DIR="${1:-build-spack}"
ECBUILD_LOG_FILE="${ECBUILD_LOG_FILE:-${BUILD_DIR}/ecbuild.log}"
ECBUILD_LOG_LEVEL="${ECBUILD_LOG_LEVEL:-INFO}"

source "${SPACK_STACK_DIR}/setup.sh"
module use "${MODULE_PATH}"
module load "${MODULE_NAME}"

FC="${FC:-$(command -v mpifort)}"
CC="${CC:-$(command -v mpicc)}"
NC_CONFIG="${NC_CONFIG:-$(command -v nc-config)}"
NF_CONFIG="${NF_CONFIG:-$(command -v nf-config)}"

if [[ -z "${FC}" || -z "${CC}" || -z "${NC_CONFIG}" || -z "${NF_CONFIG}" ]]; then
  echo "Missing required tools (mpifort/mpicc/nc-config/nf-config) in PATH." >&2
  exit 1
fi

openblas_lib=""
IFS=':' read -r -a ld_paths <<< "${LD_LIBRARY_PATH:-}"
for d in "${ld_paths[@]}"; do
  if [[ -f "${d}/libopenblas.so" ]]; then
    openblas_lib="${d}/libopenblas.so"
    break
  fi
  if [[ -f "${d}/libopenblas.so.0" ]]; then
    openblas_lib="${d}/libopenblas.so.0"
    break
  fi
done

if [[ -z "${openblas_lib}" ]]; then
  echo "Could not find libopenblas.so in LD_LIBRARY_PATH." >&2
  exit 1
fi

gomp_lib="$("${FC}" -print-file-name=libgomp.so)"

mkdir -p "${BUILD_DIR}"
cmake -S . -B "${BUILD_DIR}" \
  -DOPENMP=ON \
  -DCMAKE_Fortran_COMPILER="${FC}" \
  -DCMAKE_C_COMPILER="${CC}" \
  -DNetCDF_C_CONFIG_EXECUTABLE="${NC_CONFIG}" \
  -DNetCDF_Fortran_CONFIG_EXECUTABLE="${NF_CONFIG}" \
  -DECBUILD_LOG_FILE="${ECBUILD_LOG_FILE}" \
  -DECBUILD_LOG_LEVEL="${ECBUILD_LOG_LEVEL}" \
  -DBLA_VENDOR=OpenBLAS \
  -DBLAS_LIBRARIES="${openblas_lib}" \
  -DLAPACK_LIBRARIES="${openblas_lib}" \
  -DOpenMP_Fortran_FLAGS=-fopenmp \
  -DOpenMP_Fortran_LIB_NAMES=gomp \
  -DOpenMP_gomp_LIBRARY="${gomp_lib}" \
  -DOpenMP_Fortran_HAVE_OMPLIB_MODULE=TRUE

cmake --build "${BUILD_DIR}" -j8
