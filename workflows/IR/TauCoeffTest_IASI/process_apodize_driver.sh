#!/bin/sh
#------------------------------------------------------------------------------
#
# NAME:
#       process_apodize_files
#
# PURPOSE:
#       Shell script to process the output LBLRTM netCDF files using the defined
#       parameters.
#
# CATEGORY:
#       Transmittance Apodization
#
# LANGUAGE:
#       Bourne shell script
#
# CALLING SEQUENCE:
#       process_apodize_files -h
#
# CREATION HISTORY:
#       Written by:     Patrick Stegmann, JCSDA 2020-09-21
#                       
#
#  Copyright (C) 2020 Patrick Stegmann
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                        -- USAGE DESCRIPTION FUNCTION --
#-------------------------------------------------------------------------------

usage()
{
  echo
  echo " Usage: process_apodize_files [-h] [-a angle_begin[:angle_end]] [-b band_begin[:band_end]]"
  echo "                           [-d TAPE5_directory] [-p profile_begin[:profile_end]]"
  echo "                           [-q batch_queue] [-s TAPE3_identifier]"
  echo "                           [-t TAPE3_index_list]"
  echo
  echo "   -h           Print this message and exit"
  echo
  echo "   -a           Use this option to specify the begin and end angle limits"
  echo "                for the calculation."
  echo "                Valid values are:"
  echo "                  1 == SEC(z)=1.00, z= 0.000"
  echo "                  2 == SEC(z)=1.25, z=36.870"
  echo "                  3 == SEC(z)=1.50, z=48.190"
  echo "                  4 == SEC(z)=1.75, z=55.150"
  echo "                  5 == SEC(z)=2.00, z=60.000"
  echo "                  6 == SEC(z)=2.25, z=63.612"
  echo "                  7 == SEC(z)=3.00, z=70.529"
  echo "                Default action is determined by defaults file."
  echo
  echo "   -b           Use this option to specify the band at which to begin"
  echo "                and end the calculation."
  echo "                Default action is determined by defaults file."
  echo
  echo "   -d           Use this option to specify the location of the LBLRTM"
  echo "                TAPE5 input files."
  echo "                Default action is determined by defaults file."
  echo
  echo "   -p           Use this option to specify the begin and end profile limits"
  echo "                for the calculation."
  echo "                Default action is determined by defaults file."
  echo
  echo "   -q           Use this option to specify the batch queue to which the"
  echo "                generated script file will be submitted."
  echo "                Default action is determined by defaults file."
  echo
  echo "   -s           Use this option to specify the TAPE3 spectroscopic database"
  echo "                to use."
  echo "                Valid options are:"
  echo "                  2000_AER == the HITRAN 2000 database with AER updates"
  echo "                  1996_JPL == the HITRAN 1996 database with JPL/Toth updates"
  echo "                Default action is determined by defaults file."
  echo
  echo "   -t           Use this option to specify the LBLRTM TAPE3 index list"
  echo "                to process."
  echo "                Valid TAPE3 indices are:"
  echo "                   1-7 == individual molecule numbers (no continua)"
  echo "                   8   == all first seven molecules (no continua)"
  echo "                   9   == continua only"
  echo "                  10   == all first seven molecules (and their continua)"
  echo "                  11   == water vapor + ozone only (and their continua)"
  echo "                  12   == water vapor only (and it's continua)"
  echo "                  13   == dry gases. Everything but h2o and o3 (and their continua)"
  echo "                  14   == ozone only (and it's continua)"
  echo "                  15   == water vapor continua only"
  echo "                  17   == O2"
  echo "                  18   == O2 + CH4"
  echo "                  19   == O2 + CH4 + CO"
  echo "                  20   == O2 + CH4 + CO + N2O"
  echo "                  21   == O2 + CH4 + CO + N2O + CO2"
  echo "                  22   == O2 + CH4 + CO + N2O + CO2 + H20"
  echo "                The list *must* be enclosed in quotes."
  echo "                Default action is determined by defaults file."
  echo
}


#-------------------------------------------------------------------------------
#                    -- THE SCRIPT NAME FOR ERROR MESSAGES --
#-------------------------------------------------------------------------------

SCRIPT_NAME="`basename $0`"
Apodize_TauSpc_with_IRF=/data/users/pstegmann/lib/CRTM/trunk/src/TauProd/Infrared/Apodize_TauSpc_with_IRF/Apodize_TauSpc_with_IRF

#-------------------------------------------------------------------------------
#                             -- DEFINE DEFAULTS --
#-------------------------------------------------------------------------------

  # -- Defaults file and required quantities
  DEFAULTS_FILE="Transmittance_Production.processing_defaults"
  DEFAULTS_LIST="QUEUE BAND1 BAND2 F1_BAND1 DF_BAND DF_AVG TAPE3_LIST TAPE3_ID "\
"TAPE5_DIR PROFILE1 PROFILE2 PROFILE_SET_NUMBER PROFILE_SET_ID ANGLE1 ANGLE2 CO2MR"

  # -- Check existence of defaults file
  if [ ! -f ${DEFAULTS_FILE} ]; then
    echo "${SCRIPT_NAME}: ${DEFAULTS_FILE} file not found."
    exit 1
  fi

  # -- Get defaults from file
  for ITEM_NAME in ${DEFAULTS_LIST}; do

    ITEM_VALUE=`grep ":${ITEM_NAME}:" ${DEFAULTS_FILE} | cut -d":" -f3`

    case "${ITEM_NAME}" in

      # -- Default batch processing queue
      QUEUE) QUEUE=${ITEM_VALUE};;

      # -- Default band limits
      BAND1) BAND1=${ITEM_VALUE};;
      BAND2) BAND2=${ITEM_VALUE};;

      # -- The begin frequency of band #1
      # -- and the bandwidth of each band
      F1_BAND1) F1_BAND1=${ITEM_VALUE};;
      DF_BAND) DF_BAND=${ITEM_VALUE};;

      # -- The averaging kernel frequency width
      # -- Must have 4dp
      DF_AVG) DF_AVG=${ITEM_VALUE};;

      # -- The TAPE3 list
      TAPE3_LIST) TAPE3_LIST=${ITEM_VALUE};;

      # -- The TAPE3 ID
      TAPE3_ID) TAPE3_ID=${ITEM_VALUE};;

      # -- The TAPE5 directory
      TAPE5_DIR) TAPE5_DIR=${ITEM_VALUE};;

      # -- Default PROFILE limits and ID
      PROFILE1) PROFILE1=${ITEM_VALUE};;
      PROFILE2) PROFILE2=${ITEM_VALUE};;
      PROFILE_SET_NUMBER) PROFILE_SET_NUMBER=${ITEM_VALUE};;
      PROFILE_SET_ID) PROFILE_SET_ID=${ITEM_VALUE};;

      # -- Default zenith angle limits
      ANGLE1) ANGLE1=${ITEM_VALUE};;
      ANGLE2) ANGLE2=${ITEM_VALUE};;

      # -- Default CO2 mixing ratio in ppmv
      CO2MR) CO2MR=${ITEM_VALUE};;

      *) ;; # Ignore invalid default
    esac

  done



#-------------------------------------------------------------------------------
#                       -- PARSE THE COMMAND LINE OPTIONS --
#-------------------------------------------------------------------------------

  while getopts :ha:b:d:p:q:s:t: OPTVAL; do


    # --------------------------------------------
    # If option argument looks like another option
    # exit the loop
    # --------------------------------------------

    case ${OPTARG} in
      -*) break;;
    esac


    # ----------------------------
    # Parse the valid options here
    # ----------------------------

    case ${OPTVAL} in
      a)  ANGLE_LIMITS=${OPTARG}
          ANGLE1=`echo ${ANGLE_LIMITS} | cut -d: -f1`
          ANGLE2=`echo ${ANGLE_LIMITS} | cut -d: -f2`;;
      b)  BAND_LIMITS=${OPTARG}
          BAND1=`echo ${BAND_LIMITS} | cut -d: -f1`
          BAND2=`echo ${BAND_LIMITS} | cut -d: -f2`;;
      d)  TAPE5_DIR=${OPTARG};;
      p)  PROFILE_LIMITS=${OPTARG}
          PROFILE1=`echo ${PROFILE_LIMITS} | cut -d: -f1`
          PROFILE2=`echo ${PROFILE_LIMITS} | cut -d: -f2`;;
      q)  QUEUE=${OPTARG};;
      s)  TAPE3_ID=${OPTARG};;
      t)  TAPE3_LIST=${OPTARG};;
      h)  usage
          exit 0;;
      :|\?) OPTVAL=${OPTARG}
            break;;
    esac

  done


  # ----------------------------
  # Remove the options processed
  # ----------------------------

  shift `expr ${OPTIND} - 1`


  # ------------------------------------------
  # Now output invalidities based on OPTVAL
  # Need to do this as getopts does not handle
  # the situations where an option is passed
  # as an argument to another option.
  # ------------------------------------------

  case ${OPTVAL} in


    # --------------------------------------------
    # If OPTVAL contains nothing, then all options
    # have been successfully parsed
    # --------------------------------------------

    \?) if [ $# -ne 0 ]; then
          ( echo "${SCRIPT_NAME}: Invalid argument(s) $*" ; usage ) | more
          #usage | more
          exit 2
        fi;;


    # ------------------------------------
    # Valid options, but missing arguments
    # ------------------------------------

    a|b|d|p|q|s|t) ( echo "${SCRIPT_NAME}: '-${OPTVAL}' option requires an argument" ; usage ) | more
                 exit 2;;


    # --------------
    # Invalid option
    # --------------

    ?) ( echo "${SCRIPT_NAME}: Invalid option '-${OPTARG}'" ; usage ) | more
       exit 2;;

  esac



#------------------------------------------------------------------------------#
#                       -- ASSIGN PROCESSING PARAMETERS --                     #
#------------------------------------------------------------------------------#

  # ---------------------------------------------------
  # The default continua, i.e. none. Other settings for
  # TAPE3 selections are in the TAPE3 loop.
  # ---------------------------------------------------

  H2O_SC=0.0
  H2O_FC=0.0
  CO2_C=0.0
  O3_C=0.0
  O2_C=0.0
  N2_C=0.0
  RAYLEIGH_EXT=0.0

  CONTINUA="${H2O_SC} ${H2O_FC} ${CO2_C} ${O3_C} ${O2_C} ${N2_C} ${RAYLEIGH_EXT}"


  # ------------------
  # The root directory
  # ------------------

  ROOT_DIR=${PWD}
  cd ${ROOT_DIR}

  if [[ -e crtm_idtag_list.txt ]]; then
    rm crtm_idtag_list.txt
    rm crtm_resultsdir_list.txt
    rm crtm_tape3_list.txt
    rm crtm_profile_list.txt
    rm crtm_angle_list.txt
    rm crtm_band_list.txt
  fi

  touch crtm_idtag_list.txt
  touch crtm_resultsdir_list.txt
  touch crtm_tape3_list.txt 
  touch crtm_profile_list.txt 
  touch crtm_angle_list.txt 
  touch crtm_band_list.txt


  # ------------------------
  # Tab character for output
  # ------------------------

  TAB='\011'


  # ----------------------------------------------------------------
  # Determine the fractional part of the frequency endpoint for the
  # averaging kernel
  # E.g. If the averaging kernel width is 0.0025, then the endpoint
  #      frequency for the averaging is XXX.9975. The value assigned
  #      here is "9975"
  # ----------------------------------------------------------------

  F2_SCNMRG_FRACTION=`echo ${DF_AVG} | cut -d"." -f2`
  F2_SCNMRG_FRACTION=`expr 10000 - ${F2_SCNMRG_FRACTION}`



#------------------------------------------------------------------------------#
#                  -- INITIALISE SUBMITTED JOBS COUNTER --                     #
#------------------------------------------------------------------------------#

  N_JOBS=0



#------------------------------------------------------------------------------#
#                          -- LOOP OVER PROFILES --                            #
#------------------------------------------------------------------------------#

  # ------------------------------
  # Initialise profile job counter
  # ------------------------------

  N_PROFILE_JOBS=0


  # ----------
  # Begin loop
  # ----------

  THIS_PROFILE=${PROFILE1}
  while [ ${THIS_PROFILE} -le ${PROFILE2} ]; do


    # ---------------------
    # Assign profile id tag
    # ---------------------

    if [ ${THIS_PROFILE} -lt 10 ]; then
      PROFILE_TAG="profile0${THIS_PROFILE}"
    else
      PROFILE_TAG="profile${THIS_PROFILE}"
    fi


    #--------------------------------------------------------------------------#
    #                           -- LOOP OVER ANGLES --                         #
    #--------------------------------------------------------------------------#

    # ----------------------------
    # Initialise angle job counter
    # ----------------------------

    N_ANGLE_JOBS=0


    # ----------
    # Begin loop
    # ----------

    THIS_ANGLE=${ANGLE1}
    while [ ${THIS_ANGLE} -le ${ANGLE2} ]; do


      # -------------------
      # Assign angle id tag
      # -------------------

      ANGLE_TAG="angle${THIS_ANGLE}"



      #------------------------------------------------------------------------#
      #                        -- LOOP OVER TAPE3 FILES --                     #
      #------------------------------------------------------------------------#

      # -----------------------------------
      # Initialise molecule set job counter
      # -----------------------------------

      N_TAPE3_JOBS=0


      # ----------
      # Begin loop
      # ----------

      for THIS_TAPE3 in ${TAPE3_LIST}; do


        # -------------------------------------------------------
        # Assign TAPE3 id tag, and specific continua
        # -------------------------------------------------------

        case ${THIS_TAPE3} in


          # -- Individual molecules
          1|2|3|4|5|6|7) TAPE3_TAG="mol${THIS_TAPE3}";;

          # -- All first seven molecules but no continua
          8) TAPE3_TAG="anc";;

          # -- Just the continua
          9) TAPE3_TAG="con";;

          # -- All first seven molecules + continua
          10) TAPE3_TAG="awc";;

          # -- Water vapor + ozone only (and their continua)
          11) TAPE3_TAG="wvo";;

          # -- Water vapor only (and its continua)
          12) TAPE3_TAG="wet";;

          # -- "Dry" gases. Everything except water vapor and ozone
          13) TAPE3_TAG="dry";;

          # -- Ozone only (and its continua)
          14) TAPE3_TAG="ozo";;

          # -- Water vapor continua only (NO line data)
          15) TAPE3_TAG="wco";;

          # -- O2 + CH4 (and their continua)
          17) TAPE3_TAG="molc1";;

          # -- O2 + CH4 (and their continua)
          18) TAPE3_TAG="molc2";;

          # -- O2 + CH4 + CO (and their continua)
          19) TAPE3_TAG="molc3";;

          # -- O2+CH4+CO+N2O (and their continua)
          20) TAPE3_TAG="molc4";;

          # -- O2+CH4+CO+N2O+CO2 (and their continua)
          21) TAPE3_TAG="molc5";;

          # -- O2+CH4+CO+N2O+CO2+H20 (and their continua)
          22) TAPE3_TAG="molc6";;


          # -- Invalid option
          *) ( echo "${SCRIPT_NAME}: Invalid TAPE3_LIST index" ; usage ) | more
             exit 2;;

        esac


        #----------------------------------------------------------------------#
        #      -- CREATE JOB COMMAND FILENAME FOR THE CURRENT BAND SET --      #
        #----------------------------------------------------------------------#

        JOB_COMMAND_FILE="${PROFILE_TAG}_${ANGLE_TAG}_${TAPE3_TAG}_Band${BAND1}-${BAND2}.jcf"


        #----------------------------------------------------------------------#
        #                        -- LOOP OVER BANDS --                         #
        #----------------------------------------------------------------------#

        # ------------------------------------
        # Initialise spectral band job counter
        # ------------------------------------

        N_BAND_JOBS=0


        # ------------------------------------
        # Assign frequency for first loop pass
        # ------------------------------------

        F2=${F1_BAND1}
        THIS_BAND=1
        while [ ${THIS_BAND} -lt ${BAND1} ]; do
          F2=`expr ${F2} + ${DF_BAND}`
          THIS_BAND=`expr ${THIS_BAND} + 1`
        done


        F_BEGIN=(500 1400 2300)
        F_END=(1400 2300 3200)

        F1=${F_BEGIN[0]}
        F2=${F_END[0]}


        # ----------
        # Begin loop
        # ----------
        EXEC_BAND=""
        THIS_BAND=${BAND1}
        while [ ${THIS_BAND} -le ${BAND2} ]; do


          # ------------------
          # Assign band id tag
          # ------------------

          if [ ${THIS_BAND} -ge 100 ]; then
            BAND_TAG="band${THIS_BAND}"
          elif [ ${THIS_BAND} -lt 100 -a ${THIS_BAND} -ge 10 ]; then
            BAND_TAG="band0${THIS_BAND}"
          else
            BAND_TAG="band00${THIS_BAND}"
          fi



          # -------------------------------------------------
          # Assign frequency parameters for this band
          # Actual frequencies used in LBLRTM calculation has
          # one unit of slop either side.
          # -------------------------------------------------

          # -- The required data boundaries
          #F1=${F2}
          #F2=`expr ${F1} + ${DF_BAND}`


          F1=${F_BEGIN[THIS_BAND-1]}
          F2=${F_END[THIS_BAND-1]}
          echo ${THIS_BAND} ${F1} ${F2} 


          # -------------------------------------------------------
          # Write the TAPE3, profile, angle, and band to text file:
          # -------------------------------------------------------
          echo ${THIS_TAPE3} >> crtm_tape3_list.txt 
          echo ${THIS_PROFILE} >> crtm_profile_list.txt 
          echo ${THIS_ANGLE} >> crtm_angle_list.txt 
          echo ${THIS_BAND} >> crtm_band_list.txt


          # -----------------------------------------------------------------
          # Define an Id tag for filenames, and an attribute for netCDF files
          # -----------------------------------------------------------------

          ID_TAG="${PROFILE_TAG}_${ANGLE_TAG}_${TAPE3_TAG}_${BAND_TAG}"
          ID_ATTRIBUTE="("`echo ${ID_TAG} | tr '_' ':'`")"
          echo ${ID_TAG} >> crtm_idtag_list.txt


          # -------------------------------------------------------
          # Assemble TAPE5 input filename and results directory
          # Note that the TAPE5 file is tagged with the TAPE3 id
          # tag. This is necessary as, even though the TAPE5's for
          # different TAPE3 inputs are exactly the same, the TAPE5
          # files are deleted after the LBLRTM run so the names
          # have to be unique for subsequent molecule runs to work.
          # -------------------------------------------------------

          RESULTS_DIR="${PROFILE_TAG}/${ANGLE_TAG}/${TAPE3_TAG}/${BAND_TAG}"
          echo ${RESULTS_DIR} >> crtm_resultsdir_list.txt

          # -----------------------------
          # Define the error log filename
          # -----------------------------

          ERROR_LOG_FILE="Error_Log.${ID_TAG}"


          # -----------------------------------------
          # Begin constructing executable script file
          # -----------------------------------------

          # -- Create output script file name
          SCRIPT_FILE="${ID_TAG}.sh"

          # -- Create output file names
          UPWELLING_TAPE_FILE="TAPE20"
          UPWELLING_TAU_LBLRTM_FILE="upwelling_tau.${ID_TAG}"
          UPWELLING_TAU_NETCDF_FILE="${UPWELLING_TAU_LBLRTM_FILE}.nc"

          # -- Create LBLRTM/HITRAN version string
          LBLRTM_HITRAN_VERSION="LBLRTM v9.4; HITRAN 2000 + AER updates"

          # -- Start the script file build
          #echo "#!/bin/sh" > ${SCRIPT_FILE}
          #echo >> ${SCRIPT_FILE}
          #echo "# --------------------------------------------" >> ${SCRIPT_FILE}
          #echo "# LBLRTM transmittance apodization run script" >> ${SCRIPT_FILE}
          #echo "# file for the output netCDF file:"             >> ${SCRIPT_FILE}
          #echo "# ${UPWELLING_TAU_NETCDF_FILE}"                 >> ${SCRIPT_FILE}
          #echo "# --------------------------------------------" >> ${SCRIPT_FILE}


          # -- Change to the directory containing the
          # -- current TAPE5 file
          #echo >> ${SCRIPT_FILE}; echo >> ${SCRIPT_FILE}
          #echo "# ----------------------------" >> ${SCRIPT_FILE}
          #echo "# Change to required directory" >> ${SCRIPT_FILE}
          #echo "# ----------------------------" >> ${SCRIPT_FILE}
          #echo >> ${SCRIPT_FILE}
          #echo "cd ${ROOT_DIR}" >> ${SCRIPT_FILE}

          # -- Add time stamp to the error log file
          #echo >> ${SCRIPT_FILE}; echo >> ${SCRIPT_FILE}
          #echo "# --------------------------------------" >> ${SCRIPT_FILE}
          #echo "# Add begin time stamp to error log file" >> ${SCRIPT_FILE}
          #echo "# --------------------------------------" >> ${SCRIPT_FILE}
          #echo >> ${SCRIPT_FILE}
          #echo "echo >> ${RESULTS_DIR}/${ERROR_LOG_FILE}; echo '--------------------------' >> ${RESULTS_DIR}/${ERROR_LOG_FILE}" >> ${SCRIPT_FILE}
          #echo "echo \"Processing run started at:  \`date\`\" >> ${RESULTS_DIR}/${ERROR_LOG_FILE}" >> ${SCRIPT_FILE}

          # -- Start the LBLRTM run
          #echo >> ${SCRIPT_FILE}; echo >> ${SCRIPT_FILE}
          #echo "# ---------------------" >> ${SCRIPT_FILE}
          #echo "# Start Apodization run" >> ${SCRIPT_FILE}
          #echo "# ---------------------" >> ${SCRIPT_FILE}
          #echo >> ${SCRIPT_FILE}
          #echo "cd ${RESULTS_DIR}" >> ${SCRIPT_FILE}
          #ISENSOR=3 # -- iSensor = IASI
          #IDIR=1    # -- iDir = UPWELLING_DIRECTION (See Tau_Production_Parameters.f90)
          #echo "ulimit -s unlimited" >> ${SCRIPT_FILE}
          #echo "${Apodize_TauSpc_with_IRF} ${UPWELLING_TAU_LBLRTM_FILE}.nc.signal ${PROFILE_SET_NUMBER} ${THIS_TAPE3} ${THIS_PROFILE} ${THIS_ANGLE} ${IDIR} ${ISENSOR} ${THIS_BAND}"  >> ${SCRIPT_FILE}

          # -- Remove the script files
          #echo >> ${SCRIPT_FILE}; echo >> ${SCRIPT_FILE}
          #echo "# ----------------------" >> ${SCRIPT_FILE}
          #echo "# Remove the script file" >> ${SCRIPT_FILE}
          #echo "# ----------------------" >> ${SCRIPT_FILE}
          #echo >> ${SCRIPT_FILE}
          #echo "cd ${ROOT_DIR}" >> ${SCRIPT_FILE}
          #echo "# rm -f ${SCRIPT_FILE} 2>>${RESULTS_DIR}/${ERROR_LOG_FILE}" >> ${SCRIPT_FILE}

          # -- Add time stamp to the error log file
          #echo >> ${SCRIPT_FILE}; echo >> ${SCRIPT_FILE}
          #echo "# ------------------------------------" >> ${SCRIPT_FILE}
          #echo "# Add end time stamp to error log file" >> ${SCRIPT_FILE}
          #echo "# ------------------------------------" >> ${SCRIPT_FILE}
          #echo >> ${SCRIPT_FILE}
          #echo "echo \"Processing run finished at: \`date\`\" >> ${RESULTS_DIR}/${ERROR_LOG_FILE}" >> ${SCRIPT_FILE}


          # -- Make the script file executable
          #chmod 700 ${SCRIPT_FILE}


          # ------------------------------------------------------------
          # Add the current script as a job step in the job command file
          # ------------------------------------------------------------

          echo ${ID_TAG}

          # Move script file and tape5 to the directory where results are saved
          #mv ${SCRIPT_FILE} ${RESULTS_DIR}/
         

         #if [ $THIS_BAND -gt 48 ] && [ $THIS_BAND -lt 85 ] 
         #then
         #EXEC_BAND="${EXEC_BAND} cd ${RESULTS_DIR}; srun --export=ALL --share ${SCRIPT_FILE}; \rm -f ${SCRIPT_FILE}; cd ${ROOT_DIR}; "
         #fi
          # ------------------
          # Increment counters
          # ------------------

          N_BAND_JOBS=`expr ${N_BAND_JOBS} + 1`
          N_JOBS=`expr ${N_JOBS} + 1`

          THIS_BAND=`expr ${THIS_BAND} + 1`

        done  # BAND LOOP


        # ---------------------------
        # Submit the job command file
        # ---------------------------
        # the jobs were submitted on the fly

        echo "      Number of spectral band job steps in ${JOB_COMMAND_FILE}: ${N_BAND_JOBS}"

#        # Submit the job on the fly
#        sbatch <<EOF
# #!/bin/bash
# # Job step for: ${ID_TAG}
# #SBATCH --partition=serial
# #SBATCH --export=ALL
# #SBATCH --ntasks=1
# #SBATCH --nodes=1  # should be equal to ntasks
# #SBATCH --cpus-per-task=1
# #SBATCH --mem-per-cpu=4gb  
# #SBATCH --time=1:00:00
# #SBATCH --output=output/${SCRIPT_FILE}.apod.out
# #SBATCH --error=output/${SCRIPT_FILE}.apod.err
##SBATCH --job-name=Apod_${PROFILE_TAG}_${ANGLE_TAG}_${TAPE3_TAG}
# ${EXEC_BAND}
# EOF

        # -----------------------------------
        # Increment molecule set job counter
        # -----------------------------------

        N_TAPE3_JOBS=`expr ${N_TAPE3_JOBS} + 1`


      done  # TAPE3 loop

      echo "    Number of molecule set jobs submitted: ${N_TAPE3_JOBS}"


      # ------------------------
      # Increment ANGLE counters
      # ------------------------

      N_ANGLE_JOBS=`expr ${N_ANGLE_JOBS} + 1`
      THIS_ANGLE=`expr ${THIS_ANGLE} + 1`

    done  # ANGLE LOOP

    echo "  Number of angle jobs submitted: ${N_ANGLE_JOBS}"

    # -------------------------
    # Increment profile counter
    # -------------------------

    N_PROFILE_JOBS=`expr ${N_PROFILE_JOBS} + 1`
    THIS_PROFILE=`expr ${THIS_PROFILE} + 1`

  done  # PROFILE LOOP

  sbatch --array=1-${N_JOBS} --export=ALL --share apodize_slurm_template.sh

  # -----------------------------------------
  # Output the total number of submitted jobs
  # -----------------------------------------

  echo "Number of profile jobs submitted: ${N_PROFILE_JOBS}"
  echo "Total number of submitted jobs: ${N_JOBS}"

exit
