'''@package docstring

Script name: mod_config.py

Usage: import mod_config

Copyright UCAR 2022

License: CC0


Record of revisions:
====================

Date:        Author:               Description:
=====        =======               ============
2022-09-17   P. Stegmann           Initial code skeleton


'''

# stl modules
import json # Configuration

def Load_Configuration(config_file_name):
	#----------------------------------------------------------------------------------------
	# @brief:Description:
	#			   send a jobs to a job queue, and if all the assigned number (MAX_CPUs) of
	#                          CUPs are running, jobs in the queue are on the waiting status until there
	#                          are freed CPUs.  
	#                          Note that the script is terminated if the waiting time exceeds
	#                          one-day, a situation most likely caused by errors if anticipated execution time
	#                          of a job is much less than 24 hours.
	# MAX_CPUs		== the number of CPUs to use
	# CH_INT                == channel increment, i.e the number of channles for a job to process. Default=1
	# EXE_FILE 		== exe file to search a set of best predictors
	# WORK_DIR 		== top directory holding execution directory. results, and etc.
	# PROF_SET 		== profile set tag, i.g "umbc48101" or "cimss32101"
	# SPC_COEFF_DIR 	== directory holding *.SpcCoeff.nc files
	# TAU_PROFILE_DIR 	== directory hodling *.TauProfile.nc files
	# ATM_PROFLE_FILE	== file containing atmospheric profile set (netCDF), should match
	#			   with $PROF_SET
	# SENSOR_INFO_FILE      == the sensor info file SensorInfo
	# GET_SEN_INFO          == the fortran program returing sensor type information from the SpcCoeff file
	# IR_TYPE               == IR sensor transmittance type.
	#                            IF IR_TYPE=1 THEN the transmittance components are given by COMPONENT_GROUP1
	#                            IF IR_TYPE=2 THEN the transmittance components are given by COMPONENT_GROUP2
	#                            (see the parameters COMPONENT_GROUP1 & COMPONENT_GROUP2 below
	# COMPONENTS            == a subset of component to be processed in the list COMPONENT_GROUP1, 
	#                          COMPONENT_GROUP2, or COMPONENT_GROUP3 
	#                          IF COMPONENTS=DEFAULT THEN 
	#                             for MW sensor the components are given by COMPONENT_GROUP3
	#                             for IR sensor, the components are given by COMPONENT_GROUP1 if IR_TYPE=1 and
	#                             are given by COMPONENT_GROUP2 if IR_TYPE=2
	# COMPONENT_GROUP1      == list of component group 1
	# COMPONENT_GROUP2      == list of component group 2
	# COMPONENT_GROUP3      == list of component group 3
	#----------------------------------------------------------------------------------------

	## Load json string from text file
	with open(config_file_name+'.json', 'r') as config_file:
		parameter_namelist = json.load(config_file)
	print(type(parameter_namelist))
	## Deserializing json string
	#configuration = json.loads(parameter_namelist)
	return parameter_namelist