#!/usr/bin/python3

'''@package docstring

Script name: odps_regression.py

Usage: ./odps_regression.py

Inputs:

Output: ODPS transmittance coefficients

Required input files:

  - Configuration file: 

Copyright UCAR 2022

License: CC0


Record of revisions:
====================

Date:        Author:               Description:
=====        =======               ============
2022-09-16   P. Stegmann           Initial code skeleton.
2022-10-06   P. Stegmann           Adding procedures from mod_odps.
2022-10-07   P. Stegmann           Completing initialization loops.


'''

## stl modules
import os 
from multiprocessing import Pool # Concurrency
## local modules
import mod_odps as mo
import mod_config as mc
import mod_instructions as mi


if __name__ == '__main__':

	#----------------------
	# Read JSON config file
	#----------------------
	config_file_name = 'parameter_namelist'
	config = mc.Load_Configuration(config_file_name)

	## Create list for odpsParams objects:
	odps_parameters = []

	#----------------------
	# Loop over sensors
	#----------------------
	SensorList = config["instrument"]
	for SatSen in SensorList:
		print("SatSen: ", SatSen)

		#----------------------
		# Loop over gases
		#----------------------
		GasList = []
		if config["IR_type"] == 1:
			GasList = config["component_group_1"]
		elif config["IR_type"] == 2:
			GasList = config["component_group_2"]
		elif config["IR_type"] == 3:
			GasList = config["component_group_3"]
		else:
			print("Error in the IR_Type configuration!")
		print("GasList: ", GasList)
		idx = 0
		for GasName in GasList:
			print("Absorber gas:", GasName)

			#----------------------
			# Loop over channels
			#----------------------
			idx += 1
			for Current_Channel in range(config["start_channel"],config["end_channel"]+1):

				#----------------------
				# Loop over angles
				#----------------------

				for Current_Angle in range(config["start_angle"],config["end_angle"]+1):
					odps_param_temp = mi.odpsParams( 
						os.getcwd(), \
						config["prof_set"], \
						SatSen, \
						GasName, \
						Current_Channel, \
						config["exe_file"], \
						idx, \
						Current_Angle, \
						config["spc_coeff_dir"], \
						config["atm_profile_file"], \
						config["tau_profile_dir"], \
						"NotExist" )
						#'TauCoeff.'+SatSen+config["prof_set"]+'.nc', \
						#) 
					# Append temporary ODPS parameters to list:
					odps_parameters.append(odps_param_temp)

	#----------------------------------------------------
	#
	# Concurrent execution of the ODPS regression process
	#
	# for each:
	#
	#  - instrument
	#  - absorber gas
	#  - instrument channel
	#  - slant path angle
	#
	#-----------------------------------------------------
	with Pool(config["max_cpu"]) as p:
		p.map(mo.Compute_ODPS_Regression,odps_parameters)


