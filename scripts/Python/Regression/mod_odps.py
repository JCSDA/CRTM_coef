'''@package docstring

Script name: odps_regression.py

Usage: import mod_odps

Copyright UCAR 2022

License: CC0


Record of revisions:
====================

Date:        Author:               Description:
=====        =======               ============
2022-09-16   P. Stegmann           Initial code skeleton
2022-10-03   P. Stegmann           NameList class
2022-10-05   P. Stegmann           Compute_ODPS_Regression function

'''


# stl modules
import os
import subprocess
import mod_instructions
from string import Template 

class NameList(object):
	"""docstring for NameList"""
	def __init__(self):
		self.TextTemplate = Template("""

    &SATSEN
      File_Prefix  = "$SatSen",
      Ichan_start    =  $TopSeqCh,
      Ichan_end   =  $LastSeqCh 
/
    &GENCOEF
      icom = $COMP_IDX,
      Nangle_regression    = $Nincang      
/
    &FILENAMES
      inFilename_spcCoef = "$SpcCoeffFile",
      inFilename_atmProfile = "$ATM_PROFLE_FILE",
      inFilename_tauProfile = "$TauProfileFile",
      inFilename_tauCoeff = "$TauCoeffFile"
/     
		""")
		self.listText = ''

	def create_NameList(self, namelist_dict):
		try:
			self.listText = self.TextTemplate.substitute( **namelist_dict )
		except KeyError:
			print('Incomplete substitution resulted in KeyError!')
		return


def Compute_ODPS_Regression(odps_params):
	## Check if case directories exist:
	## If not: Create them one by one.
	if os.path.exists(odps_params.workdir):
		os.chdir(odps_params.workdir)
	else:
		print("Work directory for ODPS process doesn't exist!")
		error_status = -1
		return error_status
	prof_dir = odps_params.workdir+'/'+odps_params.prof_set
	if not os.path.exists(prof_dir):
		os.mkdir(prof_dir)
	os.chdir(prof_dir)
	instrument_dir = prof_dir+'/'+odps_params.instrument
	if not os.path.exists(instrument_dir):
		os.mkdir(instrument_dir)
	os.chdir(instrument_dir)
	gas_dir = instrument_dir+'/'+odps_params.absorber
	if not os.path.exists(gas_dir):
		os.mkdir(gas_dir)
	os.chdir(gas_dir)
	channel_dir = gas_dir+'/'+str(odps_params.channel)
	if not os.path.exists(channel_dir):
		os.mkdir(channel_dir)
	os.chdir(channel_dir)
	print(os.getcwd())

	## Create Namelist file for ODPS_Regress
	namelist_dict = {'SatSen':odps_params.instrument,\
	'TopSeqCh':odps_params.channel,\
	'LastSeqCh':odps_params.channel,\
	'COMP_IDX':odps_params.comp_idx,\
	'Nincang':odps_params.nincang,\
	'SpcCoeffFile':odps_params.SpcCoeffFile,\
	'ATM_PROFLE_FILE':odps_params.Atmprofile_File,\
	'TauProfileFile':odps_params.TauProfileFile,\
	'TauCoeffFile':odps_params.TauCoeffFile}
	list_text = NameList()
	list_text.create_NameList(namelist_dict)
	File_obj = open(file='Namelist.txt', mode='w', encoding='utf-8')
	File_obj.write(list_text.listText)
	File_obj.close()
	## Run ODPS executable
	command = [odps_params.executable, str(odps_params.comp_idx)]
	error_status = subprocess.run(command, check=False, capture_output=True)
	print(f"returncode = {error_status.returncode}")
	if error_status.returncode != 0:
		# there was an error, we assume the traceback was printed to stderr
		print("there was an error :\n")
		print(error_status.stderr.decode("utf-8"))
		print(error_status.stdout.decode("utf-8"))
	os.chdir(odps_params.workdir)
	return error_status

