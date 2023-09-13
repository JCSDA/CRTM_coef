'''@package docstring

Module name: mod_instructions.py

Usage: import mod_instructions

Copyright UCAR 2022

License: CC0


Record of revisions:
====================

Date:        Author:               Description:
=====        =======               ============
2022-10-03   P. Stegmann           Initial code skeleton

'''

class odpsParams:
	'''
	@brief:Description:
	The odpsParams class is a simple container that
	carries a single instance of the full configuration
	of the ODPS process.
	'''
	def __init__(self, \
		workdir, \
		prof_set, \
		instrument, \
		absorber, \
		channel, \
		odps_executable, \
		comp_idx, \
		nincang, \
		SpcCoeffFile, \
		Atmprofile_File, \
		TauProfileFile, \
		TauCoeffFile):
		self.workdir = workdir
		self.prof_set = prof_set
		self.instrument = instrument
		self.absorber = absorber
		self.channel = channel
		self.executable = odps_executable
		self.comp_idx = comp_idx
		self.nincang = nincang
		self.SpcCoeffFile = SpcCoeffFile
		self.Atmprofile_File = Atmprofile_File
		self.TauProfileFile = TauProfileFile
		self.TauCoeffFile = TauCoeffFile

