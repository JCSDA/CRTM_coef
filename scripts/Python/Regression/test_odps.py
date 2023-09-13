#!/usr/bin/python3

'''@package docstring

Description:
============
Unit testing code for the following modules:
- mod_config
- mod_odps

Script name: test_odps.py

Usage: ./test_odps.py

Copyright UCAR 2022

License: CC0


Record of revisions:
====================

Date:        Author:               Description:
=====        =======               ============
2022-10-06   P. Stegmann           Initial code skeleton


'''

import unittest
import mod_config as mc
import mod_odps as mo


class Test_ODPS(unittest.TestCase):
  """
    Description:
    Unit test class for mod_config and mod_odps
  """
  def test_loadConfig(self):
    config = mc.Load_Configuration('parameter_namelist')
    print(config)

  def test_NameList(self):
    nl = mo.NameList()
    print(type(nl))

  def test_NameList_substitute(self):
    nl = mo.NameList()
    namelist_dict = {'SatSen':"amsua_metop-c",\
    'TopSeqCh':'001',\
    'LastSeqCh':'001',\
    'COMP_IDX':2,\
    'Nincang':7,\
    'SpcCoeffFile':"../../../../cases/amsua_metop-c_benchmark/amsua_metop-c.SpcCoeff.nc",\
    'ATM_PROFLE_FILE':"../../../../src/TauProd/Microwave/Compute_MW_Transmittance/ECMWF83.AtmProfile.nc",\
    'TauProfileFile':"../../../../cases/amsua_metop-c_benchmark/upwelling.amsua_metop-c.TauProfile.nc",\
    'TauCoeffFile':"NotExist"}
    nl.create_NameList(namelist_dict)
    print(nl.listText)

    validation_string = """

    &SATSEN
      File_Prefix  = "amsua_metop-c",
      Ichan_start    =  001,
      Ichan_end   =  001 
/
    &GENCOEF
      icom = 2,
      Nangle_regression    = 7      
/
    &FILENAMES
      inFilename_spcCoef = "../../../../cases/amsua_metop-c_benchmark/amsua_metop-c.SpcCoeff.nc",
      inFilename_atmProfile = "../../../../src/TauProd/Microwave/Compute_MW_Transmittance/ECMWF83.AtmProfile.nc",
      inFilename_tauProfile = "../../../../cases/amsua_metop-c_benchmark/upwelling.amsua_metop-c.TauProfile.nc",
      inFilename_tauCoeff = "NotExist"
/     
    """
    assert(nl.listText.strip() == validation_string.strip())


if __name__ == "__main__":
  print("Starting checks.")
  unittest.main()