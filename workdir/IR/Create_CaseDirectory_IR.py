#!/usr/bin/python3
# -*- coding: utf-8 -*-

'''

 File: Create_CaseDirectoryi_IR.py 

 Description:
  This python script creates an IR case
  directory for CRTM instrument coeff-
  icient generation.


 Copyright UCAR, 2021-2022


 Record of Revisions:

  Date:       Author:       Description:
  2021-12-03  P. Stegmann   Original Code
  2022-11-14  P. Stegmann   Modification for IR case

'''

import sys
import getopt
import os

def input(argv):
  instrument_name = ''
  try:
    opts, args = getopt.getopt(argv,"hi:",["iname="])
  except getopt.GetoptError:
    print('Create_CaseDirectory_IR.py -i <instrument name>')
    sys.exit(2)
  for opt, arg in opts:
    if opt == '-h':
      print('Create_CaseDirectory_IR.py -i <instrument name>')
      sys.exit()
    elif opt in ("-i", "--iname"):
      instrument_name = arg
      print('Instrument name is ', instrument_name)
    else:
      print('Unrecognized input!')
      sys.exit(2)
  return instrument_name

def make_dir(path):
  try: 
    # Attempt to create the directory
    if not os.path.exists(path):
      print("Creating new case directory: ", path)
      os.mkdir(path) 
    else:
      print("The directory already exists: ", path)
  except OSError as error: 
    print(error)  
  return

def link_executables(destination):
  try:
    executable_names = ['var.out',\
                        'Create_SpcCoeff',\
                        'SpcCoeff_NC2BIN']
    executable_dirs = ['oSRF/oSRF_Create_from_ASCII/',\
                       'SpcCoeff/Create_SpcCoeff/',\
                       'SpcCoeff/SpcCoeff_NC2BIN/']
    relative_base_dir = '../../src/mains/'
    for ii in range(0,len(executable_dirs)):
      executable_dirs[ii] = relative_base_dir + executable_dirs[ii] + executable_names[ii]
    current_dir = os.path.dirname(__file__)
    for ii in range(0,len(executable_dirs)):
      filename = os.path.join(current_dir, executable_dirs[ii])
      print("Linking executable: ",filename)
      os.link(filename,os.path.join(destination,executable_names[ii]))
  except OSError as error:
    print(error)
  return

if __name__ == "__main__":
  instrument_name = input(sys.argv[1:])
  cwdir = os.getcwd()
  mwdir = instrument_name
  case_directory = os.path.join(cwdir,mwdir)
  make_dir( case_directory )
  link_executables( case_directory )
