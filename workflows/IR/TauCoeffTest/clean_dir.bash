#!/bin/bash

if [ $1 == "all" ]; then
\rm -rf ODd* *.jcf *.sh *.rdk profile* TAPE10 TAPE11 TAPE12 TAPE13 TAPE20 TAPE6 TAPE7 TAPE9 output *.signal *.log
mkdir output
fi


if [ $1 == "tape5" ]; then
\rm -rf profile*/angle*/*/band*/*
fi

if [ $1 == "TauSpc" ]; then
for d in profile*; do
\rm $d/angle*/*/SensorInfo
\rm -f ./$d/angle*/*/*.*
done
fi

if [ $1 == "TauProfile" ]; then
\rm -f process_TauProfile_files.upwelling.sh
\rm -f *.signal  
\rm -f *.log 
\rm -f Error_Log.*.upwelling
\rm -f TauProfile_data/upwelling.*.TauProfile.nc
fi

if [ $1 == "logfiles" ]; then
\rm -f ./output/*
fi


