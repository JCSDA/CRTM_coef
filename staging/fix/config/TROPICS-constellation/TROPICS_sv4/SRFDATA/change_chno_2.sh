#!/bin/bash

BASE="TROPICS_SV3_passband_MIT_LL_Ch"

for ((ii=1; ii<13; ii++))
do
  if ((ii<10));
  then 
  	old="${BASE}0${ii}.txt"
  else
  	old="${BASE}${ii}.txt"
  fi
  new="tropics_sv3_ch${ii}.txt"
  echo $old
  echo $new
  mv $old $new
done






