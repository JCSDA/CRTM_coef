#!/bin/bash

BASE="TROPICS_SV2_passband_MIT_LL_Ch"

for ((ii=1; ii<10; ii++))
do
  old="${BASE}0${ii}.txt"
  new="${BASE}${ii}.txt"
  echo $old
  echo $new
  mv $old $new
done






