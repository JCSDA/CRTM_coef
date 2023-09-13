#!/bin/bash

BASE="TROPICS_SV2_passband_MIT_LL_Ch"

for ((ii=10; ii<13; ii++))
do
  old="${BASE}${ii}.txt"
  new="tropics_sv2_ch${ii}.txt"
  echo $old
  echo $new
  mv $old $new
done






