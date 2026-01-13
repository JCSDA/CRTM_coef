#!/bin/bash

BASE="atms_j2"

for ((ii=1; ii<22; ii++))
do
  old="${BASE}_ch${ii}.txt"
  new="atms_n21_ch${ii}.txt"
  echo $old
  echo $new
  mv $old $new
done






