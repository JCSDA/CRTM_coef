#!/bin/bash

BASE="viirs-m_j2"

for ((ii=12; ii<17; ii++))
do
  old="${BASE}_ch${ii}.txt"
  new="viirs-m_n21_ch${ii}.txt"
  echo $old
  echo $new
  mv $old $new
done






