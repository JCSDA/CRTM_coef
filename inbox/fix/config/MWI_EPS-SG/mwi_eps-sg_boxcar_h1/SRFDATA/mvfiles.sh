
list="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18"

for var in $list;
do
  cp mwi_eps-sg_v1_ch"$var".txt mwi_eps-sg_h1_ch"$var".txt
done
