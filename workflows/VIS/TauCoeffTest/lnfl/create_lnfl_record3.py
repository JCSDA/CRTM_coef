'''
 (M):  AVAILABLE  ( 1)  H2O  ( 2)  CO2  ( 3)    O3 ( 4)   N2O ( 5)    CO ( 6)   CH4 ( 7)    O2 
       MOLECULAR  ( 8)   NO  ( 9)  SO2  (10)   NO2 (11)   NH3 (12)  HNO3 (13)    OH (14)    HF 
        SPECIES   (15)  HCL  (16)  HBR  (17)    HI (18)   CLO (19)   OCS (20)  H2CO (21)  HOCL 
                  (22)   N2  (23)  HCN  (24) CH3CL (25)  H2O2 (26)  C2H2 (27)  C2H6 (28)   PH3 
                  (29) COF2  (30)  SF6  (31)   H2S (32) HCOOH (33)   HO2 (34)     O (35)CLONO2 
                  (36)  NO+  (37) HOBR  (38)  C2H4 (39) C3HOH (40) CH3Br (41) CH3CN (42)   CF4 
                  (43) C4H2  (44) HC3N  (45)    H2 (46)    CS (47)   SO3  
'''
import sys
from collections import OrderedDict

species = OrderedDict([
           ('H2O', 0),  ('CO2',  0),  ('O3',    0),  ('N2O',   0),  ('CO',    0),  ('CH4',   0),  ('O2',     0),
           ('NO',  0),  ('SO2',  0),  ('NO2',   0),  ('NH3',   0),  ('HNO3',  0),  ('OH',    0),  ('HF',     0),
           ('HCL', 0),  ('HBR',  0),  ('HI',    0),  ('CLO',   0),  ('OCS',   0),  ('H2CO',  0),  ('HOCL',   0),
           ('N2',  0),  ('HCN',  0),  ('CH3CL', 0),  ('H2O2',  0),  ('C2H2',  0),  ('C2H6',  0),  ('PH3',    0),
           ('COF2',0),  ('SF6',  0),  ('H2S',   0),  ('HCOOH', 0),  ('HO2',   0),  ('O',     0),  ('CLONO2', 0),
           ('NO+', 0),  ('HOBR', 0),  ('C2H4',  0),  ('C3HOH', 0),  ('CH3Br', 0),  ('CH3CN', 0),  ('CF4',    0),
           ('C4H2',0),  ('HC3N', 0),  ('H2',    0),  ('CS',    0),  ('SO3',   0)])


print('''
 (M):  AVAILABLE  ( 1)  H2O  ( 2)  CO2  ( 3)    O3 ( 4)   N2O ( 5)    CO ( 6)   CH4 ( 7)    O2 
       MOLECULAR  ( 8)   NO  ( 9)  SO2  (10)   NO2 (11)   NH3 (12)  HNO3 (13)    OH (14)    HF 
        SPECIES   (15)  HCL  (16)  HBR  (17)    HI (18)   CLO (19)   OCS (20)  H2CO (21)  HOCL 
                  (22)   N2  (23)  HCN  (24) CH3CL (25)  H2O2 (26)  C2H2 (27)  C2H6 (28)   PH3 
                  (29) COF2  (30)  SF6  (31)   H2S (32) HCOOH (33)   HO2 (34)     O (35)CLONO2 
                  (36)  NO+  (37) HOBR  (38)  C2H4 (39) C3HOH (40) CH3Br (41) CH3CN (42)   CF4 
                  (43) C4H2  (44) HC3N  (45)    H2 (46)    CS (47)   SO3  
''')

'''
           VMIN,   VMAX
   
           1-10,  11-20
   
          F10.3,  F10.3
   
           VMIN    low wavenumber limit for the line file
                   VMIN should be 25 cm-1 less than V1 for LBLRTM calculation
   
           VMAX    high wavenumber limit for the line file
                   VMAX should be 25 cm-1 greater than V2 for LBLRTM calculation
'''

print("Enter Vmin  and Vmax and list of species separated by : [e.g., 0.0  20000.0 H2O:O3:CO2]")

wavenumber = [float(x) for x in sys.argv[1:3]]
selected_species = sys.argv[3].split(":")

print("vmin %d vmax %d"%(wavenumber[0], wavenumber[1]))
print("selected species: ", end="")
print(selected_species)

fid = open("TAPE5", "w")
fid.write("$ f100 format \n")
fid.write("%10.3f%10.3f\n"%(wavenumber[0], wavenumber[1]))

for s in selected_species:
    print("set %s to 1"%s)
    species[s] = 1

for s in species:
    print(species[s], end="") # print on the same line
    fid.write("%i"%species[s])

print("    LNOUT")
fid.write("    LNOUT")
