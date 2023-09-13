'''

Description:
============
Simple Python script to create and plot a set of boxcar
SRFs for multiple channels of a single MW instrument.


Copyright Patrick Stegmann 2021

Record of Revisions:
====================


'''

import numpy as np
import matplotlib.pyplot as plt

band1 = np.linspace(20,30,num=100) # MW Frequency array.

epsilon = 1e-9
jj = 0
srf = np.zeros((398,3)) # Actual boxcar SRF array for output:
                        # - Index (:,0): Channel no.
                        # - Index (:,1): Frequency
                        # - Index (:,2): Normalized SRF value.

for ii in range(0,99):
	srf[jj,0] = ii   # channel
	srf[jj+1,0] = ii 
	srf[jj+2,0] = ii
	srf[jj+3,0] = ii 

	srf[jj,1] = band1[ii] - epsilon# frequency []
	srf[jj+1,1] = band1[ii]
	srf[jj+2,1] = band1[ii+1]
	srf[jj+3,1] = band1[ii+1] + epsilon

	srf[jj,2] = 0 # SRF Phi [-]
	srf[jj+1,2] = 1./100.
	srf[jj+2,2] = 1./100.
	srf[jj+3,2] = 0
	jj = jj + 4 


print(jj)
print(srf[99:,1])

np.savetxt('boxcar_srf.txt',srf) # ASCII output
plt.semilogy(srf[:,1],srf[:,2],lw=0.3)
plt.xlabel(r'Wavenumber $\nu$ $[cm^{-1}]$')
plt.ylabel(r'SRF $\Phi$ $\left[ cm \right]$')
plt.show()

plt.figure()
for ii in range(2,98):
	x = [band1[ii-1],band1[ii],band1[ii],band1[ii+1],band1[ii+1],band1[ii+2]]
	y = [0.,0.,1./100, 1./100,0.,0.]
	plt.semilogy(x,y,'b',lw=0.1)

plt.xlabel(r'Frequency $f$ [GHz]')
plt.ylabel(r'SRF $\Phi$ $\left[ \frac{1}{MHz} \right]$')
plt.show()


