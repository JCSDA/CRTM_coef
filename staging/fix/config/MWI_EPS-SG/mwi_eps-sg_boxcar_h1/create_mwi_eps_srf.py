'''

Description:
============
Simple Python script to create and plot a set of boxcar
SRFs for multiple channels of a single MW instrument.


Copyright Patrick Stegmann 2021

Record of Revisions:
====================

Date:      Author:            Description:
=====      =======            ============
2021-12-21 P. Stegmann        Rewrite for MWI EPS-SG.


'''

import numpy as np
import matplotlib.pyplot as plt

# MWI EPS-SG Channels 1 to 8
center_frequencies = np.array([18.7, 23.8, 31.4, 50.3, 52.610, 53.24, 53.750, 89.0])
bandwidth = np.array([0.2,0.4,0.2,0.4,0.4,0.4,0.4,4.0])
bandwidth = bandwidth/2.0

epsilon = 1e-9

plt.figure()

for ii in range(8):
	band = np.arange(center_frequencies[ii]-bandwidth[ii],
		               center_frequencies[ii]+bandwidth[ii],
		               step=0.001) # channel ii band
	srf = np.zeros((band.size))  # channel ii SRF
	srf[0] = epsilon
	srf[-1] = epsilon
	for jj in range(1,srf.size-1):
		srf[jj] = 1./bandwidth[ii]
	dat_out = np.transpose(np.array((band,srf)))
	np.savetxt('mwi_eps-sg_v1_ch'+str(ii+1)+'.txt',dat_out) # ASCII output
	plt.plot(band,srf,label='Channel '+str(ii+1))

# MWI EPS-SG Channels 9 to 18 (dual band channels)
center_frequencies = np.array([118.7503, 118.7503, 118.7503, 118.7503, 165.5, 183.31, 183.31, 183.31, 183.31, 183.31])
bandwidth = np.array([0.5,0.4,0.4,0.4,1.350,2.000,1.500,1.500,1.500,1.500])
bandwidth = bandwidth/2.0
offset = np.array([3.20,2.10,1.40,1.20,0.75,7.0,6.1,4.9,3.4,2.0])

def dualband_boxcar(frq, centerfrq, bndwdth, ofst):
	if ( frq > (centerfrq - ofst - bndwdth) and frq < (centerfrq - ofst + bndwdth) ):
		srf_val = 1./bndwdth
	elif ( frq > (centerfrq + ofst - bndwdth) and frq < (centerfrq + ofst + bndwdth) ):
		srf_val = 1./bndwdth
	else:
		srf_val = 0.0 
	return srf_val

for ii in range(0,10):
	band = np.arange(center_frequencies[ii]-offset[ii]-bandwidth[ii],
		               center_frequencies[ii]+offset[ii]+bandwidth[ii],
		               step=0.001) # channel ii band
	srf = np.zeros((band.size))  # channel ii SRF
	srf[0] = epsilon
	srf[-1] = epsilon
	for jj in range(1,srf.size-1):
		srf[jj] = dualband_boxcar( band[jj], center_frequencies[ii], bandwidth[ii], offset[ii] )
	dat_out = np.transpose(np.array((band,srf)))
	np.savetxt('mwi_eps-sg_v1_ch'+str(ii+9)+'.txt',dat_out) # ASCII output
	plt.plot(band,srf,label='Channel '+str(ii+9))


plt.xlabel(r'Frequency $f$ [GHz]')
plt.ylabel(r'SRF $\Phi$ $\left[ \frac{1}{GHz} \right]$')
plt.legend()
plt.show()

