# fix
The `CRTM_coef/inbox/fix/` directory is the literal *inbox* for new coefficients that were produced using the CRTM_coef package.
A new coefficient delivery as of CRTM v2.4.1 needs to include the following items:
* The *actual* TauCoeff and SpcCoeff coefficient files in the following formats:
	* Little-endian binary
	* Big-endian binary
	* netCDF4
* oSRF data files in netCDF4 format
* Configuration files for the coefficient generation
* Documentation in Latex format on the coefficient generation

