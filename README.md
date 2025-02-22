### Overview 

This repository contains a collection of functions used to run the simulations for assessing controllability metrics in VARs. 

The `controlOrdTS` library should be installed in a conda environment. 
The conda environment should also contain the `netcontrol` package.

We also installed `tseries` (and associated packages for local copy of the environment)
- This was to handle the empirical data section
- This does not affect any computations done on the server

We also need the `Amelia` package for imputation of the empirical data (local only)
Also installed `graphicalVAR` for analysis of empirical data (local only)
Added `fit_lavaan_VAR` to exports in local installation of `controlOrdTS` (local only)
Installed `vars` locally for debugging (local only)
