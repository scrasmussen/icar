# Convected Air Parcels


## Prerequisites for Parcel Test Generation
* Clone copy of [ICAR](https://github.com/NCAR/icar) repo.
* Add `scrasmussen` version of fork.
```
$ git remote add scrasmussen https://github.com/scrasmussen/icar
$ git fetch scrasmussen
```
* Checkout `feature/air-parcels` branch.
```
$ git checkout scrasmussen/feature/air-parcels
```
* Build ICAR.

## Run Parcel Generation
* Change to `icar/tests` directory.
* Generate parcel test files with `$ ./gen_parcel_test.py`. Edit python script to change options if desired.
    * Note: See `icar/helpers/generateTestFiles/ICARoptions.py` for list of options that can generated for `icar_options.nml`
* Run icar with `$ ../src/icar icar_options.nml`

## Examine Parcel Output
Open Jupyter Notebook file `plot-parcels.ipynb` in `icar/tests` directory.
Note: the `ipywidgets` Python module is required. Here are [installation instructions](https://ipywidgets.readthedocs.io/en/latest/user_install.html).
```
$ jupyter-notebook plot-parcels.ipynb
```

Note: the notebook needs to be restarted after new runs to properly plot the new output, this needs to be fixed.