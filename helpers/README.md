# Install Python Dependencies
The following instructions and dependecy files work for the core ICAR scripts.
Tools in `make_domain.py` and ccsm, cesm, cmip, erai, and wrf directories will require Bunch and mygis packages as well.
The Python script `ideal_linear.py` will require Nio to be installed with `pip install nio`.

## Install With Conda
```bash
$ conda env create -f environment.yml --prefix /path/to/install/icar_env
$ conda activate icar_env
```

## Install With Pip
```bash
$ pip install -r requirements.txt
```
