file=gen_basic_ideal_test.py
# file=gen_ideal_advection_test.py
# file=gen_parcel_test.py

all: clean g

build:
	(cd ../src; make)

cp:
	cp ../src/icar .

gen_basic_ideal_test.py:
	python3 $@
gen_ideal_advection_test.py:
	python3 $@
gen_parcel_test.py:
	python3 $@

# generate testing files
g: gen
gen: $(file)
	python3 $(file)

# Running ICAR
s: cp
	./icar
r: run
run: cp
	mpiexec -np 4 ./icar

# Aggregate output files
agg:
	python3 ../helpers/aggregate_parallel_files.py

clean:
	rm -f *~ icar icar_out_* icar_rst_*
	rm -f init.nc forcing.nc icar_options.nml init_parcel_conditions.txt
