all: build

build: cp
	./gen_ideal_test.py

cp: build
	cp ../src/icar .

run:
	mpiexec -np 2 ./icar

clean:
	rm -f *~ icar

cleanall: clean
	rm -f *.nc icar_options.nml
