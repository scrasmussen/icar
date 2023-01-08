all: ci

cp_nml:
	cp ../../run/complete_icar_options.nml icar_options.nml

ci: cp_nml
	bash cheyenne_gcm_test.sh

clean:
	rm -f *~ complete_icar_options.nml icar_options.nml new_*_icar_options.nml
	rm -f icar_gcm_test.tgz
