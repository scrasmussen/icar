#!/bin/bash
#
# Script extracts GCM test case on Cheyenne and produces icar_options.nml
# files to test
#

# sed_exe is used since on OSX the empty string '' is needed after -i
sed_exe="sed -i"
nml_f=icar_options.nml

### Functions ###
function main {
    prepare_cheyenne_gcm_test_tar
    prepare_icar_exe

    # prepare namelists
    cp icar_gcm_test/icar_options.nml ${nml_f}
    gen_physics_namelist_permutations

    # copy new namelist file into test directory and run it
    new_nml_files=$(ls new_*_icar_options.nml)
    for nml in ${new_nml_files}; do
	cp ${nml} icar_gcm_test
	cd icar_gcm_test
	./icar ${nml}
	cd ..
    done
}

# copy Cheyenne test case, extract, cleanup
function prepare_cheyenne_gcm_test_tar {
    if [ ! -d icar_gcm_test ]; then
	if [ ! -f icar_gcm_test.tgz ]; then
	    cp /glade/work/gutmann/icar/benchmark/icar_gcm_test.tgz .
	fi
	tar zxf icar_gcm_test.tgz
	rm icar_gcm_test.tgz
    fi
}

function prepare_icar_exe {
    rm -f icar_gcm_test/icar
    cp ../../src/icar icar_gcm_test/
}

# from a string of the permutatation of physics options, for example
# "020010011", parse the characters into the variables
function parse_physics_opts {
    opt=$1
    pbl_opt=${opt:0:1}
    lsm_opt=${opt:1:1}
    water_opt=${opt:2:1}
    mp_opt=${opt:3:1}
    rad_opt=${opt:4:1}
    conv_opt=${opt:5:1}
    adv_opt=${opt:6:1}
    wind_opt=${opt:7:1}
}

function fix_version_num {
    ${sed_exe} "s#version *= *\"2.0\"#version=\"2.1\"#" ${1}
}

function write_physics_opts {
    ${sed_exe} "s#pbl *= *[0-9]#pbl = ${1}#" ${9}
    ${sed_exe} "s#lsm *= *[0-9]#lsm = ${2}#" ${9}
    ${sed_exe} "s#water *= *[0-9]#water = ${3}#" ${9}
    ${sed_exe} "s#mp *= *[0-9]#mp = ${4}#" ${9}
    ${sed_exe} "s#rad *= *[0-9]#rad = ${5}#" ${9}
    ${sed_exe} "s#conv *= *[0-9]#conv = ${6}#" ${9}
    ${sed_exe} "s#adv *= *[0-9]#adv = ${7}#" ${9}
    ${sed_exe} "s#wind *= *[0-9]#wind = ${8}#" ${9}
}

function gen_physics_namelist_permutations {
    file_n=0
    # lists with physics options to skip
    pbl_opt_skip=(1)


    # number of possibilities per physics options:
    #     pbl 2, lsm 4, water 3, mp 6, rad 3, conv 5, adv 2, wind 5
    # TODO NOTE: there are 67200 possible combinations, too many, slim down
    #                    pbl   lsm   water mp    rad   conv  adv   wind
    for physics_opts in {0..2}{0..0}{0..0}{0..1}{0..0}{0..0}{1..1}{1..1}
    do
	parse_physics_opts ${physics_opts}
	# skip physics options that aren't available
	[[  " ${pbl_opt_skip[*]} " =~ " ${pbl_opt} " ]] && continue

	# write physics options to file
	f=new_${file_n}_${nml_f}
	cp ${nml_f} ${f}
	fix_version_num ${f}

	echo "${pbl_opt} ${lsm_opt} ${water_opt} ${mp_opt} \
${rad_opt} ${conv_opt} ${adv_opt} ${wind_opt}"
	write_physics_opts ${pbl_opt} ${lsm_opt} ${water_opt} ${mp_opt} \
			   ${rad_opt} ${conv_opt} ${adv_opt} ${wind_opt} ${f}
	((file_n+=1))
    done

    echo "${file_n} files produced"
}

main "$@"
