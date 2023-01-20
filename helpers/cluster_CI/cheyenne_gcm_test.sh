#!/bin/bash
#
# Script extracts GCM test case on Cheyenne and produces icar_options.nml
# files to test
#

# sed_exe is used since on OSX the empty string '' is needed after -i
sed_exe="sed -i"
nml_f=icar_options.nml
summary_f=summary.txt

### Functions ###
function main {
    prepare_cheyenne_gcm_test_tar
    prepare_icar_exe

    # prepare namelists
    declare -A physics_permutations
    cp icar_gcm_test/icar_options.nml ${nml_f}
    gen_physics_namelist_permutations
    run_physics_namelist_permutations
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

function write_physics_namelist_permutation {
    # save phys opts for later output formatting
    physics_permutations[${file_n}]="${pbl_opt} ${lsm_opt} ${water_opt} ${mp_opt}\
 ${rad_opt} ${conv_opt} ${adv_opt} ${wind_opt}"
    echo "  ${physics_permutations[${file_n}]}"
    echo "${physics_permutations[${file_n}]}" >> ${summary_f}

    ((file_n+=1))
    f=new_${file_n}_${nml_f}
    cp ${nml_f} ${f}
    fix_version_num ${f}
    write_physics_opts ${pbl_opt} ${lsm_opt} ${water_opt} ${mp_opt} \
		       ${rad_opt} ${conv_opt} ${adv_opt} ${wind_opt} ${f}
}

function run_physics_namelist_permutations {
    i=0
    # copy new namelist file into test directory and run it
    new_nml_files=$(ls new_*_icar_options.nml)
    for nml in ${new_nml_files}; do
	cp ${nml} icar_gcm_test
	cd icar_gcm_test
	start=`date +%s`
	echo "./icar ${nml}"
	end=`date +%s`
	user_time=$((end-start))
	output_s="${nml} | runtime ${user_time} s. | phys_opt = ${physics_permutations[${i}]}"
	echo "${output_s}"
	echo "${output_s}" >> ../${summary_f}
	((i+=1))
	cd ..
    done
    echo "performance timings are also in ${summary_f}"
}

function gen_physics_namelist_permutations {
    file_n=0
    echo "Physics Permutations:" >> ${summary_f}
    # Note: the physics options line up as follows:
    #                      pbl   lsm   water mp    rad   conv  adv   wind
    # for physics_opts in {0..2}{0..4}{0..3}{0..6}{0..3}{0..5}{0..2}{0..5}

    echo "Physics namelist permutations generated:"
    # # mp 1 adv 1 wind 1235
    # wind_opt_skip=(4)
    # for physics_opts in {0..0}{0..0}{0..0}{1..1}{0..0}{0..0}{1..1}{1..5}
    # do
    # 	parse_physics_opts ${physics_opts}
    # 	[[  " ${wind_opt_skip[*]} " =~ " ${wind_opt} " ]] && continue
    # 	write_physics_namelist_permutation
    # done

    # # mp 23456 adv 1 wind 3
    # for physics_opts in {0..0}{0..0}{0..0}{2..6}{0..0}{0..0}{1..1}{3..3}
    # do
    # 	parse_physics_opts ${physics_opts}
    # 	write_physics_namelist_permutation
    # done

    # # pbl 2 lsm 3 water 2 mp 1 rad 2 conv 0145  adv 1 wind 3
    # conv_opt_skip=(2 3)
    # for physics_opts in {2..2}{3..3}{2..2}{1..1}{2..2}{0..5}{1..1}{3..3}
    # do
    # 	parse_physics_opts ${physics_opts}
    # 	[[  " ${conv_opt_skip[*]} " =~ " ${conv_opt} " ]] && continue
    # 	write_physics_namelist_permutation
    # done

    # pbl 2 lsm 3 water 2 mp 1 rad 3 conv 0 adv 1 wind 3
    for physics_opts in {2..2}{3..3}{2..2}{1..1}{3..3}{0..0}{1..1}{3..3}
    do
	parse_physics_opts ${physics_opts}
	write_physics_namelist_permutation
    done

    # pbl 2 lsm 4 water 3 mp 1 rad 2 conv 0 adv 1 wind 3
    for physics_opts in {2..2}{4..4}{3..3}{1..1}{2..2}{0..0}{1..1}{3..3}
    do
	parse_physics_opts ${physics_opts}
	write_physics_namelist_permutation
    done

    echo "${file_n} files produced"
}

main "$@"
