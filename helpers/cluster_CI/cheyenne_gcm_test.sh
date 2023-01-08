#!/bin/bash
#
# Script extracts GCM test case on Cheyenne and produces icar_options.nml
# files to test
#
nml_f=icar_options.nml

### Functions ###
# copy Cheyenne test case, extract, cleanup
function prepare_cheyenne_gcm_test {
    if [ ! -d icar_gcm_test ]; then
	if [ ! -f icar_gcm_test.tgz ]; then
	    cp /glade/work/gutmann/icar/benchmark/icar_gcm_test.tgz .
	fi
	tar zxf icar_gcm_test.tgz
	rm icar_gcm_test.tgz
    fi
}

function run {
    echo "$1"
}

function run_icar {
    run "./icar ${nml_f}"
}

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

function write_physics_opts {
    f=new_${9}_${nml_f}
    cp ${nml_f} ${f}
    sed -i '' "s#pbl *= *[0-9]#pbl = ${1}#" ${f}
    sed -i '' "s#lsm *= *[0-9]#lsm = ${2}#" ${f}
    sed -i '' "s#water *= *[0-9]#water = ${3}#" ${f}
    sed -i '' "s#mp *= *[0-9]#mp = ${4}#" ${f}
    sed -i '' "s#rad *= *[0-9]#rad = ${5}#" ${f}
    sed -i '' "s#conv *= *[0-9]#conv = ${6}#" ${f}
    sed -i '' "s#adv *= *[0-9]#adv = ${7}#" ${f}
    sed -i '' "s#wind *= *[0-9]#wind = ${8}#" ${f}
}


### START ###
# prepare_cheyenne_gcm_test

# run "cp ../../src/icar ."
# cp ../../run/complete_icar_options.nml ${nml_f}
# run_icar

# --- physic options ---
file_n=0
pbl_opt_skip=(1)
#              pbl 2, lsm 4, water 3, mp 6, rad 3, conv 5, adv 2, wind 5
#                    pbl   lsm   water mp    rad   conv  adv   wind
# for physics_opts in {0..2}{0..4}{0..3}{0..6}{0..3}{0..5}{1..2}{1..5} # this creates 67200 options
for physics_opts in {0..2}{0..0}{0..0}{0..1}{0..0}{0..0}{1..1}{1..1}
do
    parse_physics_opts ${physics_opts}
    # skip physics options that aren't available
    [[  " ${pbl_opt_skip[*]} " =~ " ${pbl_opt} " ]] && continue

    # write physics options to file
    echo "${pbl_opt} ${lsm_opt} ${water_opt} ${mp_opt} ${rad_opt} ${conv_opt} \
${adv_opt} ${wind_opt}"
    write_physics_opts ${pbl_opt} ${lsm_opt} ${water_opt} ${mp_opt} ${rad_opt} \
		       ${conv_opt} ${adv_opt} ${wind_opt} ${file_n}
    ((file_n+=1))
done

echo "fin with ${file_n} files produced"
