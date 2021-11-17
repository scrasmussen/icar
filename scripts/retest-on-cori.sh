#!/bin/bash

set -e

source scripts/load-modules-on-cori.sh
source /global/homes/s/sameer/pkgs/OpenCoarrays/install/opencoarrays/setup.sh
source scripts/export-cori-environment-variables.sh

./scripts/fpm-test.sh
