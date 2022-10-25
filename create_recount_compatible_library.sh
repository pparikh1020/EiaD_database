#!/bin/sh

# This script will create a combined recount3-compatible library for 
# samples independently processed using Snakerail, which is a wrapper for the Monorail RNA-seq processing pipeline:
# https://github.com/davemcg/Snakerail

# This script should be run in the parent Snakerail directory, containing separate directories for each Snakerail study.
# An example of the necessary directory structure is pictured below. EiaD_data is the parent directory
# and SRP018405 along with SRP070938 are two example studies of RNA-expression data which have been processed using Snakerail.

# └─ EiaD_data (Parent directory containing all processed Snakerail samples)
#    ├─ SRP018405 (Directory containing the Snakerail output for your first study of interest)
#    │  └─ rse
#    │     └─ human
#    │        └─ ...
#    ├─ SRP070938 (Directory containing the Snakerail output for your second study of interest)
#    │  └─ rse
#    │     └─ human
#    │        └─ ...
#    └─ Additional Studies can be included as formatted above

# Specifies the project name used in the snakerail_config.yaml when running Snakerail.
# This should be consistent across all of your studies.

project_name=$1

# Specifies the path to your recount3-compatible library

library_path=$2

rsync -a */rse/* $library_path --exclude=$project_name\.recount_project.MD.gz

# Constructing metadata file

cd $library_path\/human/data_sources/$project_name\/
zcat metadata/*/*/*recount_project.* | head -n 1 | gzip > metadata/$project_name\.recount_project.MD.gz # this grabs just the header
zcat metadata/*/*/*recount_project.* | grep -v rail_id | gzip >> metadata/$project_name\.recount_project.MD.gz # copy the rest of the meta sans the headers
