###Loading relevant packages -----
library(tidyverse)
library(tibble)
library(recount)
library(recount3)

#Setting the working directory and source folder
setwd("/Users/parikhpp/git/EiaD_build")
source("/Users/parikhpp/git/EiaD_build/src/create_rse2.R")
source("/Users/parikhpp/git/EiaD_build/src/create_count_data_function.R")

#Importing Metadata
eyeIntegration22 <- read_csv("combined_metadata.csv", col_types = cols(...1 = col_skip()))

###Downloading count data from the recount3 database -----

create_count_data_frames("http://duffel.rail.bio/recount3/",
                         c("SRP002881", "SRP011895", "SRP012585", "SRP015336", "SRP016140", "SRP034875",
                           "SRP035641", "SRP045639", "SRP053034", "SRP055101", "SRP061670", "SRP062870",
                           "SRP064956", "SRP070148", "SRP075990", "SRP079002", "SRP080002", "SRP090226",
                           "SRP091605", "SRP091675", "SRP092413", "SRP093877", "SRP094572", "SRP097696",
                           "SRP098761", "SRP105756", "SRP106457", "SRP108292", "SRP110135", "SRP111145",
                           "SRP115908", "SRP117613", "SRP119291", "SRP119766", "SRP151763", "SRP159246",
                           "ERP022243"),
                         "recount3_transformed_counts",
                         "aggregated_recount3_transformed_counts",
                         eyeIntegration22)

###Downloading GTEX count data from the recount3 database -----

create_gtex_count_data_frames("http://duffel.rail.bio/recount3/",
                              c("ADIPOSE_TISSUE", "MUSCLE", "BLOOD_VESSEL", "HEART", "OVARY", "UTERUS",
                                "VAGINA", "BREAST", "SKIN", "SALIVARY_GLAND", "BRAIN", "ADRENAL_GLAND",
                                "THYROID", "LUNG", "SPLEEN", "PANCREAS", "ESOPHAGUS", "STOMACH", "COLON",
                                "SMALL_INTESTINE", "PROSTATE", "TESTIS", "NERVE", "PITUITARY", "BLOOD",
                                "LIVER", "KIDNEY",   "CERVIX_UTERI", "FALLOPIAN_TUBE", "BLADDER", "BONE_MARROW"),
                              "gtex_transformed_counts",
                              "aggregated_gtex_transformed_counts",
                              eyeIntegration22)

###Downloading count data from the "local" database (Either not included in recount3 or run locally using the Monorail RNA-seq processing pipeline) -----

create_count_data_frames("/Users/parikhpp/recount3/recount3_local",
                         c( "SRP018405", "SRP326606", "SRP323408", "SRP329409",
                            "SRP331221", "SRP090027", "SRP287234", "SRP070938",
                            "SRP090040", "SRP310948", "SRP287152", "SRP056957",
                            "SRP288670", "E-MTAB-4377", "SRP257684", "SRP080886",
                            "SRP300190", "SRP255891"),
                         "local_data_additions_transformed_counts",
                         "aggregated_local_data_additions_transformed_counts",
                         eyeIntegration22)


