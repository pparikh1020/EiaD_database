### Loading relevant packages and functions -----
library(tidyverse)
library(recount)
library(recount3)
setwd("/Users/parikhpp/NEI Projects/BigWigs")
source("/Users/parikhpp/NEI Projects/BigWigs/src/get_bigwig_url.R")

eyeIntegration22 <- read_csv("https://hpc.nih.gov/~parikhpp/2022_metadata.csv", col_types = cols(...1 = col_skip()))
gtex_metadata <- read_csv("/Users/parikhpp/NEI Projects/BigWigs/gtex_data_final.csv", col_types = cols(...1 = col_skip()))

#Creating ocular_bigwig_metadata -----

#Extract all samples which were pulled from the recount3 database
ocular_bigwig_metadata <-
  eyeIntegration22 %>% 
  filter(study_accession %in% c("ERP022243", "SRP002881", "SRP011895", "SRP012585", "SRP015336",
                                "SRP016140", "SRP034875", "SRP035641", "SRP045639", "SRP053034",
                                "SRP055101", "SRP061670", "SRP062870", "SRP064956", "SRP070148",
                                "SRP075990", "SRP079002", "SRP080002", "SRP090226", "SRP091605",
                                "SRP091675", "SRP092413", "SRP093877", "SRP094572", "SRP097696",
                                "SRP098761", "SRP105756", "SRP106457", "SRP108292", "SRP110135",
                                "SRP111145", "SRP115908", "SRP117613", "SRP119291", "SRP119766",
                                "SRP151763", "SRP159246")) %>% select(run_accession, study_accession)

#Creating gtex_bigwig_metadata -----

gtex_bigwig_metadata <- gtex_metadata %>% select(external_id, study)

#Creating URLs for bigwig download -----

get_ocular_bigwig_urls(ocular_bigwig_metadata)
get_gtex_bigwig_urls(gtex_bigwig_metadata)