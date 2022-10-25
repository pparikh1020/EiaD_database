### Loading relevant packages -----
library(tidyverse)
library(tibble)
library(recount)
library(recount3)
library(dtplyr)
library(data.table)

# Setting the working directory and source folder
setwd("/Users/parikhpp/git/EiaD_build")
source("/Users/parikhpp/git/EiaD_build/src/create_count_data_function.R")

# Importing Metadata
eyeIntegration22 <- read_csv("https://hpc.nih.gov/~parikhpp/EiaD/2022_metadata.csv", col_types = cols(...1 = col_skip()))

### Downloading count data from the recount3 database -----

create_count_data_frames("http://duffel.rail.bio/recount3/",
                         c("SRP002881", "SRP011895", "SRP012585", "SRP015336", "SRP016140", "SRP034875",
                           "SRP035641", "SRP045639", "SRP053034", "SRP055101", "SRP061670", "SRP062870",
                           "SRP064956", "SRP070148", "SRP075990", "SRP079002", "SRP080002", "SRP090226",
                           "SRP091605", "SRP091675", "SRP092413", "SRP093877", "SRP094572", "SRP097696",
                           "SRP098761", "SRP105756", "SRP106457", "SRP108292", "SRP110135", "SRP111145",
                           "SRP115908", "SRP117613", "SRP119291", "SRP119766", "SRP151763", "SRP159246",
                           "ERP022243", "SRP193153"),
                         "recount3_transformed_counts",
                         "aggregated_recount3_transformed_counts",
                         "recount3_mapping_information",
                         eyeIntegration22)

### Downloading GTEX count data from the recount3 database -----

create_gtex_count_data_frames(c("ADIPOSE_TISSUE", "MUSCLE", "BLOOD_VESSEL", "HEART", "OVARY", "UTERUS",
                                "VAGINA", "BREAST", "SKIN", "SALIVARY_GLAND", "BRAIN", "ADRENAL_GLAND",
                                "THYROID", "LUNG", "SPLEEN", "PANCREAS", "ESOPHAGUS", "STOMACH", "COLON",
                                "SMALL_INTESTINE", "PROSTATE", "TESTIS", "NERVE", "PITUITARY", "BLOOD",
                                "LIVER", "KIDNEY",   "CERVIX_UTERI", "FALLOPIAN_TUBE", "BLADDER", "BONE_MARROW"),
                              "gtex_transformed_counts",
                              "aggregated_gtex_transformed_counts",
                              "gtex_mapping_information",
                              eyeIntegration22)

### Downloading count data from the "local" database (Either not included in recount3 or run locally using the Monorail RNA-seq processing pipeline) -----

create_count_data_frames("/Users/parikhpp/EiaD_library",
                         c("ERP126780", "SRP056957", "SRP090027", "SRP257684", "SRP287234", "SRP310948", "SRP329409", "SRP070938",
                           "SRP090040", "SRP273695", "SRP288670", "SRP323408", "SRP331221", "SRP018405", "SRP080886", "SRP255891",
                           "SRP287152", "SRP300190", "SRP326606"),
                         "local_data_additions_transformed_counts",
                         "aggregated_local_data_additions_transformed_counts",
                         "local_data_additions_mapping_information",
                         eyeIntegration22)

### Creating final mapping information file for use in sqlite database -----

# Calling mapping data
recount3_mapping_information <- vroom::vroom("mapping_data/recount3_mapping_information.csv")
gtex_mapping_information <- vroom::vroom("mapping_data/gtex_mapping_information.csv")
local_data_additions_mapping_information <- vroom::vroom("mapping_data/local_data_additions_mapping_information.csv")

mapping_data <- bind_rows(recount3_mapping_information,
                          gtex_mapping_information,
                          local_data_additions_mapping_information)

### Write final mapping_data csv file
write.csv(mapping_data, "mapping_data/mapping_data.csv", row.names = FALSE)

### Creating final gene_counts file for use in sqlite database creation using the aggregated data -----

# Calling aggregated data
aggregated_recount3_transformed_counts <- vroom::vroom("gene_counts/aggregated_recount3_transformed_counts.csv")
aggregated_gtex_transformed_counts <- vroom::vroom("gene_counts/aggregated_gtex_transformed_counts.csv")
aggregated_local_data_additions_transformed_counts <- vroom::vroom("gene_counts/aggregated_local_data_additions_transformed_counts.csv")

# Combine aggregated data
gene_counts <- bind_rows(aggregated_recount3_transformed_counts,
                         aggregated_gtex_transformed_counts,
                         aggregated_local_data_additions_transformed_counts)

gene_counts <- gene_counts %>% select(gene_id, sample_accession, value)

### Write final gene_counts csv file
write.csv(gene_counts, "gene_counts/gene_counts.csv", row.names = FALSE)
