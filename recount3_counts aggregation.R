#Loading necessary packages
library(readr)
library(dplyr)
library(tibble)
library(recount3)
recount3_cache_rm()
options(recount3_url = "http://duffel.rail.bio/recount3")
human_projects<-available_projects()
library(tidyr)
library(stringr)

#Loading in the metadata
combined_metadata <- read_csv("combined_metadata.csv", col_types = cols(...1 = col_skip()))
recount3_transformed_counts <- read_csv("gene_counts/recount3_transformed_counts.csv", col_types = cols(...1 = col_skip()))

#Aggregating the run data at the sample level
recount3_counts_data_list <- list()
projects_local <- c("SRP002881", "SRP011895", "SRP012585", "SRP015336", "SRP016140", "SRP034875",
                    "SRP035641", "SRP045639", "SRP053034", "SRP055101", "SRP061670", "SRP062870",
                    "SRP064956", "SRP070148", "SRP075990", "SRP079002", "SRP080002", "SRP090226",
                    "SRP091605", "SRP091675", "SRP092413", "SRP093877", "SRP094572", "SRP097696",
                    "SRP098761", "SRP105756", "SRP106457", "SRP108292", "SRP110135", "SRP111145",
                    "SRP115908", "SRP117613", "SRP119291", "SRP119766", "SRP151763", "SRP159246",
                    "ERP022243")

for (i in projects_local) {
  run_accessions = combined_metadata %>% filter(study_accession == i) %>% pull(run_accession)
  recount3_counts_subset <- recount3_transformed_counts[, c("gene_id", run_accessions)]
  long_recount3_counts_subset <- recount3_counts_subset %>% pivot_longer(-gene_id)
  names(long_recount3_counts_subset) <- c("gene_id", "run_accession", "value")
  
  long_recount3_counts_subset_srs <- long_recount3_counts_subset %>% left_join(combined_metadata %>% select(sample_accession, run_accession),
                                                                                                 by = c('run_accession')) %>% 
    group_by(sample_accession, gene_id) %>% 
    summarise(value = mean(value))
  
  recount3_counts_data_list[[i]] <- long_recount3_counts_subset_srs
}

recount3_counts_data_list <- recount3_counts_data_list %>% bind_rows()
write.csv(recount3_counts_data_list, file = "gene_counts/recount3_counts_aggregated.csv", row.names=FALSE)
