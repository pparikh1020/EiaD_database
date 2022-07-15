#Loading necessary packages
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

#Loading transformed_local_counts data
#Adding new and EyeIntegration unique (SSRP018405", "SRP326606", "SRP323408", "SRP329409", "SRP331221", "SRP090027", "SRP287234", 
#"SRP070938", "SRP090040", "SRP310948", "SRP287152", "SRP056957", "SRP288670", "E-MTAB-4377", "SRP257684", "SRP080886", 
#"SRP300190", "SRP255891) samples

transformed_local_counts <- read_csv("gene_counts/transformed_local_counts.csv", col_types = cols(...1 = col_skip()))

#Loading in the metadata
combined_metadata <- read_csv("combined_metadata.csv", col_types = cols(...1 = col_skip()))

#Aggregating the run data at the sample level
transformed_local_counts_data_list <- list()
projects_local <- c( "SRP018405", "SRP326606", "SRP323408", "SRP329409",   
                     "SRP331221", "SRP090027", "SRP287234", 
                     "SRP070938", "SRP090040", "SRP310948", "SRP287152", 
                     "SRP056957", "SRP288670", "E-MTAB-4377", "SRP257684", 
                     "SRP080886", "SRP300190", "SRP255891")

for (i in projects_local) {
  run_accessions = combined_metadata %>% filter(study_accession == i) %>% pull(run_accession)
  transformed_local_counts_subset <- transformed_local_counts[, c("gene_id", run_accessions)]
  long_transformed_local_counts_subset <- transformed_local_counts_subset %>% pivot_longer(-gene_id)
  names(long_transformed_local_counts_subset) <- c("gene_id", "run_accession", "value")
  
  long_transformed_local_counts_subset_srs <- long_transformed_local_counts_subset %>% left_join(combined_metadata %>% select(sample_accession, run_accession),
                                                                                                 by = c('run_accession')) %>% 
    group_by(sample_accession, gene_id) %>% 
    summarise(value = mean(value))
  
  transformed_local_counts_data_list[[i]] <- long_transformed_local_counts_subset_srs
}

transformed_local_counts_data_list <- transformed_local_counts_data_list %>% bind_rows()
write.csv(transformed_local_counts_data_list, 
          file = "gene_counts/transformed_local_counts_aggregated.csv", row.names=FALSE)