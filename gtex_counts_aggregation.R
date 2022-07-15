#Loading necessary packages
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)

#Loading in the metadata
combined_metadata <- read_csv("combined_metadata.csv", col_types = cols(...1 = col_skip()))
gtex_transformed_counts <- read_csv("gene_counts/gtex_transformed_counts.csv", col_types = cols(...1 = col_skip()))

#Aggregating the run data at the sample level
gtex_counts_data_list <- list()

long_gtex_counts_subset <- gtex_transformed_counts %>% pivot_longer(-gene_id)
names(long_gtex_counts_subset) <- c("gene_id", "run_accession", "value")
  
long_gtex_counts_subset_srs <- long_gtex_counts_subset %>% left_join(combined_metadata %>% select(sample_accession, run_accession),
                                                                               by = c('run_accession')) %>% 
    group_by(sample_accession, gene_id) %>% 
    summarise(value = mean(value))
  
gtex_counts_data_list[[i]] <- long_gtex_counts_subset_srs

gtex_counts_data_list <- gtex_counts_data_list %>% bind_rows()
write.csv(gtex_counts_data_list, file = "gene_counts/gtex_counts_aggregated.csv", row.names=FALSE)
