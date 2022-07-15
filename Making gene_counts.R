library(readr)
library(dplyr)

aggregated_local_counts <- read_csv("gene_counts/transformed_local_counts_aggregated.csv")
aggregated_recount3_counts <- read_csv("gene_counts/recount3_counts_aggregated.csv")
aggregated_gtex_counts <- read_csv("gene_counts/gtex_counts_aggregated.csv")

gene_counts <- bind_rows(aggregated_local_counts, aggregated_recount3_counts, aggregated_gtex_counts)

write_csv(gene_counts, "gene_counts/gene_counts.csv")
