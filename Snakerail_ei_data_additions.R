library(dplyr)
library(tibble)
library(recount)
library(recount3)
recount3_cache_rm()
options(recount3_url = "/Users/parikhpp/recount3/recount3_local")
hp<-available_projects()
setwd("/Users/parikhpp/NEI Projects/")
src("/Users/parikhpp/NEI Projects/src/create_rse2.R")

data_additions_transformed_counts <- list()
projects_local <- c( "SRP018405", "SRP326606", "SRP323408", "SRP329409",   
                     "SRP331221", "SRP090027", "SRP287234", 
                     "SRP070938", "SRP090040", "SRP310948", "SRP287152", 
                     "SRP056957", "SRP288670", "E-MTAB-4377", "SRP257684", 
                     "SRP080886", "SRP300190", "SRP255891")

for (i in projects_local) {
  rse_gene = create_rse_manual2(i, annotation = "gencode_v29")
  assays(rse_gene)$counts <- transform_counts(rse_gene)
  
  rse_gene_counts <- recount::getTPM(rse_gene) %>% data.frame()
  rse_gene_counts <- rownames_to_column(rse_gene_counts, var = "gene_id")
  
  data_additions_transformed_counts[[i]] <- rse_gene_counts
}

SRP018405_counts <- data_additions_transformed_counts$SRP018405 %>% as.data.frame()
SRP326606_counts <- data_additions_transformed_counts$SRP326606 %>% as.data.frame()
SRP323408_counts <- data_additions_transformed_counts$SRP323408 %>% as.data.frame()
SRP329409_counts <- data_additions_transformed_counts$SRP329409 %>% as.data.frame()
SRP331221_counts <- data_additions_transformed_counts$SRP331221 %>% as.data.frame()
SRP090027_counts <- data_additions_transformed_counts$SRP090027 %>% as.data.frame()
SRP287234_counts <- data_additions_transformed_counts$SRP287234 %>% as.data.frame()
SRP070938_counts <- data_additions_transformed_counts$SRP070938 %>% as.data.frame()
SRP090040_counts <- data_additions_transformed_counts$SRP090040 %>% as.data.frame()
SRP310948_counts <- data_additions_transformed_counts$SRP310948 %>% as.data.frame()
SRP287152_counts <- data_additions_transformed_counts$SRP287152 %>% as.data.frame()
SRP056957_counts <- data_additions_transformed_counts$SRP056957 %>% as.data.frame()
SRP288670_counts <- data_additions_transformed_counts$SRP288670 %>% as.data.frame()
E_MTAB_4377_counts <- data_additions_transformed_counts$`E-MTAB-4377` %>% as.data.frame()
SRP257684_counts <- data_additions_transformed_counts$SRP257684 %>% as.data.frame()
SRP080886_counts <- data_additions_transformed_counts$SRP080886 %>% as.data.frame()
SRP300190_counts <- data_additions_transformed_counts$SRP300190 %>% as.data.frame()
SRP255891_counts <- data_additions_transformed_counts$SRP255891 %>% as.data.frame()

data_additions_transformed_counts_final <- SRP326606_counts %>% left_join(SRP018405_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP323408_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP329409_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP331221_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP090027_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP287234_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP070938_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP090040_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP310948_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP287152_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP056957_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP288670_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(E_MTAB_4377_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP257684_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP080886_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP300190_counts, by="gene_id")
data_additions_transformed_counts_final <- data_additions_transformed_counts_final %>% left_join(SRP255891_counts, by="gene_id")

write.csv(data_additions_transformed_counts_final, file="gene_counts/transformed_local_counts.csv")