create_pca <- 
  function(gene_counts, metadata, output_file_name, sdev_file_name) {
    
    # Adding variable names for the rows so metadata can be integrated more easily
    # put gene names into `row.names`
    gene_counts <- gene_counts %>% column_to_rownames(var="gene_id")
    gene_counts <- gene_counts %>% as.matrix()
    gene_counts <- log2(gene_counts + 1)
    
    # Transpose matrix to add metadata
    t_gene_counts <- t(gene_counts)
    
    #Adding metadata
    t_gene_counts_data <- as.data.frame(t_gene_counts)
    t_gene_counts_data <- t_gene_counts_data %>% tibble::rownames_to_column(var = "run_accession")
    t_gene_counts_data <- t_gene_counts_data %>% left_join(metadata, by="run_accession")
    
    #Rename tissue so GTEx separation is more evident
    t_gene_counts_data <- within(t_gene_counts_data, Tissue[study_accession == "SRP012682"] <- 'GTEx')
    
    top_var <- apply(t_gene_counts_data, 2, var)
    
    # take top 2000 genes
    top_2k <- top_var %>% sort() %>% tail(2000)
    
    pca_2k <- prcomp(t_gene_counts_data[,names(top_2k)])
    
    components <- pca_2k[["x"]]
    components <- data.frame(components)
    components$run_accession <- t_gene_counts_data$run_accession
    components <- components %>% left_join(t_gene_counts_data %>% select("sample_accession", "Tissue", "Sub_Tissue",
                                                                         "Origin", "Age_Days", "Kept", "study_title",
                                                                         "sample_attribute", "study_abstract",
                                                                         "run_accession", "study_accession", "mapping_rate",
                                                                         "region"), by = "run_accession")
    components_sdev <- pca_2k[["sdev"]]
    
    # Write pca output to a csv
    write.csv(components, file = paste0("pca_output/", output_file_name, ".csv"), row.names = FALSE)
    write.csv(components_sdev, file = paste0("pca_output/", sdev_file_name, ".csv"), row.names = FALSE)
  }
