# BigWig URL function for Ocular Data -----

# To obtain BigWig files for samples which were not processed locally, 
# we must pull them from the recount3 database using their associated urls,
# which can be found using this function

# The ocular_bigwig_metadata will be a data frame with column one containing run
# accessions and column two containing project accessions for each sample of
# interest

get_ocular_bigwig_urls <- 
  function(ocular_bigwig_metadata) {
    
    urls <- matrix(NA, nrow=nrow(ocular_bigwig_metadata), ncol=1)
    
    for (i in 1:nrow(ocular_bigwig_metadata)) {
      url <- locate_url(
        ocular_bigwig_metadata[i, 2],
        "data_sources/sra",
        type = "bw",
        sample = ocular_bigwig_metadata[i, 1]
        )
      urls[i] <- url
      }
    urls <- paste("wget", urls[1:nrow(urls)])

    write.csv(urls, file="url_files/ocular_bigwig_urls.csv")
  }

# BigWig URL function for GTEx Data -----

# The gtex_bigwig_metadata will be a data frame with column one containing gtex
# external_ids and column two containing the GTEx tissue for each sample of
# interest (Here is an example format: GTEX-11ZVC-2626-SM-5FQTA.1, ADIPOSE_TISSUE)

get_gtex_bigwig_urls <- 
  function(gtex_bigwig_metadata) {
    
    urls <- matrix(NA, nrow=nrow(gtex_bigwig_metadata), ncol=1)
    
    for (i in 1:nrow(gtex_bigwig_metadata)) {
      url <- locate_url(
        gtex_bigwig_metadata[i, 2,],
        "data_sources/gtex",
        type = "bw",
        sample = gtex_bigwig_metadata[i, 1]
      )
      urls[i] <- url
    }
    
    urls <- paste("wget", urls[1:nrow(urls)])
    
    write.csv(urls, file="url_files/gtex_bigwig_urls.csv")
  }