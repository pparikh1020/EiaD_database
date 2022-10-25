### Downloading relevant packages -----
library(XML)
library(reutils)
library(tidyverse)
setwd("/Users/parikhpp/NEI Projects/metadata_creation/")

### Adding necessary functions -----

#Grabbing sample attributes
attribute_finder <- function(xml_list_obj){
  out <- list()
  for (i in 1:length(xml_list_obj)){
    out[[i]] <- xml_list_obj[[i]]$.attrs['attribute_name']
  }
  return(out)
}

value_grabber <- function(attribute, xml_list_obj){
  for (i in 1:length(xml_list_obj)){
    if (attribute %in% (xml_list_obj[[i]]$.attrs)){
      out <- xml_list_obj[[i]]$text
      return(out)
    }
  }
}

# takes SRR run accession as input
# and returns SAMN biosample ID
samn_getter <- function(srr){
  (efetch(c(srr), db = 'sra', retmode = 'xml'))$content %>% str_extract(., 'SAMN\\d+')
}

# uses NCBI sample attribute
# example: SAMN05784633
attribute_df_maker <- function(id){
  # fetch xml object from NCBI
  eutil_grab <- efetch(uid = id, db = 'biosample', retmode = 'xml')
  # extract attributes, convert to list
  xml_list_obj <- eutil_grab[["//Attributes"]] %>% XML::xmlToList()
  biosample_title <-  (eutil_grab[["//Description"]]%>% XML::xmlToList())$Title
  # scan through list obj and find all attributes
  attribute_df <-  attribute_finder(xml_list_obj) %>% as.character() %>% data.frame()
  colnames(attribute_df)[1] <- 'attribute'
  # grab the attributes and stick into DF
  attribute_df <- attribute_df %>% rowwise() %>% mutate(value = value_grabber(attribute, xml_list_obj))
  attribute_df$id = id
  attribute_df <- bind_rows(attribute_df, c(attribute = 'biosample_title', value = biosample_title, 'id' = id))
  return(attribute_df)
}

### Creating eyeIntegration22 -----
eyeIntegration19_metadata <- vroom::vroom("https://hpc.nih.gov/~mcgaugheyd/eyeIntegration/2019_metadata_04.tsv.gz",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)
newstudylist <- read_csv("newstudylist.csv", col_types = cols(...1 = col_skip()))
#Renaming to match eyeIntegration data
names(newstudylist) <- c("run_accession", "Assay.Type", "AvgSpotLen", "Bases", "BioProject", "sample_accession", 
                         "Bytes", "Center.Name", "Consent", "DATASTORE.filetype", "DATASTORE.provider", 
                         "DATASTORE.region", "Donor", "Experiment", "GEO_Accession..exp.", "Instrument", 
                         "LibraryLayout", "LibrarySelection", "LibrarySource", "Organism", "Platform", 
                         "region", "ReleaseDate", "Sample.Name", "source_name", "study_accession", 
                         "Tissue", "Kept", "Cell_type", "patient_diagnosis", "disease_state", "disease", 
                         "prognosis", "cell_line", "developmental_stage", "Genotype", "treatment", 
                         "cell_type", "age", "sex", "batch", "biomaterial_provider", "BioSampleModel", 
                         "Isolate", "Library.Name", "Replicate", "health_state", "Sample_Type", "Phenotype", 
                         "secondary_id")

### Manipulating metadata -----

##### Matching eyeIntegration E-MTAB-4377 to ERR run and ERS sample accession codes -----
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA1"] <- 'ERR5236614')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA10"] <- 'ERR5236615')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA11"] <- 'ERR5236616')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA12"] <- 'ERR5236617')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA13"] <- 'ERR5236618')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA14"] <- 'ERR5236619')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA15"] <- 'ERR5236620')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA16"] <- 'ERR5236621')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA17"] <- 'ERR5236622')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA18"] <- 'ERR5236623')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA19"] <- 'ERR5236624')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA2"] <- 'ERR5236625')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA20"] <- 'ERR5236626')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA21"] <- 'ERR5236627')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA22"] <- 'ERR5236628')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA23"] <- 'ERR5236629')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA24"] <- 'ERR5236630')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA25"] <- 'ERR5236631')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA26"] <- 'ERR5236632')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA27"] <- 'ERR5236633')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA28"] <- 'ERR5236634')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA29"] <- 'ERR5236635')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA3"] <- 'ERR5236636')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA30"] <- 'ERR5236637')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA31"] <- 'ERR5236638')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA32"] <- 'ERR5236639')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA33"] <- 'ERR5236640')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA34"] <- 'ERR5236641')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA35"] <- 'ERR5236642')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA36"] <- 'ERR5236643')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA37"] <- 'ERR5236644')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA38"] <- 'ERR5236645')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA39"] <- 'ERR5236646')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA4"] <- 'ERR5236647')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA40"] <- 'ERR5236648')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA41"] <- 'ERR5236649')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA42"] <- 'ERR5236650')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA43"] <- 'ERR5236651')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA44"] <- 'ERR5236652')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA45"] <- 'ERR5236653')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA46"] <- 'ERR5236654')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA47"] <- 'ERR5236655')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA48"] <- 'ERR5236656')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA49"] <- 'ERR5236657')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA5"] <- 'ERR5236658')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA50"] <- 'ERR5236659')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA6"] <- 'ERR5236660')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA7"] <- 'ERR5236661')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA8"] <- 'ERR5236662')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, run_accession[sample_accession == "E.MTAB.4377.RNA9"] <- 'ERR5236663')
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236614"] <- "ERS5672529")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236615"] <- "ERS5672530")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236616"] <- "ERS5672531")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236617"] <- "ERS5672532")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236618"] <- "ERS5672533")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236619"] <- "ERS5672534")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236620"] <- "ERS5672535")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236621"] <- "ERS5672536")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236622"] <- "ERS5672537")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236623"] <- "ERS5672538")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236624"] <- "ERS5672539")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236625"] <- "ERS5672540")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236626"] <- "ERS5672541")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236627"] <- "ERS5672542")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236628"] <- "ERS5672543")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236629"] <- "ERS5672544")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236630"] <- "ERS5672545")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236631"] <- "ERS5672546")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236632"] <- "ERS5672547")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236633"] <- "ERS5672548")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236634"] <- "ERS5672549")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236635"] <- "ERS5672550")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236636"] <- "ERS5672551")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236637"] <- "ERS5672552")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236638"] <- "ERS5672553")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236639"] <- "ERS5672554")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236640"] <- "ERS5672555")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236641"] <- "ERS5672556")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236642"] <- "ERS5672557")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236643"] <- "ERS5672558")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236644"] <- "ERS5672559")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236645"] <- "ERS5672560")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236646"] <- "ERS5672561")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236647"] <- "ERS5672562")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236648"] <- "ERS5672563")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236649"] <- "ERS5672564")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236650"] <- "ERS5672565")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236651"] <- "ERS5672566")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236652"] <- "ERS5672567")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236653"] <- "ERS5672568")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236654"] <- "ERS5672569")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236655"] <- "ERS5672570")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236656"] <- "ERS5672571")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236657"] <- "ERS5672572")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236658"] <- "ERS5672573")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236659"] <- "ERS5672574")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236660"] <- "ERS5672575")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236661"] <- "ERS5672576")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236662"] <- "ERS5672577")
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, sample_accession[run_accession == "ERR5236663"] <- "ERS5672578")

#Changing the E-MTAB-4377 accession to its associated ERP126780 accession
eyeIntegration19_metadata <- within(eyeIntegration19_metadata, study_accession[study_accession == "E-MTAB-4377"] <- "ERP126780")

##### Matching the SAMN sample accessions from eyeIntegration to the SRS format

#Obtain order of SAMN accessions
SAMN_accessions <- newstudylist$sample_accession
#Find associated SRS associations
SRS_accessions <- c("SRS8476047", "SRS8476048", "SRS8476049", "SRS8476039", "SRS8476038", "SRS8476040",
                    "SRS8476041", "SRS8476042", "SRS8476043", "SRS8476044", "SRS8476045", "SRS8476046",
                    "SRS7504865", "SRS7504866", "SRS7504867", "SRS7504868", "SRS7504869", "SRS7504870",
                    "SRS7961495", "SRS7961496", "SRS7961497", "SRS6509681", "SRS6509682", "SRS6509683",
                    "SRS6509684", "SRS6509685", "SRS6509686", "SRS6509687", "SRS6509688", "SRS6509689",
                    "SRS6509690", "SRS6509691", "SRS6509692", "SRS6509693", "SRS6509694", "SRS6509695",
                    "SRS6509696", "SRS6509697", "SRS6457374", "SRS6457373", "SRS6457375", "SRS6457376",
                    "SRS6457377", "SRS6457378", "SRS6457379", "SRS6457380", "SRS7842170", "SRS7842171",
                    "SRS7842172", "SRS7842173", "SRS7842174", "SRS7842175", "SRS7842176", "SRS7842177",
                    "SRS7842178", "SRS7842179", "SRS7842180", "SRS7842181", "SRS9558354", "SRS9558355",
                    "SRS9558356", "SRS9761682", "SRS9761683", "SRS9761684", "SRS9761685", "SRS9761686",
                    "SRS9761687", "SRS9761688", "SRS9761689", "SRS9761691", "SRS9761692", "SRS9761690",
                    "SRS9761693", "SRS9761694", "SRS9761695", "SRS9761698", "SRS9761697", "SRS9761699",
                    "SRS9761700", "SRS9761705", "SRS9761701", "SRS9761702", "SRS9761703", "SRS9761704",
                    "SRS9761706", "SRS9761707", "SRS9761709", "SRS9761696", "SRS9761708", "SRS7512844",
                    "SRS7512845", "SRS7512846", "SRS7512847", "SRS7512848", "SRS7512849", "SRS7512850",
                    "SRS7512851", "SRS7512852", "SRS7512853", "SRS7512854", "SRS7512855", "SRS7512856",
                    "SRS7590278", "SRS7590277", "SRS7590276", "SRS7590275", "SRS7590274", "SRS7590273",
                    "SRS7590272", "SRS7590271", "SRS7590270", "SRS7590269", "SRS7590268", "SRS7590267",
                    "SRS7590266", "SRS7590265", "SRS7590264", "SRS8746366", "SRS8746367", "SRS8746370",
                    "SRS8746369", "SRS8746376", "SRS8746379", "SRS8746381", "SRS8746365", "SRS8746368",
                    "SRS8746372", "SRS8746371", "SRS8746373", "SRS8746374", "SRS8746375", "SRS8746377",
                    "SRS8746378", "SRS8746380", "SRS8746382", "SRS9166479", "SRS9166480", "SRS9166481",
                    "SRS9166482", "SRS9373988", "SRS9373979", "SRS9373978", "SRS9688109", "SRS9688108",
                    "SRS9688107", "SRS9688106", "SRS9688105")
#Replace
newstudylist$sample_accession <- SRS_accessions

### Creating combined metadata -----
combined_metadata <- bind_rows(eyeIntegration19_metadata, newstudylist)

##### Editing sub_tissue data -----
combined_metadata <- within(combined_metadata, Sub_Tissue[region == "perifovea"] <- "Retina - Macula")
combined_metadata <- within(combined_metadata, Sub_Tissue[region == "fovea"] <- "Retina - Macula")
combined_metadata <- within(combined_metadata, Sub_Tissue[region == "macula"] <- "Retina - Macula")

#Providing Sub-Tissue classification
combined_metadata <- combined_metadata %>% mutate(Sub_Tissue = ifelse(is.na(Sub_Tissue), source_name, Sub_Tissue))
combined_metadata <- combined_metadata %>% mutate(Sub_Tissue = ifelse(is.na(Sub_Tissue), Tissue, Sub_Tissue))

#Renaming sub_tissue
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "choroidplexus"] <- "Choroid Plexus - Cell Line")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "cell from cornea"] <- "Cornea - Endothelium")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "eye"] <- "Retina - Adult Tissue")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Healthy conjuncitva"] <- "Conjunctiva")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "human fetal RPE cells"] <- "RPE - Stem Cell Line")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "iRPE cells"] <- "RPE - Stem Cell Line")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Macula Choroid"] <- "RPE/Choroid - Macula")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Macula Retina"] <- "Retina - Macula")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Peripheral Choroid"] <- "RPE/Choroid - Peripheral")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Periphral Retina"] <- "Retina - Peripheral")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Retina central"] <- "Retina - Macula")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Retina Fetal Tissue"] <- "Retina - Fetal Tissue")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Retina periphery"] <- "Retina - Peripheral")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Retina"] <- "RPE - Cell Line")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "Cornea"] <- "Cornea - Adult Tissue")
combined_metadata <- within(combined_metadata, Sub_Tissue[Sub_Tissue == "RPE"] <- "RPE - Adult Tissue")

#Cleaning up Tissue names
combined_metadata <- within(combined_metadata, Tissue[Tissue == "choroidplexus"] <- "Choroid Plexus")
combined_metadata <- within(combined_metadata, Tissue[Tissue == "cornea"] <- "Cornea")
combined_metadata <- within(combined_metadata, Tissue[Tissue == "Retina central"] <- "Retina")
combined_metadata <- within(combined_metadata, Tissue[Tissue == "Retina periphery"] <- "Retina")
combined_metadata <- within(combined_metadata, Tissue[Tissue == "eye"] <- "Retina")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "WIBR3 hESC Choroid plexus Organoids"] <- "Choroid Plexus")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "Conjunctiva"] <- "Conjunctiva")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "RPE - Stem Cell Line"] <- "RPE")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "Retina - Peripheral"] <- "Retina")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "RPE/Choroid - Peripheral"] <- "RPE/Choroid")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "Retina - Macula"] <- "Retina")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "RPE/Choroid - Macula"] <- "RPE/Choroid")
combined_metadata <- within(combined_metadata, Tissue[Sub_Tissue == "RPE - Adult Tissue"] <- "RPE")

### Creating final combined metadata -----

#Eliminating columns which were not present in previous iterations of eyeIntegration
#Additional columns for this iteration: region
combined_metadata <- combined_metadata[,c(1:12, 32)]

### Filling in missing data -----

#Age_Days Column
combined_metadata <- within(combined_metadata, Age_Days[is.na(Age_Days)] <- ".")
combined_metadata <- within(combined_metadata, Age_Days[Sub_Tissue == "WIBR3 hESC Choroid plexus Organoids"] <- 30)

#Kept Column
# combined_metadata <- within(combined_metadata, Kept[Kept == "yes"] <- "Kept")
combined_metadata <- within(combined_metadata, Kept[Kept == "yes?"] <- "removed") # "Removing" new organoid data
combined_metadata <- within(combined_metadata, Kept[study_accession == "SRP326606"] <- "removed") # "Removing" new choroid plexus data

#Region Column
combined_metadata <- within(combined_metadata, region[region == "macula"] <- "Macula")
combined_metadata <- within(combined_metadata, region[region == "perifovea"] <- "Perifovea")
combined_metadata <- within(combined_metadata, region[region == "fovea"] <- "Fovea")
combined_metadata <- within(combined_metadata, region[grepl("Macula", Sub_Tissue) & study_accession != "SRP310948"] <- "Macula")
combined_metadata <- within(combined_metadata, region[grepl("Peripheral", Sub_Tissue) & study_accession != "SRP310948"] <- "Peripheral")

#Origin Column
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP296832"] <- "Organoid")
combined_metadata <- within(combined_metadata, Origin[Sub_Tissue == "Choroid Plexus - Cell Line"] <- "Cell Line")
combined_metadata <- within(combined_metadata, Origin[Sub_Tissue == "RPE - Stem Cell Line"] <- " Stem Cell")
combined_metadata <- within(combined_metadata, Origin[Sub_Tissue == "RPE - Cell Line"] <- "Cell Line")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP287234"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP300190"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP331221"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP323408"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP273695"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP255891"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP257684"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP287152"] <- "Adult Tissue")
combined_metadata <- within(combined_metadata, Origin[study_accession == "SRP310948"] <- "Adult Tissue")

#Adding metadata to ensure these samples are not filtered out (SRP193153)
combined_metadata <- within(combined_metadata, study_accession[sample_accession == "SRS4653981"] <- "SRP193153")	
combined_metadata <- within(combined_metadata, study_accession[sample_accession == "SRS4653980"] <- "SRP193153")
combined_metadata <- within(combined_metadata, study_accession[sample_accession == "SRS4653978"] <- "SRP193153")

combined_metadata <- within(combined_metadata, run_accession[sample_accession == "SRS4653981"] <- "SRR8934740")
combined_metadata <- within(combined_metadata, run_accession[sample_accession == "SRS4653980"] <- "SRR8934741")
combined_metadata <- within(combined_metadata, run_accession[sample_accession == "SRS4653978"] <- "SRR8934743")

### Removing incorrectly labeled or misrepresented samples (Jason Miller edits) -----

#SRP252574 does not exist in our study, maybe it was removed at another time?

#SRP057295
combined_metadata <- combined_metadata %>% filter(study_accession != "SRP057295")

#SRP011895 (only removed the fetal samples)
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS302236")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS302237")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS402526")

#SRP061670
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS1054265") #This is a fetal sample so we eliminate it from this study and category

#SRP062870 (not wild type)
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS1509095")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS1509096")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS1509097")

#SRP070938 (actually RPE Culture, not cell line)
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS1314626")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS1314627")

#SRP108292 (these are fibroblasts, not RPE)
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS2240216")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS2240224")

#SRP115908 (represent cultured RPE that is poorly differentiated and NOT from RPE in vivo)
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS2445756")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS2445757")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS2445758")
combined_metadata <- combined_metadata %>% filter(sample_accession != "SRS2445759")

### Removing samples which are causing issues with the Snakerail pipeline we used to obtain our gene_count data -----

#combined_metadata <- combined_metadata %>% filter(study_accession != "SRP273695") #Currently working on adding this data back in
combined_metadata <- combined_metadata %>% filter(study_accession != "SRP296832")
combined_metadata <- combined_metadata %>% filter(study_accession != "SRP315688")

### Manual annotation of studies to be used -----

#SRP310948
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP310948"] <- "Human photoreceptor cells from different macular subregions have distinct transcriptional profiles")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP310948"] <- "The human neural retina is a light sensitive tissue with remarkable spatial and cellular organization. Compared with the periphery, the central retina contains more densely packed cone photoreceptor cells with unique morphologies and synaptic wiring. Some regions of the central retina exhibit selective degeneration or preservation in response to retinal disease and the basis for this variation is unknown. In this study, we used both bulk and single-cell RNA sequencing to compare gene expression within concentric regions of the central retina. We identified unique gene expression patterns of foveal cone photoreceptor cells, including many foveal-enriched transcription factors. In addition, we found that the genes RORB1, PPFIA1 and KCNAB2 are differentially spliced in the foveal, parafoveal and macular regions. These results provide a highly detailed spatial characterization of the retinal transcriptome and highlight unique molecular features of different retinal regions.")

#SRP287152
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP287152"] <- "The Human Eye Transcriptome Atlas: A searchable comparative transcriptome database for healthy and diseased human eye tissue")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP287152"] <- "The applications of deep sequencing technologies in life science research and clinical diagnostics have increased rapidly over the last decade. Although fast algorithms for data processing exist, intuitive, portable solutions for data analysis are still rare. For this purpose, we developed a web-based transcriptome database, which provides a platform-independent, intuitive solution to easily explore and compare ocular gene expression of 100 diseased and healthy human tissue samples from 15 different tissue types collected at the Eye Center of the University of Freiburg. To ensure comparability of expression between different tissues, reads were normalized across all 100 samples. Differentially expressed genes were calculated between each tissue type to determine tissue-specific genes. Unsupervised analysis of all 100 samples revealed an accurate clustering according to different tissue types and a high tissue specificity by analyzing known tissue-specific marker genes. Bioinformatic cell type deconvolution using xCell provided detailed insights into the cellular profiles of each tissue type. Several new tissue-specific marker genes were identified. These genes were involved in tissue- or disease-specific processes, such as myelination for the optic nerve, visual perception for retina, keratinocyte differentiation for conjunctival carcinoma, as well as endothelial cell migration for choroidal neovascularization membranes. The results are accessible at the Human Eye Transcriptome Atlas website at https://www.eye-transcriptome.com. In summary, this searchable transcriptome database enables easy exploration of ocular gene expression in healthy and diseased human ocular tissues without bioinformatics expertise. Thus, it provides rapid access to detailed insights into the molecular mechanisms of various ocular tissues and diseases, as well as the rapid retrieval of potential new diagnostic and therapeutic targets.")

#SRP300190
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP300190"] <- "The Human Eye Transcriptome Atlas: A searchable comparative transcriptome database for healthy and diseased human eye tissue")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP300190"] <- "The applications of deep sequencing technologies in life science research and clinical diagnostics have increased rapidly over the last decade. Although fast algorithms for data processing exist, intuitive, portable solutions for data analysis are still rare. For this purpose, we developed a web-based transcriptome database, which provides a platform-independent, intuitive solution to easily explore and compare ocular gene expression of 100 diseased and healthy human tissue samples from 15 different tissue types collected at the Eye Center of the University of Freiburg. To ensure comparability of expression between different tissues, reads were normalized across all 100 samples. Differentially expressed genes were calculated between each tissue type to determine tissue-specific genes. Unsupervised analysis of all 100 samples revealed an accurate clustering according to different tissue types and a high tissue specificity by analyzing known tissue-specific marker genes. Bioinformatic cell type deconvolution using xCell provided detailed insights into the cellular profiles of each tissue type. Several new tissue-specific marker genes were identified. These genes were involved in tissue- or disease-specific processes, such as myelination for the optic nerve, visual perception for retina, keratinocyte differentiation for conjunctival carcinoma, as well as endothelial cell migration for choroidal neovascularization membranes. The results are accessible at the Human Eye Transcriptome Atlas website at https://www.eye-transcriptome.com. In summary, this searchable transcriptome database enables easy exploration of ocular gene expression in healthy and diseased human ocular tissues without bioinformatics expertise. Thus, it provides rapid access to detailed insights into the molecular mechanisms of various ocular tissues and diseases, as well as the rapid retrieval of potential new diagnostic and therapeutic targets.")

#SRP257684
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP257684"] <- "3′ MACE RNA-sequencing allows for transcriptome profiling in human tissue samples after long-term storage")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP257684"] <- "This study aims to compare the potential of standard RNA-sequencing (RNA-Seq) and 3′ massive analysis of c-DNA ends (MACE) RNA-sequencing for the analysis of fresh tissue and describes transcriptome profiling of formalin-fixed paraffin-embedded (FFPE) archival human samples by MACE. To compare MACE to standard RNA-Seq on fresh tissue, four healthy conjunctiva from four subjects were collected during vitreoretinal surgery, halved and immediately transferred to RNA lysis buffer without prior fixation and then processed for either standard RNA-Seq or MACE RNA-Seq analysis. To assess the impact of FFPE preparation on MACE, a third part was fixed in formalin and processed for paraffin embedding, and its transcriptional profile was compared with the unfixed specimens analyzed by MACE. To investigate the impact of FFPE storage time on MACE results, 24 FFPE-treated conjunctival samples from 24 patients were analyzed as well. Nineteen thousand six hundred fifty-nine transcribed genes were detected by both MACE and standard RNA-Seq on fresh tissue, while 3251 and 2213 transcripts were identified explicitly by MACE or RNA-Seq, respectively. Standard RNA-Seq tended to yield longer detected transcripts more often than MACE technology despite normalization, indicating that the MACE technology is less susceptible to a length bias. FFPE processing revealed negligible effects on MACE sequencing results. Several quality-control measurements showed that long-term storage in paraffin did not decrease the diversity of MACE libraries. We noted a nonlinear relation between storage time and the number of raw reads with an accelerated decrease within the first 1000 days in paraffin, while the numbers remained relatively stable in older samples. Interestingly, the number of transcribed genes detected was independent on FFPE storage time. RNA of sufficient quality and quantity can be extracted from FFPE samples to obtain comprehensive transcriptome profiling using MACE technology. We thus present MACE as a novel opportunity for utilizing FFPE samples stored in histological archives.")

#SRP255891
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP255891"] <- "Transcriptional characterization of conjunctival melanoma identifies the cellular tumor microenvironment and prognostic gene signatures")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP255891"] <- "This study characterizes the transcriptome and the cellular tumor microenvironment (TME) of conjunctival melanoma (CM) and identifies prognostically relevant biomarkers. 12 formalin-fixed and paraffin-embedded CM were analyzed by MACE RNA sequencing, including six cases each with good or poor clinical outcome, the latter being defined by local recurrence and/or systemic metastases. Eight healthy conjunctival specimens served as controls. The TME of CM, as determined by bioinformatic cell type enrichment analysis, was characterized by the enrichment of melanocytes, pericytes and especially various immune cell types, such as plasmacytoid dendritic cells, natural killer T cells, B cells and mast cells. Differentially expressed genes between CM and control were mainly involved in inhibition of apoptosis, proteolysis and response to growth factors. POU3F3, BIRC5 and 7 were among the top expressed genes associated with inhibition of apoptosis. 20 genes, among them CENPK, INHA, USP33, CASP3, SNORA73B, AAR2, SNRNP48 and GPN1, were identified as prognostically relevant factors reaching high classification accuracy (area under the curve: 1.0). The present study provides new insights into the TME and the transcriptional profile of CM and additionally identifies new prognostic biomarkers. These results add new diagnostic tools and may lead to new options of targeted therapy for CM.")

#SRP329409
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP329409"] <- "Human Amniotic Epithelial Stem Cell-Derived Retinal Pigment Epithelium Cells Repair Retinal Degeneration")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP329409"] <- "Age-related macular degeneration (AMD), featured with dysfunction and loss of retinal pigment epithelium (RPE), is lacking efficient therapeutic approaches. According to our previous studies, human amniotic epithelial stem cells (hAESCs) may serve as a potential seed cell source of RPE cells for therapy because they have no ethical concerns, no tumorigenicity, and little immunogenicity. Herein, trichostatin A and nicotinamide can direct hAESCs differentiation into RPE like cells. The differentiated cells display the morphology, marker expression and cellular function of the native RPE cells, and noticeably express little MHC class II antigens and high level of HLA-G. Moreover, visual function and retinal structure of Royal College of Surgeon (RCS) rats, a classical animal model of retinal degeneration, were rescued after subretinal transplantation with the hAESCs-derived RPE like cells. Our study possibly makes some contribution to the resource of functional RPE cells for cell therapy. Subretinal transplantation of hAESCs-RPE could be an optional therapeutic strategy for retinal degeneration diseases.")

#SRP273695
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP273695"] <- "Implication of specific retinal cell-type involvement and gene expression changes in AMD progression using integrative analysis of single-cell and bulk RNA-seq profiling")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP273695"] <- "Age‐related macular degeneration (AMD) is a blinding eye disease with no unifying theme for its etiology. We used single-cell RNA sequencing to analyze the transcriptomes of ~93,000 cells from the macula and peripheral retina from two adult human donors and bulk RNA sequencing from fifteen adult human donors with and without AMD. Analysis of our single-cell data identified 267 cell-type-specific genes. Comparison of macula and peripheral retinal regions found no cell-type differences but did identify 50 differentially expressed genes (DEGs) with about 1/3 expressed in cones. Integration of our single-cell data with bulk RNA sequencing data from normal and AMD donors showed compositional changes more pronounced in macula in rods, microglia, endothelium, Müller glia, and astrocytes in the transition from normal to advanced AMD. KEGG pathway analysis of our normal vs. advanced AMD eyes identified enrichment in complement and coagulation pathways, antigen presentation, tissue remodeling, and signaling pathways including PI3K-Akt, NOD-like, Toll-like, and Rap1. These results showcase the use of single-cell RNA sequencing to infer cell-type compositional and cell-type-specific gene expression changes in intact bulk tissue and provide a foundation for investigating molecular mechanisms of retinal disease that lead to new therapeutic targets.")

#SRP287234
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP287234"] <- "RNA‐seq analysis of ageing human retinal pigment epithelium: Unexpected up‐regulation of visual cycle gene transcription")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP287234"] <- "Ageing presents adverse effects on the retina and is the primary risk factor for age‐related macular degeneration (AMD). We report the first RNA‐seq analysis of age‐related transcriptional changes in the human retinal pigment epithelium (RPE), the primary site of AMD pathogenesis. Whole transcriptome sequencing of RPE from human donors ranging in age from 31 to 93 reveals that ageing is associated with increasing transcription of main RPE‐associated visual cycle genes (including LRAT, RPE65, RDH5, RDH10, RDH11; pathway enrichment BH‐adjusted P = 4.6 × 10−6). This positive correlation is replicated in an independent set of 28 donors and a microarray dataset of 50 donors previously published. LRAT expression is positively regulated by retinoid by‐products of the visual cycle (A2E and all‐trans‐retinal) involving modulation by retinoic acid receptor alpha transcription factor. The results substantiate a novel age‐related positive feedback mechanism between accumulation of retinoid by‐products in the RPE and the up‐regulation of visual cycle genes.")

#SRP288670
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP288670"] <- "Reversed Senescence of Retinal Pigment Epithelial Cell by Coculture With Embryonic Stem Cell via the TGFβ and PI3K Pathways")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP288670"] <- "Retinal pigment epithelium (RPE) cellular senescence is an important etiology of age-related macular degeneration (AMD). Aging interventions based on the application of stem cells to delay cellular senescence have shown good prospects in the treatment of age-related diseases. This study aimed to investigate the potential of the embryonic stem cells (ESCs) to reverse the senescence of RPE cells and to elucidate its regulatory mechanism. The hydrogen peroxide (H2O2)-mediated premature and natural passage-mediated replicative senescent RPE cells were directly cocultured with ESCs. The results showed that the proliferative capacity of premature and replicative senescent RPE cells was increased, while the positive rate of senescence-associated galactosidase (SA-β-GAL) staining and levels of reactive oxygen species (ROS) and mitochondrial membrane potential (MMP) were decreased. The positive regulatory factors of cellular senescence (p53, p21WAF1/CIP1, p16INK4a) were downregulated, while the negative regulatory factors of cellular senescence (Cyclin A2, Cyclin B1, Cyclin D1) were upregulated. Furthermore, replicative senescent RPE cells entered the S and G2/M phases from the G0/G1 phase. TGFβ (TGFB1, SMAD3, ID1, ID3) and PI3K (PIK3CG, PDK1, PLK1) pathway-related genes were upregulated in premature and replicative senescent RPE cells after ESCs application, respectively. We further treated ESCs-cocultured premature and replicative senescent RPE cells with SB531542 and LY294002 to inhibit the TGFβ and PI3K pathways, respectively, and found that p53, p21WAF1/CIP1 and p16INK4a were upregulated, while Cyclin A2, Cyclin B1, Cyclin D1, TGFβ, and PI3K pathway-related genes were downregulated, accompanied by decreased proliferation and cell cycle transition and increased positive rates of SA-β-GAL staining and levels of ROS and MMP. In conclusion, we demonstrated that ESCs can effectively reverse the senescence of premature and replicative senescent RPE cells by a direct coculture way, which may be achieved by upregulating the TGFβ and PI3K pathways, respectively, providing a basis for establishing a new therapeutic option for AMD.")

#SRP326606
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP326606"] <- "Capsule-dependent impact of MAPK signalling on host cell invasion and immune response during infection of the choroid plexus epithelium by Neisseria meningitidis")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP326606"] <- 
                                                   "Background: The Gram-negative bacterium Neisseria meningitidis (Nm) can cause meningitis in humans, but the host signalling pathways manipulated by Nm during central nervous system (CNS) entry are not completely understood.
Methods: We investigate the role of the mitogen-activated protein kinases (MAPK) Erk1/2 and p38 in an in vitro model of the blood-cerebrospinal fuid barrier (BCSFB) based on human epithelial choroid plexus (CP) papilloma (HIBCPP) cells during infection with Nm serogroup B (NmB) and serogroup C (NmC) strains. A transcriptome analysis of HIBCPP cells following infection with Nm by massive analysis of cDNA ends (MACE) was done to further characterize the cellular response to infection of the barrier.
Results: Interestingly, whereas NmB and NmC wild type strains required active Erk1/2 and p38 pathways for infection, invasion by capsule-defcient mutants was independent of Erk1/2 and, in case of the NmB strain, of p38 activity. The transcriptome analysis of HIBCPP cells following infection with Nm demonstrated specifc regulation of genes involved in the immune response dependent on Erk1/2 signalling. Gene ontology (GO) analysis confrmed loss of MAPK signalling after Erk1/2 inhibition and revealed an additional reduction of cellular responses including NFκB and JAK-STAT signalling. Interestingly, GO terms related to TNF signalling and production of IL6 were lost specifcally following Erk1/2 inhibition during infection with wild type Nm, which correlated with the reduced infection rates by the wild type in absence of Erk1/2 signalling.
Conclusion: Our data point towards a role of MAPK signalling during infection of the CP epithelium by Nm, which is strongly infuenced by capsule expression, and afects infection rates as well as the host cell response.")

#SRP193153
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP193153"] <- "A Comparative Transcriptome Analysis of Human and Porcine Choroid Plexus Cells in Response to Streptococcus suis Serotype 2 Infection Points to a Role of Hypoxia")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP193153"] <- "Streptococcus suis (S. suis) is an important opportunistic pathogen, which can cause septicemia and meningitis in pigs and humans. Previous in vivo observations in S. suis-infected pigs revealed lesions at the choroid plexus (CP). In vitro experiments with primary porcine CP epithelial cells (PCPEC) and human CP epithelial papilloma (HIBCPP) cells demonstrated that S. suis can invade and traverse the CP epithelium, and that the CP contributes to the inflammatory response via cytokine expression. Here, next generation sequencing (RNA-seq) was used to compare global transcriptome profiles of PCPEC and HIBCPP cells challenged with S. suis serotype (ST) 2 infected in vitro, and of pigs infected in vivo. Identified differentially expressed genes (DEGs) were, amongst others, involved in inflammatory responses and hypoxia. The RNA-seq data were validated via quantitative PCR of selected DEGs. Employing Gene Set Enrichment Analysis (GSEA), 18, 28, and 21 enriched hallmark gene sets (GSs) were identified for infected HIBCPP cells, PCPEC, and in the CP of pigs suffering from S. suis ST2 meningitis, respectively, of which eight GSs overlapped between the three different sample sets. The majority of these GSs are involved in cellular signaling and pathways, immune response, and development, including inflammatory response and hypoxia. In contrast, suppressed GSs observed during in vitro and in vivo S. suis ST2 infections included those, which were involved in cellular proliferation and metabolic processes. This study suggests that similar cellular processes occur in infected human and porcine CP epithelial cells, especially in terms of inflammatory response.")

#SRP119766
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP119766"] <- "Clinical and genetic heterogeneity associated with retinal diseases makes stem cell-based therapies an attractive strategy for personalized medicine. However, we have limited understanding of the timing of key events in the developing human retina, and in particular the factors critical for generating the unique architecture of the fovea and surrounding macula. Here we define three key epochs in the transcriptome dynamics of human retina from fetal day (D) 52 to 150. Coincident histological analyses confirmed the cellular basis of transcriptional changes and highlighted the dramatic acceleration of development in the fovea compared to peripheral retina. Human and mouse retinal transcriptomes show remarkable similarity in developmental stages, though morphogenesis was greatly expanded in humans. Integration of DNA accessibility data allowed us to reconstruct transcriptional networks controlling photoreceptor differentiation. Our studies provide insights into human retinal development and serve as resource for molecular staging of human stem cell-derived retinal organoids. Overall design: Whole human fetal retina samples [spanning 12 time points: D52/54, D53, D57, D67, D80, D94 (2 samples), D105, D107, D115, D125, D132 and D136] or dissected retinal regions [at four time points: D59 (periphery and central, 2 samples each), D73 (periphery and fovea/macula), D96 (periphery, fovea/macula, and nasal central) and D132 (periphery, fovea/macula, and nasal central)] were used to generate RNA-seq libraries.")

#SRP159246
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP159246"] <- "The mechanisms underlying specification of neuronal subtypes within the human nervous system are largely unknown. The blue/S, green/M and red/L cones of the retina enable high-acuity daytime and color vision. To determine the mechanism controlling S vs. L/M fates, we studied the differentiation of human retinal organoids. Organoids and retinas have similar distributions, expression profiles, and morphologies of cone subtypes. S cones are specified first, followed by L/M cones, and thyroid hormone signaling controls this temporal switch. Dynamic expression of thyroid hormone-degrading and activating proteins within the retina ensures low signaling early to specify S cones and high signaling late to produce L/M cones. This work establishes organoids as a model for determining mechanisms of human development with promising utility for therapeutics and vision repair. Overall design: EP1 iPSC-derived organoids were analyzed at time points ranging from day 10 to day 250 of differentiation. We took samples at day 10 (n=3), day 20 (n=2), day 35 (n=3), day 69 (n=3), day 111 (n=3), day 128 (n=3), day 158 (n=2), day 173 (n=3), day 181 (n=3), day 200 (n=3), and day 250 7 (n=3). RNA from individual organoids was extracted using the Zymo Direct-zol RNA Microprep Kit (Zymo Research) according to manufacturer's instructions. Individual libraries were prepared for each sample using the Illumina TruSeq stranded mRNA kit and sequenced on an Illumina NextSeq 500 with single 200 bp reads.")

#SRP323408
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP323408"] <- "Integrated analysis of transcriptome and proteome of the human cornea and aqueous humor reveal novel biomarkers for corneal endothelial cell dysfunction")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP323408"] <- "Previous studies have reported that elevated protein levels in the aqueous humor (AH) are associated with corneal endothelial cell dysfunction (CECD), but the detailed underlying mechanism as well as specific biomarkers for CECD remain elusive. In the present study, we aimed to identify protein markers in AH directly associated with changes in Corneal endothelial cells (CECs), as AH can be easily obtained for analysis. We carried out an in-depth proteomic analysis of patient-derived AH as well as transcriptomic analysis of CECs from the same patients with bullous keratopathy (BK) resulting from CECD. We first determined differentially expressed genes (DEGs) and differentially expressed proteins (DEPs) from CECs and AH in CECD, respectively. By combining transcriptomic and proteomic analyses, 13 shared upregulated markers were observed between DEGs and DEPs. Through data integration with individual analysis via data-independent acquisition, three upregulated transcripts and proteins were identified, namely metallopeptidase inhibitor 1 (TIMP1), Fc fragment of IgG binding protein (FCGBP), and angiopoietin-related protein 7 (ANGPTL7). Furthermore, we confirmed AH biomarkers for CECD using individual proteome verification and immunoassay validation. Conclusively, our findings may provide valuable insights into the disease process and identify biofluid markers for the assessment of CEC function during BK development. Overall design: human corneal endothelium mRNA profiles of Normal and CECD were generated by deep sequencing, in triplicate, using Illumina NovaSeq 6000.")

#SRP331221
combined_metadata <- combined_metadata <- within(combined_metadata, study_title[study_accession == "SRP331221"] <- "Transcitptome of retinablastoma and retina")
combined_metadata <- combined_metadata <- within(combined_metadata, study_abstract[study_accession == "SRP331221"] <- "RNAseq data of retinablastoma related study")

### Renaming GTEX run_accessions to include external_id / Omitting repeated metadata since counts will be aggregated to the sample level in the eyeIntegration database -----

#Assigning GTEX samples in the metadata an external_id
combined_metadata <- combined_metadata %>% mutate(gtex_external_id_a = str_extract(sample_attribute, "GTEX-[A-Z0-9]+-[0-9]+-SM-[A-Z0-9]+"))
combined_metadata <- combined_metadata %>% mutate(gtex_external_id_b = str_extract(sample_attribute, "GTEX-[A-Z0-9]+-[0-9]+-[a-zA-Z0-9]+-SM-[A-Z0-9]+"))
combined_metadata <- combined_metadata %>% mutate(gtex_external_id_c = str_extract(sample_attribute, "K-[A-Z0-9]+-SM-[A-Z0-9]+"))
combined_metadata <- combined_metadata %>% mutate(gtex_external_id = ifelse((is.na(gtex_external_id_a)), ifelse((is.na(gtex_external_id_b)), gtex_external_id_c, gtex_external_id_b), gtex_external_id_a))
#Removing the temporarily produced gtex_id columns
combined_metadata <- combined_metadata %>% select(-c(gtex_external_id_a, gtex_external_id_b, gtex_external_id_c))

#The gtex_data_final csv file was generated from a separate script and contains sample level information to convert our GTEX run accessions into external_ids
gtex_data <- vroom::vroom("/Users/parikhpp/NEI Projects/bigwigs/gtex_data_final.csv", col_types = cols(...1 = col_skip()))
gtex_data$external_id <- gsub("-", ".", gtex_data$external_id)
#Splitting the metadata to perform the necessary join and run_accession replacement on GTEX samples
combined_metadata_gtex <- combined_metadata %>% filter(study_accession == "SRP012682")
combined_metadata_non_gtex <- combined_metadata %>% filter(study_accession != "SRP012682")
#Joining sample level data
combined_metadata_gtex_final <- gtex_data %>% select("external_id", "gtex_external_id") %>% left_join(combined_metadata_gtex, by = "gtex_external_id")
#Rewriting the run_accessions
combined_metadata_gtex_final$run_accession <- combined_metadata_gtex_final$external_id
#Removing duplicates resulting from rewriting the data to the sample level
combined_metadata_gtex_final <- combined_metadata_gtex_final[!duplicated(combined_metadata_gtex_final[,c("run_accession")]),]

#Reconstructing the combined metadata and removing external_id columns as they are no longer of use
combined_metadata <- bind_rows(combined_metadata_non_gtex, combined_metadata_gtex_final) %>% select(-c("gtex_external_id", "external_id"))

### Filling in sample_attribute data -----

#Replacing blank SRR attributes
srr_blank_attributes <- which(is.na(combined_metadata$sample_attribute) & grepl("SRR", combined_metadata$run_accession))

for (i in 1:length(srr_blank_attributes)) {
  attribute_dataframe <-
    combined_metadata[srr_blank_attributes[i],] %>% 
    pull("run_accession") %>% 
    samn_getter() %>% 
    attribute_df_maker() %>%
    select(-id)
  sample_attr <- paste(do.call(paste, c(attribute_dataframe, sep = ": ")), collapse = " || ")
  #Make final attribute
  combined_metadata <- combined_metadata %>% mutate(sample_attribute = sample_attr)
  Sys.sleep(5)
}

combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236614"] <- "Name: Sample 1_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236615"] <- "Name: Sample 10_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236616"] <- "Name: Sample 11_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236617"] <- "Name: Sample 12_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236618"] <- "Name: Sample 13_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236619"] <- "Name: Sample 14_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236620"] <- "Name: Sample 15_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236621"] <- "Name: Sample 16_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236622"] <- "Name: Sample 17_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236623"] <- "Name: Sample 18_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236624"] <- "Name: Sample 19_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236625"] <- "Name: Sample 2_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236626"] <- "Name: Sample 20_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236627"] <- "Name: Sample 21_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236628"] <- "Name: Sample 22_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236629"] <- "Name: Sample 23_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236630"] <- "Name: Sample 24_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236631"] <- "Name: Sample 25_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236632"] <- "Name: Sample 26_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236633"] <- "Name: Sample 27_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236634"] <- "Name: Sample 28_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236635"] <- "Name: Sample 29_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236636"] <- "Name: Sample 3_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236637"] <- "Name: Sample 30_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236638"] <- "Name: Sample 31_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236639"] <- "Name: Sample 32_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236640"] <- "Name: Sample 33_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236641"] <- "Name: Sample 34_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236642"] <- "Name: Sample 35_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236643"] <- "Name: Sample 36_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236644"] <- "Name: Sample 37_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236645"] <- "Name: Sample 38_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236646"] <- "Name: Sample 39_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236647"] <- "Name: Sample 4_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236648"] <- "Name: Sample 40_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236649"] <- "Name: Sample 41_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236650"] <- "Name: Sample 42_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236651"] <- "Name: Sample 43_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236652"] <- "Name: Sample 44_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236653"] <- "Name: Sample 45_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236654"] <- "Name: Sample 46_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236655"] <- "Name: Sample 47_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236656"] <- "Name: Sample 48_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236657"] <- "Name: Sample 49_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236658"] <- "Name: Sample 5_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236659"] <- "Name: Sample 50_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236660"] <- "Name: Sample 6_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236661"] <- "Name: Sample 7_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236662"] <- "Name: Sample 8_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")
combined_metadata <- within(combined_metadata, sample_attribute[run_accession == "ERR5236663"] <- "Name: Sample 9_p || Instrument: Illumina HiSeq 1000 || Strategy: RNA-Seq || Source: TRANSCRIPTOMIC || Selection: cDNA || Layout: PAIRED")

### Quality Check -----

# Import mapping data for all samples
mapping_data <- vroom::vroom("/Users/parikhpp/git/EiaD_build/mapping_data/mapping_data.csv")
mapping_data$external_id <- gsub('-','.', mapping_data$external_id)

#Only samples in eyeIntegration
mapping_data_keep <- intersect(mapping_data$external_id, eyeIntegration22$run_accession)
mapping_data_final <- mapping_data[mapping_data$external_id %in% mapping_data_keep,]

# intron percentage (high is bad)
mapping_data %>%
  left_join(combined_metadata, by = c('external_id' = 'run_accession')) %>%
  mutate(Tissue = case_when(Tissue == 'RPE' ~ 'RPE/Choroid', TRUE ~ Tissue)) %>%
  filter(!grepl("GTEX", external_id)) %>%
  ggplot(aes(x=recount_qc.intron_sum_., y = Tissue)) +
  ggridges::geom_density_ridges2() +
  cowplot::theme_cowplot()

# alignment percentage (high is good)
mapping_data %>% mutate(align_perc = (recount_qc.star.all_mapped_reads %>% as.numeric()) / (recount_qc.star.number_of_input_reads %>% as.numeric())) %>%
  left_join(combined_metadata, by = c('external_id' = 'run_accession')) %>%
  mutate(Tissue = case_when(Tissue == 'RPE' ~ 'RPE/Choroid', TRUE ~ Tissue)) %>%
  filter(!grepl("GTEX", external_id)) %>%
  ggplot(aes(x=align_perc, y = Tissue)) +
  ggridges::geom_density_ridges2() +
  cowplot::theme_cowplot()

srr_keep <- mapping_data %>%
  mutate(align_perc = (recount_qc.star.all_mapped_reads %>% as.numeric()) / (recount_qc.star.number_of_input_reads %>% as.numeric())) %>%
  filter(align_perc > 0.6, (recount_qc.intron_sum_. %>% as.numeric()) < 20) %>%
  pull(external_id)
srr_keep <- gsub('-','.', srr_keep)

low_alignment_percentage <- mapping_data %>%
  mutate(align_perc = (recount_qc.star.all_mapped_reads %>% as.numeric()) / (recount_qc.star.number_of_input_reads %>% as.numeric())) %>%
  filter(align_perc < 0.6) %>%
  pull(external_id)

high_intron_percentage <- mapping_data %>%
  filter(recount_qc.intron_sum_. %>% as.numeric() > 20) %>%
  pull(external_id)

# Labeling samples to use and discard
combined_metadata_final <- combined_metadata %>% mutate(Kept = case_when(run_accession %in% srr_keep ~ 'Kept',
                                                                         run_accession %in% low_alignment_percentage ~ 'low alignment percentage',
                                                                         run_accession %in% high_intron_percentage ~ 'high_intron_percentage'))

### Creating final metadata file for publication -----

# write.csv(combined_metadata_final, "2022_metadata.csv", row.names = FALSE)