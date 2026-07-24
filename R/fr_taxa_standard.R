##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                        Standardize taxonomy                          ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## v2.0, October 2025                                                   ##
##########################################################################

fr_taxa_standard <- function(dataset = NULL, 
                               use_log = FALSE, 
                               save_to_disk = FALSE,
                               data_dir = data_dir
                               ){
  
  if (is.null(dataset) || !is.data.table(dataset)) {
    stop("Error: argument 'dataset' must be a non-null data.table.")
  }
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # Open log file for appending
    } else {
      sink(log_file, append = FALSE) # Create new log file
    }
  }
  cat("\nSTEP 2: Standardize taxa") 

  # --- 1. Clean taxon names ---
  dataset[, originalNameUsage := str_squish(trimws(originalNameUsage))] # remove leading, trailing and multiple white spaces
  dataset <- dataset[originalNameUsage != "" & !is.na(originalNameUsage)] # delete rows without taxa
  cat("\n - Removed leading and trailing whitespace\n - Deleted extra internal white spaces\n - Deleted empty rows") 
  
  # --- 2. Call GBIF check function ---
  
  # if (use_log == TRUE){
  #   sink()
  # }
  
  gbif_result <- check_GBIF_taxa_parallel(taxon_names = dataset$originalNameUsage, # vector or data.frame
                                          # save_interm=TRUE,
                                          data_dir=data_dir)
  matched_taxa <- unique(gbif_result[[1]]) # extract unique standardized species names
  mismatches <- unique(gbif_result[[2]][order(gbif_result[[2]]$taxon)]) # extract unique unmatched species names, ordered by taxa
  
  setDT(matched_taxa)
  setDT(mismatches)
  
  matched_taxa[, taxon := fifelse(is.na(taxon), originalNameUsage, taxon)] # fill NA in taxon with originalNameUsage in matched_taxa
  matched_taxa[, GBIFstatus := fifelse(is.na(GBIFstatus), "NoMatch", GBIFstatus)] # replace unmatched species by "NoMatch" in matched_taxa

  ## identify taxa with deviating taxonomic trees, remove from matched_taxa and add to mismatches
  unique_taxa <- unique((matched_taxa[, .(phylum, taxon)])) # extract unique, non-missing taxon names from matched_taxa
  setorder(unique_taxa)
  ind_dupl <- which(duplicated(unique_taxa$taxon)) # same taxon but with different taxaGroup -> unresolved taxonomic tree
  further_mismatches_ind <- matched_taxa$taxon%in%unique_taxa$taxon[ind_dupl] # extract unresolved taxa
  mismatches <- rbind(mismatches, matched_taxa[further_mismatches_ind]) # add to mismatches
  matched_taxa <- matched_taxa[!further_mismatches_ind] # remove from matched

  cat("\n - Standardized taxon names across the GBIF backbone taxonomy") 
  
  # --- 3. Define taxonomic groups: fill in the column taxaGroup, based on class and order ---
  matched_taxa$taxaGroup <- NA
  matched_taxa$taxaGroup[matched_taxa$class=="Mammalia"] <- "Mammals"
  matched_taxa$taxaGroup[matched_taxa$class=="Aves"] <- "Birds"
  matched_taxa$taxaGroup[matched_taxa$class%in%c("Cephalaspidomorphi","Actinopterygii","Elasmobranchii","Sarcopterygii", "Petromyzonti")] <- "Fishes"
  matched_taxa$taxaGroup[matched_taxa$order%in%c("Polypteriformes", "Acipenseriformes", "Lepisosteiformes", "Amiiformes", "Osteoglossiformes", "Hiodontiformes", 
                                                                                       "Elopiformes", "Albuliformes", "Notacanthiformes", "Anguilliformes", "Saccopharyngiformes", "Clupeiformes", "Ceratodontiformes",
                                                                                       "Gonorynchiformes", "Cypriniformes", "Characiformes", "Gymnotiformes", "Siluriformes", "Salmoniformes", "Esociformes", 
                                                                                       "Osmeriformes", "Ateleopodiformes", "Stomiiformes", "Aulopiformes", "Myctophiformes", "Lampriformes", "Polymixiiformes", 
                                                                                       "Percopsiformes", "Batrachoidiformes", "Lophiiformes", "Gadiformes", "Ophidiiformes", "Mugiliformes", "Atheriniformes", 
                                                                                       "Beloniformes", "Cetomimiformes", "Cyprinodontiformes", "Stephanoberyciformes", "Beryciformes", "Zeiformes", 
                                                                                       "Gobiesociformes", "Gasterosteiformes", "Syngnathiformes", "Synbranchiformes", "Tetraodontiformes", "Pleuronectiformes", 

                                                                                       "Scorpaeniformes", "Perciformes")] <- "Fishes"
  matched_taxa$taxaGroup[matched_taxa$class%in%c("Reptilia", "Testudines","Squamata", "Crocodylia")] <- "Reptiles"
  matched_taxa$taxaGroup[matched_taxa$class=="Amphibia"] <- "Amphibians"
  matched_taxa$taxaGroup[matched_taxa$class%in%c("Insecta")] <- "Insects"
  matched_taxa$taxaGroup[matched_taxa$class%in%c("Arachnida", "Pycnogonida")] <- "Arachnids"
  matched_taxa$taxaGroup[matched_taxa$class%in%c("Collembola", "Chilopoda", "Diplopoda", "Diplura", "Merostomata", "Pauropoda", "Protura", "Symphyla")] <- "Other arthropods"
  matched_taxa$taxaGroup[matched_taxa$class%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca", "Copepoda")] <- "Crustaceans"
  matched_taxa$taxaGroup[matched_taxa$family%in%c("Elminiidae")] <- "Crustaceans"
  matched_taxa$taxaGroup[matched_taxa$phylum=="Mollusca"] <- "Molluscs"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%"Tracheophyta"] <- "Vascular plants"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%c("Bryophyta","Anthocerotophyta", "Marchantiophyta")] <- "Bryophytes"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Euglenozoa","Haptophyta","Foraminifera","Ciliophora","Ochrophyta","Myzozoa","Cercozoa")] <- "Algae"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%c("Ascomycota", "Dothideomycetes", "Sordariomycetes", "Chytridiomycota","Basidiomycota","Microsporidia","Zygomycota", "Entomophthoromycota")] <- "Fungi"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria") |
                           matched_taxa$class == "Ichthyosporea"] <- "Bacteria and protozoans"
  matched_taxa$taxaGroup[matched_taxa$kingdom%in%c("Bacteria", "Protozoa","Euglenozoa")] <- "Bacteria and protozoans"
  # matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Oomycota"))$scientificName] <- "Oomycetes"
  matched_taxa$taxaGroup[matched_taxa$kingdom%in%c("Viruses")] <- "Viruses"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%c("Annelida", "Nematoda", "Platyhelminthes", "Sipuncula", "Nemertea", "Onychophora", "Acanthocephala")] <- "Annelids, nematodes, platyhelminthes, and other worms"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%c("Bryozoa", "Entoprocta", "Chaetognatha", "Cnidaria", "Ctenophora", "Echinodermata", "Phoronida", "Porifera", "Rotifera", "Xenacoelomorpha","Brachiopoda")] <- "Other aquatic animals"
  matched_taxa$taxaGroup[matched_taxa$class=="Ascidiacea"] <- "Other aquatic animals"
  matched_taxa$taxaGroup[matched_taxa$phylum%in%c("Foraminifera","Cercozoa","Ciliophora","Ochrophyta","Oomycota","Myzozoa","Peronosporea", "Bigyra")] <- "SAR"
  # matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, genus%in%c("Plasmodium"))$scientificName] <- "SAR"
  # matched_taxa$taxaGroup[is.na(matched_taxa$taxaGroup)] <- "Other" # avoid "other" as also un-matched taxa will be assigned
  
  cat("\n - Allocated taxonomic groups")

  # --- 4. Creating and adding unique taxonIDs ---
  unique_taxa <- unique((matched_taxa[, .(taxaGroup, taxon)])) # extract unique, non-missing taxon names from matched_taxa
  setorder(unique_taxa)
  unique_taxa[, taxonID := .I] # assign a unique integer ID (taxonID) to each taxon ('.I returns the row index, which is used as an ID)
  matched_taxa <- merge(matched_taxa, unique_taxa[, .(taxon, taxonID)], by = "taxon", all = TRUE) # merge taxonID in matched_taxa
  
  cat("\n - Allocated taxon IDs")
  
  # --- 5. Write output ---
  
  ## Create fr_main_dataset_step2
  
  fr_main_dataset_step2 <- merge(dataset, matched_taxa[, c("originalNameUsage", "taxon", "taxonID")], by="originalNameUsage", all.x=T)
                                 
  # fr_main_dataset_step2 <- matched_taxa[, c("locationID", "verbatimLocation", "locality", "country", "region", "taxonID", "taxon",
  #                                           "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
  #                                           "confidenceFirstRecordEvent",	"occurrenceStatus",	"establishmentMeans",
  #                                           "degreeOfEstablishment", "pathway",	"datasetName",	"bibliographicCitation",	
  #                                           "accessRights", "originalNameUsage", "scientificName", "scientificNameAuthorship", 
  #                                           "GBIFstatus","GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
  #                                           "GBIFusageKey","GBIFnote","species","genus","family",
  #                                           "order","class","phylum","kingdom", "taxaGroup"
  # )]
  
  if (save_to_disk == TRUE){
    filename <- file.path(data_dir, "tmp", "fr_main_dataset_step2.csv")
    fwrite(fr_main_dataset_step2, filename)
    cat("\n - Updated dataset available in 'tmp' folder\n ")
  }
  
  ## Create a temporary taxonomy table

  temp_taxonomy_table <- matched_taxa
  # temp_taxonomy_table <- unique(matched_taxa[,c("taxonID", "taxon", "originalNameUsage", "scientificName", "scientificNameAuthorship", 
  #                                          "GBIFstatus","GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
  #                                          "GBIFusageKey","GBIFnote","species","genus","family",
  #                                          "order","class","phylum","kingdom", "taxaGroup"
  # )])

  
  filename <- file.path(data_dir, "tmp", "temp_taxonomy_table.csv")
  fwrite(temp_taxonomy_table, filename)
  
  ## Create the unmatched taxa table
  filename <- file.path(data_dir, "tmp", "fr_check_unresolved_taxa.csv")
  fwrite(mismatches, filename)
  
  cat("Step 2 completed: taxa have been standardized. Unmatched taxa and a temporary taxonomy table are available in the 'tmp' folder\n ")
  
  if (use_log == TRUE){
    sink()
  }
  
  return(fr_main_dataset_step2)
}