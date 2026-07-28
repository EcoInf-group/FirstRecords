##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                    Standardize remaining terms                       ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, October 2025                                                   ##
##########################################################################

fr_terms_standard <- function(dataset = NULL, 
                                  save_to_disk = FALSE,
                                  use_log = FALSE, 
                                  data_dir = NULL) {
 
   if (is.null(dataset) || !is.data.table(dataset)) {
    stop("Error: argument 'dataset' must be a non-null data.table.")
   }  
  
  # --- Reset unresolved-terms log at the start of each run ---
  unlink(file.path(data_dir, "tmp", "log_unresolved_terms.csv"))
  unlink(file.path(data_dir, "tmp", "unresolved_groups"), recursive = TRUE)
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # open log file for appending
    } else {
      sink(log_file, append = FALSE, type = "message") # create new log file
    }
  }
  cat("\nSTEP 5: Standardize remaining terms") 
  
  # --- 1. degreeOfEstablishment ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    log_unresolved = TRUE,
    log_group = "speciesStatus",
    term = "degreeOfEstablishment",
    data_dir = data_dir)
  
  # --- 2. habitat ----
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    log_unresolved = TRUE,
    term = "habitat",
    data_dir = data_dir)

  # --- 3. establishmentMeans ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    log_unresolved = TRUE,
    log_group = "speciesStatus",
    term = "establishmentMeans",
    data_dir = data_dir)
  
  # --- 4. occurrenceStatus ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    log_unresolved = TRUE,
    log_group = "speciesStatus",
    term = "occurrenceStatus",
    data_dir = data_dir)

  # Clean whitespace 
  dataset[, occurrenceStatus := trimws(occurrenceStatus)]
  dataset[, occurrenceStatus := tolower(occurrenceStatus)]
  dataset[
    is.na(occurrenceStatus) |
      occurrenceStatus %in% c("", "null", "na"),
    occurrenceStatus := "present"
  ]  
  
  # --- Resolve "PresentStatus" group: keep only values unresolved in ALL 3 terms ---
  group_dir <- file.path(data_dir, "tmp", "unresolved_groups")
  group_files <- list.files(group_dir, pattern = "^PresentStatus__", full.names = TRUE)
  
  if (length(group_files) > 0) {
    group_lists <- lapply(group_files, function(f) fread(f, colClasses = "character")$unmatched_term)
    truly_unresolved <- Reduce(intersect, group_lists)
    
    if (length(truly_unresolved) > 0) {
      unresolved_final <- data.table(unmatched_term = truly_unresolved)
      filename_unres <- file.path(data_dir, "tmp", "log_unresolved_terms.csv")
      
      if (file.exists(filename_unres)) {
        existing <- fread(filename_unres, quote = "\"", header = TRUE, colClasses = "character")
        combined <- unique(rbind(existing, unresolved_final, fill = TRUE))
      } else {
        combined <- unresolved_final
      }
      
      fwrite(combined, filename_unres, quote = TRUE)
      
      cat(
        "\nWarning: ", length(truly_unresolved),
        " PresentStatus-derived terms could not be standardized against ANY of ",
        "degreeOfEstablishment/establishmentMeans/occurrenceStatus. Logged to '",
        filename_unres, "' for manual review.\n",
        sep = ""
      )
    }
  }
  
  # --- clean up temp group files ---
  unlink(group_dir, recursive = TRUE)
  
  # --- 5. pathway ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    log_unresolved = TRUE,
    term = "pathway",
    data_dir = data_dir)  
  
  # --- 6. datasetName ---
  
  dataset <- standardize_terms(
    dataset = dataset, 
    save_to_disk = FALSE,
    log_unresolved = FALSE,
    term = "datasetName",
    data_dir = data_dir) 
  
  # ---7. Save updated main dataset
  
  if (save_to_disk == TRUE){
    filename <- file.path(data_dir, "tmp", "fr_main_dataset_step5.csv")
    fwrite(dataset, filename)
    cat("\n  - Updated dataset available in 'tmp' folder\n ")
  }
  
  cat("\nStep 5 completed: remaining terms have been standardized") 
  
  if (use_log == TRUE){
    sink()
  }
  
  return(dataset)
}