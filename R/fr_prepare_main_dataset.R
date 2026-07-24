fr_prepare_main_dataset <- function (dataset = NULL,
                                     Country = NULL,
                                     originalName = NULL,
                                     FirstRecord = NULL,
                                     use_log = FALSE, 
                                     save_to_disk = FALSE, 
                                     data_dir = NULL
){
  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  
  dataset <- copy(dataset)
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)
    } else {
      sink(log_file, append = FALSE)
    }
  }
  cat("\n --- FIRST RECORD ", format(Sys.Date(), "%Y-%m-%d"), " ---\n ")
  cat("\nSTEP 1: Prepare main dataset: fr_main_dataset") 
  
  # --- Check required (user-mapped) columns exist in dataset ---
  required_map <- c(Country = Country, originalName = originalName, FirstRecord = FirstRecord)
  missing_required <- required_map[!required_map %in% names(dataset)]
  if (length(missing_required) > 0){
    stop(paste0(
      "Missing required column(s) in input dataset: ",
      paste(missing_required, collapse = ", "),
      ". Check the column-name arguments you passed to fr_prepare_main_dataset()."
    ))
  }
  
  cat("\n  - Confirmed required columns are present (mapped as: Country='", Country,
      "', originalName='", originalName, "', FirstRecord='", FirstRecord, "')", sep = "")
  
  # --- Rename user-supplied columns to internal standard names ---
  setnames(dataset, old = c(Country, originalName, FirstRecord),
           new = c("Country", "OriginalNameUsage1", "FirstRecord"),
           skip_absent = TRUE)
  
  # --- Ensure optional columns exist (create as "" if missing) ---
  optional_cols <- c(
    "Author", "Genus", "Species", "Habitat", "originalNameUsage2", 
    "DateNaturalisation", "FirstRecord_intentional", "FirstRecord1", "FirstRecord2",
    "PresentStatus", "Pathway", "Source", "DataUsage"
  )
  missing_optional <- optional_cols[!optional_cols %in% names(dataset)]
  if (length(missing_optional) > 0){
    dataset[, (missing_optional) := ""]
    cat("\n  - Created missing optional column(s) as empty strings: ", paste(missing_optional, collapse = ", "))
  }
  
  # --- Prepare master dataset ---
  
  dataset[, Author := fifelse(is.na(Author), "", as.character(Author))]
  
  # merge columns of taxonomic information without introducing NAs
  # NOTE: originalName column was already renamed to "OriginalNameUsage1" above
  dataset[, originalNameUsage1 := fifelse(!is.na(OriginalNameUsage1) & OriginalNameUsage1 != "", 
                                          paste(OriginalNameUsage1, Author), "")]
  dataset[, originalNameUsage2 := fifelse(!is.na(Genus) & Genus != "", paste(Genus, Species, Author), "")]
  
  dataset <- dataset[, .(
    locationID = "",
    verbatimLocation = Country,
    location = Country,
    country = "",
    region = "",
    originalNameUsage = "",
    originalNameUsage1 = originalNameUsage1,
    originalNameUsage2 = originalNameUsage2,
    habitat = Habitat,
    FirstRecord1,
    FirstRecord2,
    FirstRecord,
    DateNaturalisation,
    FirstRecord_intentional,
    firstRecordEvent = "",
    verbatimFirstRecordEvent = "",
    confidenceFirstRecordEvent = "",
    occurrenceStatus = PresentStatus,
    establishmentMeans = PresentStatus,
    degreeOfEstablishment = PresentStatus,
    pathway = Pathway,
    datasetName = Source,
    bibliographicCitation = Source,
    accessRights = DataUsage
  )]
  cat("\n  - Loaded relevant columns")
  
  # --- Basic cleaning ---
  dataset[is.na(dataset)] <- ""
  dataset[, names(dataset) := lapply(.SD, function(x) {
    if (is.character(x)) {
      x <- gsub("(?i)null", "", x, perl = TRUE)
      x <- gsub("\\.", "", x)
      x <- gsub("(?i)unknown", "", x, perl = TRUE)
      x <- gsub("\\?", "", x, perl = TRUE)
      x <- gsub("NA NA", "", x)
      x <- gsub("^NA$", "", x)
      x <- str_squish(x)
      x
    } else {
      x
    }
  })]
  cat("\n  - Replaced NA, 'NULL', 'unknown', 'n.d.' and '?' with empty strings")
  
  dataset <- dataset[
    rowSums(!(is.na(dataset) | dataset == "")) > 0
  ]
  cat("\n  - Deleted rows where all columns are empty")
  
  # --- Prepare first record columns ---
  dataset[, verbatimFirstRecordEvent := FirstRecord]
  dataset[
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord1 != "",
    verbatimFirstRecordEvent := FirstRecord1
  ]
  dataset[
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord2 != "",
    verbatimFirstRecordEvent := FirstRecord2
  ]
  dataset[
    verbatimFirstRecordEvent %in% c("", NA) & DateNaturalisation != "",
    verbatimFirstRecordEvent := DateNaturalisation
  ]
  dataset[
    verbatimFirstRecordEvent %in% c("", NA) & FirstRecord_intentional != "",
    verbatimFirstRecordEvent := FirstRecord_intentional
  ]
  dataset[
    FirstRecord1 != "" & FirstRecord2 != "",
    verbatimFirstRecordEvent := paste(FirstRecord1, FirstRecord2, sep = " - ")
  ]
  
  dataset[, firstRecordEvent := verbatimFirstRecordEvent]
  cat("\n  - Stored original first records in verbatimFirstRecord")
  
  dataset[, confidenceFirstRecordEvent := "low confidence"]
  cat("\n  - Created confidenceFirstRecordEvent column and initialized it with 'low confidence'")
  
  dataset[, originalNameUsage := fifelse(
    !is.na(originalNameUsage1) & originalNameUsage1 != "",
    originalNameUsage1,
    originalNameUsage2
  )]
  cat("\n  - Stored original names in originalNameUsage")
  
  dataset[, c(
    "DateNaturalisation", "FirstRecord", "FirstRecord1", "FirstRecord2",
    "FirstRecord_intentional", "originalNameUsage1", "originalNameUsage2"
  ) := NULL]
  cat("\n  - Deleted temporary columns")
  
  if (save_to_disk == TRUE){
    filename <- file.path(data_dir, "tmp", "fr_main_dataset_step1.csv")
    fwrite(dataset, filename)
    cat("  - 'fr_main_dataset_step1.csv' is available in 'tmp' folder")
  }
  cat("\nStep 1 completed: main dataset 'fr_main_dataset' ready to be processed\n ") 
  
  if (use_log == TRUE){
    sink()
  }
  
  return(dataset)
}