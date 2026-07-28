##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                    Standardize remaining terms                       ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, October 2025                                                   ##
##########################################################################

standardize_terms <- function(dataset = NULL, 
                              save_to_disk = FALSE, 
                              term = NULL, # term to standardize
                              log_unresolved = TRUE,
                              log_group = NULL,
                              data_dir = NULL) {
  
  if (is.null(dataset) || !is.data.table(dataset)) {
    stop("Error: argument 'dataset' must be a non-null data.table.")
  }
  
  cat(paste0("\n - Standardize '", term, "' terms"))
  
  # --- Load reference table ---
  standard_table <- fread(file.path(data_dir, paste0("config/standard_", term, ".csv")), header = TRUE, fill=TRUE)
  col_std <- grep("std|standard|term", names(standard_table), value = TRUE)[1]  # standard term
  col_var <- grep("var", names(standard_table), value = TRUE)[1]                # variants
  
  if (is.na(col_std) || is.na(col_var)) {
    stop("Cannot find standard or variant columns in CSV. Check 'standard_", term, ".csv'.")
  }
  
  # --- Expand variants ---
  standard_table_exp <- standard_table[, .(variant = unlist(strsplit(get(col_var), ";"))), by = col_std]
  setnames(standard_table_exp, col_std, "std_value")
  standard_table_exp[, variant := trimws(tolower(variant))]
  
  # --- Prepare dataset ---
  dat <- copy(dataset)
  
  normalize_text <- function(x) {
    x <- tolower(x) # # replaces capital letters
    x <- gsub("[\u00A0]", " ", x) # non-breaking spaces
    x <- gsub(";", ",", x) # replace semicolons by commas in dataset text
    x <- gsub("\\s*[|/,]\\s*", "|", x) # unify remaining separators into '|'
    x <- gsub("\\s+", " ", x) # normalize spaces
    x <- trimws(x) # remove trailing and ending spaces
    return(x)
  }
  
  dat[, term_lower := normalize_text(get(term))] # create temporary clean "term_lower" column
  standard_table_exp[, variant := normalize_text(variant)] # clean the variant terms too
  
  # --- Match standardized values ---
  dat[, matched_value := standard_table_exp$std_value[match(term_lower, standard_table_exp$variant)]]
  
  # --- Identify unresolved BEFORE overwriting term column ---
  unresolved <- dat[is.na(matched_value), .(unmatched_term = term_lower)]
  unresolved <- unique(unresolved[unmatched_term != "" & !is.na(unmatched_term)])
  
  # --- Replace unmatched with blank ---
  dat[, (term) := fifelse(!is.na(matched_value), matched_value, "NA")]
  
  # --- Log unresolved terms ---
  # If log_group is set, several terms share the same source column
  # (e.g. degreeOfEstablishment/establishmentMeans/occurrenceStatus all come from
  # PresentStatus). In that case we only want to flag a value as "truly unresolved"
  # once it has failed to match ANY of the terms in that group - so we store each
  # term's unresolved list in a temporary per-group file, then intersect them.
  if (log_unresolved && nrow(unresolved) > 0) {
    
    if (!is.null(log_group)) {
      
      # --- Store this term's unresolved list in a temp per-group-per-term file ---
      tmp_dir <- file.path(data_dir, "tmp", "unresolved_groups")
      if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)
      tmp_file <- file.path(tmp_dir, paste0(log_group, "__", term, ".csv"))
      fwrite(unresolved, tmp_file, quote = TRUE)
      
      # --- Check if all terms in the group have now been processed ---
      group_files <- list.files(tmp_dir, pattern = paste0("^", log_group, "__"), full.names = TRUE)
      
    } else {
      # --- Standalone term (no group): log directly ---
      filename_unres <- file.path(data_dir, "tmp", "fr_check_unresolved_terms.csv")
      
      if (file.exists(filename_unres)) {
        existing <- tryCatch(
          fread(filename_unres, quote = "\"", header = TRUE, 
                colClasses = "character", fill = TRUE),
          error = function(e) data.table(unmatched_term = character())
        )
        if (!identical(names(existing), names(unresolved))) {
          warning("Existing log file has unexpected structure — resetting it.")
          existing <- data.table(unmatched_term = character())
        }
        combined <- unique(rbind(existing, unresolved, fill = TRUE))
      } else {
        combined <- unresolved
      }
      
      fwrite(combined, filename_unres, quote = TRUE)
    }
    
    cat(
      "\nWarning: ", nrow(unresolved), " ", term,
      " terms could not be standardized.\n",
      sep = ""
    )
  }
  
  
  # --- Cleanup ---
  dat[, c("term_lower", "matched_value") := NULL]
  
  # --- Save standardized dataset ---
  if (save_to_disk) {
    filename_out <- file.path(data_dir, "tmp", paste0("fr_main_dataset_", term, "_standardized.csv"))
    fwrite(dat, filename_out)
    cat("\nUpdated dataset saved to '", filename_out, sep = "")
  }
  
  cat("\n - ", term, " standardization completed. Unmatched terms replaced with blanks.", sep = "")
  return(dat)
}
