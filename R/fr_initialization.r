##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##                            Initialization                            ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

fr_initialization <- function(data_dir=NULL, input_file=NULL){
  
  if (!dir.exists(data_dir)){
    stop("Data directory not found: ", data_dir)
  }

  # --- Call libraries ---
  packages <- c("Hmisc", "gsubfn", "stringr", "stringi", "data.table", "rgbif",
                "worrms", "openxlsx", "readODS", "tidyverse", "doParallel",  "foreach"
  ) 
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] # check which of them is not yet installed
  if (length(new.packages)) install.packages(new.packages); rm(new.packages) # install them
  
  # load all required packages
  l <- sapply(packages, function(s) suppressMessages( 
                                        suppressWarnings(
                                            require(s, quietly=T, character.only = TRUE))
                                        )
                                    ) 
  rm(packages, l)

  # --- Define or create the 'config' folder ---
  config <- file.path(data_dir, "config")
  if (!dir.exists(config)) {
    dir.create(config, recursive = TRUE)
  }
  cat(paste0("\n  - Configuration folder is ready at: ", config, ". Please make sure configuration files are uploaded in this folder.\n"))

  # --- Define or create the 'input' folder ---
  input <- file.path(data_dir, "input")
  if (!dir.exists(input)) {
    dir.create(input, recursive = TRUE)
  }
  cat(paste0("\n  - Input folder is ready at: ", input, ". Please make sure input dataset is uploaded in this folder.\n"))
  
  # --- Construct full path to input file ---
  filename <- file.path(input, input_file)
  if (!file.exists(filename)){
    stop("Input file not found: ", filename)
  }
  
  # --- Define or create the 'output' folder ---
  output <- file.path(data_dir, "output")
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE)
  }
  cat("\n  - Output folder is ready at:", output, "\n")

# --- Define and create 'tmp' folder ---
  tmp <- file.path(data_dir, "tmp")
  if (!dir.exists(tmp)) {
    dir.create(tmp, recursive = TRUE)
  }
  cat("\n  - Temporary folder is ready at:", tmp, "\n")

# --- Load functions ---
  source(file.path("R","fr_prepare_main_dataset.r"))
  source(file.path("R","fr_taxa_standard.r"))
  source(file.path("R","check_GBIF_taxa_parallel.r"))  
  source(file.path("R","fr_years_standard.r"))
  source(file.path("R","fr_locations_standard.r"))
  source(file.path("R","fr_terms_standard.R"))
  source(file.path("R","fr_save_output_dataset.R"))
  source(file.path("R","standardize_terms.R"))
  
  set.seed(17) # set seed for reproducibility

# --- Import data ---
  filename <- file.path(input, input_file)
## Extract file extension
  file_ext <- tools::file_ext(filename)
  
  # Read file based on extension
  if (tolower(file_ext) %in% c("xlsx", "xls")) {
    fr_input_data <- read.xlsx(filename)
  } else if (tolower(file_ext) %in% c("csv", "txt")) {
    fr_input_data <- fread(filename, encoding = "UTF-8")
  } else if (tolower(file_ext) == "ods") {
    fr_input_data <- readODS::read_ods(filename)
  } else {
    stop("Unsupported file type: ", file_ext)
  }
  
  setDT(fr_input_data) # convert to data.table
  
  # --- Encoding fix ---
  fr_input_data[, names(fr_input_data) := lapply(.SD, function(x) {
    if (is.character(x)) {
      x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
      x[is.na(x) | !validUTF8(x)] <- ""
      x
    } else {
      x
    }
  }), .SDcols = names(fr_input_data)]
  
  # Fix numeric-looking columns that got a trailing ".0" from Excel import
  fix_trailing_decimal <- function(x) {
    ifelse(grepl("^-?[0-9]+\\.0+$", x), sub("\\.0+$", "", x), x)
  }
  
  for (col in c("FirstRecord", "FirstRecord1", "FirstRecord2")) {
    if (col %in% names(fr_input_data)) {
      fr_input_data[[col]] <- fix_trailing_decimal(fr_input_data[[col]])
    }
  }
  
  cat("\n  - Functions and input data loaded\n")
  
  return(fr_input_data)
  }