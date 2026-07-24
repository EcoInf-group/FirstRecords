##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##                            Main workflow                             ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.1, July 2026                                                   ##
##########################################################################

## PREPARE WORKSPACE AND CONFIGURATION ###################################
# --- Clean workspace ---
graphics.off()
rm(list = ls())

ptime <- proc.time()

# --- Please define your working directory (where your code is) in "work_dir" ---
#work_dir = "C:/your/working/directory/"
#setwd(work_dir)
#getwd()

# --- Please define "data_dir" folder ("data" by default) ---
data_dir = "data"

# --- Please define "input_file" ("FirstRecords_sample_2000.xlsx" by default) ---
input_file = "sample_200.xlsx"

# --- Please define an identifier "ID" of the final dataset (date by default) ---
ID = Sys.Date()

##########################################################################
## INITIALIZATION ########################################################
##########################################################################

cat("\n Initialization")   

# --- Prepare workspace and import data ---
source(file.path("R", "fr_initialization.r"))

# --- Check folders and load packages and scripts --- #
init <- fr_initialization(data_dir = data_dir, input_file = input_file)
cat("\nIntialization completed\n ") 

##########################################################################
## 1) PREPARATION OF DATASET #############################################
##########################################################################

cat("\nSTEP 1: Prepare main dataset") 

fr_main_dataset_1 <- fr_prepare_main_dataset(dataset = init,
                                            Country = "Country", # Name of column with locations 
                                            originalName = "GenusSpecies", # Name of column with species names
                                            FirstRecord = "FirstRecord", # Name of column with first records (years)
                                            use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                           save_to_disk = TRUE, # TRUE to save fr_main_dataset_1 in 'tmp' folder
                                           data_dir = data_dir)
cat("\nStep 1 completed: main dataset 'fr_main_dataset_1' ready to be processed\n ") 

##########################################################################
## 2) STANDARDIZATION OF TAXA ############################################
##########################################################################

cat("\nSTEP 2: Standardize taxa") 

fr_main_dataset_2 <- fr_taxa_standard(dataset = fr_main_dataset_1,
                                        use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                        save_to_disk = TRUE, # TRUE to save fr_main_dataset_2 in 'tmp' folder
                                        data_dir = data_dir) 
cat("\nStep 2 completed: taxa have been standardized in 'fr_main_dataset_2'. Unmatched taxa are available in the 'tmp' folder and a taxonomy table is available in the 'output' folder\n ")

##########################################################################
## 3) STANDARDIZATION OF YEARS############################################
##########################################################################

cat("\nSTEP 3: Standardize years") 

fr_main_dataset_3 <- fr_years_standard(dataset = fr_main_dataset_2, 
                                       fr_column_name = "firstRecordEvent", 
                                       use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                       save_to_disk = TRUE, # TRUE to save fr_main_dataset_3 in 'tmp' folder
                                       data_dir = data_dir) 
cat("\nStep3 completed: first records (years) have been standardized in 'fr_main_dataset_3'. Years that couldn't be standardized are available in the 'tmp' folder\n ")

##########################################################################
## 4) STANDARDIZATION OF LOCATIONS #######################################
##########################################################################

cat("\nSTEP 4: Standardize location names") 

fr_main_dataset_4 <- fr_locations_standard(dataset = fr_main_dataset_3, 
                                            use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                            save_to_disk = TRUE, # TRUE to save fr_main_dataset_4 in 'tmp' folder
                                            data_dir = data_dir,
                                            identifier = ID) 
cat("\nStep 4 completed: locations have been standardized in 'fr_main_dataset_4'. The location table is available in the 'output' folder. Locations that couldn't be standardized are available in the 'tmp' folder\n ") 

##########################################################################
## 5) STANDARDIZATION OF REMAINING TERMS #################################
##########################################################################

cat("\nSTEP 5: Standardize remaining terms") 

fr_main_dataset_5 <- fr_terms_standard(dataset = fr_main_dataset_4, 
                                      use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                                      save_to_disk = TRUE, # TRUE to save fr_main_dataset_5 in 'tmp' folder
                                      data_dir = data_dir) 
cat("\nStep 5 completed: habitats, pathways, occurence status, degree of invasion have been standardized in 'fr_main_dataset_5'. Terms that couldn't be standardized are available in the 'tmp' folder\n ")

##########################################################################
## 6) SAVE FINAL DATASET #################################################
##########################################################################

cat("\nSTEP 6: Save final dataset") 

fr_final_dataset <- fr_save_output_dataset(dataset = fr_main_dataset_5, 
                    use_log = TRUE, # TRUE to record progress in log file in 'output' folder
                    data_dir = data_dir,
                    identifier = ID)
cat("\n  Final dataset available in the 'output' folder\n ")

print(proc.time()-ptime)
