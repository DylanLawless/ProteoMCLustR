library(STRINGdb)
library(parallel)
library(dplyr)
library(ggplot2)
library(expm)
library(GeneTonic)

# Functions from the individual files
# 2_load_string_database.R
load_string_database <- function(file_path) {
  string_db <- readRDS(file = file_path)
  return(string_db)
}

# 3_extract_string_ids.R
extract_string_ids <- function(file_path) {
  string_id_df <-
    data.table::fread(file_path) %>%
    dplyr::select(
      STRING_ID = "#string_protein_id",
      Gene_ID = preferred_name
    )
  return(string_id_df)
}

# 4_mcl_functions_infl.R
GetSubNetwork <- function(string_db, STRING_IDs) {
  string_subgraph <- string_db$get_subnetwork(STRING_IDs)
  string_subgraph <- igraph::simplify(string_subgraph)
}

ChooseInflation <- function(cur_subnetwork,
                            mcl_inflation_param = c(1.5),
                            size_limits = c(5, 50)) {
  # Code from the 4_mcl_functions_infl.R file
}

# 5_mcl_functions_runmcl.R
RunMCL <- function(string_db,
                   genes_to_incl,
                   iter_limit = 2,
                   cur_iter = 1,
                   size_limits = c(5, 50)) {
  # Code from the 5_mcl_functions_runmcl.R file
}

# Primary function
run_proteoMCLustR <- function(
  string_data_file,
  string_id_file,
  user_protein_selection,
  iter_limit = 4,
  mcl_inflation_param = c(1.5),
  size_limits = c(5, 50)
) {
  # Load STRING database
  string_db <- load_string_database(string_data_file)

  # Extract STRING IDs
  string_id_df <- extract_string_ids(string_id_file)

  # Run MCL clustering
  mcl_clusters <- RunMCL(
    string_db = string_db,
    genes_to_incl = string_id_df$STRING_ID[1:user_protein_selection],
    iter_limit = iter_limit,
    size_limits = size_limits
  )

  # Code from 6_run_mcl_clustering.R, 7_export_clusters.R and 8_generate_plots_check_overlaps.R
}

# Export the run_proteoMCLustR function
#' @export
run_proteoMCLustR

