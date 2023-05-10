file_suffix <- "whole_genome_v5_c7_700"

cat("\nDownloading \n")

# string_db <-
#         STRINGdb$new(
#                 species = 9606,
#                 input_directory = "../data/ppi",
#                 version = "11.5",
#                 score_threshold = 700
#         )
# string_db
# saveRDS(string_db, file = "../data/ppi/string_data_700.rds")

string_db <- readRDS(file = "./data/ppi/string_data_700.rds")

# Change dir to manually run snippet
# string_db <- readRDS(file = "../data/ppi/string_data_700.rds")


