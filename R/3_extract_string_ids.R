cat("\nExtract\n")

string_id_df <-
        data.table::fread('./data/ppi/9606.protein.info.v11.5.txt.gz') %>%
        dplyr::select(#STRING_ID = protein_external_id,
                      STRING_ID = "#string_protein_id",
                      Gene_ID = preferred_name)


# Change dir to manually run snippet
# string_id_df <-
#   data.table::fread('../data/ppi/9606.protein.info.v11.5.txt.gz') %>%
#   dplyr::select(#STRING_ID = protein_external_id,
#     STRING_ID = "#string_protein_id",
#     Gene_ID = preferred_name)
