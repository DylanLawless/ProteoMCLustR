cat("Running RunMCL function at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

all_proteins <- length(string_id_df$STRING_ID)
max_proteins <- 200

USER_PROTEIN_SELECTION <- max_proteins
cat("Running clutering for:", USER_PROTEIN_SELECTION,  "proteins in database")

# This is the main function for real data
# For testing and documentation we only use the first value [1:1000] instead of all vaulues: string_id_df$STRING_ID
mcl_clusters <-
        RunMCL(
                string_db = string_db,
                genes_to_incl = string_id_df$STRING_ID[1:USER_PROTEIN_SELECTION], # Use only first for testing
                iter_limit = 4
        )

cat("Running RunMCL function at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

cat("Clustering summary:\n")
mcl_clusters_table <- table(lengths(mcl_clusters))
print(mcl_clusters_table)

cat("\nchange ID \n")
mcl_clusters_recode <-
        lapply(mcl_clusters, function(x)
                string_id_df$Gene_ID[match(x, string_id_df$STRING_ID)])

cat("\nSave \n")

saveRDS(mcl_clusters, file=paste0("./data/ppi_user_ouput/mcl_clusters_joint_", file_suffix, ".rds"))
saveRDS(mcl_clusters_recode, file=paste0("./data/ppi_user_ouput/mcl_clusters_joint_recode_", file_suffix, ".rds"))


#originally saved:
# ./data/ppi/mcl_clusters_joint_whole_genome_v5_c7_700.rds
# ./data/ppi/mcl_clusters_joint_recode_whole_genome_v5_c7_700.rds
