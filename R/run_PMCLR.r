library(ProteoMCLustR)

results <- run_proteoMCLustR(
  string_data_file = "./data/ppi/string_data_700.rds",
  string_id_file = './data/ppi/9606.protein.info.v11.5.txt.gz',
  user_protein_selection = 200
)

