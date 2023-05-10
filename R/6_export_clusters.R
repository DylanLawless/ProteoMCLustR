df <-
  data.frame(ID = integer(),
             Items = character(),
             stringsAsFactors = FALSE)

for (i in seq_along(mcl_clusters_recode)) {
  items <- unlist(mcl_clusters_recode[[i]])
  new_df <-
    data.frame(
      ID = rep(i, length(items)),
      Items = items,
      stringsAsFactors = FALSE
    )
  df <- rbind(df, new_df)
}

df$ID <- as.numeric(df$ID)
