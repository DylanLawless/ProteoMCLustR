library(ggplot2)
library(dplyr)


 grouped_df <- df %>%
   group_by(ID) %>%
   summarize(count = n())

 p1 <- ggplot(grouped_df, aes(x = ID, y = count)) +
   geom_bar(stat = "identity", fill = "blue") +
   xlab("ID") +
   ylab("Count") +
   ggtitle("Number of genes per pathway ID")
 ggsave(paste0("./data/ppi_user_ouput/ppi_bar_", file_suffix, ".pdf"), plot = p1)

 p2 <- ggplot(grouped_df, aes(x = ID, y = count)) +
   geom_point() +
   xlab("ID") +
   ylab("Count") +
   ggtitle("Number of genes per pathway ID")
 ggsave(paste0("./data/ppi_user_ouput/ppi_point_", file_suffix, ".pdf"), plot = p2)


#  export CheckOverlaps <- function(df) {
#  combs <- combn(unique(df$ID), 2, simplify = FALSE)
# 
#  overlap_counts <- lapply(combs, function(x) {
#    ids <- x[1:2]
#    items1 <-
#      df %>% filter(ID == ids[1]) %>% select(Items) %>% unlist()
#    items2 <-
#      df %>% filter(ID == ids[2]) %>% select(Items) %>% unlist()
#    count <- length(intersect(items1, items2))
#    data.frame(Group1 = ids[1],
#               Group2 = ids[2],
#               Overlap = count)
# })
# 
#  overlap_df <- do.call(rbind, overlap_counts)
# 
#  overlapping_groups <- overlap_df %>% filter(Overlap > 0)
#  return(overlapping_groups)
# }
