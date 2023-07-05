# Not run automatically
# To make custom plots based on output

file_suffix <- "whole_genome_v1_compared"

df_400 <- read.csv("../data/ppi_examples/mcl_clusters_recode_df_whole_genome_v1_c7_400.csv")

df_700 <- read.csv("../data/ppi_examples/mcl_clusters_recode_df_whole_genome_v1_c7_700.csv")

df_900 <- read.csv("../data/ppi_examples/mcl_clusters_recode_df_whole_genome_v1_c7_900.csv")

library(ggplot2)
library(dplyr)

# Adding a 'Quality Score' column
df_400$confidence_score <- 400
df_700$confidence_score <- 700
df_900$confidence_score <- 900

# Combining the dataframes
grouped_df <- bind_rows(df_400, df_700, df_900)

# Group by confidence_score and ID, and count the number of rows per group
grouped_df <- grouped_df %>%
  group_by(confidence_score, ID) %>%
  summarise(count = n())

# Add a new column 'Rank' based on the descending order of count, within each confidence_score group
grouped_df <- grouped_df %>%
  group_by(confidence_score) %>%
  arrange(desc(count)) %>%
  mutate(Rank = row_number())

# Plotting
p3 <- grouped_df %>%
  ggplot(aes(x = ID, y = count, fill = factor(confidence_score))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ confidence_score) +
  xlab("MCL sort ID") +
  ylab("Gene count") +
  scale_fill_manual(values = c("#ee5d6c", "#eeaf61", "#6a0d83"), name = "Confidence\nscore", 
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw()

p3

p3_line <- grouped_df %>%
  ggplot(aes(x = Rank, y = count, fill = factor(confidence_score))) +
  geom_area(aes(group = confidence_score), alpha = 0.4, position = 'identity') +
  geom_line(aes(color = factor(confidence_score), group = confidence_score)) +
  xlab("MCL group rank by size") +
  ylab("Gene count") +
  scale_color_manual(values = c("#ee5d6c", "#eeaf61", "#6a0d83"), name = "Confidence\nscore", 
                    guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ee5d6c", "#eeaf61", "#6a0d83"), name = "Confidence\nscore", 
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw()

p3_line


ggsave(paste0("../data/ppi_user_ouput/ppi_line_unranked_", file_suffix, ".pdf"), plot = p3)

ggsave(paste0("../data/ppi_user_ouput/ppi_line_rank_", file_suffix, ".pdf"), plot = p3_line)


