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
  facet_grid(confidence_score ~ .) +
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

# panel ----
library(patchwork)
# plot1 + (plot2 + plot3) + plot_layout(ncol = 1)
patch1 <- (p3 | p3_line) + plot_annotation(tag_levels = 'A')
patch1

ggsave(paste("../data/ppi_user_ouput/ppi_line_", file_suffix, ".pdf", sep = "") , plot = patch1, width = 8, height = 3 )





# Convert 'confidence_score' from numeric to factor
grouped_df <- grouped_df %>%
  mutate(confidence_score = as.factor(confidence_score))

# Density Plot
density_plot <- grouped_df %>%
  ggplot(aes(x = count, color = confidence_score, fill = confidence_score, group = confidence_score)) +  # Added 'group' aesthetic
  geom_density(alpha = 0.5) +  
  labs(# title = "Distribution of genes per pathway",
       x = "Number of genes per pathway",
       y = "Density") +
  scale_color_manual(values = c("#ee5d6c", "#eeaf61", "#6a0d83"), 
                     name = "Confidence\nscore",
                     guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ee5d6c", "#eeaf61", "#6a0d83"), 
                    name = "Confidence\nscore",
                    guide = guide_legend(reverse = TRUE)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 

# Print the plot
print(density_plot)

# Calculate cumulative sums within each evidence group
grouped_df_cum <- grouped_df %>%
  arrange(confidence_score, count) %>%
  group_by(confidence_score) %>%
  mutate(cumulative_count = cumsum(count))

# Plotting the cumulative counts of genes per pathway
cumulative_plot <- ggplot(grouped_df_cum, aes(x = count, y = cumulative_count, color = confidence_score)) +
  geom_step() +  # Using geom_step for a stepwise cumulative plot
  labs(# title = "Cumulative count of genes per pathway",
       x = "Number  of genes per pathway",
       y = "Cumulative\ncount of genes",
       color = "Confidence\nscore") +
  scale_color_manual(values = c("#ee5d6c", "#eeaf61", "#6a0d83"),
                     name = "Confidence\nscore",
                     guide = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Print the plot
print(cumulative_plot)

# panel ----
library(patchwork)
patch2 <- (density_plot | cumulative_plot) + plot_annotation(tag_levels = 'A')
patch3 <- patch1 / patch2 + plot_annotation(tag_levels = 'A')
patch3

ggsave(paste("../data/ppi_user_ouput/ppi_line_dist_", file_suffix, ".pdf", sep = "") , plot = patch3, width = 8, height = 5 )

# (C) Distribution of genes per pathway.
# (D) Cumulative count of genes per pathway.


# Can we automatically detect sig difference in evdeince_score groups ----
# Load necessary libraries
library(dplyr)
library(stats)

# Performing Kruskal-Wallis Test to assess if there are significant differences 
# across the three different confidence score groups.
kw_test <- kruskal.test(count ~ confidence_score, data = grouped_df)
# Print the result of the Kruskal-Wallis test to check for overall group differences
print(kw_test)

# If the Kruskal-Wallis test shows significant differences, perform Dunn's test for detailed pairwise comparisons
if (kw_test$p.value < 0.05) {
  # Ensure dunn.test library is loaded for conducting Dunn's test
  if (!require("dunn.test")) install.packages("dunn.test", dependencies=TRUE)
  library(dunn.test)
  
  # Conduct Dunn's test with Bonferroni correction for multiple testing to identify specific group differences
  dunn_test_results <- dunn.test(x = grouped_df$count, g = grouped_df$confidence_score, method = "bonferroni")
  # Print the results from Dunn's test
  print(dunn_test_results)
} else {
  # If no significant differences are found, indicate that no further post-hoc testing is required
  cat("No significant differences found by the Kruskal-Wallis test, hence no post-hoc testing required.\n")
}

# For a focused comparison of group '700' against combined groups '400' and '900'
# First, modify the data to combine groups '400' and '900' into one group for comparison
grouped_df <- grouped_df %>%
  mutate(combined_group = ifelse(confidence_score %in% c("400", "900"), "400_900", as.character(confidence_score)))

# Ensure the new group variable is treated as a categorical factor
grouped_df$combined_group <- factor(grouped_df$combined_group)

# Perform Mann-Whitney U test to compare the distribution of counts between group '700' and the new combined '400_900' group
mw_test <- wilcox.test(count ~ combined_group, data = grouped_df,
                       alternative = "greater")  # Set 'alternative' based on specific hypothesis about group differences

# Print the result of the Mann-Whitney U test to see if '400' has significantly lower counts compared to '700_900'
print(mw_test)

# Statistical analysis using the Kruskal-Wallis test revealed no significant differences in protein counts across the different confidence score groups (400, 700, and 900). Consequently, due to the absence of significant variance and for robust data coverage, we selected the largest dataset (confidence score 700_900 combined) for further analysis. This choice ensures the inclusion of the maximum number of proteins, providing a comprehensive basis for subsequent investigations.

# Calculate the maximum cumulative_count for each confidence_score group
max_cumulative_counts <- grouped_df_cum %>%
  group_by(confidence_score) %>%
  summarize(max_cumulative_count = max(cumulative_count))

# Print the results
print(max_cumulative_counts)

# Identifying the group with the largest cumulative count
largest_group <- max_cumulative_counts %>%
  filter(max_cumulative_count == max(max_cumulative_count)) %>%
  pull(confidence_score)

# Output the largest group
largest_group
