# library(tidyverse)
library(patchwork)
library(ggplot2)

# Define sunset colours
colorSUNSET0 <- "#f8de87"
colorSUNSET1 <- "#eeaf61"
colorSUNSET2 <- "#fb9062"
colorSUNSET3 <- "#ee5d6c"
colorSUNSET4 <- "#ce4993"
colorSUNSET5 <- "#6a0d83"

df$`B cell count` |> unique() |> length()
df$B_cell_count_clean |> unique() |> length()

df$`T cell count` |> unique() |> length()
df$T_cell_count_clean |> unique() |> length()

df$`Immunoglobulin levels` |> unique() |> length()
df$Immunoglobulin_levels_clean |> unique()  |> length()

df$`Neutrophil count` |> unique() |> length()
df$Neutrophil_count_clean |> unique() |> length()

df |> group_by(B_cell_count_clean) |> summarise(n())
df |> group_by(T_cell_count_clean) |> summarise(n())
df |> group_by(Immunoglobulin_levels_clean) |> summarise(n())
df |> group_by(Neutrophil_count_clean) |> summarise(n())

df_counts <- tibble(
  metric = c("B cell", "T cell", "Immunoglobulin", "Neutrophil"),
  Before = c(length(unique(df$`B cell count`)),
             length(unique(df$`T cell count`)),
             length(unique(df$`Immunoglobulin levels`)),
             length(unique(df$`Neutrophil count`))),
  After = c(length(unique(df$B_cell_count_clean)),
            length(unique(df$T_cell_count_clean)),
            length(unique(df$Immunoglobulin_levels_clean)),
            length(unique(df$Neutrophil_count_clean)))
)

df_counts_long <- df_counts %>%
  pivot_longer(cols = c("Before", "After"), names_to = "source", values_to = "unique_count") %>%
  mutate(source = factor(source, levels = c("Before", "After")))

p0 <- ggplot(df_counts_long, aes(x = metric, y = unique_count, fill = source)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = unique_count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Before" = colorSUNSET0, "After" = colorSUNSET5)) +
  labs(subtitle = "Unique labels before and after cleaning",
       x = "Metric", y = "Unique Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ source) +
  guides(fill = "none")

p0

# library(tidyverse)



p1 <- df %>%
  group_by(B_cell_count_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = B_cell_count_clean, y = count)) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  geom_col(fill = colorSUNSET1, color = "black") +
  labs(subtitle = "B cell", x = "New category", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- df %>%
  group_by(T_cell_count_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = T_cell_count_clean, y = count)) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  geom_col(fill = colorSUNSET2, color = "black") +
  labs(subtitle = "T cell", x = "New category", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- df %>%
  group_by(Immunoglobulin_levels_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = Immunoglobulin_levels_clean, y = count)) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  geom_col(fill = colorSUNSET3, color = "black") +
  labs(subtitle = "Immunoglobulin", x = "New category", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- df %>%
  group_by(Neutrophil_count_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = Neutrophil_count_clean, y = count)) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  geom_col(fill = colorSUNSET4, color = "black") +
  labs(subtitle = "Neutrophil", x = "New category", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots with patchwork, collecting axis titles and guides

p0 <- p0 + labs(tag = "(A)")
pB <- p1 + labs(tag = "(B)")  + p2 + p3 + p4 + plot_layout(ncol = 2) 

patch1 <- p0 | pB +
  plot_layout(guides = "collect", axis_titles = "collect") 
  # plot_annotation(title = "Comparison of cleaned metrics", theme = theme(plot.title = element_text(hjust = 0)))

print(patch1)

ggsave(patch1, file = "../output/plot_patch1.pdf", width = 9, height = 4.5)


# polot 2 ----
library(stringr)
p1 <- ggplot(df, aes(x = `Major category`)) +
  geom_bar(fill = colorSUNSET1, color = "black") +
  labs(title = "Information/genes per major category",
       x = "Major category", y = "Count") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(df, aes(x = `Subcategory`)) +
  geom_bar(fill = colorSUNSET2, color = "black") +
  labs(title = "Information/genes per subcategory",
       x = "Subcategory", y = "Count") +
  scale_x_discrete(labels = function(x) str_trunc(x, width = 10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p1 <- p1 + labs(tag = "(A)")
p2 <- p2 + labs(tag = "(B)")
patch2 <- (p1 / p2)

ggsave(patch2, file = "../output/plot_patch2.pdf", width = 8, height = 5)

# plot 3 ---- 
# Pivot the cleaned metric columns into a long format
df_long <- df %>%
  pivot_longer(
    cols = c(B_cell_count_clean, T_cell_count_clean, Immunoglobulin_levels_clean, Neutrophil_count_clean),
    names_to = "metric",
    values_to = "label"
  )

p1 <- ggplot(df_long, aes(x = label)) +
  geom_bar(fill = colorSUNSET3, color = "black") +
  # facet_wrap(~ `Major category`, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Cell/Ig labels total",
    x = "Cleaned Label",
    y = "Count"
  )

p2 <- ggplot(df_long, aes(x = label)) +
  geom_bar(fill = colorSUNSET4, color = "black") +
  facet_wrap(~ `Major category`, scales = "free_x",
             labeller = labeller(`Major category` = function(x) str_trunc(x, width = 15))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Cell/Ig labels per major category",
    x = "Cleaned Label",
    y = "Count"
  )

p1 <- p1 + labs(tag = "(A)")
p2 <- p2 + labs(tag = "(B)")

patch3 <- (p1 | p2)
ggsave(patch3, file = "../output/plot_patch3.pdf", width = 8, height = 5)
