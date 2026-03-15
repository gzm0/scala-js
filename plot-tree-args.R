library(readr)
library(ggplot2)
library(dplyr)

d <- read_csv("tree-arg-sizes.csv", col_types = "ffn")

stats <- d %>%
  group_by(tree, field) %>%
  summarise(
    n   = n(),
    p50 = quantile(size, 0.50),
    p90 = quantile(size, 0.90),
    p99 = quantile(size, 0.99),
    max = max(size),
  ) %>%
  mutate(category = case_when(
    field %in% c("args", "params", "captureParams", "captureValues")
                            ~ "params",
    (tree == "Block"      & field == "stats") |
    (tree == "ArrayValue" & field == "elems") |
    (tree == "JSArrayConstr" & field == "items")
                            ~ "data",
    TRUE                    ~ "meh"
  )) %>%
  arrange(category, desc(n))

# --- Plot: histograms faceted by (tree, field), coloured by category ---

d_cat <- d %>%
  left_join(stats %>% select(tree, field, category), by = c("tree", "field"))

# Cap x-axis per panel for readability (still show true max in table)
d_capped <- d_cat %>%
  group_by(tree, field) %>%
  mutate(cap = pmin(size, quantile(size, 0.995))) %>%
  ungroup()

category_colors <- c(
  "params"      = "#2196F3",
  "data"  = "#4CAF50",
  "meh"   = "#F44336"
)

ggplot(d_capped, aes(x = cap, fill = category)) +
  geom_histogram(binwidth = 1, boundary = -0.5, color = "white", linewidth = 0.2) +
  facet_wrap(~ tree + field, scales = "free") +
  scale_fill_manual(values = category_colors) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 4)) +
  labs(
    title = "IR tree list-argument sizes, coloured by storage category",
    subtitle = "x-axis capped at 99.5th percentile per panel; see table for true maxima",
    x = "List size",
    y = "Count",
    fill = "Category"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("tree-arg-sizes.png", width = 16, height = 10)
