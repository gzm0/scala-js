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

ggplot(d_cat, aes(x = size, fill = category)) + geom_histogram(binwidth = 1) + xlim(0, 60) + facet_grid(cols = vars(category))
