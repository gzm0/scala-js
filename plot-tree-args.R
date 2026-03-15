library(readr)
library(ggplot2)
library(dplyr)

d <- read_csv("tree-arg-sizes.csv", col_types = "ffn")

# One faceted histogram per tree+field combination, x = list size
ggplot(d, aes(x = size)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "steelblue", color = "white") +
  facet_wrap(~ tree + field, scales = "free_y") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  labs(
    title = "Distribution of list argument sizes in IR tree constructors",
    x = "List size",
    y = "Count"
  ) +
  theme_bw()

ggsave("tree-arg-sizes.png", width = 16, height = 10)


d %>%
 group_by(tree, field) %>%
 summarise(
   n = n(),
   p50 = quantile(size, 0.5),
   p90 = quantile(size, 0.9),
   p99 = quantile(size, 0.99)
 ) %>% arrange(desc(n))