> d %>% filter(category == "params") %>% summarise(p = mean(size <= 2))

# A tibble: 1 × 1
      p
  <dbl>
1 0.962

> d %>% filter(tree == "Block") %>% summarise(p = mean(size <= 4))


# A tibble: 1 × 1
      p
  <dbl>
1 0.730

> d %>% summarise(p = mean(tree == "Block" & size <= 4 | category == "params" & size <= 2))

# A tibble: 1 × 1
      p
  <dbl>
1 0.814





library(readr)
library(ggplot2)
library(dplyr)

d <- read_csv("tree-arg-sizes.csv", col_types = "ffn") %>%
  mutate(category = if_else(
    field %in% c("args", "params", "captureParams", "captureValues"),
    "params", "data"
  ))

d %>%
  group_by(category) %>%
  summarise(
    n   = n(),
    p50 = quantile(size, 0.50),
    p75 = quantile(size, 0.75),
    p90 = quantile(size, 0.90),
    p99 = quantile(size, 0.99),
    max = max(size),
    .groups = "drop",
  ) %>%
  arrange(category, desc(n))

d %>%
  group_by(tree, field, category) %>%
  summarise(
    n   = n(),
    p50 = quantile(size, 0.50),
    p75 = quantile(size, 0.75),
    p90 = quantile(size, 0.90),
    p99 = quantile(size, 0.99),
    max = max(size),
    .groups = "drop",
  ) %>%
  arrange(category, desc(n))

ggplot(d, aes(x = size, fill = category)) +
  geom_histogram(binwidth = 1) +
  xlim(0, 60) +
  facet_grid(cols = vars(category))

ggsave("tree-arg-sizes.png", width = 12, height = 5)

d_block <- d %>% filter(tree == "Block")

ggplot(d_block, aes(x = size)) +
  geom_histogram(binwidth = 1) +
  xlim(0, 60)


#%p <- ggplot(data, aes(x=x) ) +
%%  geom_histogram( aes(x = var1, y = ..density..), fill="#69b3a2" ) +
%%  geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
%%  geom_histogram( aes(x = var2, y = -..density..), fill= "#404080") +
%%  geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
%%  theme_ipsum() +
%%  xlab("value of x")
%%
%%p <- data %>%
%%  ggplot( aes(x=value, fill=type)) +
%%    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
%%    scale_fill_manual(values=c("#69b3a2", "#404080")) +
%%    theme_ipsum() +
%%    labs(fill="")
%%
%%ggplot(d, aes(x = size)) +
%%  geom_histogram(data = d %>% filter(category == "params"), aes(y =  after_stat(density)), fill = "#69b3a2", binwidth = 1) +
%%  geom_histogram(data = d %>% filter(category == "data"),   aes(y = -after_stat(density)), fill = "#404080", binwidth = 1) +
%%  geom_label(aes(x = 30, y =  0.4, label = "params"), color = "#69b3a2") +
%%  geom_label(aes(x = 30, y = -0.4, label = "data"),   color = "#404080") +
%%  xlim(0, 60)
%%
%%
%%ggsave("tree-arg-sizes.png", width = 12, height = 5)
%%