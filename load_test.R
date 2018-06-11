library(ggplot2)

bs <- c("master", "split")

read <- function (b) {
     d <- read.csv(sprintf("time-%s.log", b), sep = "|", header=F, col.names=c("op", "t_ns"))
     d$branch <- b
     return(d)
}

d <- do.call(rbind, lapply(bs, read))

d$branch <- factor(d$branch, levels = bs)

ggplot(d, aes(x = op, color = branch, y = t_ns)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("speed.png", width = 9, height = 9)
