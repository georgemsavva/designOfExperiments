
## Simulation for the crops/days example

set.seed(123)
library(data.table)
library(ggplot2)

# Make a dataset with the genotype
dat <- expand.grid(genotype=LETTERS[1:8], rep=1:8) |>as.data.table()

# Assign each genotype to a day of analysis
dat[genotype %in% c("A", "H") , day := 1]
dat[genotype %in% c("D", "E") , day := 2]
dat[genotype %in% c("G", "B") , day := 3]
dat[genotype %in% c("C") & rep %in% (1:4) , day := 1]
dat[genotype %in% c("C") & rep %in% (5:8) , day := 2]
dat[genotype %in% c("F") & rep %in% (1:4) , day := 2]
dat[genotype %in% c("F") & rep %in% (5:8) , day := 3]

# Calculate the outcome as a function of day not genotype
dat[, outcome := rnorm(.N) + (4*(day==1))+10]

# It would look like the outcome depends on genotype
ggplot(dat) + aes(x=genotype, y=outcome) + geom_point() + theme_bw()

# Unless you split by day
ggplot(dat) + aes(x=genotype, y=outcome) + geom_point() + theme_bw() + 
  facet_wrap(~day, scale="free_x", labeller = as_labeller(\(x) paste0("Analysis day ", x)))


