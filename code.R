
## Simple code for the initial experiment discussion

library(ggplot2)
library(broom)
library(gtsummary)
library(ggpubr)

dat1 <- read.table(text="Medium Height
A     10
A     12
A     15
B      9
B     10
B     11
B     10
B      3", header=TRUE)
means <- aggregate(Height ~ Medium , 
          data=dat1 , 
          FUN=mean)
barplot(Height ~ Medium , data=means)

# A simple t-test
t.test(data=dat1, Height ~ Medium)$estimate |> diff()

# A dynamite plot
ggplot(dat1) + 
  aes(x=Medium, y=Height) + 
  stat_summary(geom="col") + 
  stat_summary(geom="errorbar", width=0.5) + 
  stat_compare_means(method="t.test")

# Make a plot with the effect size and confidence intervals indicated
lm(data=dat1, Height ~ Medium)  |> 
  modelsummary::modelplot(coef_omit = "(Intercept)",coef_rename = c("MediumB"="Medium B vs A")) + 
  geom_vline(xintercept=0) + labs(x="Mean difference with 95% confidence interval")

# Estimate a linear model


# Make a nice model summary
lm(data=dat1, Height ~ Medium) |> tbl_regression() 
