
library(ggplot2)

dat1 <- read.csv("data1.csv")[,-1]


means <- aggregate(formula = Height ~ Medium , 
          data=dat1 , 
          FUN=mean)
barplot(Height ~ Medium , data=means)

library(dabestr)

dabest_obj <- load(tibble::as_tibble(dat1),
                   x = Medium, y = Height,
                   idx = c("B", "A"))

dabest_obj <- load(non_proportional_data,
                   x = Group, y = Measurement,
                   idx = c("Control 1", "Test 1")
)
 
dabest_obj$is_paired
dabest_obj.mean_diff <- mean_diff(dabest_obj)

# Plotting an estimation plot
dabest_plot(dabest_obj.mean_diff, TRUE)


t.test(data=dat1, Height ~ Medium)$estimate |> diff()

library(ggpubr)

ggplot(dat1) + 
  aes(x=Medium, y=Height) + 
  stat_summary(geom="col") + 
  stat_summary(geom="errorbar", width=0.5) + 
  stat_compare_means(method="t.test")

lm(data=dat1, Height ~ Medium)  |> 
  modelsummary::modelplot(coef_omit = "(Intercept)",coef_rename = c("MediumB"="Medium B vs A")) + 
  geom_vline(xintercept=0) + labs(x="Mean difference with 95% confidence interval")

library(broom)
dat1$Medium <- relevel(factor(dat1$Medium), "B")
est=tidy(lm(data=dat1, Height ~ Medium), conf.int = TRUE) |> subset(term=="MediumA")
b_mean = mean(dat1$Height[dat1$Medium=="B"])
a_mean = mean(dat1$Height[dat1$Medium=="A"])

ggplot(dat1)+ aes(x=Medium, y=Height) + geom_point(pch=4) + stat_summary(geom="point") + 
  geom_errorbar(data=est, aes(x=term, ymax=conf.high+b_mean, ymin=conf.low+b_mean, y=NULL),width=0.2) +
  geom_point(data=est, aes(x=term, y=estimate+b_mean),width=0.2) + 
  theme_bw() + 
  geom_hline(yintercept = b_mean, lty="dashed") + 
  geom_hline(yintercept = a_mean, lty="dotted") + 
  scale_x_discrete(labels=c("MediumA"="A-B"))
  
# OK forgot this


