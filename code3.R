
# Analysis of block randomised plant height study

library(ggplot2)
library(ggbeeswarm)
library(lmerTest)

set.seed(10012024)

#####################
# Generate dataset  #
#####################

# Suppose there is a real difference between blocks
betweenBlockSD = 1

# Now set the between-plant variance
betweenPlantSD = 1

# Choose the 'true' treatment effect
treatmentEffect = 2

# How many blocks?
Nblocks <- 8

# Calculate the block effects
blockEffects <- rnorm(Nblocks,0,betweenBlockSD)

dat2 <- expand.grid(treatment=factor(c("A","B")) |> relevel("B") , block=1:Nblocks, rep=1:2)
dat2$height <- 10 + blockEffects[dat2$block] + treatmentEffect*(dat2$treatment=="A") + rnorm(nrow(dat2),0,betweenPlantSD)
dat2$plot <- ave(dat2$height, dat2$block , FUN = \(x) sample(1:4))
dat2$block <- factor(dat2$block)

#################
## Basic plots  #
################# 

# Simple plot without blocks
ggplot(dat2) + aes(x=treatment, y=height) + geom_boxplot(outlier.color = NA) + geom_beeswarm(cex=2)

# Plot showing blocks
ggplot(dat2) + aes(x=treatment, y=height ) +  
  geom_boxplot(outlier.color = NA) + geom_beeswarm(cex=2,aes(shape=block)) + 
  scale_shape_manual(values=1:8)

# Estimate a model without blocks
model1 <- lm(data=dat2 , height ~ treatment)

summary(model1)

# Model with blocks as fixed effect
model2 <- lm(data=dat2 , height ~ treatment + factor(block))

summary(model2)

# Model with blocks as random effects
model3 <- lmer(data=dat2 , height ~ treatment + (1|block))

summary(model3)

#* Diagnostic checks are important
performance::check_model(model3)

#* We can get a nicely formatted table of the result
sgtsummary::tbl_regression(model3)


#* Make a graph of the estimated effects
modelsummary::modelplot(list("Ignoring blocks"=model1, 
                             "Blocks as fixed effects"=model2, 
                             "Blocks as random effects"=model3), 
                        coef_omit = c(-2), 
                        facet = TRUE, 
                        coef_rename = c("treatmentA"="Effect of A vs B") )  + 
  geom_vline(xintercept=0)


#* Make a list of the estimated effects
modelsummary::modelsummary(list("Ignoring blocks"=model1, 
                                "Blocks as fixed effects"=model2, 
                                "Blocks as random effects"=model3), 
                           coef_omit = c(-2), 
                           facet = TRUE, 
                           coef_rename = c("treatmentA"="Effect of A vs B") , 
                           estimate = "{estimate}" , 
                           statistic = c("95% CI"="{conf.low} to {conf.high}"))  


#* Make the graph of the data
ggplot(dat2) + aes(x=block, y=plot) + geom_tile(col="black", aes(fill=factor(block))) + 
  geom_text(aes(label=sprintf("%s\n%0.2f",treatment,height)))

