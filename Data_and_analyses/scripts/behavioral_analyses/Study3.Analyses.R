# Load helper functions
source('scripts/behavioral_analyses/helper_functions.R')

# Import and load libraries
packages = c("tidyverse","sjstats","ggplot2","lme4","lmerTest","Hmisc","car","lmtest","ROCR","brms", "tidybayes", "bayestestR")
ipak(packages)

#Load Data
Study3Data <- read.csv("data/Study3Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
Study3Data$Participant <- as.character(Study3Data$Participant) #make participant factor

#code categorical variables
#####
Study3Data$Condition_dum <- as.factor(Study3Data$Condition)
contrasts(Study3Data$Condition_dum) <- contr.treatment(2)
colnames(contrasts(Study3Data$Condition_dum)) = c("Touchdown")
#ethnicity
Study3Data$ethnicity_eff <- as.factor(Study3Data$ethnicity)
contrasts(Study3Data$ethnicity_eff) <- contr.treatment(5)
#####

#run mixed models for learning by condition
######
Study3.model<- glmer(acc~scale(Trial)*Condition_dum+ (scale(Trial)|Participant)+(1|Face_Shown), data = Study3Data, family = "binomial")
Study3.coef <- summary(Study3.model) 
Study3.effects <- exp(fixef(Study3.model))
Study3.effects.CI <- log(exp(confint(Study3.model,'Condition_dumTouchdown', level=0.95)))
#does race moderate learning?
Study3.race.model<- glmer(acc~scale(Trial)*Condition_dum+ ethnicity_eff+(scale(Trial)|Participant)+(1|Face_Shown), data = Study3Data, family = "binomial")
Study3.race.model.coef <- summary(Study3.race.model)

#run as Bayesian model to avoid interpreting null in NHST. Using uninformative priors
#####
Bayes_Model <- brm(formula = acc ~ Condition_dum+scale(Trial)+(scale(Trial)|Participant)+(1|Face_Shown),  
                             data=Study3Data, 
                             family = bernoulli(link = "logit"),
                             warmup = 1000, 
                             iter = 2000,
                             inits= "0", 
                             cores=6,
                             chains = 4,
                             seed = 123,
                             save_all_pars = TRUE)
#check model performance
mcmc_conv <- mcmc_plot(Bayes_Model, type = "trace")
#summarize model
Bayes_Model_coef <- summary(Bayes_Model)
posterior_desc <- describe_posterior(
  Bayes_Model,
  centrality = "median",
  test = c("p_direction", "p_significance")
)

#estimate reasonable ROPE
rope_range <- rope_range(Bayes_Model)

#calculate ROPE
percent_rope <- rope(
  Bayes_Model,
  range = c(-0.1813799,  0.1813799),
  ci = .89,
  ci_method = "HDI",
  effects = c("fixed", "random", "all"),
  component = c("conditional", "zi", "zero_inflated", "all"),
  parameters = NULL,
  verbose = TRUE,
)

#plot ROPE
rope_figure <- plot(percent_rope)
#calculate probability of direction
pd <- p_direction(Bayes_Model)
#####

#Figure 4
######
plotcurve4 <- ggplot(Study3Data, aes(Trial, acc, fill = as.factor(Condition))) + 
  geom_smooth(method = "loess", color ="grey50")+
  scale_y_continuous(name = "Accuracy")+scale_fill_manual(breaks = c("1", "2"),
                                                          values = c("#E6A0C4", "#D8A499"))+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none", axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  coord_cartesian(ylim = c(.6, .9))+theme(axis.ticks=element_blank())+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())
#ggsave("plotcurve4", device='jpeg', width = 6, height = 5,dpi=700)
#####

#####Print the results in the order they appear in the manuscript
print('RESULTS FOR STUDY 3')
print('logistic mixed model for learning rates')
print(Study3.coef)
print(Study3.effects)
print(Study3.effects.CI)
print('race does not moderate learning')
print(Study3.race.model.coef)

print('RESULTS FOR BAYES')
print('convergence')
print(mcmc_conv)
print('model summary')
print(Bayes_Model_coef)
print(posterior_desc)
print('estimate and calculate region of practical equivilance')
print(rope_range)
print(percent_rope)
print('vizualize rope')
print(rope_figure)
print('probability of direction')
print(pd)