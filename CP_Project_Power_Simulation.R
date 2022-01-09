library(tidyverse)
library(lme4)
library(faux)
library(pander)
theme_set(theme_bw())
library(readr)
library(simr)
library(lmerTest)
set.seed(13295) 


CPS <- read_csv("D:/Program Files/MATLAB/eeglab2019_0/CPS.csv")   ###3 Load data from OSF repository
View(CPS)

CPS$Factor_Exp_Condition <- as.factor(CPS$Exp_Condition) # change type of categorical variables
CPS$Factor_Confidence <- as.factor(CPS$Confidence) # change type of categorical variables
CPS$Factor_Subject <- as.factor(CPS$Subject)

            ########### Run RT Model #####################

Model_RT <- lmer(RT_log ~ Coherence + Factor_Exp_Condition + (1|Factor_Subject),
                 data = CPS)  ## Fit the model for RT data

summary(Model_RT) ## See model 

doTest(Model_RT, fixed("Factor_Exp_Condition2", "z")) ## See effect size of interest

powerSim(Model_RT, fixed("Factor_Exp_Condition2", "z"), nsim=100) ## run 100 times simulation for calculating observed power

Ext_Model_RT <- extend(Model_RT, along="Factor_Subject", n=30)  ## extend current model by increasing the number of subjects

powerSim(Ext_Model_RT, fixed("Factor_Exp_Condition2", "z"), nsim=100) ## re calculate power for new subject number

Model_RT_Sample_Size <- powerCurve(Ext_Model_RT ,fixed("Factor_Exp_Condition2", "z"), along="Factor_Subject", breaks=c(5,10,15,20,25,30), nsim = 500)


plot(Model_RT_Sample_Size)


                        #### DO THE SAME FOR COEFFS #################

Model_Slope <- lmer(Coeffs ~ Coherence + Factor_Exp_Condition + (1|Factor_Subject),
                    data = CPS)  ## Fit the model for Slope data

summary(Model_Slope ) ## See model 

doTest(Model_Slope, fixed("Factor_Exp_Condition2", "z")) ## See effect size of interest

powerSim(Model_Slope, fixed("Factor_Exp_Condition2", "z"), nsim=100) ## run 100 times simulation for calculating observed power

Ext_Model_Slope <- extend(Model_Slope, along="Factor_Subject", n=30)  ## extend current model by increasing the number of subjects

powerSim(Ext_Model_Slope, fixed("Factor_Exp_Condition2", "z"), nsim=100) ## re calculate power for new subject number

Model_Slope_Sample_Size <- powerCurve(Ext_Model_Slope ,fixed("Factor_Exp_Condition2", "z"), along="Factor_Subject", 
                                      breaks=c(5,10,15,20,25,30), nsim = 500)

summary(Model_Slope_Sample_Size)

plot(Model_Slope_Sample_Size)





                            ##### DO THE SAME FOR CONFIDENCE DATA #####

Model_Conf <- glmer(Factor_Confidence ~ Coherence + Factor_Exp_Condition + (1|Factor_Subject), family = "binomial",
                 data = CPS)  ## Fit the model for Conf data

summary(Model_Conf) ## See model 

doTest(Model_Conf, fixed("Factor_Exp_Condition2", "z")) ## See effect size of interest

powerSim(Model_Conf, fixed("Factor_Exp_Condition2", "z"), nsim=100) ## run 100 times simulation for calculating observed power

Ext_Model_Conf <- extend(Model_Conf, along="Factor_Subject", n=100)  ## extend current model by increasing the number of subjects

powerSim(Ext_Model_Conf, fixed("Factor_Exp_Condition2", "z"), nsim=1000) ## re calculate power for new subject number

Model_Conf_Sample_Size <- powerCurve(Ext_Model_Conf ,fixed("Factor_Exp_Condition2", "z"), along="Factor_Subject", breaks=c(5,10,15,20,25,30), nsim = 500)

plot(Model_Conf_Sample_Size)








