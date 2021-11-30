rm(list=ls())
# This are the packages we (might) use:
packs <- c("lavaan", "semPlot", "semTools", "tidyverse",
           "kableExtra", "nonnest2", "broom", "qgraph")
package.check <- lapply(
  packs,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Sadly, this data cannot be posted online!
data_dir <- paste0(getwd(), '/', 'Data/')
df_study <- readRDS(paste0(data_dir, 'df_wm_kriterier_raven_audiogram.rds'))


# Saving a dataframe for creating audiograms
df_study %>%
  filter_at(vars(ends_with("50")), any_vars(!is.na(.))) %>%
  select(contains("audiogram"), fp_nr, group, age) %>%
  saveRDS('./Fig_Table_Data/for_audiogram.RDS')

df_study <- df_study %>%
  # Selecting the variables to use
  select(c(ends_with("50"),
           "sematicwordpair_span_itemscoring",
           "reading_span_itemscoring",
           "vswm_span_itemscoring",
           "raven_sum",
           "group", "age", "fp_nr", 
           "audiogram_pta5124air_better_ear", "sex")) %>%
  
  # Renaming the variables
  rename("np_ssn_50" = "hagerman_no-processing_ssn_srt50",
         "nr_ssn_50" = "hagerman_nr_ssn_srt50",
         "f_ssn_50" = "hagerman_fast_ssn_srt50",
         "np_4t_50" = "hagerman_no-processing_4talker_srt50",
         "nr_4t_50" = "hagerman_nr_4talker_srt50",
         "f_4t_50" = "hagerman_fast_4talker_srt50",
         "SWPair" = "sematicwordpair_span_itemscoring",
         "RSpan" = "reading_span_itemscoring",
         "VSWM" = "vswm_span_itemscoring" ,
         "Raven" = "raven_sum",
         "Group" = "group",
         "PTA4" = "audiogram_pta5124air_better_ear"
         
  ) %>%
  # Let's filter out the Ss' without any Hagerman values 
  # i.e., we remove thouse who did not participate in all sessions we need data from
  filter_at(vars(ends_with("50")), any_vars(!is.na(.)))

# Calculate the sample size eligable for data analysis:
df_study %>%
  count(Group)

# Rounding to digits:
rn = 2


# First, non-improved, model:
sem.mod <- '
  #1. Latent Variables
  # Speech Recognition in Noise
  # Only 50%
  SPiN   =~ f_4t_50 + np_4t_50 + nr_4t_50 + f_ssn_50 + np_ssn_50 + nr_ssn_50

  # Cognition
  CogF =~ RSpan + SWPair + VSWM + Raven

  
  #2. Mediators
  SPiN ~ CogF + PTA4 + age
  CogF ~ age
'

## Here we set the model with all parameters "free"
configural.sem <- sem(sem.mod, missing='fiml', data=df_study, estimator='ml',
                      group="Group",
                      fixed.x = FALSE)
summary(configural.sem, fit.measures = TRUE)  

# Let's have a look at how we can improve the model:
modificationIndices(configural.sem,
                    minimum.value = 4, sort = TRUE)


# Second model:
sem.mod.cov <- '
  #1. Latent Variables
  # Speech Recognition in Noise
  # Only 50%
  SPiN   =~ f_4t_50 + np_4t_50 + nr_4t_50 + f_ssn_50 + np_ssn_50 + nr_ssn_50

  # Cognition
  CogF =~ RSpan + SWPair + VSWM + Raven

  
  #2. Mediators
  SPiN ~ CogF + age + PTA4
  CogF ~ age

  # First addition based on MIs
  RSpan ~~ SWPair
'


## Here we set the model with all parameters "free" and we have set covariance between
## Reading Span and semantic wordpair (error terms)
configural.sem.cov <- sem(sem.mod.cov, missing='fiml', data=df_study, estimator='ml',
                      group="Group",
                      fixed.x = FALSE)

summary(configural.sem, fit.measures = TRUE)  

# Testing the models

# Anova of chi-square
test1 <- anova(configural.sem, configural.sem.cov)

modificationIndices(configural.sem.cov,
                    minimum.value = 4, sort = TRUE)

# Model 2 updated based on modificationindices
sem.mod.cov2 <- '
  #1. Latent Variables
  # Speech Recognition in Noise
  # Only 50%
  SPiN   =~ f_4t_50 + np_4t_50 + nr_4t_50 + f_ssn_50 + np_ssn_50 + nr_ssn_50

  # Cognition
  CogF =~ RSpan + SWPair + VSWM + Raven
  
  #2. Mediators
  SPiN ~ CogF + PTA4 + age
  CogF ~ age

  # First addition
  RSpan ~~SWPair
  
  # Second Addition
  f_4t_50 ~~  f_ssn_50
'

## Here we set the model with all parameters "free" and we have set covariance between
## Reading Span and semantic wordpair, and f_4t_50 and f_ssn50
configural.sem.cov2 <- sem(sem.mod.cov2, missing='fiml', data=df_study, estimator='ml',
                          group="Group",
                          fixed.x = FALSE)
summary(configural.sem.cov2, fit.measures = TRUE)  

# Testing the models
# Anova of chi-square
test2 <- anova(configural.sem.cov, configural.sem.cov2)


modificationIndices(configural.sem.cov2,
                    minimum.value = 4, sort = TRUE)


## We have our winner model and let's set up the regression equal model
regression.equal.em <- sem(sem.mod.cov2, missing='fiml', data=df_study, estimator='ml',
                                                  group="Group",
                                                  group.equal="regressions",
                                                  fixed.x = FALSE)


# Testing the models
# Anova of chi-square. Here, on the other hand, we don't want to have a significant difference.
invariance.test1 <- anova(configural.sem.cov2, regression.equal.em)

# Equal loadings:
loadings.equal.em <- sem(sem.mod.cov2, missing='fiml', data=df_study, estimator='ml',
                           group="Group",
                           group.equal=c("loadings"),
                           fixed.x = FALSE)


# Testing the models
# Anova of chi-square
invariance.test2 <- anova(configural.sem.cov2, loadings.equal.em)

# Equal loadings and regressions:
loadingsreg.equal.em <- sem(sem.mod.cov2, missing='fiml', data=df_study, estimator='ml',
                         group="Group", std.lv = T,
                         group.equal=c("loadings", "regressions"),
                         fixed.x = FALSE)


# Testing the models
# Anova of chi-square
invariance.test3 <- anova(regression.equal.em, loadingsreg.equal.em)

# Seems like both of the tests are kinda good?

# Let's get the fit statistics
# Function for creating a readable fit measures table with multiple models
fit.measure.tab <- function(model.fit, model.name){
  
  measures <- c("chisq", "df", "rmsea", 
                "rmsea.ci.lower", "rmsea.ci.upper", "tli", "cfi", "bic", "pvalue")
  fit.measures <- round(fitmeasures(model.fit, fit.measures = measures), 3)
  vec <- c(fit.measures[1:2], 
           paste0(fit.measures[3], " (", fit.measures[4], "-", fit.measures[5], ")"), 
           fit.measures[6:9])
  fit.tab <- matrix(vec, nrow = 1, ncol= 7)
  rownames(fit.tab) <- model.name
  colnames(fit.tab) <- c("Chisq", "df", "RMSEA (95% CI)", "TLI", "CFI", "BIC", "P")
  fit.tab
}

# Getting all fit stats together
fit.stats.50 <- rbind(fit.measure.tab(configural.sem, 'Model 1'),
                      fit.measure.tab(configural.sem.cov, "Model 2"),
                      fit.measure.tab(configural.sem.cov2, "Model 3"),
                      fit.measure.tab(loadings.equal.em, "Model 4"),
                      fit.measure.tab(regression.equal.em, "Model 5"),
                      fit.measure.tab(loadingsreg.equal.em, "Final Model"))  %>% as.data.frame() %>%
  rownames_to_column(var = "Model")

fit.stats.50

# For creating figure in paper (theoretical model)
saveRDS(configural.sem, file = "./Table_Fig_Data/for_vis_sep_revision.rds")

# Data presented in paper
saveRDS(fit.stats.50, file = "./Table_Fig_Data/fitStats_sep_revision.rds")
saveRDS(loadingsreg.equal.em, file = "./Table_Fig_Data//finalModel50_sep_revision.rds")


# For Table with estimates
tabl.sem <- parameterEstimates(fit50, standardized = T) %>%
  filter(op == "~") %>%
  mutate(stars = ifelse(pvalue < .001, "***",
                        ifelse(pvalue < .01, "**",
                               ifelse(pvalue < .05, "*", "")))) %>%
  select('Latent Factor' = lhs,
         'Regressor' = rhs,
         B=est,
         SE=se, Z=z,
         Beta=std.all,
         sig=stars,
         Group=group)

sem.tab1 <- cbind(
  tabl.sem[tabl.sem$Group == 1,],
  tabl.sem[tabl.sem$Group == 2,])





# Let's create the LRT-table:
lrt.tabl <- tibble("Models Compared" = c("2 vs. 1", "3 vs. 2",
                                         "4 vs. 3", "5 vs. 4",
                                         "6 vs. 5"),
                   "LRT" = c(round(test1$`Chisq diff`[2], 2),
                             round(test2$`Chisq diff`[2], 2),
                             round(invariance.test1$`Chisq diff`[2], 2),
                             round(invariance.test2$`Chisq diff`[2], 2),
                             round(invariance.test3$`Chisq diff`[2], 2)
                             ),
                   "P-value" = c(ifelse(test1$`Pr(>Chisq)`[2] < 0.01, "< 0.01", test1$`Pr(>Chisq)`[2]),
                                 ifelse(test2$`Pr(>Chisq)`[2] < 0.01, "< 0.01", test1$`Pr(>Chisq)`[2]),
                               round(invariance.test1$`Pr(>Chisq)`[2], 2),
                               round(invariance.test2$`Pr(>Chisq)`[2], 2),
                               round(invariance.test3$`Pr(>Chisq)`[2], 2)
                               ),
                   "df2" = c(test1$`Df diff`[2],
                             test2$`Df diff`[2],
                             invariance.test1$`Df diff`[2],
                             invariance.test2$`Df diff`[2],
                             invariance.test3$`Df diff`[2])
                   )

lrt.tabl <- lrt.tabl %>% add_column("Type of Test"  = c("Model Optimization", " ", "Invariance Testing", rep(" ", 2)), .before = 1)
saveRDS(lrt.tabl, './Table_Fig_Data/lrtResults.Rds')