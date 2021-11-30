rm(list=ls())
# This are the packages we (might) use:
packs <- c("tidyverse", "psych")

# Install the packages that we use in this script:
package.check <- lapply(
  packs,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

library(tidyverse)
# Data Directory:
data_dir <- paste0(getwd(), '/', 'Data/')

# Read the data (we cannot share this online, open to everyone):
df_study <- readRDS(paste0(data_dir, 'df_wm_kriterier_raven_audiogram.rds')) %>%
  # Selecting the variables to use
  select(c(ends_with("50"),
           "sematicwordpair_span_itemscoring",
           "reading_span_itemscoring",
           "vswm_span_itemscoring",
           "raven_sum",
           "group", "age", "fp_nr", "sex",
           "audiogram_pta5124air_better_ear")) %>%
  
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
         "PTA4" = "audiogram_pta5124air_better_ear",
         "Age" = "age"
         
  ) %>%
  
  # Remove the subjects from which we miss Hagerman data
  filter_at(vars(ends_with("50")), any_vars(!is.na(.)))

# Just to check how many that are left in each group:
df_study %>%
  count(Group)

# Should be 200 and 199



###################
# Variables
# Hagerman
spin <- c("np_ssn_50", "nr_ssn_50", "f_ssn_50",
  "np_4t_50", "nr_4t_50", "f_4t_50")
# Cog
cog <- c("SWPair","RSpan", "VSWM", "Raven")
# All
allvars <- c(spin, cog, "PTA4", "Age", "Group")
# Round to digits
rn = 2

# (Potential) Row names
# Alternative 1
tblRownames <-  c("Hagerman np SSN", "Hagerman nr SSN", "Hagerman fast SSN", 
                  "Hagerman np 4-Talker", "Hagerman nr 4-Talker", "Hagerman fast 4-Talker",
                  "Semantic Word-Pair", "Reading Span", "Visuospatial WM", "Raven", "PTA4", "Age")

# Alternative 2
tblRownames2 <- c("Hagerman SSN: Linear amp., NNR",
                  "Hagerman SSN: Linear amp., W/NR",
                  "Hagerman SSN: Fast-acting compr., NNR",
                  "Hagerman 4-Talker: Linear amp., NNR",
                  "Hagerman 4-Talker: Linear ampl. W/NR",
                  "Hagerman 4-Talker: Fast-acting compr., NNR",
                  "Semantic Word-Pair", "Reading Span", "Visuospatial WM", "Raven", "PTA4", "Age")

#
##############

######################
# Functions
# Correlation matrices
# Create correlation matrix
corr.table <- function(x, rnames) {
  # Round the values:
  cortbl <- x %>% 
    mutate_if(is.numeric, round, digits = rn)
  
  # Set the rownames
  colnames(cortbl) <- rnames
  diag(cortbl) <- "-"
  
  # Change 1 correlations to "-" and remove lower "triangle"
  cortbl[lower.tri(cortbl)] <- ''	

  # Change the row names (numbers and var names)
  rownames(cortbl) <- paste(1:ncol(cortbl), colnames(cortbl), sep = " ") # define number and name
  colnames(cortbl) <- paste0(1:ncol(cortbl))
  # Remove the first column:
  cortbl <- cortbl[,-grep("^1$", colnames(cortbl))]
}

# N of missing values across variables:
total_n_grouped <- df_study %>%
  group_by(Group) %>% tally()



# Separate the tables by group
cor.all <- df_study %>%
  select(c(all_of(allvars))) %>%
  # We want to get the data by group:
  psych::statsBy('Group', cors = TRUE)

# Correlation matrix for HA group:
cor.all.ha <- cor.all$r$hearingaidusers %>%
  as.data.frame() %>%
  corr.table(.,tblRownames)  %>%
  add_column("Mean (SD)" = paste0(round(cor.all$mean[2,1:12], rn), ' (',
                                  round(cor.all$sd[2,1:12], rn), ')'),
             .before = "2")  %>%
  rownames_to_column(var = "Variables")

# NH Group:
cor.all.na <- cor.all$r$nonhearingaidusers_nothearingloss %>%
  as.data.frame() %>%
  corr.table(tblRownames)  %>%

  add_column("Mean (SD)" = paste0(round(cor.all$mean[1,1:12], rn), ' (',
                                  round(cor.all$sd[1,1:12], rn), ')'),
             .before = "2") %>%
  rownames_to_column(var = "Variables")


# In the Manuscript:
saveRDS(cor.all.na, './Table_Fig_Data/Cor_Table_All_Variables_PTA4_NH.Rds')
saveRDS(cor.all.ha, './Table_Fig_Data/Table_data/Cor_Table_All_Variables_PTA4_HA.Rds')