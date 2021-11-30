require(tidyverse)

rm(list=ls())
# Read the data created for the Audiogram purposes (note, this data cannot be shared online)
df_study <- readRDS('./Data/for_audiogram.RdS')

# Get the gains:
df_gains <- readRDS('./Data/Gains_Cleaned.RDs') %>% 
  filter(Group != 'Hearing Loss') %>%
  mutate(Freq = round(Freq, 1)) %>%
  filter(Freq %in% c(125,  250,  500, 1000, 2000, 3174.8, 4000, 6349.6, 8000)) %>%
  rename(fp_nr = ID,
         dBGain = dB)
df_gains$Group <- as_factor(df_gains$Group)
df_gains2 <- df_gains
df_gains2$Freq <- recode(df_gains$Freq, `3174.8` = 3000,
       `6349.6`  = 6000)

# Note that this RDS contains the number of subjects analyzed in the manuscript
# That is, the # that did not complete Hagerman were excluded
df_study %>%
  count(group)

# Adds upp with manuscript
# Let's create audiograms


# Calculate the mean age, group size, and pta
df_study %>% group_by(group) %>% summarise(
                                     n = sum(!is.na(audiogram_pta5124air_better_ear)),
                                     PTA4 = mean(audiogram_pta5124air_better_ear, na.rm = T),
                                     PTA4SD = sd(audiogram_pta5124air_better_ear, na.rm = T)
)


# Factor level names so that we can easily change this later
# Normal Hearing:
nh = "Normal Hearing"
# Hearing Loss:
hl = "Hearing Aid Users"

df.aud <- df_study %>% 
  # Change type to factor
  mutate(Group = as_factor(group)) %>%
  # Recode the factor levels
  mutate(Group = recode_factor(group, nonhearingaidusers_nothearingloss = nh,
                               hearingaidusers = hl)
  ) %>% select(-group)

# Re-organize data
# Here we get the ear frequencies as rows, instead of columns, and decibel as 
# a separate column:

if("Group" %in% colnames(df.aud)){
  long_data <- gather(df.aud, key = "ear-freq", value = "dB", -fp_nr, -Group)
} else {
  long_data <- gather(df.aud, key = "ear-freq", value = "dB", -fp_nr)
}

# Creating a new dataframe extracting ear and frequencies from the values in the 
# long_data we created above. That is we split the characters such as we get
# columns with ear and frequencies
d <- long_data %>% 
  extract("ear-freq", into = c("ear", "freq"), "(left|right)_ear_(\\d+)",
          convert= TRUE)


d <- na.omit(d)


# Ca
d <- d %>%
  group_by(fp_nr, Group, ear) %>%
  pivot_wider(names_from = ear, values_from = dB) %>%
  mutate(Left = mean(left, na.rm = TRUE), Right = mean(right, na.rm = TRUE)) %>%
  mutate(Left1 = ifelse(Left < Right, "Better", "Worse"),
         Right1 = ifelse(Left > Right, "Better", "Worse")) %>%
  gather(key = "ear", value = "Ear", Left1, Right1) %>%
  gather(key = "ear", value = "dB", left, right)

# Creating labels:
d$freqlabs <- d$freq/1000

# Calculate mean for gains
gains <- df_gains2 %>%
  dplyr::group_by(SignalP, Freq, Group) %>%
  dplyr::summarise(mean.Gain = mean(dBGain, na.rm = TRUE))

gains_wider <- gains %>% pivot_wider(names_from = "SignalP", values_from = "mean.Gain")





# Dodge for minimizing overlap
pd <- position_dodge(width = 0.03)


### Calculate mean and confidence intervals for better and worse ear:
cis_btr <- d %>% 
  mutate(Ear = recode(Ear, "Worse" = "Worse Ear",
                      "Better" = "Better Ear")) %>%
  group_by(Ear, freq, Group) %>%
  summarise(mean.dB = mean(dB, na.rm = TRUE),
            sd.dB = sd(dB, na.rm = TRUE),
            n.dB = n()) %>%
  mutate(se.dB = sd.dB / sqrt(n.dB),
         lower.ci.dB = mean.dB - qt(1 - (0.05 / 2), n.dB - 1) * se.dB,
         upper.ci.dB = mean.dB + qt(1 - (0.05 / 2), n.dB - 1) * se.dB,
         freqlabs = freq/1000) 


# Let's get the linear gain and fast-compression into to means
gains_wider$LinearGain <- filter(cis_btr, Ear == "Better Ear")$mean.dB - gains_wider$LIN
gains_wider$FASTGain <- filter(cis_btr, Ear == "Better Ear")$mean.dB - gains_wider$FAST
cis_btr$LinearGain <- c(gains_wider$LinearGain, gains_wider$LinearGain)
cis_btr$FASTGain <- c(gains_wider$FASTGain, gains_wider$FASTGain)

# We just want the gains for HA group:
cis_btr2 <- cis_btr %>% mutate(FASTGain = if_else(Group == "Normal Hearing", mean.dB, FASTGain))
cis_btr2 <- cis_btr2 %>% mutate(LinearGain = if_else(Group == "Normal Hearing", mean.dB, LinearGain))
TSize = 16

# Audiogram better worse grouped with CIs:
audgg.bw <- ggplot(data = cis_btr2, aes(x = freq, y = mean.dB)) +
  geom_line(aes(group = Ear, linetype = Ear), size = 1, position = pd) + 
  geom_line(aes(y = LinearGain, group=1), linetype="dotted", size = 1, position = pd) +
  geom_line(aes(y = FASTGain, group=1), linetype="longdash", size = 1, position = pd) +
  
  geom_point(aes(group = Ear, shape = Ear), size = 2, position = pd) + 
  geom_errorbar(aes(ymin = lower.ci.dB, ymax = upper.ci.dB, group = Ear), 
                width = 0.025, size = 0.8, position = pd) +
  scale_y_reverse() +
  scale_x_log10(breaks = cis_btr$freq, labels = cis_btr$freqlabs) +
  jtools::theme_apa(legend.pos = c(.87, .94)) +
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)", linetype = "Ear", shape = "Ear" ) +  
  theme(legend.background = element_rect(fill = "transparent"),
        text=element_text(size=TSize), 
        axis.text=element_text(size=14), 
        axis.title.y=element_text(size=TSize),
        axis.title.x=element_text(size=TSize), 
        plot.title=element_text(size=TSize), 
        legend.text=element_text(size=14), 
        legend.title=element_blank(),
        strip.text.x = element_text(size=18) ) 

audgg.bw + facet_wrap(~Group) 

# Note that DPI should be 1200 for the publication ready plot:
ggsave("./Figures/Figure-Audiogram_better_worse_Fast_Linear.tiff", width=14.4, height = 7.2, dpi = 300)



## Below are results submitted in our first revision letter but these are not added 
## to the manuscript.
# Dist plots age:
ggplot(df.aud, aes(x=age)) +
  geom_histogram(binwidth = 2, colour="black", fill="white") + facet_wrap(~Group, scales = "free") +
  jtools::theme_apa(legend.pos = "topright") +
  
  labs(x = "Age (years)", y = "Freuqency", linetype = "Ear", shape = "Ear")

# Testing age:
res.t <- t.test(filter(df.aud, Group == "Normal Hearing")$age,
       filter(df.aud, Group == "Hearing Aid Users")$age)
report::report(res.t)

res.levene <- car::leveneTest(age~Group, df.aud)