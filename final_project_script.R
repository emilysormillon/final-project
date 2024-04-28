## Project:  STA 215, Spring 2024, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
# generate summary statistics for bpm
mean(data$bpm_quant)
sd(data$bpm_quant)
hist(data$bpm_quant)
summary(data$bpm_quant)

# generate summary statistics for listeners of song
mean(data$listeners_quant)
sd(data$listeners_quant)
summary(data$listeners_quant)

# generate summary statistics for relatability
table(data$relatability_qual)

# generate summary statistics for tone
table(data$tone_qual)
##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
ggplot(raw_data, aes(x = data$tone_qual, y = data$bpm_quant)) +
  geom_boxplot() +
  labs(title = "Box Plot of BPM by Tone",
       x = "Tone",
       y = "BPM") +
  theme_minimal()

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(raw_data$listeners_quant, raw_data$bpm_quant)
print(linear_plot)

# add x line and y line for means
meany <- mean(raw_data$listeners_quant)
meanx <- mean(raw_data$bpm_quant)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$tone_qual, data$relatability_qual)
chisq.test(data$tone_qual, data$relatability_qual)
