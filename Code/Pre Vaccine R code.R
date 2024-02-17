#install.packages("lavaan")
#install.packages("xlsx")
library(lavaan)
library(xlsx)
setwd("~/DATASCIENCE_RESEARCH_PROJECT")

pre_vac = read.xlsx("pre_vac2_new.xlsx", 1, header=TRUE)

df = subset(pre_vac, select = -c(NA.,iso_code, date, location) )

df = scale(df, center = TRUE, scale = TRUE)
df = as.data.frame(df)






cvdmodel <- '
f1 =~ total_cases_per_million + new_cases_smoothed_per_million + hospital_beds_per_thousand
f2 =~ total_deaths_per_million + hospital_beds_per_thousand + new_deaths_smoothed_per_million + cardiovasc_death_rate
f3 =~ aged_65_older + cardiovasc_death_rate
f4 =~ stringency_index + population
f1 ~~ f2 
f2 ~~ f3
'



fit <- sem(cvdmodel, data = df, se = "robust.sem", test = "standard", mimic = "EQS")
summary(fit, fit.measures = TRUE)

