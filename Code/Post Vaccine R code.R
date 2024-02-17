library(lavaan)
library(xlsx)
setwd("~/DATASCIENCE_RESEARCH_PROJECT")

post_vac = read.xlsx("post_vac2_new.xlsx", 1, header=TRUE)

df = subset(post_vac, select = -c(NA.,iso_code, date, location) )

df = scale(df, center = TRUE, scale = TRUE)
df = as.data.frame(df)



cvdmodel <- '
f1 =~ total_deaths_per_million + total_cases_per_million + new_cases_smoothed_per_million + population + new_deaths_smoothed_per_million
f2 =~ total_deaths_per_million + stringency_index + new_deaths_smoothed_per_million + proportion_vaccinated + total_cases_per_million
f3 =~ new_deaths_smoothed_per_million + cardiovasc_death_rate + total_deaths_per_million
f4 =~ total_cases_per_million + new_cases_smoothed_per_million + hospital_beds_per_thousand + proportion_vaccinated
f1 ~~ f2 + f3 + f4
'


fit <- sem(cvdmodel, data = df, se = "robust.sem", test = "standard", mimic = "EQS")
summary(fit, fit.measures = TRUE)


