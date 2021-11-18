library('dplyr')
library('gtsummary')
library('stargazer')
library('ivreg')

# Read in data:
crime_data <- read.csv('https://github.com/christineqn/researchmethods/raw/main/HW4/crime-iv.csv')

# Perform a balance test:
crime_data %>%
  select(c(Republican.Judge, Severity.Of.Crime)) %>%
  tbl_summary(by='Republican.Judge',
              statistic=list(all_continuous() ~ '{mean}'),
              digits=everything() ~ 1) %>%
  add_difference()
# or
t.test(Severity.Of.Crime ~ Republican.Judge, crime_data)

# First stage regression:
reg_1st <- lm(Months.In.Jail ~ Republican.Judge + Severity.Of.Crime, crime_data); summary(reg_1st)
stargazer(reg_1st, type='html',
          dep.var.labels=c('Months in Jail'),
          covariate.labels=c('Constant','Republican Judge','Severity of Crime'),
          dep.var.caption='', omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.',
          out='hw4-firststage-table.doc')
# * p<0.1, ** p<0.05, *** p<0.01

# Reduced form regression:
reg_reduced <- lm(Recidivates ~ Republican.Judge + Severity.Of.Crime, crime_data); summary(reg_reduced)

# Ratio of the reduced form:
reg_reduced$coefficients['Republican.Judge'] / reg_1st$coefficients['Republican.Judge']

# IV regression:
reg_IV <- ivreg(Recidivates ~ Months.In.Jail + Severity.Of.Crime |
                  Republican.Judge + Severity.Of.Crime, data=crime_data); summary(reg_IV)
stargazer(reg_IV, type='html',
          covariate.labels=c('Constant','Months in Jail','Severity of Crime'),
          dep.var.caption='', omit.stat=c('LL','ser'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.',
          out='hw4-IV-table.doc')