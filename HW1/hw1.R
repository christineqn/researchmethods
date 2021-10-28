setwd('C:/Users/c1/OneDrive/Desktop/Fall 2021/Research methods II/model assignment')

library('stargazer')
library('dplyr')

# Read in data:
data <- read.csv('assignment1-research-methods.csv')

# Label your variables
data <- rename(data,
               CandidateID=candidateid,
               CalledBack=calledback,
               RecruiterIsWhite=recruiteriswhite,
               RecruiterIsMale=recruiterismale)

# Run regression: 
# effect of having an elite college on whether the fictitious candidate's job application was called back
regression <- lm(CalledBack ~ EliteSchoolCandidate, data)
regression2 <- lm(CalledBack ~ EliteSchoolCandidate*MaleCandidate, data)
regression3 <- lm(CalledBack ~ EliteSchoolCandidate*BigCompanyCandidate, data)
regression4 <- lm(CalledBack ~ EliteSchoolCandidate*RecruiterIsWhite, data)
regression5 <- lm(CalledBack ~ EliteSchoolCandidate + 
                    RecruiterIsWhite + RecruiterIsMale + 
                    MaleCandidate + BigCompanyCandidate, data)

# Output:
stargazer(regression, type='text', dep.var.caption='', omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.')
stargazer(regression, regression2, regression3, regression4, regression5,
          type='html', dep.var.caption='', omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.',
          out='hw1-table.doc')
# * p<0.1, ** p<0.05, *** p<0.01