setwd('C:/Users/c1/OneDrive/Desktop/Fall 2021/Research methods II/researchmethods/HW1')

library('stargazer')
library('dplyr')

# Read in data:
data <- read.csv('assignment1-research-methods.csv')

# Run regression: 
# effect of having an elite college on whether the fictitious candidate's job application was called back
regression <- lm(calledback ~ EliteSchoolCandidate, data)
regression2 <- lm(calledback ~ EliteSchoolCandidate*MaleCandidate, data)
regression3 <- lm(calledback ~ EliteSchoolCandidate*BigCompanyCandidate, data)
regression4 <- lm(calledback ~ EliteSchoolCandidate*recruiteriswhite, data)
regression5 <- lm(calledback ~ EliteSchoolCandidate + 
                    recruiteriswhite + recruiterismale + 
                    MaleCandidate + BigCompanyCandidate, data)
regression6 <- lm(calledback ~ EliteSchoolCandidate + recruiteriswhite +
                    recruiterismale + BigCompanyCandidate, data)

# Output:
stargazer(regression, type='text', dep.var.caption='', omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.')
stargazer(regression, regression2, regression3, regression4, regression5,
          type='text', dep.var.caption='', dep.var.labels=c('Called Back'), omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.',
          covariate.labels=c('Constant','Elite School Candidate',
                             'Male Candidate','Elite School Candidate x Male Candidate',
                             'Big Company Candidate','Elite School Candidate x Big Company Candidate',
                             'Recruiter is White','Elite School Candidate x Recruiter is White',
                             'Recruiter is Male'),
          out='hw1-table.doc')
# removing control for whether the candidate was male or female 
stargazer(regression, regression3, regression4, regression6,
          type='html', dep.var.caption='', dep.var.labels=c('Called Back'), omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.',
          covariate.labels=c('Constant','Elite School Candidate',
                             'Big Company Candidate','Elite School Candidate x Big Company Candidate',
                             'Recruiter is White','Elite School Candidate x Recruiter is White',
                             'Recruiter is Male'),
          out='hw1-table.doc')
# * p<0.1, ** p<0.05, *** p<0.01