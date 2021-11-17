library('dplyr')
library('rstatix')
library('rtf')
library('tidyr')
library('ggplot2')
library('stargazer')

# Read in data:
college_data <- read.csv('https://github.com/christineqn/researchmethods/raw/main/HW3/sports-and-education.csv')

# Create balance table:
bal_tab <- college_data %>%
  select(-c(College.Id, Alumni.Donations.2018)) %>%
  pivot_longer(-Ranked.2017, names_to='covars', values_to='value') %>%
  group_by(covars) %>%
  t_test(value ~ Ranked.2017, detailed=T) %>%
  add_significance() %>%
  mutate(across(starts_with('estimate'), ~round(.x,3)),
         p.signif=replace(p.signif, p.signif=='ns', ' '),
         covars=gsub('.', ' ', covars, fixed=T)) %>%
  select(c(' '=covars, Control=estimate1, Treatment=estimate2,
           Difference=estimate, '  '=p.signif))
rtf <- RTF('hw3-balance-table.doc', font.size=12)
addTable(rtf, bal_tab, col.widths=c(2,1,1,1,.5), col.justify='C', header.col.justify='C')
done(rtf)

# Build propensity score model:
m_pscore <- glm(Ranked.2017 ~ College.Id + Academic.Quality + Athletic.Quality +
     Near.Big.Market, family=binomial, college_data)
college_data$Propensity.Score <- predict(m_pscore, type='response')

# Use stacked histograms to show overlap:
ggplot(college_data, aes(x=Propensity.Score, fill=factor(Ranked.2017))) +
  geom_histogram() +
  scale_fill_discrete(name='Ranked 2017', labels=c('no','yes'))

# Drop all observations that lie within regions containing no-overlap

# Group observations into blocks based on propensity score
blocked_college_data <- college_data %>%
  arrange(Propensity.Score) %>%
  mutate(Block.Id = gl(4, nrow(college_data)/4))

# Analyze treatment effect of being ranked on alumni donations
reg1 <- lm(Alumni.Donations.2018 ~ Ranked.2017 + factor(Block.Id) +
             College.Id + Academic.Quality + Athletic.Quality + Near.Big.Market,
           blocked_college_data)
summary(reg1)
reg2 <- lm(Alumni.Donations.2018 ~ Ranked.2017 + factor(Block.Id),
           blocked_college_data); summary(reg2) # <--
reg3 <- lm(Alumni.Donations.2018 ~ Ranked.2017 +
             Academic.Quality + Athletic.Quality + Near.Big.Market,
           blocked_college_data); summary(reg3)

# Regression output:
stargazer(reg1, type='html',
          omit='factor+', omit.labels='Blocked propensity score fixed effects',
          dep.var.labels=c('Alumni Donations in 2018'),
          covariate.labels=c('Constant','Ranked in 2017','College ID','Academic Quality','Athletic Quality','Near Big Market'),
          dep.var.caption='', omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.',
          out='hw3-table.doc')
# * p<0.1, ** p<0.05, *** p<0.01