setwd('C:/Users/c1/OneDrive/Desktop/Fall 2021/Research methods II/researchmethods/HW2')

library('stargazer')
library('dplyr')
library('plm')
library('ggplot2')

# Read in data:
data <- read.csv('vaping-ban-panel.csv')

# Label variables:
# data <- rename_with(data, ~ gsub('.', ' ', .x, fixed=T))

# data <- data %>% mutate(across(c(State.Id, Year), as.factor))
treatment_states <- data %>%
  filter(Year==2021 & Vaping.Ban==1) %>%
  pull(State.Id)
data <- data %>%
  mutate(treatment = ifelse(State.Id %in% treatment_states,1,0))

# Run regression: "parallel trends" requirement of a DnD estimate
pretreat <- data %>% filter(Year<2021)
regression1 <- lm(Lung.Hospitalizations ~ treatment*Year, pretreat)
summary(regression1)

# Create DnD line graph:
grouped_data <- data %>%
  group_by(Year, treatment) %>%
  summarize(Lung.Hospitalizations=mean(Lung.Hospitalizations))
ggplot(grouped_data, aes(x=Year, y=Lung.Hospitalizations, color=factor(treatment))) +
  geom_line() + 
  geom_vline(xintercept=2021) +
  labs(y='Lung Hospitalizations') +
  scale_colour_discrete(name='States', labels=c('without ban','with ban'))

# Run regression: causal effect of the anti-vaping laws on the number of lung-related
# hospitalizations, including time period fixed effects, state fixed effects
regression2 <- lm(Lung.Hospitalizations ~ -1 + factor(State.Id) + factor(Year) + Vaping.Ban, data)
# regression2 <- plm(Vaping.Ban ~ , data, model='within', effect='twoways')
summary(regression2)

# Output:
stargazer(regression1, regression2, type='html',
          omit='factor+', omit.labels='Year and state fixed effects',
          dep.var.labels=c('Lung Hospitalizations'),
          covariate.labels=c('Constant', 'Treatment', 'Year', 'Treatment x Year', 'Vaping Ban'),
          dep.var.caption='', omit.stat=c('LL','ser','f'), digit.separator='', digits=2, initial.zero=F, intercept.bottom=F, no.space=T, notes='Standard errors in parentheses.',
          out='hw2-table.doc')
# * p<0.1, ** p<0.05, *** p<0.01

# Test whether all fixed effects are zero:
regTermTest(regression2, c('factor(State.Id)','factor(Year)'))
