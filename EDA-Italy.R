library(tidyverse)

# Data source:
# https://ramikrispin.github.io/covid19Italy/
# https://github.com/RamiKrispin/italy_dash

# Number of Cases in relation to Sex & Comorbidities

covid_disease <- read_csv("Italy/covid-disease.csv")

covid_disease <- covid_disease %>%
  pivot_longer(
    c(Men, Women),
    names_to = "Sex",
    values_to = "cases")

head(covid_disease)

(d <- covid_disease %>%
  ggplot(
    aes(
      x= Disease,
      y= cases,
      fill= Sex
    )
  ) +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500)) +
  labs(
    x= "Comorbidities",
    y= "No. of Cases",
    title = "Number of Cases in relation to Sex & Comorbidities"
  ) + coord_flip() +
  theme(plot.title = element_text(hjust = 1)))

################


age_italy <- read_csv("Italy/covid-age.csv")
head(age_italy)
age_italy <- filter(age_italy[,c(1:8)])
head(age_italy)

# Distribution of cases in relation to age in males

age_italy %>%
  ggplot(
    aes(x = age_classes,
        y = male_cases)
    ) +
  geom_bar(stat = "identity")

# Distribution of cases in relation to age in females

age_italy %>%
  ggplot(
    aes(x = age_classes,
        y = female_cases)
  ) +
  geom_bar(stat = "identity")

# Tidying data
# Sex & cases

age_italy <- age_italy %>%
  pivot_longer(
    c(male_cases, female_cases),
    names_to = "sex",
    values_to = "cases")


age_italy <- age_italy %>%
  mutate(sex = ifelse(sex == "male_cases", "male","female"))

# Rearranging age classes
age_italy <- age_italy %>%
  mutate(age_classes = ifelse(age_classes == ">90", "90 or older", age_classes))

# Sex & deaths
age_italy <- age_italy %>%
  pivot_longer(
    c(male_deaths, female_deaths),
    names_to = "Sex",
    values_to = "deaths")

age_italy <- age_italy %>%
  mutate(Sex = ifelse(Sex == "male_deaths", "male","female"))

# Case-Fatality Rate CFR
age_italy <- age_italy %>%
  mutate(cfr = round((deaths/cases)*10,2))
age_italy$cfr

head(age_italy)

# Cases in relation to age & sex

(b <- age_italy %>%
  ggplot(
    aes(
      x= age_classes,
      cases,
      fill= sex
    )
  ) +
  geom_col(position = "dodge") +
  labs(x = "Age",
       y = "No. of Cases",
       title = "Case Distribution",
       subtitle = "in relation to Age & Sex") +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  coord_flip())

# Mortality in relation to age & sex

(c <- age_italy %>%
  ggplot(
    aes(
      x= age_classes,
      y= deaths,
      fill= Sex
    )
  ) +
  geom_col(position = "dodge") +
  labs(x = "Age",
       y = "No. of Deaths",
       title = "Mortality",
       subtitle = "in relation to Age & Sex") +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  coord_flip())

# Case-Fataliy Rate

(a <- age_italy %>%
  ggplot(
    aes(
      x= age_classes,
      y= cfr,
      fill= Sex
    )
  ) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1, scale = 10
      )) +
  labs(x = "Age",
       y = "% of Deaths",
       title = "Age-Sex-Specific Case-Fatality Rate in Italy") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5)) +
  coord_flip())

# Creating a grid

library(gridExtra)

grid.arrange(a, arrangeGrob(b, c, ncol = 2), nrow = 2)

# FAILED Attempts at pyramid plots

library(plyr)  

ggplot(data = age_italy,
    aes(
      x= age_classes,
      y = ifelse(Sex == "Male", yes = -cfr, no = cfr),
      fill = Sex
    )
  ) + geom_bar(stat = "identity") +
 scale_y_continuous(
   labels = abs,
   limits = max(age_italy$cfr) * c(-1,1)
  ) +
  labs(x = "Age",
       y = "% of Deaths",
       title = "Age-Sex-Specific Case-Fatality Rate in Italy") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5)) +
  coord_flip()


library(plyr)  

ggplot(data = age_italy,
       aes(
         x= age_classes,
         y = ifelse(Sex %in% c("Male"), -cfr, cfr),
         fill = Sex
       )
) + geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = abs,
    limits = max(age_italy$cfr) * c(-1,1)
  ) +
  labs(x = "Age",
       y = "% of Deaths",
       title = "Age-Sex-Specific Case-Fatality Rate in Italy") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5)) +
  coord_flip()

ggplot(data = age_italy,
       aes(
         x= age_classes,
         y = ifelse( grepl("Male", Sex), -cfr, cfr ),
         fill = Sex
       )
) + geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = abs,
    limits = max(age_italy$cfr) * c(-1,1)
  ) +
  labs(x = "Age",
       y = "% of Deaths",
       title = "Age-Sex-Specific Case-Fatality Rate in Italy") +
  theme(axis.title.y = element_text(angle = 0,
                                    vjust = 0.5)) +
  coord_flip()




library(ggcharts)

ggcharts::pyramid_chart(
  data = age_italy,
  x = age_classes,
  y = cfr,
  group = Sex,
  title = "Age-Sex-Specific Case-Fatality Rate in Italy",
  xlab = "Age")


  mapping = aes(x = age, y = ifelse(test = sex == "Male", yes = -pop, no = pop), 
                fill = sex)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = max(d$pop) * c(-1,1)) +
  labs(y = "Population")


labels = scales::percent_format(
  accuracy = 1, scale = 10)*(-1)
geom_bar(data = subset(age_italy, Sex=="female"), stat = "identity") + 
  geom_bar(data = subset(age_italy, Sex=="male"),stat = "identity", , aes(y=..count..*(-1))
           
##############################

library(tidyverse)
library(ggcharts)

# SUCCESSFUL Attempts at pyramid plots

#Mortality in relation to Age & Sex

covid <- read.csv("Italy/covid-age.csv") %>%
  select(age_classes, male_deaths, female_deaths) %>%
  pivot_longer(
    cols = c(male_deaths, female_deaths),
    values_to = "deaths",
    names_to = "sex"
  ) %>%
  mutate(sex = ifelse(sex == "male_deaths", "male","female"))

(a <- ggcharts::pyramid_chart(
  data = covid,
  x = age_classes,
  y = deaths,
  group = sex,
  title = "Mortality in relation to Age & Sex",
  xlab = "Age"
))

# Case distribution in relation to Age & Sex

covid <- read.csv("Italy/covid-age.csv") %>%
  select(age_classes, male_cases, female_cases) %>%
  pivot_longer(
    cols = c(male_cases, female_cases),
    values_to = "cases",
    names_to = "sex"
  ) %>%
  mutate(sex = ifelse(sex == "male_cases", "male","female"))

(b <- ggcharts::pyramid_chart(
  data = covid,
  x = age_classes,
  y = cases,
  group = sex,
  title = "Case distribution in relation to Age & Sex",
  xlab = "Age"
))

covid <- read.csv("Italy/covid-age.csv") %>%
  select(age_classes,
         male_cases, female_cases,
         male_deaths, female_deaths) %>%
  mutate(cfr_male = ((male_deaths/male_cases)*100),
         cfr_female = ((female_deaths/female_cases)*100)) %>%
  pivot_longer(
    cols = c(cfr_male, cfr_female),
    values_to = "cfr",
    names_to = "gender"
  ) %>%
  mutate(gender = ifelse(gender == "cfr_male", "male","female"))

(c <- ggcharts::pyramid_chart(
  data = covid,
  x = age_classes,
  y = cfr,
  group = gender,
  title = "Age-Sex-Specific Case-Fatality Rate (%)",
  xlab = "Age"
)) 

######################

covid_disease <- read.csv("Italy/covid-disease.csv")

covid_disease <- covid_disease %>%
  pivot_longer(
    c(Men, Women),
    names_to = "Sex",
    values_to = "cases")

(d <- ggcharts::pyramid_chart(
  data = covid_disease,
  x = Disease,
  y = cases,
  group = Sex,
  title = "Case Distribution in relation to Sex & Comorbidities",
  xlab = "Disease"
))
