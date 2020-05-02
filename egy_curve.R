library(tidyverse)
library(ggplot2)
library(ggthemes)

# The data used is gathered from the Egyptian Ministry
# of Health and Population's Facebook page
# Link: https://www.facebook.com/egypt.mohp

# Importing data
egy <- read.csv("Egypt-data/Egypt.csv")
attach(egy)
View(egy)

# Converting datetime variable

egy$date <- as.Date(egy$date, format = "%m/%d/%Y")
class(egy$date)

# Calculating current cases

egy <- egy %>%
  mutate(current_cases = all_cases- (recovered + all_deaths))

# Plotting new cases and new deaths

# Tidying data for the plot

egypt_long <- egy %>%
  select(date, new_cases, new_deaths) %>%
  pivot_longer(-date, names_to = "COVID19", values_to = "count")

head(egypt_long)

# Converting datetime variable

egypt_long$date <- as.POSIXct(egypt_long$date, format = "%m/%d/%Y")
class(egypt_long$date)

# Plotting the curve

(b <- ggplot(data = egypt_long,
    aes(
      x= date,
      y= count,
      colour = COVID19
    )
  ) +
  geom_line() +
  geom_path(size = 1) +
  labs(title = "Egypt: Epidemic curve",
       x = "Date",
       y = "Count") +
  scale_colour_manual(labels = c("New cases",
                                 "New deaths"),
                      values = c("salmon",
                                 "cyan3")) +
  theme_economist() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.title = element_blank()))


# Plotting total cases, recovery and total deaths

# Tidying data for the plot

egypt_long2 <- egy %>%
  select(date, all_cases, recovered, all_deaths) %>%
  pivot_longer(-date, names_to = "COVID19", values_to = "count")

head(egypt_long2)

# Converting datetime variable

egypt_long2$date <- as.POSIXct(egypt_long2$date, format = "%m/%d/%Y")
class(egypt_long2$date)

# Plotting the curve

(c <- ggplot(data = egypt_long2,
       aes(
         x= date,
         y= count,
         colour = COVID19
       )
) +
  geom_line() +
  geom_path(size = 1) +
  labs(title = "Egypt: Epidemic curve",
       x = "Date",
       y = "Count") +
  scale_colour_manual(labels = c("Total cases",
                                 "Total deaths",                                 "Recovered",
                                 "Recovered"),
                      values = c("salmon",
                                 "springgreen3",
                                 "cyan3")) +
  theme_economist() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank(),
        legend.title = element_blank()))


# Plotting total, current and new cases

# Tidying data for the plot

egypt_long3 <- egy %>%
  select(date, new_cases, current_cases, all_cases) %>%
  pivot_longer(-date, names_to = "COVID19", values_to = "count")

head(egypt_long3)

# Converting datetime variable

egypt_long3$date <- as.POSIXct(egypt_long3$date, format = "%m/%d/%Y")
class(egypt_long3$date)

# Plotting the curve

(a <- ggplot(data = egypt_long3,
       aes(
         x= date,
         y= count,
         colour = COVID19
       )
) +
  geom_line() +
  geom_path(size = 1) +
  labs(title = "Egypt: Epidemic curve",
       subtitle = "March 13 to April 30",
       x = "Date",
       y = "Count") +
  scale_colour_manual(labels = c("Total cases",
                                 "Current cases",
                                 "New cases"),
                      values = c("salmon",
                                 "springgreen3",
                                 "cyan3")) +
  theme_economist() +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 12,
                                    color = "royalblue4"),
        axis.title.y = element_text(face = "bold",
                                    size = 12,
                                    color = "royalblue4"),
        plot.title = element_text(size = 20,
                                  color = "indianred3"),
        plot.subtitle = element_text(size = 12,
                                     color = "indianred3"),
        legend.title = element_text(face = "bold",
                                    size = 20,
                                    color = "royalblue4")))

# Creating a grid

library(gridExtra)

grid.arrange(a, arrangeGrob(b, c, ncol = 2), nrow = 2)

