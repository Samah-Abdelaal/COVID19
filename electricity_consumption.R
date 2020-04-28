# This is NOT MINE, I copied it from Thomas Neitmann
# and am putting here for safe-keeping
# This is the link: https://lnkd.in/geDScan

library(ggplot2)
library(dplyr)

electrial_consumption <- tibble::tribble(
  ~country, ~pct,
  "Italy", -27,
  "Spain", -21,
  "Belgium", -17,
  "Austria", -16,
  "France", -16,
  "Portugal", -15,
  "United Kingdom", -14,
  "Germany", -8
)

electrial_consumption %>%
  mutate(
    country = reorder(country, -pct),
    label = paste0(pct, "%"),
    code = tolower(countrycode::countrycode(country, "country.name", "iso2c"))
  ) %>%
  ggplot(aes(country, pct)) +
  geom_col(fill = "#617784", width = .7) +
  geom_text(aes(label = label), hjust = -.1, color = "white", size = 10) +
  ggfalgs::geom_flag(aes(country = code), y = 1, size = 12) +
  geom_hline(yintercept = 0, size = 1) +
  coord_flip() +
  scale_x_discrete(position = "top") +
  scale_y_continuous(expand = expansion(c(0, .07))) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#f4f8fb", color = "#f4f8fb"),
    plot.background = element_rect(fill = "#f4f8fb", color = "#f4f8fb"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black", size = 28),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(face = "bold", size = 54),
    plot.subtitle = element_text(color = "#4a504e", size = 30),
    plot.caption = element_text(hjust = 0, color = "#4a504e", size = 24),
    plot.margin = margin(50, 30, 100, 30)
  ) +
  labs(
    title = "How Covid-19 is affecting\nelectricity consumption",
    subtitle = "Change in electrical consumption in selected\ncountries on 8 April 2020 compared to 2019*",
    caption = "* Average peak hour consumption. Comparison with corresponding day of the\nweek (10 April 2019). Percentages are adjusted for differences in temperature\nfrom 2019 to 2020.\nSources: Bruegel, ENTSO-E"
  )
