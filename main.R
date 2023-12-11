# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")

library(dplyr)
library(lubridate)
library(ggplot2)

# Load data
all_matches <- data.frame()
for (year in 1968:2023) {
  file_name <- paste0("dataset/atp_matches_", year, ".csv")

  matches_year <- read.csv(file_name, stringsAsFactors = FALSE)

  all_matches <- rbind(all_matches, matches_year)
}

# Data summary
print(str(all_matches))
print(names(all_matches))
print(summary(all_matches))

# Task 1
t1 <- all_matches %>%
  filter(!is.na(surface) & !is.na(tourney_date) & surface != "")

t1 <- select(t1, surface, tourney_date)
print(t1)

t1$tourney_date <- as.Date(as.character(t1$tourney_date), format = "%Y%m%d", origin = "1970-01-01")
t1$month <- month(t1$tourney_date)
t1$season <- case_when(
  t1$month %in% c(3, 4, 5) ~ "Spring",
  t1$month %in% c(6, 7, 8) ~ "Summer",
  t1$month %in% c(9, 10, 11) ~ "Fall",
  t1$month %in% c(12, 1, 2) ~ "Winter",
  TRUE ~ "Unknown"
)
print(t1)

surface_counts <- table(t1$season, t1$surface)
surface_counts_df <- as.data.frame(surface_counts)
names(surface_counts_df) <- c("Season", "Surface", "Frequency")

ggplot(t1, aes(x = season, fill = factor(season))) +
  geom_bar() +
  facet_wrap(~ surface, scales = "free") +
  labs(title = "Season Frequencies for Each Surface", x = "Season", y = "Frequency") +
  theme_minimal()

ggplot(surface_counts_df, aes(x = Surface, y = Frequency, fill = Surface)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Season, scales = "free") +
  labs(title = "Surface Frequencies for Each Season", x = "Surface", y = "Frequency") +
  theme_minimal()


#Task 3
t3 <- all_matches %>%
  filter(!is.na(w_ace) & !is.na(l_ace) & !is.na(surface) & w_ace != "" & l_ace != "" & surface != "")
t3 <- select(t3, surface, w_ace, l_ace)
t3 <- t3 %>%
  mutate(aces = w_ace + l_ace)

ggplot(t3, aes(x = aces, fill = factor(aces))) +
  geom_bar() +
  facet_wrap(~ surface, scales = "free") +
  labs(title = "Aces Frequencies for Each Surface", x = "Aces", y = "Frequency") +
  theme_minimal()