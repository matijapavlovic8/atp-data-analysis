# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("caret")

library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)

# Load data
all_matches <- data.frame()
for (year in 1991:2023) {
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

# Task 2
t2 <- all_matches %>%
  filter(!is.na(w_df) & !is.na(l_df) & !is.na(surface) & w_df != "" & l_df != "" & surface != "")
t2 <- select(t2, w_df, l_df, surface)
t2$court_type <- ifelse(t2$surface %in% c("Clay", "Grass", "Hard"), "Outdoor", "Indoor")

t2 <- t2 %>%
  mutate(df = w_df + l_df)

open_surface_data <- t2$df[t2$court_type == 'Outdoor']
closed_surface_data <- t2$df[t2$court_type == 'Indoor']

# Nul hipoteza je da su srednje vrijednosti double faultova iste,
# p vrijednost ispadne visoka pa je ne mozemo odbacit sta ima smisla
t_test_result <- t.test(open_surface_data, closed_surface_data)

print(t_test_result)

# Task 3
t3 <- all_matches %>%
  filter(!is.na(w_ace) & !is.na(l_ace) & !is.na(surface) & w_ace != "" & l_ace != "" & surface != "")
t3 <- select(t3, surface, w_ace, l_ace)
t3 <- t3 %>%
  mutate(aces = w_ace + l_ace)
t3

# Rezultirajuca p vrijednost je mala sto sugerira da je srednja vrijednost asova na barem jednoj podlozi znazno razlicita
# Moga bi bit dodatni zadatak provjerit na kojoj povrsini je razlika
aov_res <- aov(aces~surface, data=t3)
print(summary(aov_res))

# Task 4
t4 <- all_matches[all_matches$best_of == 5, ]
t4$sets_played <- sapply(strsplit(as.character(t4$score), ""), function(x) sum(x == "-"))

contingency_table <- table(t4$surface, t4$sets_played == 5)
print(contingency_table)

# Vjerojatnost da ce mec otic u peti set ne ovisi o podlozi, jer hi kvadrat p-value = 0.361
# Nul hipoteza je da vjerojatnost odlaska u peti set ne ovisi o podlozi
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

# Task 5
features <- c("tourney_id", "w_ace", "l_ace", "w_1stIn", "l_1stIn", "w_1stWon", "l_1stWon", "winner_ht", "loser_ht", "winner_hand", "loser_hand", "winner_seed", "loser_seed", "winner_id", "loser_id")

# Create a new dataframe with selected features
t5 <- all_matches %>% select(features)
t5 <- na.omit(t5)

# Convert categorical variables to factors (if needed)
t5$winner_hand <- as.factor(t5$winner_hand)
t5$loser_hand <- as.factor(t5$loser_hand)

# Extract the year from the tourney_id
t5$year <- as.numeric(substring(t5$tourney_id, 1, regexpr("-", t5$tourney_id)-1))

# Create a new column for the target variable (sum of aces in the following year)
t5$aces_in_following_year <- NA

# Aggregate features by player and year
aggregated_data <- t5 %>%
  group_by(player_id = ifelse(!is.na(winner_id), winner_id, loser_id), year) %>%
  summarize(
    total_aces = sum(w_ace + l_ace, na.rm = TRUE),
    avg_w_1stIn = mean(w_1stIn, na.rm = TRUE),
    avg_l_1stIn = mean(l_1stIn, na.rm = TRUE),
    avg_w_1stWon = mean(w_1stWon, na.rm = TRUE),
    avg_l_1stWon = mean(l_1stWon, na.rm = TRUE),
    winner_ht = mean(winner_ht, na.rm = TRUE),
    loser_ht = mean(loser_ht, na.rm = TRUE),
    winner_hand = names(sort(table(winner_hand), decreasing = TRUE))[1],
    loser_hand = names(sort(table(loser_hand), decreasing = TRUE))[1],
    winner_seed = mean(winner_seed, na.rm = TRUE),
    loser_seed = mean(loser_seed, na.rm = TRUE)
  )

print(aggregated_data[aggregated_data$player_id == 104925, ])

# Drop rows with NA in the target variable column
aggregated_data <- aggregated_data[complete.cases(aggregated_data$total_aces), ]

# Split the data into training and testing sets
set.seed(123)  # for reproducibility
train_indices <- createDataPartition(aggregated_data$total_aces, p = 0.8, list = FALSE)
train_data <- aggregated_data[train_indices, ]
test_data <- aggregated_data[-train_indices, ]

# Train the linear regression model
lm_model <- lm(total_aces ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model
rmse <- sqrt(mean((test_data$total_aces - predictions)^2))
print(paste("Root Mean Squared Error:", rmse))