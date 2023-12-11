all_matches <- data.frame()

for (year in 1968:2023) {
  file_name <- paste0("dataset/atp_matches_", year, ".csv")

  matches_year <- read.csv(file_name, stringsAsFactors = FALSE)

  all_matches <- rbind(all_matches, matches_year)
}

print(str(all_matches))

print(names(all_matches))

print(summary(all_matches))

for (col in names(all_matches)) {
  print(paste("Summary for column", col))
  print(summary(all_matches[[col]]))
}


