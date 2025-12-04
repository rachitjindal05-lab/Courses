library(dplyr)

# 1) Read
# (Keep check.names = FALSE so headers remain exactly as in the CSV)
raw_students <- read.csv(
  "data/students_raw_simple.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 2) Transform
students_clean <- raw_students %>%
  mutate(
    # --- Section normalization (TODO: keep first alphabetic, uppercase) ---
    Section = {
      pos <- regexpr("[A-Za-z]", Section) 
      dplyr::if_else(pos > 0L,
                     toupper(substr(Section, pos, pos)),
                     NA_character_)
    },
    
    # --- Score parsing (TODO: leading number; comma->dot; numeric) ---
    Score = gsub("^[[:space:]]*([0-9]+([.,][0-9]+)?).*", "\\1", Score),
    Score = gsub(",", ".", Score, fixed = TRUE),
    Score = as.numeric(Score),
    
    # --- Attendance parsing (TODO: digits + dot/comma only; comma->dot; numeric) ---
    Attendance = gsub("[^0-9.,]", "", Attendance),
    Attendance = gsub(",", ".", Attendance, fixed = TRUE),
    Attendance = as.numeric(Attendance),
    
    # --- Pass flag (TODO: thresholds) ---
    Pass = !is.na(Score) & !is.na(Attendance) & Score >= 40 & Attendance >= 75
  ) %>%
  select(ID, Name, Dept, Section, Score, Attendance, Pass) %>%
  tibble::as_tibble()

# 3) Display & Save (REQUIRED)
print(students_clean, n = min(10, nrow(students_clean)), width = Inf)
