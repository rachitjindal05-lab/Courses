library(ggplot2)

# 1. Load the data
# Ensure 'journals.csv' is in your working directory
df <- read.csv("journals.csv", check.names = FALSE)

# 2. Robust Column Renaming
# This ensures we find the correct columns even if headers vary slightly
colnames(df)[grep("^SJR$", colnames(df), ignore.case = TRUE)] <- "SJR"
colnames(df)[grep("H index|H_index", colnames(df), ignore.case = TRUE)] <- "H_Index"
colnames(df)[grep("^Qs$|Quality|Quartile", colnames(df), ignore.case = TRUE)] <- "Quality_Index"

# 3. Data Cleaning
# Remove rows with missing values or values <= 0 (since we are using Log scales)
df <- df[!is.na(df$SJR) & df$SJR > 0, ]
df <- df[!is.na(df$H_Index) & df$H_Index > 0, ]
df <- df[!is.na(df$Quality_Index), ]

# 4. Set Factor Levels
# This ensures the legend is ordered Q1 -> Q2 -> Q3 -> Q4
df$Quality_Index <- factor(df$Quality_Index, levels = c("Q1", "Q2", "Q3", "Q4"))

# 5. Generate the Plot
final_plot <- ggplot(df, aes(x = H_Index, y = SJR, color = Quality_Index)) +
  
  # Use shape = 8 for the asterisk (*) symbol
  geom_point(shape = 8, size = 2) +
  
  # X-Axis: Log base 10 with manual breaks at 1, 10, 100
  scale_x_log10(
    breaks = c(1, 10, 100),
    labels = c("1", "10", "100")
  ) +
  
  # Y-Axis: Log base 10 with manual breaks at 0.1, 0.3, 1.0, 3.0
  scale_y_log10(
    breaks = c(0.1, 0.3, 1.0, 3.0),
    labels = c("0.1", "0.3", "1.0", "3.0")
  ) +
  
  # Manual Colors to match the image
  scale_color_manual(
    values = c(
      "Q1" = "#A4A400",  # Olive
      "Q2" = "#00B050",  # Green
      "Q3" = "#00B0F0",  # Blue
      "Q4" = "#E066FF"   # Pink/Purple
    ),
    name = "Quality Index"
  ) +
  
  # Labels
  labs(
    title = "Relationship between SJR/Hindex and Qs",
    x = "Log of H Index",
    y = "Log of SJR"
  ) +
  
  # Theme settings to match the gray background and grid
  theme_gray() +
  theme(
    plot.title = element_text(size = 14, hjust = 0),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    legend.key = element_blank()
  )
final_plot
