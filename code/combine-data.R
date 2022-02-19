melb_df <- read.table(
  file = "data/Melbourne Rainfall.csv",
  header = TRUE,
  sep = ",",
  skip = 11)

mean_rainfall_melb <- as.numeric(melb_df[24,2:13])

ox_df <- read.table(
  file = "data/Oxford Data.txt",
  header = FALSE,
  col.names = c("year", "month", "x1", "x2", "x3", "rain", "x4"),
  skip = 7,
  na.strings = "---",
  nrows = 12 * (2020 - 1853))

year_mask <- (1855 <= ox_df$year) & (ox_df$year <= 2015)
tmp <- ox_df[year_mask,c("month","rain")]
tmp$rain <- as.numeric(
  gsub(
    pattern = "\\*",
    replacement = "",
    x = tmp$rain))

mean_rainfall_ox <- aggregate(
  x = tmp,
  by = list(month = tmp$month),
  FUN = mean)$rain

months <- c(
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December")

plot_df <- data.frame(
  month = rep(months, 2),
  month_num = rep(1:12, 2),
  location = rep(c("Melbourne", "Oxford"), each = 12),
  average_rainfall = c(mean_rainfall_melb, mean_rainfall_ox))

write.table(x = plot_df,
            file = "data/average-rainfall.csv",
            sep = ",",
            row.names = FALSE)

#the second section makes the R plot

library(ggplot2)

plot_df <- read.csv("data/average-rainfall.csv")

g <- ggplot(
  data = plot_df,
  mapping = aes(
    x = as.integer(month_num),
    y = average_rainfall,
    colour = location)) +
  geom_point() +
  geom_line() +
  labs(
    x = NULL,
    y = "Average rainfall (mm)",
    colour = "City",
    title = "Rainfall",
    subtitle = "Monthly average (1855--2015)") +
  scale_x_continuous(
    breaks = plot_df$month_num[seq(2,12,2)],
    labels = plot_df$month[seq(2,12,2)]) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

ggsave(filename = "outputs/result.png",
       plot = g,
       height = 10.5, width = 14.8,
       units = "cm")

sink(file = "outputs/package-versions.txt")
sessionInfo()
sink()
