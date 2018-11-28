library(dplyr)
library(plotly)
library(lubridate)

# From https://www.kaggle.com/drgilermo/nba-players-stats
stats <- read.csv(file="Seasons_Stats.csv", header=TRUE, sep=",")

# From https://data.world/datadavis/nba-salaries/
salaries <- read.csv(file="nba_salaries_1990_to_2018.csv", header=TRUE, sep=",")

# From https://en.wikipedia.org/wiki/NBA_salary_cap (already adjusted for inflation)
salary.cap <- c(26.9, 30, 34, 35, 42.5, 40.271, 43.840, 43.870, 49.5, 53.135, 55.630, 58.680, 57.700, 58.044,
                58.044, 58.044, 58.679, 63.065, 70, 94.143, 99.093)
salary.cap <- data.frame(season_end=1998:2018,salary.cap)

# Don't care about stats per team, just per year
stats <- stats %>% filter(Year >= 1998) %>% rename(season_end = Year, player = Player) %>%
  group_by(season_end, player) %>% summarize_if(is.numeric, mean, na.rm = TRUE)
stats$player <- sub("\\*", "", stats$player)
stats$X3PAr <- stats$X3PA / stats$FGA
  
salaries <- salaries %>% filter(season_end >= 1998) %>% filter(season_end != 2018)

merged <- merge(stats, salaries)
merged <- merge(merged, salary.cap)
merged$salary.ratio <- merged$salary / (merged$salary.cap * 1000000 * 30)

x3p.stats <- merged %>% group_by(season_end) %>% 
  summarize(avg.3p.percent = sum(X3P, na.rm = TRUE) / sum(X3PA, na.rm = TRUE))

avg.3p.percent <- sapply(merged$season_end, function(x) {
  x3p.stats[x3p.stats$season_end == x,]$avg.3p.percent})

specialists <- merged %>% filter(X3PAr > 0.5 & (X3P / X3PA) >= avg.3p.percent & X3P >= 82)

linear_model <- lm(salary.ratio ~ season_end, specialists)

salary.over.time <- plot_ly(data = specialists, x = ~season_end, text = ~player) %>%
  add_markers(y = ~salary.ratio) %>%
  layout(xaxis = list(title = "Year"), yaxis = list(title = "Salary Ratio"))

specialists.freq <- as.data.frame(table(specialists$season_end))
specialists.over.time <- plot_ly(data = specialists.freq, x = ~Var1, y = ~Freq, type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "Year"), yaxis = list(title = "Number of 3-point specialists"))

