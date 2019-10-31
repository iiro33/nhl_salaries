library(tidyverse)
library(ggplot2)
library(ggthemes)

salaries <- readxl::read_excel("nhl_salaries.xlsx", sheet = "salaries2")
points <- readxl::read_excel("nhl_salaries.xlsx", sheet = "season_points")

length(unique(salaries$Team))
length(unique(points$Team))
colnames(salaries) <- c("Player", "TeamLong", "Pos", "CapHit", "Salary")
salaries <- merge(salaries, points[, c(1, 3)])
summary(salaries)

plotdata <- salaries %>% 
  group_by(Team) %>% 
  summarize(salarysum = sum(Salary), caphitsum = sum(CapHit)) %>% 
  arrange(desc(salarysum))

point <- scales::format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

ggplot(plotdata, aes(x = salarysum, y = caphitsum)) +
  geom_point() +
  scale_y_continuous(labels = point) +
  scale_x_continuous(labels = point) +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(label = plotdata$Team) +
  theme_bw()

ggplot(plotdata, aes(x = reorder(Team, salarysum), y = salarysum)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  scale_y_continuous(labels = point) +
  coord_flip() +
  theme_economist_white()

plotdata2 <- inner_join(plotdata, points, by = c("Team" = "Team"))
plotdata2 <- plotdata2 %>% arrange(Points)

ggplot(plotdata2, aes(x = Points, y = caphitsum)) +
  geom_point() +
  scale_y_continuous(labels = point, limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(60, 140, by = 10)) +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_text_repel(label = plotdata2$Team) +
  theme_bw() +
  labs(title = "Cap hit by season points 2018-2019",
       x = "Points in regular season",
       y = "Season cap hit in $")

# look at the variance
plotdata3 <- salaries %>% 
  group_by(Team) %>% 
  summarize(meancap = mean(CapHit), sdcap = sd(CapHit), varcap = var(CapHit), capsum = sum(CapHit)) %>% 
  inner_join(points, by = c("Team" = "Team"))
plotdata3 <- arrange(plotdata3, Points)

salaries %>% 
  ggplot(aes(x = Team, y = CapHit)) +
    geom_boxplot() +
    scale_y_continuous(labels = point) +
    theme_bw()

ggplot(plotdata3, aes(x = Points, y = sdcap)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggrepel::geom_text_repel(label = plotdata3$Team) +
  theme_bw()

ggplot(plotdata3, aes(x = capsum, y = sdcap)) +
  geom_point() +
  geom_smooth(method = "lm")

# how do salaries for different position vary ?
# create function to detect outlier so they can be labeled on plot
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

positionsalary <- salaries %>% 
  arrange(desc(CapHit)) %>% 
  head(300) %>% 
  filter(Pos != "D" & !is.na(Pos)) %>% 
  group_by(Pos) %>% 
  mutate(is_outlier = ifelse(is_outlier(CapHit), Player, as.character(NA))) # replace salary with player and numeric to character

positionsalary$is_outlier[which(!is.na(positionsalary$is_outlier))] <- positionsalary$Player[which(!is.na(positionsalary$is_outlier))]

ggplot(positionsalary, aes(x = reorder(Pos, CapHit, FUN = median), y = CapHit, fill = Pos)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.size = 2, outlier.color = "darkgreen") +
  scale_y_continuous(labels = point, breaks = seq(0, 15000000, 2000000), limits = c(0, 11000000)) +
  ggrepel::geom_text_repel(aes(label = is_outlier), col = "darkgreen") +
  labs(title = "Cap hits of top 300 players in NHL (18-19)",
       y = "Cap hit",
       x = "Position",
       caption = "Vertical line in box = Median in group\nBox size = Q2 - Q3\nOutliers > 1.5*IQR + Q3") +
  theme_gdocs() +
  theme(legend.position = "none",
        plot.caption = element_text(color = "darkgreen", size = 12)) +
  scale_fill_manual(values = c("#96FD1A", "#13A0FF", "#13FFC2", "#96FD1A", "#13FFC2", "#96FD1A"))
  

# C, G, LD, LW, RD, RW





