##### import ####
library(readr)
library(lubridate)
library(tidyverse)
library(dunn.test)

dat <- read_csv("transport_times_longerer.csv")
colnames(dat) <- make.names(colnames(dat))

## change hour:min format to minutes for certain columns
make_min <- function(x) {
  hour(x)*60 + minute(x)
}

cols <- c("First.Begun.Diff", "Ended.Last.Diff", "Scan.Time",
          "Exam.Length", "Time.Last.Picture.and.Next.Exam.First.Picture", "Reporting.Finding")
new <- sapply(dat[cols], make_min) # apply function only to specified columns
dat <- dat %>%                     
  select(-all_of(cols)) %>%        # remove old columns and bind new ones
  cbind(new) %>%
  mutate(Tech = recode(Tech,       # anonymize tech names
                       'DB' = 'A',
                       'NP' = 'B', 
                       'SH' = 'C', 
                       'KLO' = 'D', 
                       'JL' = 'E', 
                       'JK' = 'F'),
         Portable = ifelse(CCU == 1 | Non.CCU.Portable == 1, 1, 0)) %>%
  rename(Time.Between = Time.Last.Picture.and.Next.Exam.First.Picture)

#### ggplot stuff ####

theme_np <- function() {
  theme(
    line = element_line(color = "black"),
    rect = element_rect(fill = "#FFFFFF",
                        linetype = 0, colour = NA),
    text = element_text(color = "#333333"),
    #axis.title = element_blank(),
    axis.text = element_text(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text.x=element_text(size=rel(1.3)),
    axis.text.y=element_text(size=rel(1.3)),
    legend.background = element_rect(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    panel.background = element_blank(),
    panel.grid = element_line(colour = NULL),
    panel.grid.major = element_line(colour = "#BFBFBF"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = rel(1.6), face = "bold"),
    #plot.subtitle = element_text(hjust = 0, size = rel(0.7)),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.background = element_rect())
}

theme_np2 <- function() {
  theme(
    line = element_line(color = "black"),
    rect = element_rect(fill = "#FFFFFF",
                        linetype = 0, colour = NA),
    text = element_text(color = "#333333"),
    axis.title = element_text(size = 14),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text.x=element_text(size = 16),
    axis.text.y=element_text(size = 14),
    legend.background = element_rect(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    panel.background = element_blank(),
    panel.grid = element_line(colour = NULL),
    panel.grid.major = element_line(colour = "#BFBFBF"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = 26, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 18),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.background = element_rect())
}

color_ramp <- colorRampPalette(c("#f4ba55", "#bc2354"))

#### scan time by sonographer ####
dat %>% ggplot() +
  geom_boxplot(aes(y = Scan.Time, x = Tech)) +
  geom_boxplot(data = dat, aes(y = Scan.Time, x = ""), fill = "grey", alpha = 0.5) +
  labs(title = "Boxplot of Scan Time by Tech",
       subtitle = "All exam types, with and without Definity",
       x = "Tech",
       y = "Scan Time") +
  scale_x_discrete(labels = c("Overall", sort(unique(dat$Tech))))

## anova
anova <- aov(Scan.Time ~ Tech, data = dat)
summary(anova)
# tukey hsd post hoc test
posthoc <- TukeyHSD(anova)
plot(posthoc)

## kruskal-wallis
kruskal.test(Scan.Time ~ Tech, data = dat)
# dunn's test
dunn <- dunn.test(dat$Scan.Time, dat$Tech, method = "holm")

#### reporting and finding by sonographer ####
dat %>% ggplot() +
  geom_boxplot(aes(y = Reporting.Finding.Lunch.Adjusted, x = Tech, fill = Tech)) +
  geom_boxplot(data = dat, aes(y = Reporting.Finding.Lunch.Adjusted, x = ""), fill = "grey", alpha = 0.8) +
  labs(title = "Reporting and Finding Times by Sonographer",
       subtitle = "Time spent on preliminary reporting and finding the next patient",
       x = "Tech",
       y = "Reporting + Finding Time") +
  scale_x_discrete(labels = c("Overall", sort(unique(dat$Tech)))) + 
  scale_fill_manual(values = color_ramp(6)) +
  guides(fill = "none") +
  theme_np()


#### scan time by exam type ####
dat %>% 
  ggplot() +
  geom_boxplot(aes(y = Scan.Time, x = Exam.Type, fill = Exam.Type)) +
  labs(title = "Scanning Time",
       subtitle = "Definity doesn't add much time to a complete echo",
       x = "Exam Type", y = "Scan Time (minutes)") +
  scale_fill_manual(values = rev(color_ramp(6))) +
  guides(fill = "none") +
  theme_np2()

kruskal.test(Scan.Time ~ Exam.Type, data = dat)

#### exam length by exam type ####
dat %>% ggplot() +
  geom_boxplot(aes(y = Exam.Length, x = Exam.Type))

#### scan time by portable ####
dat %>% 
  ggplot(aes(y = Scan.Time, x = Portable, group = Portable)) +
  geom_boxplot()

kruskal.test(Scan.Time ~ Portable, data = dat)

#### estimate lunch time based on time between ####
summary_means <- dat %>%
  summarise(tb_mean = mean(Time.Between, na.rm = T),
            multi_port_mean = mean(Time.Between[More.Than.1.Portable == 1], na.rm = T),
            one_port_mean = mean(Time.Between[More.Than.1.Portable == 0], na.rm = T),
            tb_lunch_mean = mean(Time.Between[Lunch.Between == 1], na.rm = T),
            tb_nolunch_mean = mean(Time.Between[Lunch.Between == 0], na.rm = T),
            tb_lunch_med = median(Time.Between[Lunch.Between == 1], na.rm = T),
            tb_nolunch_med = median(Time.Between[Lunch.Between == 0], na.rm = T))
lunch_tmed <- as.numeric(summary_means[6] - summary_means[7])

dat <- dat %>%
  mutate(Time.Between.Lunch.Adjusted = ifelse(Lunch.Between == 1, Time.Between - lunch_tmed, Time.Between),
         Reporting.Finding.Lunch.Adjusted = ifelse(Lunch.Between == 1, 
                                                   ifelse(Reporting.Finding - lunch_tmed <= 0, 0, Reporting.Finding - lunch_tmed), 
                                                   Reporting.Finding))
#### time between, portable exams ####

# multi port vs one port vs no port

dat %>%
  mutate(p = ifelse(dat$Portable == 1, 1, 0) + ifelse(dat$More.Than.1.Portable == 1, 1, 0)) %>%
  filter(Exam.Type %in% c("TTE", "TTE D", "LTD", "LTD D")) %>%
  ggplot(aes(y = Time.Between.Lunch.Adjusted, x = factor(p), group = p, fill = factor(p))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Not Portable", "Only One Portable", "More Than One Portable")) +
  labs(title = "Time Between Exams",
       subtitle = "Going portable reduces time between exams, especially if you take \nmore than one at a time",
       y = "Time Between Exams (minutes)", x = "") +
  scale_fill_manual(values = color_ramp(3)) +
  guides(fill = "none") +
  theme_np2()


vec <- ifelse(dat$Portable == 1, 1, 0) + ifelse(dat$More.Than.1.Portable == 1, 1, 0)
dat2 <- cbind(dat, vec)

kruskal.test(Time.Between.Lunch.Adjusted ~ vec, 
             data = dat2, 
             subset = Exam.Type %in% c("TTE", "TTE D", "LTD", "LTD D"))

dunn.test(dat2$Time.Between.Lunch.Adjusted, dat2$vec, method = "holm")

dat2 %>% 
  group_by(vec) %>%
  summarise(mean = mean(Time.Between.Lunch.Adjusted, na.rm = T))

#### sonographer table ####
table <- table(dat$Exam.Type, dat$Tech)
table <- cbind(table, total = rowSums(table))
table <- rbind(table, total = colSums(table))
write.table(table, file = "sono_tabs.txt", sep = ",")

vars <- c("First.Begun.Diff", "Ended.Last.Diff", "Scan.Time",
                  "Exam.Length", "Time.Between.Lunch.Adjusted", "Reporting.Finding.Lunch.Adjusted") 
all_sum <- dat %>%
  summarise(across(all_of(vars), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(),
               names_to = c(".value", "variable"),
               names_pattern = "(.*)_(mean|sd)")

names(all_sum)

#### stacked bar chart by sonographer ####
dat %>%
  select(First.Begun.Diff, Ended.Last.Diff, Scan.Time, Reporting.Finding.Lunch.Adjusted, Tech, Date) %>%
  mutate(id = row_number()) %>%
  pivot_longer(!c(Tech, id, Date), names_to = "Variable", values_to = "Value") %>%
  group_by(Tech, Variable) %>%
  summarise(mean = mean(Value, na.rm = T)) %>%
  ggplot(aes(x = Tech, y = mean, fill = factor(Variable, 
                                               levels = c("First.Begun.Diff", 
                                                          "Scan.Time",
                                                          "Ended.Last.Diff", 
                                                          "Reporting.Finding.Lunch.Adjusted"),
                                               labels = c("Transport To",
                                                          "Scan Time",
                                                          "Transport From",
                                                          "Reporting and Finding New Patient"))))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_ramp(4)) +
  labs(title = "Average Time For Each Stage of an Echo",
       subtitle = "Slower scanners can make up for lost time in the reporting phase... \nand vice versa",
       x = "Tech", y = "Time (minutes)") +
  theme_np2() +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 14))

#### are more echoes done when transport is fast? ####
dat %>%
  select(Date, Tech, First.Begun.Diff) %>%
  group_by(Date) %>%
  summarise(Avg.Time = mean(First.Begun.Diff, na.rm = T),
            Count = n()) %>%
  ggplot(aes(Avg.Time, Count)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_np()

# linear model
quota_sum <- dat %>%
  select(Date, Tech, First.Begun.Diff) %>%
  group_by(Date, Tech) %>%
  summarise(Avg.Time = mean(First.Begun.Diff, na.rm = T),
            Count = n()) %>%
  mutate(Transport = ifelse(Avg.Time < 36.5, "Fast", "Slow"),
         Quota = ifelse(Tech %in% c("A", "D"), 4, 5),
         Ratio = Count / Quota,
         Quota.Met = ifelse(Ratio >= 1, "Quota Met", "Quota Unmet"))

summary(lm(Ratio ~ Avg.Time, quota_sum))

table(quota_sum$Quota.Met, quota_sum$Transport)

quota_sum %>%
  ggplot(aes(y = Ratio, x = Avg.Time)) + 
  #geom_point(size = 1.8) + 
  geom_jitter(height = 0.01, width = 0.01) +
  geom_smooth(method = lm, color = color_ramp(6)[6], fill = color_ramp(6)[4]) + 
  labs(title = "No Relationship",
       subtitle = "Ratio of exams performed vs. expected against average transport time \n95% Confidence band shown in pink",
       x = "Average Daily Transport Time (minutes)", y = "Performed/Expected Ratio") +
  theme_np2()
