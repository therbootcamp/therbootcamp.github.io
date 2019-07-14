DIR = "~/Dropbox (2.0)/Work/Software/newpage/img/portfolio/tidyverse/"

# 1. Load packages
library(tidyverse) # makes all tidyverse packages available
titanic <- read_csv("https://osf.io/aupb4/download") # loads titanic data

# 2. data transformation and aggregation using dplyr
titanic <- titanic %>%
  mutate(sex = case_when(Sex == "male" ~ "Male",
                         TRUE ~ "Female"),
         age = case_when(Age >= 18 ~ "Adult",
                           Age <  18 ~ "Minor"),
         class = factor(PClass)) %>%
  drop_na() %>%
  rename(gender = sex) %>%
  group_by(class, gender, age) %>%
  summarise(survival = mean(Survived))

# 3. data transformation and aggregation using ggplot2
ggplot(titanic, aes(class, survival, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Class",
       y = "Proportion Survived",
       title = "Titanic Survivors") +
  facet_wrap(~ age) +
  theme_light()

g = ggplot(titanic, aes(class, survival, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Class",
       y = "Proportion Survived",
       title = "Titanic Survivors") +
  facet_wrap(~ age) +
  theme_light()
ggsave(filename = "titanic.png",plot = g,device = 'png',width =6, height = 4,path = DIR,units = "in")


titanic <- read_csv("https://osf.io/aupb4/download") # loads titanic data

# in base R we do things step by step. We start with relabeling Sex
titanic$gender = titanic$Sex
titanic$gender[titanic$gender == "male"] <- "Male"
titanic$gender[titanic$gender == "female"] <- "Female"
titanic$age <- ifelse(titanic$Age >= 18, "Adult", "Child")
titanic$class <- factor(titanic$PClass)
titanic <- aggregate(titanic$Survived,
                     list(class = titanic$class,
                          gender = titanic$gender,
                          age = titanic$age),
                     mean)
names(titanic)[4] <- "survival"

### create plot

png(paste0(DIR, "titanic_base.png"),width=6,height=4,units = "in",res=300)

# set up window for two plots
layout(matrix(c(1,2,3), nrow = 1), width = c(.4,.4,.2))
par(mar = c(5.1, 4.1, 6.1,0))
cols = c(rgb(232, 125, 114, maxColorValue = 255),
         rgb(84, 188, 194, maxColorValue = 255))
barplot(survival ~ gender + class,
        data = subset(titanic, age == "Adult"),
        beside = TRUE, ylim = c(0, 1), ylab = "Proportion Survived",
        xlab = "Class", main = "Titanic Survivors", las = 1,
        cex.lab = 1.5, cex.axis = 1.25, cex.main = 1.8, 
        col = cols, border = NA)
axis(side = 1, at = c(-1, 12))
rect(0.68, 1.01, 9, 1.08, col = "lightgrey", border = NA, xpd = TRUE)
text(5, 1.04, "Adult", xpd = TRUE, cex = 1.6)

# create second barplot
barplot(survival ~ gender + class,
        data = subset(titanic, age == "Child"),
        beside = TRUE, ylim = c(0, 1), ylab = "Proportion Survived",
        xlab = "Class", las = 1, cex.lab = 1.5, cex.axis = 1.25, 
        col = cols, border = NA)
axis(side = 1, at = c(-1, 12))
rect(0.68, 1.01, 9, 1.08, col = "lightgrey", border = NA, xpd = TRUE)
text(5, 1.04, "Minor", xpd = TRUE, cex = 1.6)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
par(mar = c(0, 0, 0, 0))
points(c(-.5, -.5), c(.525, .445), pch = 15, col = cols,
       cex = 4, xpd = TRUE)
text(c(-.25, -.25), c(.525, .445), labels = c("Female", "Male"),
     offset = 0, cex = 1.8, adj = 0)
text(-.55, .65, "Gender", cex = 2, font = 2, adj = 0, offset = 0)

dev.off()


