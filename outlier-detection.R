
# Load packages
library(tidyverse)
library(mvoutlier)
library(outliers)
library(ggplot2)
library(skimr)


### Question 1
# Import data and select variables
chdb <- read_csv("chdb.csv")
chdb <- select(chdb,
               cs = city.state, pollution, binge, cp = child.poverty, inequality,
               le = life.expectancy, obesity, overdose, parks, smoking, walkability)

# Turn off scientific notation
options(scipen = 999)

# Pop-out window
windows()

# Boxplot for binge
boxplot(chdb$binge, main = "Binge")

# Find numerical value for outliers
skim(chdb$binge)
IQR(chdb$binge) * 1.5 + 19.4 
16.4 - (IQR(chdb$binge) * 1.5)
# The outliers are < 11.825 or > 23.975 
view(select(chdb, cs, binge))

# Boxplot for smoking
boxplot(chdb$smoking, main = "Smoking")

# Find numerical value for outliers
skim(chdb$smoking)
IQR(chdb$smoking) * 1.5 + 19.7
14.2 - (IQR(chdb$smoking) * 1.5)
# The outliers are < 5.95 or > 27.95
view(select(chdb, cs, smoking))


### Question 2
# Remove character variable
chdb_num <- select(chdb, -cs)
outlier(chdb_num)


## Question 3
chisq.out.test(chdb$binge, variance = var(chdb$binge))
chisq.out.test(chdb$smoking, variance = var(chdb$smoking))


## Question 4
# Convert the values to z-scores
zchdb <- scores(chdb_num, type = c("z"), prob = NA, lim = NA)

# Uni Plot
windows()
uni <- as.data.frame(uni.plot(zchdb, symb = TRUE))

# Mahalanobis Statistic
md <- mahalanobis(zchdb, colMeans(zchdb), cov (zchdb))
zchdb$md <- round(md, 3)
# Add city names
zchdb$cs <- chdb$cs

view(zchdb)

# GGPlot to visualize two outlier cities
ggplot(zchdb, aes(x = binge, y = md, label = cs)) +
  geom_point() +

  geom_text(aes(label = cs), check_overlap = TRUE)

ggplot(zchdb, aes(x = smoking, y = md, label = cs)) +
  geom_point() +
  geom_smooth() +
  geom_text(aes(label = cs), check_overlap = TRUE)


