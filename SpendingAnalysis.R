#Import required packages
library(dplyr)
library(ggplot2)

#Import data as data frames
original.project.a <- read.csv("projectA.csv", header = TRUE)
original.project.b <- read.csv("projectB.csv", header = TRUE)
original.project.c <- read.csv("projectC.csv", header = TRUE)
original.project.d <- read.csv("projectD.csv", header = TRUE)
original.project.e <- read.csv("projectE.csv", header = TRUE)

#Collect project data frames in a list
project.list <- list(original.project.a, original.project.b, original.project.c, original.project.d,
                     original.project.e)

#Create normalization function
norm.fun <- function(x){
  y <- transmute(x, prop.time = x$time / max(x$time), prop.spend = x$spend / max(x$spend))
  return(y)
}

#Normalize time and spend variables to proportions of total time and total spending
project.list <- lapply(project.list, norm.fun)

#Label all observations with their respective project names
project.names <- c("A", "B", "C", "D", "E")
for(i in 1:length(project.list))
  project.list[[i]] <- mutate(project.list[[i]], project = project.names[i])

#Create a new combined data frame that includes all projects
project.all <- do.call(rbind, project.list)
project.all <- arrange(project.all, prop.time)

#Perform least squares on a logistic curve
s.curve <- nls(prop.spend ~ max.y / (1 + exp(-steepness * (prop.time - mid.x))), project.all, 
              list(max.y = 1, steepness = 8, mid.x = 0.5), trace=TRUE)

#Print summary of nonlinear least squares estimate
summary(s.curve)

#Set parameters of prediction model
max.y <- coef(s.curve)[1]
steepness <- coef(s.curve)[2]
mid.x <- coef(s.curve)[3]
x <- seq(0, 1, 0.01)
y <- max.y / (1 + exp(-steepness * (x - mid.x)))

#Plot project data
final.chart <- ggplot(data = project.all) +
  geom_point(aes(prop.time, prop.spend, color = project),  size = 3) +
  labs(title = "Project Spending", x = "Proportion of Time", y = "Proportion of Spending") +
  geom_line(data = predict.df, aes(x, y), size = 1)

#Display chart
final.chart

#Create function for predicting new spending curves
prediction.fun <- function(number.months, budget){
  x.spend <- 0:number.months
  y.spend <- max.y * budget / (1 + exp(- (steepness / number.months) * (x.spend - number.months * mid.x)))
  prediction.df <- data.frame(x.spend, y.spend)
  predict.chart <- ggplot(data = prediction.df) +
    labs(title = "Project Spending", x = "Proportion of Time", y = "Proportion of Spending") +
    geom_line(aes(x.spend, y.spend), size = 1, color = "steelblue")
  return(prediction.df)
}
