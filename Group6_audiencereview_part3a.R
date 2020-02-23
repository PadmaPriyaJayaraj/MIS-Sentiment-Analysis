install.packages("dplyr")
library(dplyr)
library(ggplot2)
box <- read.csv("boxoffice_final.csv", header = TRUE)
tickets <- read.csv("tickets_final.csv", header = TRUE)
colnames(box)[1] <- "Date"
colnames(box)[4] <- "Total Gross"
colnames(box)[5] <- "Change"
library(ggplot2)
# Basic scatter plot

data<-lm(formula = box$Week~box$Gross)

options(scipen=999)
theme_set(theme_classic())

gg <- ggplot(box, aes(x=Week, y=Gross, color=Change)) +
  geom_smooth(method="loess", se=F) + 
  geom_point(aes(col=Change)) +
  labs(subtitle="Gross Earnings Vs Week", 
       y="Week", 
       x="Gross earnings per week", 
       title="Scatterplot", 
       caption = "Box Office Sales")+geom_point()

plot(gg)


#VideoTapes of Harry Potter Part 6 sold in 10 weeks
colnames(tickets)[6] <- "Sold.in.week"
colnames(tickets)[7] <- "Total"
colnames(tickets)[8] <- "Week"
data<-lm(formula = tickets$Sold.in.week~tickets$Week)

gg2 <- ggplot(tickets, aes(x=Week, y=Sold.in.week)) + 
  geom_bar(stat='identity', aes(fill=Sold.in.week), width=.5)  +
  labs(subtitle="Video Tapes Sold", 
       x ="Sold in the Week",
       y = "Earnings",
       title= "Harry Potter 6")
plot(gg2)
