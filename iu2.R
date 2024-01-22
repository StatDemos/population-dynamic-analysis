# load package

library(TropFishR)
library(readr)


lfq2 <- read_csv("C:/Users/STAT/Downloads/iu2.csv")
View(lfq2)

lfq2$DATE <- as.Date(lfq2$DATE, format = "%d/%m/%y")

lfq2new <- lfqCreate(data = lfq2, Lname = "Length", Dname = "DATE", Fname = "Frequency")

plot(lfq2new, Fname = "catch")


