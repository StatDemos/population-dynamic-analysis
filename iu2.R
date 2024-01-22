# installing package
library(devtools)
remotes::install_github("tokami/TropFishR")


# load package

library(TropFishR)
library(readr)


lfq2 <- read_csv("iu3.csv")
View(lfq2)

lfq2$Date <- as.Date(lfq2$Date, format = "%d.%m.%Y")


lfq2new <- lfqCreate(data = lfq2, Lname = "Length", Dname = "Date", Fname = "Frequency")

plot(lfq2new, Fname = "catch")

#############################################################################################

#Growth parameters

## set seed value for reproducible results
set.seed(1)

## adjust bin size
lfq_bin2 <- lfqModify(lfq2new, bin_size = 2)

## plot raw and restructured LFQ data
ma <- 7
lfq_bin2_res <- lfqRestructure(lfq_bin2, MA = 7, addl.sqrt = FALSE)

opar <- par(mfrow = c(2,1), mar = c(2,5,2,3), oma = c(2,0,0,0))
plot(lfq_bin2_res, Fname = "catch", date.axis = "modern")
plot(lfq_bin2_res, Fname = "rcounts", date.axis = "modern")
par(opar)

