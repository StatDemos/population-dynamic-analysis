# installing package
library(devtools)
remotes::install_github("tokami/TropFishR")


# load package

library(TropFishR)
library(readr)


lfq2 <- read_csv("iu3.csv")
#View(lfq2)

lfq2$Date <- as.Date(lfq2$Date, format = "%d.%m.%Y")


# Convert raw length measurements to length frequency data
lfq2new <- lfqCreate(data = lfq2, Lname = "Length", Dname = "Date", 
                     Fname = "Frequency")

plot(lfq2new, Fname = "catch")

#############################################################################################

#Growth parameters

# Restructuring of Data

## set seed value for reproducible results
#set.seed(1)

## adjust bin size
lfq_bin2 <- lfqModify(lfq2new, bin_size = 7)

## plot raw and restructured LFQ data
lfq_bin2_res <- lfqRestructure(lfq_bin2, MA = 7, addl.sqrt = FALSE)
#lfq_bin2_res <- lfqRestructure(lfq2new, MA = 7, addl.sqrt = FALSE)
plot(lfq_bin2_res, hist.sc = 0.75)

#opar <- par(mfrow = c(2,1), mar = c(2,5,2,3), oma = c(2,0,0,0))
#plot(lfq_bin2_res, Fname = "catch", date.axis = "modern")
#plot(lfq_bin2_res, Fname = "rcounts", date.axis = "modern")
#par(opar)


# Powell-Wetherall

PW <- powell_wetherall(lfq_bin2_res, catch_columns = 1:23, reg_int = c(1,6)) # select 1 to 6

PW$Linf_est
PW$confidenceInt_Linf



# Asymptotic length (Linf)

plot(lfq_bin2_res, hist.sc = 0.75)

plot(lfq_bin2_res, hist.col = c("white", "black"),
     image.col = c(rep(rgb(1,0.8,0.8),1000), "white", rep(rgb(0.8,0.8,1),1000)),
     ylim = c(40,max(lfq_bin2_res$midLengths+0.5)))

tmp <- lfqFitCurves(lfq_bin2_res, par = list(Linf=85, K=0.5, t_anchor=0.5),
                    draw = TRUE, col=4, lty=2)



# Response surface analysis (RSA)

alba2 <- ELEFAN(
  lfq = lfq_bin2_res ,  MA = 7,
  Linf_range = seq(50, 110, length.out = 30),
  K_range = exp(seq(log(0.1),log(3.5), length.out = 30)),
  method = "cross",
  cross.date = lfq_bin2_res$dates[3],
  cross.midLength = lfq_bin2_res$midLengths[5],
  contour = TRUE, add.values = FALSE,
  hide.progressbar = TRUE # change to 'TRUE' to follow algorithm's progression
)
points(alba2$par["Linf"], alba2$par["K"], pch="*", cex=2, col=2)

unlist(alba2$par)

alba2$Rn_max


plot(alba2)
points(lfq_bin2_res$dates[3], lfq_bin2_res$midLengths[5], pch="*", cex=2, col=2)
