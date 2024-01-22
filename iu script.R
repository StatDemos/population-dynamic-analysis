# load package

library(TropFishR)
library(readr)
library(readxl)


# convert to LFQ 

library(readr)
lfq1 <- read_csv("iu.csv")

View(lfq1)

lfq1$date <- as.Date(lfq1$date, format = "%d.%m.%Y")

lfq1new <- lfqCreate(data = lfq1, Lname = "length", Dname = "date")

plot(lfq1new, Fname = "catch")

?lfqCreate

#Growth parameters

## set seed value for reproducible results
set.seed(1)

## adjust bin size
lfq_bin2 <- lfqModify(lfq1new, bin_size = 2)

## plot raw and restructured LFQ data
ma <- 7
lfq_bin2_res <- lfqRestructure(lfq_bin2, MA = 7, addl.sqrt = FALSE)

opar <- par(mfrow = c(2,1), mar = c(2,5,2,3), oma = c(2,0,0,0))
plot(lfq_bin2_res, Fname = "catch", date.axis = "modern")
plot(lfq_bin2_res, Fname = "rcounts", date.axis = "modern")
par(opar)

#################################################################################

# ## coarse estimate of Linf
# linf_guess <- max(lfq_bin2$midLengths) / 0.95
# 
# ## lower search space bounds
# low_par <- list(Linf = 0.8 * linf_guess,
#                 K = 0.01,
#                 t_anchor = 0,
#                 C = 0,
#                 ts = 0)
# 
# ## upper search space bounds
# up_par <- list(Linf = 1.2 * linf_guess,
#                K = 1,
#                t_anchor = 1,
#                C = 1,
#                ts = 1)
# 
# ## run ELEFAN with simulated annealing
# res_SA <- ELEFAN_SA(lfq_bin2, SA_time = 60*0.5, SA_temp = 6e5,
#                     MA = ma, seasonalised = TRUE, addl.sqrt = FALSE,
#                     init_par = list(Linf = linf_guess,
#                                     K = 0.5,
#                                     t_anchor = 0.5,
#                                     C=0.5,
#                                     ts = 0.5),
#                     low_par = low_par,
#                     up_par = up_par)
# 
# ## show results
# res_SA$par
# res_SA$Rn_max


######################################################################################

plot(lfq_bin2_res, hist.sc = 0.75)

plot(lfq_bin2_res, hist.col = c("white", "black"),
     image.col = c(rep(rgb(1,0.8,0.8),1000), "white", rep(rgb(0.8,0.8,1),1000)),
     ylim = c(0,max(lfq_bin2_res$midLengths+0.5)))

tmp <- lfqFitCurves(lfq_bin2_res, par = list(Linf=11, K=2.5, t_anchor=0.5),
                    draw = TRUE, col=4, lty=2)

PW <- powell_wetherall(lfq_bin2_res, catch_columns = 1:7, reg_int = c(2,9) )

PW$Linf_est
PW$confidenceInt_Linf


# Response surface analysis (RSA)

alba2 <- ELEFAN(
  lfq = lfq_bin2_res ,  MA = 7,
  Linf_range = seq(7, 20, length.out = 30),
  K_range = exp(seq(log(0.1),log(4), length.out = 30)),
  method = "cross",
  cross.date = lfq_bin2_res$dates[3],
  cross.midLength = lfq_bin2_res$midLengths[5],
  contour = TRUE, add.values = FALSE,
  hide.progressbar = TRUE # change to 'TRUE' to follow algorithm's progression
)
points(alba2$par["Linf"], alba2$par["K"], pch="*", cex=2, col=2)

