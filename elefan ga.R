# load package

library(TropFishR)
library(readr)
library(readxl)

# convert to LFQ 
lfq <- read_csv("iu2.csv") # Using data arrangement 2

View(lfq)

lfq$DATE <- as.Date(lfq$DATE, format = "%d/%m/%Y")

lfqnew <- lfqCreate(data = lfq, Lname = "Length", Dname = "DATE", Fname = "Frequency")

plot(lfqnew, Fname = "catch")

?lfqCreate

#Growth parameters

## set seed value for reproducible results
set.seed(1)

## adjust bin size
lfq_bin <- lfqModify(lfqnew, bin_size = 5)

## plot raw and restructured LFQ data
ma <- 5
lfq_bin_res <- lfqRestructure(lfq_bin, MA = 5, addl.sqrt = FALSE)


plot(lfq_bin_res, Fname = "catch", date.axis = "modern")
plot(lfq_bin_res, Fname = "rcounts", date.axis = "modern")


################################################################################

#-------------------------------------------------------------------------------
# Biological stock characteristics
# Growth parameters


## coarse estimate of Linf
linf_guess <- max(lfq_bin$midLengths) / 0.95

## lower search space bounds
low_par <- list(Linf = 0.8 * linf_guess,
                K = 0.01,
                t_anchor = 0,
                C = 0,
                ts = 0)

## upper search space bounds
up_par <- list(Linf = 1.2 * linf_guess,
               K = 1,
               t_anchor = 1,
               C = 1,
               ts = 1)


#-------------------------------------------------------------------------------
# Parameter optimization Method 2
## Run ELEFAN with genetic algorithm
res_GA <- ELEFAN_GA(lfq_bin, 
                    MA = ma, 
                    seasonalised = TRUE,
                    maxiter = 50, 
                    addl.sqrt = FALSE,
                    low_par = low_par,
                    up_par = up_par,
                    monitor = FALSE)

## show results
res_GA$par
res_GA$Rn_max


#-------------------------------------------------------------------------------
# The jack knife technique allows to estimate a confidence interval around the parameters of the soVBGF

## list for results
JK <- vector("list", length(lfq_bin$dates))

## loop
for(i in 1:length(lfq_bin$dates)){
  loop_data <- list(dates = lfq_bin$dates[-i],
                    midLengths = lfq_bin$midLengths,
                    catch = lfq_bin$catch[,-i])
  tmp <- ELEFAN_GA(loop_data, 
                   MA = ma, 
                   seasonalised = TRUE,
                   maxiter = 50, 
                   addl.sqrt = FALSE,
                   low_par = low_par,
                   up_par = up_par,
                   monitor = FALSE, 
                   plot = FALSE)
  JK[[i]] <- unlist(c(tmp$par, list(Rn_max=tmp$Rn_max)))
}

## bind list into dataframe
JKres <- do.call(cbind, JK)

## mean
JKmeans <- apply(as.matrix(JKres), MARGIN = 1, FUN = mean)

## confidence intervals
JKconf <- apply(as.matrix(JKres), MARGIN = 1, FUN = function(x) quantile(x, probs=c(0.025,0.975)))
JKconf <- t(JKconf)
colnames(JKconf) <- c("lower","upper")

## show results
JKconf


## plot LFQ and growth curves (Only for ELEFAN GA)
plot(lfq_bin_res, Fname = "rcounts",date.axis = "modern", ylim=c(50,110))

lt <- lfqFitCurves(lfq_bin, 
                   par = list(Linf=112, K=0.6, t_anchor=0.57, C=0.4, ts=0),
                   draw = TRUE, col = "grey", lty = 1, lwd=1.5)

lt <- lfqFitCurves(lfq_bin, 
                   par = res_GA$par,
                   draw = TRUE, col = "darkgreen", lty = 1, lwd=1.5)

## assign estimates to the data list
# For further analysis, we use the outcomes of ELFAN with genetic algorithm 
# by adding them to the Thumbprint Emperor data list.

# Here par is growth parameters as resulting from e.g. ELEFAN
lfq_bin <- lfqModify(lfq_bin, par = res_GA$par)

#-------------------------------------------------------------------------------
# Estimation of natural mortality rate

## estimation of M
Ms <- M_empirical(Linf = lfq_bin$par$Linf, K_l = lfq_bin$par$K, method = "Then_growth")
Ms

## assign M to data set
lfq_bin$par$M <- as.numeric(Ms)

#-------------------------------------------------------------------------------
# Estimation of fishing mortality rates and gear selectivity

## define plus group as largest length class smaller than Linf
plus_group <- lfq_bin$midLengths[max(which(lfq_bin$midLengths < lfq_bin$par$Linf))]

## summarise catch matrix into vector and add plus group
lfq_catch_vec <- lfqModify(lfq_bin, vectorise_catch = TRUE, plus_group = plus_group)

## run catch curve
res_cc <- catchCurve(lfq_catch_vec, catch_columns = c(1,2,3))
res_cc <- catchCurve(lfq_catch_vec, catch_columns = c(1,2,3), reg_int = c(6,10), calc_ogive = TRUE)

## assign estimates to the data list (parameter estimate for fishing mortality rate)
lfq_catch_vec$par$Z <- res_cc$Z
lfq_catch_vec$par$FM <- as.numeric(lfq_catch_vec$par$Z - lfq_catch_vec$par$M)
lfq_catch_vec$par$FM


################################################################################
# Stock status

# Exploitation rate

# The exploitation rate is defined as E = F/Z and relative to the reference level of 0.5 and
# provides a simple indication of the stock status.

lfq_catch_vec$par$E <- lfq_catch_vec$par$FM / lfq_catch_vec$par$Z
lfq_catch_vec$par$E 

# For this data set, the exploitation rate is equal to 0.49 and, thus, does not indicate overfishing.

#-------------------------------------------------------------------------------
# Yield per recruit modelling

## assign length-weight parameters to the data list
lfq_catch_vec$par$a <- 0.015
lfq_catch_vec$par$b <- 3

## assign maturity parameters
lfq_catch_vec$par$Lmat <- 35
lfq_catch_vec$par$wmat <- 5


## list with selectivity parameters
selectivity_list <- list(selecType = "trawl_ogive",
                         L50 = res_cc$L50, L75 = res_cc$L75)
selectivity_list


## Thompson and Bell model with changes in F
TB1 <- predict_mod(lfq_catch_vec, type = "ThompBell",
                   FM_change = seq(0,1.5,0.05),
                   stock_size_1 = 1,
                   curr.E = lfq_catch_vec$par$E,
                   s_list = selectivity_list,
                   plot = FALSE, hide.progressbar = TRUE)

## Thompson and Bell model with changes in F and Lc
TB2 <- predict_mod(lfq_catch_vec, type = "ThompBell",
                   FM_change = seq(0,1.5,0.1),
                   Lc_change = seq(25,50,0.1),
                   stock_size_1 = 1,
                   curr.E = lfq_catch_vec$par$E,
                   curr.Lc = res_cc$L50,
                   s_list = selectivity_list,
                   plot = FALSE, hide.progressbar = TRUE)

## plot results
par(mfrow = c(2,1), mar = c(4,5,2,4.5), oma = c(1,0,0,0))
plot(TB1, mark = TRUE)
mtext("(a)", side = 3, at = -0.1, line = 0.6)
plot(TB2, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
mtext("(b)", side = 3, at = -0.1, line = 0.6)

## Biological reference levels
TB1$df_Es

## Current yield and biomass levels
TB1$currents


















