######################################################################
# 03AusTMP_recf.R
#
# Creating an RData file of base and reconciled forecasts for
# each time series in the Australian GDP's system
#
# Input files: Aus_basef.RData
# Output files: AusTMP_recf.RData
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
######################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse", "FoReco")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

load("./Aus_basef.RData")

DF <- DF %>% filter(`F-method`=="ARIMA")

test_length <- as.numeric(max(DF$Replication))
series <- names(AusGDP)
ty_thf <- c("bu","ols", "struc", "wlsv", "wlsh", "acov",
            "strar1", "sar1", "har1", "shr")
m <- 4
Start <- Sys.time()
for (j in 1:test_length) { #test_length
  resmat <- resmat_ARIMA[[j]]
  cat("Variable number (out of ", n,"):\n", sep = "")
  for(i in 1:n){
    basef_vec <- DF %>% filter(Series==series[i], `R-method`=="Base", Replication==j) %>%
      arrange(Hk, `Forecast Horizon`) %>% pull(Forecasts)
    # Allocate the matrix for the reconcile forecasts with origin j
    Fltr <- DF %>% filter(Series==series[i], `R-method`=="Base", `Replication`==j) %>%
      arrange(Hk, `Forecast Horizon`) %>%
      dplyr::select(-"Forecasts", -"R-method")

    for(l in 1:NROW(ty_thf)){
      obj <- thfrec(basef = basef_vec, m = m, comb = ty_thf[l], res = resmat[i,],
                    type = "M")

      Recon_PointF <- obj$recf
      # Add rows to DF
      Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF), "R-method" = ty_thf[l])
      Df1 <- Df1[names(DF)]
      DF <- rbind(DF, Df1)
    }
    if(i%%5==0) cat(i, " ", sep="")
  }
  cat("\nForecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}

End <- Sys.time()
print(End - Start)

save(DF, AusGDP, m, series, Ut, n, n_EXP, n_INC, na, na_EXP, na_INC, nb, nb_EXP, nb_INC,
     file="./AusTMP_recf.RData")
