######################################################################
# 01AusHTS_recf.R
#
# Creating an RData file of base and cross-sectional reconciled
# forecasts for the Australian GDP's system
#
# Input files: Aus_basef.RData
# Output files: AusHTS_recf.RData
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
#resmat_ARIIMA

test_length <- as.numeric(max(DF$Replication))
first_train_length <- as.numeric(min(DF$`Training window_length`))-1
series <- names(AusGDP)
ty_hts <- c("ols","wls","shr", "w")

# Structural rappresentation
CI <- matrix(c(1,1,1,1,1,1,1,1,1,1,
               1,1,1,1,1,1,1,1,0,0,
               1,1,1,1,1,0,0,0,0,0,
               0,0,0,0,0,0,1,1,0,0,
               1,1,1,0,0,0,0,0,0,0,
               1,1,0,0,0,0,0,0,0,0), nrow=nb_INC) %>% t
CE <- read.csv("./S_mat.csv")[,-1] %>% as.matrix()
CE <- CE[1:na_EXP,]
Fstruc <- rbind(c(rep(0,10), rep(1,53)),
                cbind(53*CI[-1,]/10, matrix(0,5,53)),
                cbind(matrix(0,26,10), CE[-1,]),
                cbind(53*diag(1,10), matrix(0,10,53)),
                cbind(matrix(0,53,10), diag(1,53)))
Wstruc <- .sparseDiagonal(x=rowSums(Fstruc))

Start <- Sys.time()
K_u <- rev(unique(DF$Hk))
for (j in 1:test_length) { #test_length
  hres <- ncol(resmat_ARIMA[[j]])/sum(K_u)
  pos <- rep(K_u, K_u*hres)
  for(i in 1:length(K_u)){
    resmat <- t(resmat_ARIMA[[j]][,pos==K_u[i]])
    basef <- DF %>% filter(Replication==j, `R-method`=="Base", Hk==K_u[i]) %>% pull(Forecasts) %>%
      matrix(ncol = 95)
    # Allocate the matrix for the reconcile forecasts with origin j
    Fltr <- DF %>% filter(Replication==j, Hk==K_u[i], `R-method`=="Base", `Replication`==j) %>%
      dplyr::select(-"Forecasts", -"R-method")
    for(l in 1:NROW(ty_hts)){
      obj <- htsrec(basef = basef, Ut = Ut, nb = nb, comb = ty_hts[l], res = resmat,
                    type = "M", W = Wstruc)

      Recon_PointF <- obj$recf
      # Add rows to DF
      Df1 <- cbind(Fltr, "Forecasts" = as.vector(Recon_PointF), "R-method" = ty_hts[l])
      Df1 <- Df1[names(DF)]
      DF <- rbind(DF, Df1)
      #cat(l)
    }
    # cat("\nAggregate order number ", i, " (out of ", length(K_u), ")\n", sep = "")

  }

  cat("Forecast origin number ", j, " (out of ", test_length, ", ",j/test_length*100,"%)\n", sep = "")
}
End <- Sys.time()
print(End - Start)
DF$`R-method`[which(DF$`R-method`=="w")] <- "struc"
save(DF, AusGDP, series, Ut, n, n_EXP, n_INC, na, na_EXP, na_INC, nb, nb_EXP, nb_INC,
     file="./AusHTS_recf.RData")
