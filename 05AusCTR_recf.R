######################################################################
# 05AusCTR_recf.R
#
# Reconcile forecasts with the heuristic of Kourentzes &
# Athanasopoulos (2019) and the Optimal Cross-Temporal approach.
#
# Input files: AusGDP_inpdata.RData and Aus_basef.RData
# Output files: AusCTR_recf.RData
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
######################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse", "lubridate", "FoReco")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

# Load data and same tools
load("./Aus_basef.RData")
load("./AusGDP_inpdata.RData")

DF <- DF %>% filter(`F-method`=="ARIMA") %>%
  add_column(reco_mode = "Base") %>%
  mutate(Series = factor(Series, names(AusGDP), ordered = T))

# Highest frequency
m <- 4

# Function to vectorize for DF tibble the output of reconcile function in FOREC
vector_trasf <- function(mat){
  do.call("c",apply(matrix(c(1,2,4,1,3,7),3),1,
                    function(x) as.vector(t(mat[,x[1]:x[2]]))))
}

# Type of reconciliation
ty_hts <- c("ols", "shr", "wls", "w")
ty_thf <- c("bu", "ols", "struc", "wlsv", "wlsh", "acov",
            "strar1", "sar1", "har1", "shr")
# all the possible combination for the heuristic
comb_tcs <- expand.grid(ty_thf,ty_hts)
comb_cst <- expand.grid(ty_thf,ty_hts)

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

# combination for the iterative reconciliation
comb_ite <- comb_tcs

# combination for the optimal cross-temporal reconciliation
ty_ct <- c("ols", "wlsh", "wlsv", "acov",
           "Sshr", "bdshr", "shr", "w")


Ptilde <- commat(nb,7)%*%bdiag(commat(3,nb),commat(4,nb))
Pstar <- bdiag(Diagonal(na*7),Ptilde)
Sstruc <- suppressWarnings(ctf_tools(C=Fstruc[1:na,], m=4,
                                     Sstruc = TRUE)$ctf$Sstruc)
Sstruc <- Pstar%*%Sstruc
Wstruc_oct <- .sparseDiagonal(x=rowSums(Sstruc))

# use the covariance matrix (FALSE) or mse covariance matrix (TRUE)
mse <- TRUE

# What type of reconciliation?
hka <- TRUE     # Heuristic KA
rka <- TRUE     # Heuristic KA reverse
ctr <- TRUE     # Optimal Cross-temporal reconciliation
ite <- TRUE     # Iterative reconciliation thf-hts
ite_cst <- TRUE     # Iterative reconciliation thf-hts

forig <- max(DF$Replication)

# Check convergence
flag <- matrix(NA,forig,NROW(comb_ite))
iter <- matrix(NA,forig,NROW(comb_ite))
flag_cst <- matrix(NA,forig,NROW(comb_ite))
iter_cst <- matrix(NA,forig,NROW(comb_ite))

# Time
time_ite_cst <- matrix(NA,forig,NROW(comb_ite))
time_ite <- matrix(NA,forig,NROW(comb_ite))
time_oct <- matrix(NA,forig,NROW(ty_ct))
time_cst <- matrix(NA,forig,NROW(comb_cst))
time_tcs <- matrix(NA,forig,NROW(comb_tcs))

options(warn=1)
# Starting reconciliation
for(j in 1:forig){

  # Matrix for the base forecasts with origin j
  Yhat <- DF %>% filter(Replication==j, `R-method`=="Base") %>%
    select(Series, Hk, Forecasts, `Forecast Horizon`) %>%
    pivot_wider(names_from = Series, values_from = Forecasts) %>%
    arrange(Hk, `Forecast Horizon`) %>%
    select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()

  # Matrix for the residuals of forecasts with origin j
  resmat <- resmat_ARIMA[[j]]

  # Allocate the matrix for the reconcile forecasts with origin j
  Fltr <- DF %>% filter(`R-method`=="Base", `Replication`==j) %>%
    dplyr::select(-"Forecasts", -"R-method", -"reco_mode") %>% arrange(Hk, Series, `Forecast Horizon`)

  if(hka){
    cat("Heuristic KA combination (mean KA) number (out of ",NROW(comb_tcs),"): \n", sep="")
    for(i in 1:NROW(comb_tcs)){
      Start <- Sys.time()
      Recon_PointF <- tcsrec(basef = Yhat, m = m, Ut = Ut, nb = nb,
                             thf_comb = as.character(comb_tcs[i,1]),
                             hts_comb = as.character(comb_tcs[i,2]),
                             res = resmat, mse = mse, W = Wstruc)$recf
      End <- Sys.time()
      time_tcs[j,i] <- as.numeric(as.period(End - Start, unit = "sec"))
      # Add rows to DF
      if(as.character(comb_tcs[i,2] != "w")){
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_tcs[i,1], comb_tcs[i,2], sep="-"),
                     "reco_mode" = "tcs")
      }else{
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_tcs[i,1], "struc", sep="-"),
                     "reco_mode" = "tcs")
      }

      Df1 <- Df1[names(DF)]
      DF <- rbind(DF, Df1)
      cat(i, " ", sep="")
    }
  }

  if(rka){
    cat("\nHeuristic reverse combination number (out of ",NROW(comb_cst),"): \n", sep="")
    for(i in 1:NROW(comb_cst)){
      Start <- Sys.time()
      Recon_PointF <- cstrec(basef = Yhat, m = m, Ut = Ut, nb = nb,
                             thf_comb = as.character(comb_cst[i,1]),
                             hts_comb = as.character(comb_cst[i,2]),
                             res = resmat, mse = mse, W = Wstruc)$recf
      End <- Sys.time()
      time_cst[j,i] <- as.numeric(as.period(End - Start, unit = "sec"))
      # Add rows to DF
      if(as.character(comb_cst[i,2] != "w")){
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_cst[i,1], comb_cst[i,2], sep="-"),
                     "reco_mode" = "cst")
      }else{
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_cst[i,1], "struc", sep="-"),
                     "reco_mode" = "cst")
      }
      Df1 <- Df1[names(DF)]
      DF <- rbind(DF, Df1)
      cat(i, " ", sep="")
    }
  }

  if(ite){
    cat("\nIterec tcs combination number (out of ",NROW(comb_ite),"): \n", sep="")
    for(i in 1:NROW(comb_ite)){
      Start <- Sys.time()
      obj <- iterec(basef = Yhat, m = m, Ut = Ut, nb = nb,
                    thf_comb = as.character(comb_ite[i,1]),
                    hts_comb = as.character(comb_ite[i,2]),
                    res = resmat, mse = mse, note = F, start_rec="thf", W = Wstruc)
      End <- Sys.time()
      time_ite[j,i] <- as.numeric(as.period(End - Start, unit = "sec"))

      iter[j,i] <- obj$iter
      flag[j,i] <- obj$flag
      Recon_PointF <- obj$recf
      # Add rows to DF
      if(as.character(comb_ite[i,2] != "w")){
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_ite[i,1], comb_ite[i,2], sep="-"),
                     "reco_mode" = "ite")
      }else{
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_ite[i,1], "struc", sep="-"),
                     "reco_mode" = "ite")
      }
      Df1 <- Df1[names(DF)]
      DF <- rbind(DF, Df1)
      cat(i, " ", sep="")
    }
  }

  if(ite_cst){
    cat("\nIterec cst combination number (out of ",NROW(comb_ite),"): \n", sep="")
    for(i in 1:NROW(comb_ite)){
      Start <- Sys.time()
      obj <- iterec(basef = Yhat, m = m, Ut = Ut, nb = nb,
                    thf_comb = as.character(comb_ite[i,1]),
                    hts_comb = as.character(comb_ite[i,2]),
                    res = resmat, mse = mse, note = F, start_rec="hts", W = Wstruc)
      End <- Sys.time()
      time_ite_cst[j,i] <- as.numeric(as.period(End - Start, unit = "sec"))

      iter_cst[j,i] <- obj$iter
      flag_cst[j,i] <- obj$flag
      Recon_PointF <- obj$recf
      # Add rows to DF
      if(as.character(comb_ite[i,2] != "w")){
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_ite[i,1], comb_ite[i,2], sep="-"),
                     "reco_mode" = "ite_cst")
      }else{
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF),
                     "R-method" = paste(comb_ite[i,1], "struc", sep="-"),
                     "reco_mode" = "ite_cst")
      }
      Df1 <- Df1[names(DF)]
      DF <- rbind(DF, Df1)
      cat(i, " ", sep="")
    }
  }

  if(ctr){
    cat("\nOptimal Cross temporal reconciliation number (out of ",NROW(ty_ct),"): \n", sep="")
    for(i in 1:length(ty_ct)){
      Start <- Sys.time()
      Recon_PointF <- octrec(basef = Yhat, m = m, Ut = Ut, nb = nb, comb = ty_ct[i],
                             res = resmat, mse = mse, W=Wstruc_oct)$recf
      End <- Sys.time()
      time_oct[j,i] <- as.numeric(as.period(End - Start, unit = "sec"))
      # Add rows to DF
      if(ty_ct[i]=="w"){
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF), "R-method" = "struc",
                     "reco_mode" = "oct")
      }else{
        Df1 <- cbind(Fltr, "Forecasts" = vector_trasf(Recon_PointF), "R-method" = ty_ct[i],
                     "reco_mode" = "oct")
      }

      Df1 <- Df1[names(DF)]
      DF <- rbind(DF, Df1)
      cat(i, " ", sep="")
    }
  }

  cat("\n", "Forecast origin number ", j, " (out of ", forig, ", ",j/forig*100,"%)\n", sep = "")
}

DF$reco_mode[DF$reco_mode == "tcs"
             & DF$`R-method` %in% c("struc-shr", "struc-wls",
                                    "wlsh-shr", "wlsh-wls",
                                    "wlsv-shr", "wlsv-wls")] <- "kah"

DF_part1 <- DF[1:(NROW(DF)/2),]
DF_part2 <- DF[-c(1:(NROW(DF)/2)),]
save(DF_part1, ty_ct, ty_thf, ty_hts, m, n, n_EXP, n_INC, na, na_EXP, na_INC, nb, nb_EXP, nb_INC, flag,
     iter,
     flag_cst,
     iter_cst,
     time_ite_cst,
     time_ite,
     time_oct,
     time_cst,
     time_tcs,
     file = "./AusCTR_recf_part1.RData")

save(DF_part2, file = "./AusCTR_recf_part2.RData")
#save.image(file = "./AusCTR_R/ALL_recf.RData")
