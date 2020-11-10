#########################################################################
# 04AusTMP_scores.R
#
# Accuracy indices for the temporal reconciled forecasts for
# Australian GDP time series by quarter, semester and year.
#
# Input files: AusTMP_recf.RData
# Output files: AusTMP_scores.RData
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
#########################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse","forecast","ggplot2","scales","grid","gridExtra",
          "lubridate", "FoReco")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)
load("./AusTMP_recf.RData")

test_list <- list()
base_list <- list()
for(j in 1:max(DF$Replication)){
  test_list[[j]] <- DF %>% filter(Replication==j, `R-method`=="Base") %>%
    select(Series, Hk, Actual, `Forecast Horizon`) %>%
    pivot_wider(names_from = Series, values_from = Actual) %>%
    arrange(Hk, `Forecast Horizon`) %>%
    select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()

  base_list[[j]] <- DF %>% filter(Replication==j, `R-method`=="Base") %>%
    select(Series, Hk, Forecasts, `Forecast Horizon`) %>%
    pivot_wider(names_from = Series, values_from = Forecasts) %>%
    arrange(Hk, `Forecast Horizon`) %>%
    select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()
  if(j==1) cat("Replication number: ")
  if(j%%15==0 | j==91) cat(j, " ", sep="")
}

# name of total combination
combin <- unique(DF$`R-method`)

#### Score function for "mae"-"weight" ####
score_mae <- list()
score_mse <- list()
for(i in 1:NROW(combin)){
  recf_list <- list()
  DF_par <- DF %>% filter(`R-method`==combin[i])
  for(j in 1:max(DF$Replication)){
    recf_list[[j]] <- DF_par %>% filter(Replication==j) %>%
      select(Series, Forecasts, Hk, `Forecast Horizon`) %>%
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(Hk, `Forecast Horizon`) %>%
      select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()
  }
  ind_mat <- score_index(recf = recf_list, test = test_list,
                         base = base_list, m = 4, nb = nb, type = "mae")
  score_mae[[i]] <-cbind(comb=combin[i],k = rownames(ind_mat), ind_mat[,1:3])
  rownames(score_mae[[i]]) <- NULL

  ind_mat <- score_index(recf = recf_list, test = test_list,
                         base = base_list, m = 4, nb = nb, type = "mse")
  score_mse[[i]] <-cbind(comb=combin[i],k = rownames(ind_mat), ind_mat[,1:3])
  rownames(score_mse[[i]]) <- NULL

  cat("Score combination number ", i, " (out of ", NROW(combin), ", ",
      i/NROW(combin)*100,"%) - (",combin[i],")\n", sep = "")
}
mae <- do.call("rbind",score_mae)
mse <- do.call("rbind",score_mse)

save(mse, mae, file = "./AusTMP_scores.RData")
