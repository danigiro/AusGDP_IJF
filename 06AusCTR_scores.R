#########################################################################
# 06AusCTR_scores.R
#
# Accuracy indices for the cross-temporal reconciled forecasts for
# Australian GDP time series by quarter, semester and year.
#
# Input files: AusCTR_recf_part1.RData, AusCTR_recf_part2.RData
# Output files: AusCTR_scores.RData
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
#########################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)
library(forec)

load("./AusCTR_recf_part1.RData")
load("./AusCTR_recf_part2.RData")
DF <- rbind(DF_part1, DF_part2)
rm(DF_part1, DF_part2)

# test_list: list with a test observation matrix for each f.o.
# base_list: list with a base forecasts matrix for each f.o.
test_list <- list()
base_list <- list()
DF_par <- DF %>% filter(`R-method`=="Base")
for(j in 1:max(DF$Replication)){
  test_list[[j]] <- DF_par %>% filter(Replication==j) %>%
    select(Series, Hk, Actual, `Forecast Horizon`) %>%
    pivot_wider(names_from = Series, values_from = Actual) %>%
    arrange(Hk, `Forecast Horizon`) %>%
    select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()

  base_list[[j]] <- DF_par %>% filter(Replication==j) %>%
    select(Series, Hk, Forecasts, `Forecast Horizon`) %>%
    pivot_wider(names_from = Series, values_from = Forecasts) %>%
    arrange(Hk, `Forecast Horizon`) %>%
    select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()
  cat(j)
}

# name of total combination
combin <- unique(DF[,c(4,12)])

#### Score function for "mae"-"weight" ####
score_mae <- list()
score_mse <- list()
for(i in 1:NROW(combin)){
  recf_list <- list()
  DF_par <- DF %>% filter(`R-method`==pull(combin[i,1]),
                          reco_mode == pull(combin[i,2]))
  for(j in 1:max(DF$Replication)){
    recf_list[[j]] <- DF_par %>% filter(Replication==j) %>%
      select(Series, Forecasts, Hk, `Forecast Horizon`) %>%
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(Hk, `Forecast Horizon`) %>%
      select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()
  }
  ind_mat <- score_index(recf = recf_list, test = test_list,
                         base = base_list, m = m, nb = nb, type = "mae")
  score_mae[[i]] <- cbind(reco_mode = pull(combin[i,2]),
                          comb=pull(combin[i,1]),
                          k = row.names(ind_mat),
                          ind_mat)
  row.names(score_mae[[i]]) <- NULL

  ind_mat <- score_index(recf = recf_list, test = test_list,
                         base = base_list, m = m, nb = nb, type = "mse")
  score_mse[[i]] <- cbind(reco_mode = pull(combin[i,2]),
                          comb = pull(combin[i,1]),
                          k = row.names(ind_mat),
                          ind_mat)
  row.names(score_mse[[i]]) <- NULL

  cat("Score combination number ", i, " (out of ", NROW(combin), ", ",
      i/NROW(combin)*100,"%) - (",pull(combin[i,2]),"-",pull(combin[i,1]),")\n", sep = "")
}

mae <- do.call("rbind",score_mae)
mse <- do.call("rbind",score_mse)

save(mse, mae,  file = "./AusCTR_scores.RData")
