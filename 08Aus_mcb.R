######################################################################
# 08Aus_mcb.R
#
# Model Comparison with the Best Dataset
#
# Input files: AusCTR_recf.RData, AusHTS_recf.RData, AusTMP_recf.RData
# Output files: Aus_mcb.RData
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
######################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse", "FoReco")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

load("./AusCTR_recf_part1.RData")
load("./AusCTR_recf_part2.RData")
DF <- rbind(DF_part1, DF_part2)
rm(DF_part1, DF_part2)
DF_bse <- DF %>% filter(`R-method`=="Base")
DF_ctr <- DF %>% filter(`R-method`!="Base")

load("./AusHTS_recf.RData")
DF_hts <- DF %>% filter(`R-method`!="Base") %>% add_column(reco_mode = "cs")

load("./AusTMP_recf.RData")
DF_tmp <- DF %>% filter(`R-method`!="Base") %>% add_column(reco_mode = "t")

DF <- rbind(DF_bse, DF_hts, DF_tmp, DF_ctr)

# test_list: list with a test observation matrix for each forecasts origin
# base_list: list with a base forecasts matrix for each forecasts origin
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
  cat(j)
}

# name of total combination
combin <- unique(DF[,c(4,12)])[-1,]
score_al <- list()
score_sl <- list()
score_ah <- list()
score_sh <- list()
for(i in 1:NROW(combin)){
  recf_list <- list()
  DF_par <- DF %>% filter(`R-method`==pull(combin[i,1]), reco_mode == pull(combin[i,2]))
  for(j in 1:max(DF$Replication)){
    recf_list[[j]] <- DF_par %>% filter(Replication==j) %>%
      select(Series, Forecasts, Hk, `Forecast Horizon`) %>%
      pivot_wider(names_from = Series, values_from = Forecasts) %>%
      arrange(Hk, `Forecast Horizon`) %>%
      select(-Hk, -`Forecast Horizon`) %>% as.matrix() %>% t()
  }

  ind_mat <- score_index(recf = recf_list, test = test_list, base = base_list, m = m,
                         nb = nb, type = "mae", compact = F)

  score_al[[i]] <- as_tibble(ind_mat$Avg_ik) %>%
    add_column(reco_mode = pull(combin[i,2]), comb = pull(combin[i,1]), series=rownames(ind_mat$Avg_ik)) %>%
    pivot_longer(c(1,2,3,4))
  score_al[[i]] <- score_al[[i]] %>% add_column(id = 1:nrow(score_al[[i]]))

  score_ah[[i]] <- as_tibble(ind_mat$Rel_mat) %>%
    add_column(reco_mode = pull(combin[i,2]), comb = pull(combin[i,1]), series=rownames(ind_mat$Rel_mat)) %>%
    pivot_longer(c(1:7))
  score_ah[[i]] <- score_ah[[i]] %>% add_column(id = 1:nrow(score_ah[[i]]))

  ind_mat <- score_index(recf = recf_list, test = test_list, base = base_list, m = m,
                          nb = nb, type = "mse", compact = F)

  score_sl[[i]] <- as_tibble(ind_mat$Avg_ik) %>%
    add_column(reco_mode = pull(combin[i,2]), comb = pull(combin[i,1]), series=rownames(ind_mat$Avg_ik)) %>%
    pivot_longer(c(1,2,3,4))
  score_sl[[i]] <- score_sl[[i]] %>% add_column(id = 1:nrow(score_sl[[i]]))

  score_sh[[i]] <- as_tibble(ind_mat$Rel_mat) %>%
    add_column(reco_mode = pull(combin[i,2]), comb = pull(combin[i,1]), series=rownames(ind_mat$Rel_mat)) %>%
    pivot_longer(c(1:7))
  score_sh[[i]] <- score_sh[[i]] %>% add_column(id = 1:nrow(score_sh[[i]]))

  cat("Score combination number ", i, " (out of ", NROW(combin), ", ",i/NROW(combin)*100,"%)\n", sep = "")
}

mae_long <- do.call("rbind",score_al)
mse_long <- do.call("rbind",score_sl)

mae_longH <- do.call("rbind",score_ah)
mse_longH <- do.call("rbind",score_sh)

mae_longH <- mae_longH %>% filter(name!="k4h1")
mse_longH <- mse_longH %>% filter(name!="k4h1")

mae_long$name <- paste("k", mae_long$name, sep="")
mse_long$name <- paste("k", mse_long$name, sep="")

mae_long <- rbind(mae_long, mae_longH)
mae_long <- mae_long %>% rename(k=name) %>%
  mutate(reco_mode = factor(reco_mode, c("cs","t", "oct", "kah", "tcs", "ite", "cst", "ite_cst"), ordered = T),
         series = factor(series, levels(DF$Series), ordered=T)) %>%
  arrange(reco_mode, comb, series, k) %>% select(-id)

mse_long <- rbind(mse_long, mse_longH)
mse_long <- mse_long %>% rename(k=name) %>%
  mutate(reco_mode = factor(reco_mode, c("cs","t", "oct", "kah", "tcs", "ite", "cst", "ite_cst"), ordered = T),
         series = factor(series, levels(DF$Series), ordered=T)) %>%
  arrange(reco_mode, comb, series, k) %>% select(-id)

method <- unique(mse_long[,1:2])

save(mae_long, mse_long, method, file="./Aus_mcb.RData")
