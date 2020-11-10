######################################################################
# 07Aus_horizon.R
#
# Focus on performance by forecast horizon
#
# Input files: AusCTR_recf.RData, AusHTS_recf.RData, AusTMP_recf.RData
# Output files: Aus_horizon.RData
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
combin <- unique(DF[,c(4,12)])
#### Score function for "mae"-"weight" ####
mae_all <- matrix(NA, ncol=10,nrow=NROW(combin))
mae_uts <- matrix(NA, ncol=10,nrow=NROW(combin))
mae_bts <- matrix(NA, ncol=10,nrow=NROW(combin))
mse_all <- matrix(NA, ncol=10,nrow=NROW(combin))
mse_uts <- matrix(NA, ncol=10,nrow=NROW(combin))
mse_bts <- matrix(NA, ncol=10,nrow=NROW(combin))
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

  ind_mat <- score_index(recf = recf_list, test = test_list, base = base_list, m = m,
                         nb = nb, type = "mae", compact = F)
  mae_all[i,] <- c(ind_mat$Avg_k[1,4:7], ind_mat$Avg_mat[3,1],
                   ind_mat$Avg_k[1,2:3], ind_mat$Avg_mat[2,1],
                   ind_mat$Avg_mat[1,1], ind_mat$Avg_mat[4,1])
  mae_uts[i,] <- c(ind_mat$Avg_k[2,4:7], ind_mat$Avg_mat[3,2],
                   ind_mat$Avg_k[2,2:3], ind_mat$Avg_mat[2,2],
                   ind_mat$Avg_mat[1,2], ind_mat$Avg_mat[4,2])
  mae_bts[i,] <- c(ind_mat$Avg_k[3,4:7], ind_mat$Avg_mat[3,3],
                   ind_mat$Avg_k[3,2:3], ind_mat$Avg_mat[2,3],
                   ind_mat$Avg_mat[1,3], ind_mat$Avg_mat[4,3])

  ind_mat <- score_index(recf = recf_list, test = test_list, base = base_list, m = m,
                         nb = nb, type = "mse", compact = F)

  mse_all[i,] <- c(ind_mat$Avg_k[1,4:7], ind_mat$Avg_mat[3,1],
                   ind_mat$Avg_k[1,2:3], ind_mat$Avg_mat[2,1],
                   ind_mat$Avg_mat[1,1], ind_mat$Avg_mat[4,1])
  mse_uts[i,] <- c(ind_mat$Avg_k[2,4:7], ind_mat$Avg_mat[3,2],
                   ind_mat$Avg_k[2,2:3], ind_mat$Avg_mat[2,2],
                   ind_mat$Avg_mat[1,2], ind_mat$Avg_mat[4,2])
  mse_bts[i,] <- c(ind_mat$Avg_k[3,4:7], ind_mat$Avg_mat[3,3],
                   ind_mat$Avg_k[3,2:3], ind_mat$Avg_mat[2,3],
                   ind_mat$Avg_mat[1,3], ind_mat$Avg_mat[4,3])

  cat("Score combination number ", i, " (out of ", NROW(combin), ", ",
      i/NROW(combin)*100,"%)\n", sep = "")
}
proc <- paste(pull(combin[,2]), pull(combin[,1]), sep="-")
proc[proc=="Base-Base"] <- "base"
colnames(mse_bts) <- c("k1h1","k1h2","k1h3","k1h4","k1","k2h1","k2h2","k2","k4","all")
mse_bts <- data.frame(proc,mse_bts)

colnames(mse_uts) <- c("k1h1","k1h2","k1h3","k1h4","k1","k2h1","k2h2","k2","k4","all")
mse_uts <- data.frame(proc,mse_uts)

colnames(mse_all) <- c("k1h1","k1h2","k1h3","k1h4","k1","k2h1","k2h2","k2","k4","all")
mse_all <- data.frame(proc,mse_all)

colnames(mae_bts) <- c("k1h1","k1h2","k1h3","k1h4","k1","k2h1","k2h2","k2","k4","all")
mae_bts <- data.frame(proc,mae_bts)

colnames(mae_uts) <- c("k1h1","k1h2","k1h3","k1h4","k1","k2h1","k2h2","k2","k4","all")
mae_uts <- data.frame(proc,mae_uts)

colnames(mae_all) <- c("k1h1","k1h2","k1h3","k1h4","k1","k2h1","k2h2","k2","k4","all")
mae_all <- data.frame(proc,mae_all)

save(mse_bts, mse_uts, mse_all,
     mae_bts, mae_uts, mae_all,
     file = "./Aus_horizon.RData")

# level_meth <- c("cs-ols", "cs-shr", "cs-struc", "cs-wls", "t-acov", "t-bu", "t-har1", "t-ols",
#                 "t-sar1", "t-shr", "t-strar1", "t-struc", "t-wlsh", "t-wlsv", "oct-BDshr",
#                 "oct-ols", "oct-Sacov", "oct-shr", "oct-Sshr", "oct-struc", "oct-wlsh", "oct-wlsv",
#                 "kah-struc-shr", "kah-struc-wls", "kah-wlsh-shr", "kah-wlsh-wls", "kah-wlsv-shr",
#                 "kah-wlsv-wls", "tcs-acov-ols", "tcs-acov-shr", "tcs-acov-struc", "tcs-acov-wls",
#                 "tcs-bu-ols", "tcs-bu-shr", "tcs-bu-struc", "tcs-bu-wls", "tcs-har1-ols",
#                 "tcs-har1-shr", "tcs-har1-struc", "tcs-har1-wls", "tcs-ols-ols", "tcs-ols-shr",
#                 "tcs-ols-struc", "tcs-ols-wls", "tcs-sar1-ols", "tcs-sar1-shr", "tcs-sar1-struc",
#                 "tcs-sar1-wls", "tcs-shr-ols", "tcs-shr-shr", "tcs-shr-struc", "tcs-shr-wls",
#                 "tcs-strar1-ols", "tcs-strar1-shr", "tcs-strar1-struc", "tcs-strar1-wls",
#                 "tcs-struc-ols", "tcs-struc-struc", "tcs-wlsh-ols", "tcs-wlsh-struc", "tcs-wlsv-ols",
#                 "tcs-wlsv-struc", "ite-acov-ols", "ite-acov-shr", "ite-acov-struc", "ite-acov-wls",
#                 "ite-bu-ols", "ite-bu-shr", "ite-bu-struc", "ite-bu-wls", "ite-har1-ols", "ite-har1-shr",
#                 "ite-har1-struc", "ite-har1-wls", "ite-ols-ols", "ite-ols-shr", "ite-ols-struc",
#                 "ite-ols-wls", "ite-sar1-ols", "ite-sar1-shr", "ite-sar1-struc", "ite-sar1-wls",
#                 "ite-shr-ols", "ite-shr-shr", "ite-shr-struc", "ite-shr-wls", "ite-strar1-ols",
#                 "ite-strar1-shr", "ite-strar1-struc", "ite-strar1-wls", "ite-struc-ols", "ite-struc-shr",
#                 "ite-struc-struc", "ite-struc-wls", "ite-wlsh-ols", "ite-wlsh-shr", "ite-wlsh-struc",
#                 "ite-wlsh-wls", "ite-wlsv-ols", "ite-wlsv-shr", "ite-wlsv-struc", "ite-wlsv-wls",
#                 "cst-acov-ols", "cst-acov-shr", "cst-acov-struc", "cst-acov-wls", "cst-bu-ols",
#                 "cst-bu-shr", "cst-bu-struc", "cst-bu-wls", "cst-har1-ols", "cst-har1-shr",
#                 "cst-har1-struc", "cst-har1-wls", "cst-ols-ols", "cst-ols-shr", "cst-ols-struc",
#                 "cst-ols-wls", "cst-sar1-ols", "cst-sar1-shr", "cst-sar1-struc", "cst-sar1-wls",
#                 "cst-shr-ols", "cst-shr-shr", "cst-shr-struc", "cst-shr-wls", "cst-strar1-ols",
#                 "cst-strar1-shr", "cst-strar1-struc", "cst-strar1-wls", "cst-struc-ols", "cst-struc-shr",
#                 "cst-struc-struc", "cst-struc-wls", "cst-wlsh-ols", "cst-wlsh-shr", "cst-wlsh-struc",
#                 "cst-wlsh-wls", "cst-wlsv-ols", "cst-wlsv-shr", "cst-wlsv-struc", "cst-wlsv-wls",
#                 "ite_cst-acov-ols", "ite_cst-acov-shr", "ite_cst-acov-struc", "ite_cst-acov-wls",
#                 "ite_cst-bu-ols", "ite_cst-bu-shr", "ite_cst-bu-struc", "ite_cst-bu-wls", "ite_cst-har1-ols",
#                 "ite_cst-har1-shr", "ite_cst-har1-struc", "ite_cst-har1-wls", "ite_cst-ols-ols",
#                 "ite_cst-ols-shr", "ite_cst-ols-struc", "ite_cst-ols-wls", "ite_cst-sar1-ols",
#                 "ite_cst-sar1-shr", "ite_cst-sar1-struc", "ite_cst-sar1-wls", "ite_cst-shr-ols",
#                 "ite_cst-shr-shr", "ite_cst-shr-struc", "ite_cst-shr-wls", "ite_cst-strar1-ols",
#                 "ite_cst-strar1-shr", "ite_cst-strar1-struc", "ite_cst-strar1-wls", "ite_cst-struc-ols",
#                 "ite_cst-struc-shr", "ite_cst-struc-struc", "ite_cst-struc-wls", "ite_cst-wlsh-ols",
#                 "ite_cst-wlsh-shr", "ite_cst-wlsh-struc", "ite_cst-wlsh-wls", "ite_cst-wlsv-ols",
#                 "ite_cst-wlsv-shr", "ite_cst-wlsv-struc", "ite_cst-wlsv-wls")
#
# library(xtable)
# mae_all%>% mutate(proc=factor(proc, level_meth, ordered = T)) %>% arrange(proc) %>%
#   xtable(digits = 4, caption = "AvgMAE_all", label = "AvgMAE_all", align = "cr|cccccccccc") %>%
#   print(hline.after=c(-1, 0), tabular.environment = "longtable",
#                                floating = FALSE, include.rownames =F,
#                                file = "AvgMAE_all.tex")
# mae_uts%>% mutate(proc=factor(proc, level_meth, ordered = T)) %>% arrange(proc) %>%
#   xtable(digits = 4, caption = "AvgMAE_uts", label = "AvgMAE_uts", align = "cr|cccccccccc") %>%
#   print(hline.after=c(-1, 0), tabular.environment = "longtable",
#         floating = FALSE, include.rownames =F,
#         file = "AvgMAE_uts.tex")
# mae_bts%>% mutate(proc=factor(proc, level_meth, ordered = T)) %>% arrange(proc) %>%
#   xtable(digits = 4, caption = "AvgMAE_bts", label = "AvgMAE_bts", align = "cr|cccccccccc") %>%
#   print(hline.after=c(-1, 0), tabular.environment = "longtable",
#         floating = FALSE, include.rownames =F,
#         file = "AvgMAE_bts.tex")
#
# mse_all%>% mutate(proc=factor(proc, level_meth, ordered = T)) %>% arrange(proc) %>%
#   xtable(digits = 4, caption = "AvgMSE_all", label = "AvgMSE_all", align = "cr|cccccccccc") %>%
#   print(hline.after=c(-1, 0), tabular.environment = "longtable",
#         floating = FALSE, include.rownames =F,
#         file = "AvgMSE_all.tex")
# mse_uts%>% mutate(proc=factor(proc, level_meth, ordered = T)) %>% arrange(proc) %>%
#   xtable(digits = 4, caption = "AvgMSE_uts", label = "AvgMSE_uts", align = "cr|cccccccccc") %>%
#   print(hline.after=c(-1, 0), tabular.environment = "longtable",
#         floating = FALSE, include.rownames =F,
#         file = "AvgMSE_uts.tex")
# mse_bts%>% mutate(proc=factor(proc, level_meth, ordered = T)) %>% arrange(proc) %>%
#   xtable(digits = 4, caption = "AvgMSE_bts", label = "AvgMSE_bts", align = "cr|cccccccccc") %>%
#   print(hline.after=c(-1, 0), tabular.environment = "longtable",
#         floating = FALSE, include.rownames =F,
#         file = "AvgMSE_bts.tex")
#
# select_proc <- c("base","cs-shr","t-wlsv","t-acov","t-sar1","kah-wlsv-shr",
#                  "tcs-acov-shr","tcs-sar1-shr","ite-wlsv-shr","ite-acov-shr",
#                  "ite-sar1-shr","oct-wlsv","oct-BDshr","oct-Sacov")
# rdigit <- 4
# mse_bts %>% filter(proc %in% select_proc) %>%
#   mutate(proc=factor(proc, select_proc, ordered = T)) %>% arrange(proc) %>%
#   mutate_if(is.numeric, function(x) ifelse(x ==min(x), paste("\\textbf{\\textcolor{red}{",
#                                                              round(x, rdigit), "}}", sep=""),
#                                            round(x, rdigit))) %>%
#   xtable::xtable(digits=rdigit) %>% print(include.rownames=FALSE,
#                                           sanitize.text.function = identity)
#
# mse_uts %>% filter(proc %in% select_proc) %>%
#   mutate(proc=factor(proc, select_proc, ordered = T)) %>% arrange(proc) %>%
#   mutate_if(is.numeric, function(x) ifelse(x ==min(x), paste("\\textbf{\\textcolor{red}{",
#                                                              round(x, rdigit), "}}", sep=""),
#                                            round(x, rdigit))) %>%
#   xtable::xtable(digits=rdigit) %>% print(include.rownames=FALSE,
#                                           sanitize.text.function = identity)
#
# mse_all %>% filter(proc %in% select_proc) %>%
#   mutate(proc=factor(proc, select_proc, ordered = T)) %>% arrange(proc) %>%
#   mutate_if(is.numeric, function(x) ifelse(x ==min(x), paste("\\textbf{\\textcolor{red}{",
#                                                              round(x, rdigit), "}}", sep=""),
#                                            round(x, rdigit))) %>%
#   xtable::xtable(digits=rdigit) %>% print(include.rownames=FALSE,
#                                           sanitize.text.function = identity)
#
# mae_bts %>% filter(proc %in% select_proc) %>%
#   mutate(proc=factor(proc, select_proc, ordered = T)) %>% arrange(proc) %>%
#   mutate_if(is.numeric, function(x) ifelse(x ==min(x), paste("\\textbf{\\textcolor{red}{",
#                                                              round(x, rdigit), "}}", sep=""),
#                                            round(x, rdigit))) %>%
#   xtable::xtable(digits=rdigit) %>% print(include.rownames=FALSE,
#                                           sanitize.text.function = identity)
#
# mae_uts %>% filter(proc %in% select_proc) %>%
#   mutate(proc=factor(proc, select_proc, ordered = T)) %>% arrange(proc) %>%
#   mutate_if(is.numeric, function(x) ifelse(x ==min(x), paste("\\textbf{\\textcolor{red}{",
#                                                              round(x, rdigit), "}}", sep=""),
#                                            round(x, rdigit))) %>%
#   xtable::xtable(digits=rdigit) %>% print(include.rownames=FALSE,
#                                           sanitize.text.function = identity)
#
# mae_all %>% filter(proc %in% select_proc) %>%
#   mutate(proc=factor(proc, select_proc, ordered = T)) %>% arrange(proc) %>%
#   mutate_if(is.numeric, function(x) ifelse(x ==min(x), paste("\\textbf{\\textcolor{red}{",
#                                                              round(x, rdigit), "}}", sep=""),
#                                            round(x, rdigit))) %>%
#   xtable::xtable(digits=rdigit) %>% print(include.rownames=FALSE,
#                                           sanitize.text.function = identity)

