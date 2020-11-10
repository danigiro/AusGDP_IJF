###############################################################################################
# 09Aus_mcbPlot.R
#
# Model Comparison with the Best
#
# Input files: Aus_mcb.RData
# Output files: 
#
# To select the series (only quarterly, only semi-annual, only annual, ...),
# write the id in the variable "khid":
#
#  id     series       f.horizon
#  -----------------------------
#  kall   all             all
#  k1     quarterly       1:4
#  k2     semi-annual     1:2
#  k4     annual           1
#  k1h1   quarterly        1
#  k1h2   quarterly        2
#  k1h3   quarterly        3
#  k1h4   quarterly        4
#  k2h1   semi-annual      1
#  k2h2   semi-annual      2
#
# To select the methods to be compared, write the id codes in the variable "meth":
#
#  id       rec         comb |  id       rec         comb |  id       rec         comb
# ------------------------------------------------------------------------------------
#   1        cs          ols |  62       tcs   wlsv-struc | 123       cst      shr-ols
#   2        cs          shr |  63       ite     acov-ols | 124       cst      shr-shr
#   3        cs        struc |  64       ite     acov-shr | 125       cst    shr-struc
#   4        cs          wls |  65       ite   acov-struc | 126       cst      shr-wls
#   5         t         acov |  66       ite     acov-wls | 127       cst   strar1-ols
#   6         t           bu |  67       ite       bu-ols | 128       cst   strar1-shr
#   7         t         har1 |  68       ite       bu-shr | 129       cst strar1-struc
#   8         t          ols |  69       ite     bu-struc | 130       cst   strar1-wls
#   9         t         sar1 |  70       ite       bu-wls | 131       cst    struc-ols
#  10         t          shr |  71       ite     har1-ols | 132       cst    struc-shr
#  11         t       strar1 |  72       ite     har1-shr | 133       cst  struc-struc
#  12         t        struc |  73       ite   har1-struc | 134       cst    struc-wls
#  13         t         wlsh |  74       ite     har1-wls | 135       cst     wlsh-ols
#  14         t         wlsv |  75       ite      ols-ols | 136       cst     wlsh-shr
#  15       oct        BDshr |  76       ite      ols-shr | 137       cst   wlsh-struc
#  16       oct          ols |  77       ite    ols-struc | 138       cst     wlsh-wls
#  17       oct        Sacov |  78       ite      ols-wls | 139       cst     wlsv-ols
#  18       oct          shr |  79       ite     sar1-ols | 140       cst     wlsv-shr
#  19       oct         Sshr |  80       ite     sar1-shr | 141       cst   wlsv-struc
#  20       oct        struc |  81       ite   sar1-struc | 142       cst     wlsv-wls
#  21       oct         wlsh |  82       ite     sar1-wls | 143   ite_cst     acov-ols
#  22       oct         wlsv |  83       ite      shr-ols | 144   ite_cst     acov-shr
#  23       kah    struc-shr |  84       ite      shr-shr | 145   ite_cst   acov-struc
#  24       kah    struc-wls |  85       ite    shr-struc | 146   ite_cst     acov-wls
#  25       kah     wlsh-shr |  86       ite      shr-wls | 147   ite_cst       bu-ols
#  26       kah     wlsh-wls |  87       ite   strar1-ols | 148   ite_cst       bu-shr
#  27       kah     wlsv-shr |  88       ite   strar1-shr | 149   ite_cst     bu-struc
#  28       kah     wlsv-wls |  89       ite strar1-struc | 150   ite_cst       bu-wls
#  29       tcs     acov-ols |  90       ite   strar1-wls | 151   ite_cst     har1-ols
#  30       tcs     acov-shr |  91       ite    struc-ols | 152   ite_cst     har1-shr
#  31       tcs   acov-struc |  92       ite    struc-shr | 153   ite_cst   har1-struc
#  32       tcs     acov-wls |  93       ite  struc-struc | 154   ite_cst     har1-wls
#  33       tcs       bu-ols |  94       ite    struc-wls | 155   ite_cst      ols-ols
#  34       tcs       bu-shr |  95       ite     wlsh-ols | 156   ite_cst      ols-shr
#  35       tcs     bu-struc |  96       ite     wlsh-shr | 157   ite_cst    ols-struc
#  36       tcs       bu-wls |  97       ite   wlsh-struc | 158   ite_cst      ols-wls
#  37       tcs     har1-ols |  98       ite     wlsh-wls | 159   ite_cst     sar1-ols
#  38       tcs     har1-shr |  99       ite     wlsv-ols | 160   ite_cst     sar1-shr
#  39       tcs   har1-struc | 100       ite     wlsv-shr | 161   ite_cst   sar1-struc
#  40       tcs     har1-wls | 101       ite   wlsv-struc | 162   ite_cst     sar1-wls
#  41       tcs      ols-ols | 102       ite     wlsv-wls | 163   ite_cst      shr-ols
#  42       tcs      ols-shr | 103       cst     acov-ols | 164   ite_cst      shr-shr
#  43       tcs    ols-struc | 104       cst     acov-shr | 165   ite_cst    shr-struc
#  44       tcs      ols-wls | 105       cst   acov-struc | 166   ite_cst      shr-wls
#  45       tcs     sar1-ols | 106       cst     acov-wls | 167   ite_cst   strar1-ols
#  46       tcs     sar1-shr | 107       cst       bu-ols | 168   ite_cst   strar1-shr
#  47       tcs   sar1-struc | 108       cst       bu-shr | 169   ite_cst strar1-struc
#  48       tcs     sar1-wls | 109       cst     bu-struc | 170   ite_cst   strar1-wls
#  49       tcs      shr-ols | 110       cst       bu-wls | 171   ite_cst    struc-ols
#  50       tcs      shr-shr | 111       cst     har1-ols | 172   ite_cst    struc-shr
#  51       tcs    shr-struc | 112       cst     har1-shr | 173   ite_cst  struc-struc
#  52       tcs      shr-wls | 113       cst   har1-struc | 174   ite_cst    struc-wls
#  53       tcs   strar1-ols | 114       cst     har1-wls | 175   ite_cst     wlsh-ols
#  54       tcs   strar1-shr | 115       cst      ols-ols | 176   ite_cst     wlsh-shr
#  55       tcs strar1-struc | 116       cst      ols-shr | 177   ite_cst   wlsh-struc
#  56       tcs   strar1-wls | 117       cst    ols-struc | 178   ite_cst     wlsh-wls
#  57       tcs    struc-ols | 118       cst      ols-wls | 179   ite_cst     wlsv-ols
#  58       tcs  struc-struc | 119       cst     sar1-ols | 180   ite_cst     wlsv-shr
#  59       tcs     wlsh-ols | 120       cst     sar1-shr | 181   ite_cst   wlsv-struc
#  60       tcs   wlsh-struc | 121       cst   sar1-struc | 182   ite_cst     wlsv-wls
#  61       tcs     wlsv-ols | 122       cst     sar1-wls | 
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
###############################################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse", "tsutils", "ggplot2", "gridExtra")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

## Plot directory
if(!file.exists("./plotMCB"))
  dir.create("./plotMCB", showWarnings = FALSE)
path <- "./plotMCB"

load("./Aus_mcb.RData")
khid <- c("k4")  # eg: c("k1"), c("k2") ...
meth <- c(2,5,9,14,15,17,22,27,30,46,64,80,100) # eg: c(1,2,11,81), c(1:11), ...

# Check input
khid <- unique(khid[khid %in% c("kall","k1","k1h1","k1h2","k1h3","k1h4","k2","k2h1","k2h2","k4")])
meth <- unique(meth[meth %in% c(1:182)])

#### MSE ####
nemMSE_all <- mse_long %>% filter(k %in% khid) %>% mutate(comb=recode(comb, Sacov = "acov", BDshr = "bdshr")) %>%
  pivot_wider(c(value,series,k), names_from = c(reco_mode,comb), names_sep = "-") %>% 
  select(-c(series,k))
nemMSE_par <- nemMSE_all[,meth] %>% add_column(base=1)
nemMSE_par <- as.matrix(nemMSE_par)

nemMSE_test <- nemenyi(nemMSE_par, conf.level=0.95, plottype = "matrix")
nemMSE_data <- tibble(method=factor(paste(names(nemMSE_test$means), round(nemMSE_test$means,2)),
                                    paste(names(nemMSE_test$means), round(nemMSE_test$means,2)), ordered = T), 
                      mean=nemMSE_test$means, l1=nemMSE_test$means-nemMSE_test$cd/2 , l2=nemMSE_test$means+nemMSE_test$cd/2, 
                      col_p = (nemMSE_test$means-nemMSE_test$cd/2<min(nemMSE_test$means+nemMSE_test$cd/2)),
                      col_g = sapply(strsplit(names(nemMSE_test$means), split="-"), "[[", 1))

outplot <- ggplot(nemMSE_data) + geom_vline(aes(xintercept = l1[1]), col="palegreen3")  + 
  geom_vline(aes(xintercept = l2[1]), col="palegreen3")  + 
  geom_rect(aes(xmin=l1[1], xmax=l2[1], ymin=-Inf, ymax=Inf), fill = "palegreen", alpha = 0.01)+
  geom_segment(aes(y=method, x = l1, xend=l2, yend=method, col=col_g)) + 
  xlab("Mean ranks") + ylab("")+ 
  geom_point(aes(x=mean, y= method, fill=col_p), pch=21)+ 
  scale_fill_manual(values=c("khaki1", "gray44"), breaks = c("FALSE", "TRUE"))+ guides(fill=FALSE)+
  scale_colour_manual(values=c("lightcoral", "darkslateblue","goldenrod4","gray8","orangered4",
                               "steelblue4","darkslategray","darkgreen"))+
  ggtitle("",
    #paste("Nemenyi tests with AvgRelMSE      -      ", khid, sep=""), 
          subtitle = paste("Critical distance = ",round(nemMSE_test$cd,2), "\n","Friedman: ", round(nemMSE_test$fpval, digits = 4), 
                           " (", nemMSE_test$fH, ")", sep=""))+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title  = element_text(size = 10),
        plot.subtitle = element_text(size = 7))

id <- format(Sys.time(), "%d%H%M%S")
ggsave(paste("nemMSE_rank_", khid, "_m",length(meth),"_id_", id, ".pdf", sep=""),
       plot = outplot,
       path = path,
       width=5.5, 
       height=4)

#### MAE ####
nemMAE_all <- mae_long %>% filter(k %in% khid) %>% mutate(comb=recode(comb, Sacov = "acov", BDshr = "bdshr")) %>%
  pivot_wider(c(value,series,k), names_from = c(reco_mode,comb), names_sep = "-") %>% 
  select(-c(series,k))
nemMAE_par <- nemMAE_all[,meth] %>% add_column(base=1)
nemMAE_par <- as.matrix(nemMAE_par)

nemMAE_test <- nemenyi(nemMAE_par, conf.level=0.95, plottype = "matrix")
nemMAE_data <- tibble(method=factor(paste(names(nemMAE_test$means), round(nemMAE_test$means,2)),paste(names(nemMAE_test$means), round(nemMAE_test$means,2)), ordered = T), 
                      mean=nemMAE_test$means, l1=nemMAE_test$means-nemMAE_test$cd/2 , l2=nemMAE_test$means+nemMAE_test$cd/2, 
                      col_p = (nemMAE_test$means-nemMAE_test$cd/2<min(nemMAE_test$means+nemMAE_test$cd/2)),
                      col_g = sapply(strsplit(names(nemMAE_test$means), split="-"), "[[", 1))

outplot1 <- ggplot(nemMAE_data) + geom_vline(aes(xintercept = l1[1]), col="palegreen3")  + 
  geom_vline(aes(xintercept = l2[1]), col="palegreen3")  + 
  geom_rect(aes(xmin=l1[1], xmax=l2[1], ymin=-Inf, ymax=Inf), fill = "palegreen", alpha = 0.01)+
  geom_segment(aes(y=method, x = l1, xend=l2, yend=method, col=col_g)) + 
  xlab("Mean ranks") + ylab("")+ 
  geom_point(aes(x=mean, y= method, fill=col_p), pch=21, stroke =0.3)+ 
  scale_fill_manual(values=c("khaki1", "gray44"), breaks = c("FALSE", "TRUE"))+ guides(fill=FALSE)+
  scale_colour_manual(values=c("lightcoral", "darkslateblue","goldenrod4","gray8","orangered4",
                               "steelblue4","darkslategray","darkgreen"))+
  ggtitle("",
          #paste("Nemenyi tests with AvgRelMAE      -      ", khid, sep=""), 
          subtitle = paste("Critical distance = ",round(nemMAE_test$cd,2), "\n","Friedman: ", round(nemMAE_test$fpval, digits = 4), 
                           " (", nemMAE_test$fH, ")", sep=""))+
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title  = element_text(size = 10),
        plot.subtitle = element_text(size = 7))

ggsave(paste("nemMAE_rank_", khid, "_m",length(meth),"_id_",id, ".pdf", sep=""),
       plot = outplot1,
       path = path,
       width=5.5, 
       height=4)

outplot2 <- arrangeGrob(outplot,outplot1,ncol=2)
ggsave(paste("nemALL_rank_", length(khid), "_m",length(meth),"_id_",id, ".pdf", sep=""),
       plot = outplot2,
       path = path,
       width=11.69, 
       height=8.27)
