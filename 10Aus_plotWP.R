######################################################################
# 10Aus_plotWP.R
#
# Plots of the working paper
#
# Input files: AusCTR_scores.RData, AusHTS_scores.RData and AusTMP_scores.RData
# Output files:
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
######################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse","gridExtra","grid","ggplot2","gtable")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

load("./AusHTS_scores.RData")
mae_hts <- mae %>% add_column(reco_mode="cs", .before=1) %>%filter(comb!="Base")
mse_hts <- mse %>% add_column(reco_mode="cs", .before=1) %>%filter(comb!="Base")

load("./AusTMP_scores.RData")
mae_thf <- mae %>% add_column(reco_mode="t", .before=1) %>%filter(comb!="Base")
mse_thf <- mse %>% add_column(reco_mode="t", .before=1) %>%filter(comb!="Base")

load("./AusCTR_scores.RData")

mae <- rbind(mae, mae_hts, mae_thf)
mse <- rbind(mse, mse_hts, mse_thf)

## Plot directory
if(!file.exists("./plotWP"))
  dir.create("./plotWP", showWarnings = FALSE)
path <- "./plotWP"

mae$comb <- recode(mae$comb, Sacov = "acov", BDshr = "bdshr")
mse$comb <- recode(mse$comb, Sacov = "acov", BDshr = "bdshr")

rec_names <- c(
  `kah` = "Heuristic KA",
  `tcs` = "Heuristic KA Alternatives",
  `cst` = "Reverse Heuristic KA Alternatives",
  `cs` = "Cross-sectional",
  `t` = "Temporal",
  `oct` = "Optimal Cross-temporal",
  `ite` = "Iterative",
  `ite_cst` = "Reverse Iterative",
  `4` = "4",
  `2` = "2",
  `1` = "1",
  `all` = "all")

# Function to take the lengend
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

# OPT - Temporal & Cross-sectional ----
# Ranking AvgRelMAE----
table_mae <- mae %>% filter(reco_mode %in% c("cs", "t", "Base")) %>%
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% arrange(all_all,bts_all,uts_all)
score_mae <- table_mae %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))

name_xmae <- paste(score_mae$reco_mode, score_mae$comb)
name_xmae[name_xmae=="Base Base"] = "base"
rp <- score_mae %>% add_column(x=factor(name_xmae, rev(name_xmae), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mae <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4,"mean")),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts","mean")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mae <- ggplotGrob(core_rank_mae)
#gtable_show_layout(rank_mae)
rank_mae <- gtable_add_rows(rank_mae, rank_mae$heights[6], pos = 5) 
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

ggsave("Cross_sectional_temporal_Rank_AvgRelMAE.pdf",
       plot = rank_mae,
       path = path,
       width=5, 
       height=3)

# Plot AvgRelMAE value
num_mae <- as.numeric(factor(paste(mae$reco_mode, mae$comb)))+100
mae_dataplot <- mae %>% mutate(comb = paste(comb,num_mae, sep="-"),
                                k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  filter(reco_mode %in% c("cs", "t")) %>%
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

ord <- mae_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mae_core <- mae_dataplot %>%
  mutate(comb=factor(comb, rev(ord), ordered = T)) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  scale_x_discrete(labels = lab_prova)+
  facet_grid(c("k", "reco_mode"), labeller = as_labeller(rec_names), scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
# gtable_show_layout(mae_plot)
mae_plot <- ggplotGrob(mae_core+theme(legend.position = "none"))
mae_plot <- gtable_add_cols(mae_plot, mae_plot$widths[8], pos = 8) 
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=8, l=9, b=12, r=9, name = c("a", "b"))
mae_plot <- gtable_add_cols(mae_plot, unit(2/10, "line"), pos = 8)
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=14, l=8, b=14, r=10, name = c("a", "b"))
outplot <- arrangeGrob(mae_plot,g_legend(mae_core), nrow=2,heights=c(5, 0.5), 
                       left = textGrob("AvgRelMAE",rot = 90, vjust = 1, hjust = 0.25))

ggsave("Cross_sectional_temporal_AvgMAE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=7)

# Ranking AvgRelMSE ----
table_mse <- mse %>% filter(reco_mode %in% c("cs", "t", "Base")) %>%
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% arrange(all_all,bts_all,uts_all)
score_mse <- table_mse %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))

name_xmse <- paste(score_mse$reco_mode, score_mse$comb)
name_xmse[name_xmse=="Base Base"] = "base"
rp <- score_mse %>% add_column(x=factor(name_xmse, rev(name_xmse), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mse <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4,"mean")),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts","mean")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mse <- ggplotGrob(core_rank_mse)
#gtable_show_layout(rank_mse)
rank_mse <- gtable_add_rows(rank_mse, rank_mse$heights[6], pos = 5) 
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

ggsave("Cross_sectional_temporal_Rank_AvgRelMSE.pdf",
       plot = rank_mse,
       path = path,
       width=5, 
       height=3)

# Plot AvgRelMSE value
num_mse <- as.numeric(factor(paste(mse$reco_mode, mse$comb)))+100
mse_dataplot <- mse %>% mutate(comb = paste(comb,num_mse, sep="-"),
                               k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  filter(reco_mode %in% c("cs", "t")) %>%
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

ord <- mse_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mse_core <- mse_dataplot %>%
  mutate(comb=factor(comb, rev(ord), ordered = T)) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  scale_x_discrete(labels = lab_prova)+
  facet_grid(c("k", "reco_mode"), labeller = as_labeller(rec_names), scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
# gtable_show_layout(mse_plot)
mse_plot <- ggplotGrob(mse_core+theme(legend.position = "none"))
mse_plot <- gtable_add_cols(mse_plot, mse_plot$widths[8], pos = 8) 
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=8, l=9, b=12, r=9, name = c("a", "b"))
mse_plot <- gtable_add_cols(mse_plot, unit(2/10, "line"), pos = 8)
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=14, l=8, b=14, r=10, name = c("a", "b"))
outplot <- arrangeGrob(mse_plot,g_legend(mse_core), nrow=2,heights=c(5, 0.5), 
                       left = textGrob("AvgRelMSE",rot = 90, vjust = 1, hjust = 0.25))

ggsave("Cross_sectional_temporal_AvgMSE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=7)

# OCT - Optimal Cross-temporal ----
# Ranking AvgRelMAE ----
table_mae <- mae %>% filter(reco_mode %in% c("oct", "Base")) %>% 
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% 
  arrange(all_all,bts_all,uts_all)
score_mae <- table_mae %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))
name_x <- paste(score_mae$reco_mode, score_mae$comb)

# Plot score 
name_x[name_x=="Base Base"] = "base"
rp <- score_mae %>% add_column(x=factor(name_x, rev(name_x), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mae <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4)),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mae <- ggplotGrob(core_rank_mae)
#gtable_show_layout(rank_mae)
rank_mae <- gtable_add_rows(rank_mae, rank_mae$heights[6], pos = 5) 
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

outplot <- arrangeGrob(rank_mae,top="Optimal Cross-temporal Score Rank AvgRelMAE")
ggsave("OCT_Rank_AvgRelMAE.pdf",
       plot = rank_mae,
       path = path,
       width=5, 
       height=2)

# Plot AvgRelMAE value
mae_dataplot <- mae %>% mutate(comb = paste(comb,num_mae, sep="-"),
                               k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  filter(reco_mode %in% c("oct")) %>%
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

ord <- mae_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mae_core <- mae_dataplot %>% 
  mutate(comb=factor(comb, rev(ord), ordered = T)) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  scale_x_discrete(labels = lab_prova)+
  facet_grid(c("k", "reco_mode"), labeller = as_labeller(rec_names), scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
# gtable_show_layout(mae_plot)
mae_plot <- ggplotGrob(mae_core+theme(legend.position = "none"))
mae_plot <- gtable_add_cols(mae_plot, mae_plot$widths[6], pos = 6) 
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=8, l=7, b=12, r=7, name = c("a", "b"))
mae_plot <- gtable_add_cols(mae_plot, unit(2/10, "line"), pos = 6)
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=14, l=6, b=14, r=8, name = c("a", "b"))
#mae_plot$heights[19] <- unit(0, "cm")
outplot <- arrangeGrob(mae_plot,g_legend(mae_core), nrow=2,heights=c(5, 0.5), 
                       #top="Optimal Cross Temporal Results", 
                       left = textGrob("AvgRelMAE",rot = 90, vjust = 1, hjust = 0.25))

ggsave("OCT_AvgMAE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=5.5)

# Ranking AvgRelMSE ----
table_mse <- mse %>% filter(reco_mode %in% c("oct", "Base")) %>% 
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% 
  arrange(all_all,bts_all,uts_all)
score_mse <- table_mse %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))
name_x <- paste(score_mse$reco_mode, score_mse$comb)

# Plot score 
name_x[name_x=="Base Base"] = "base"
rp <- score_mse %>% add_column(x=factor(name_x, rev(name_x), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mse <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4)),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mse <- ggplotGrob(core_rank_mse)
#gtable_show_layout(rank_mse)
rank_mse <- gtable_add_rows(rank_mse, rank_mse$heights[6], pos = 5) 
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

outplot <- arrangeGrob(rank_mse,top="Optimal Cross-temporal Score Rank AvgRelMSE")
ggsave("OCT_Rank_AvgRelMSE.pdf",
       plot = rank_mse,
       path = path,
       width=5, 
       height=2)

# Plot AvgRelMSE value
mse_dataplot <- mse %>% mutate(comb = paste(comb,num_mse, sep="-"),
                               k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  filter(reco_mode %in% c("oct")) %>%
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

ord <- mse_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mse_core <- mse_dataplot %>% 
  mutate(comb=factor(comb, rev(ord), ordered = T)) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  scale_x_discrete(labels = lab_prova)+
  facet_grid(c("k", "reco_mode"), labeller = as_labeller(rec_names), scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
# gtable_show_layout(mse_plot)
mse_plot <- ggplotGrob(mse_core+theme(legend.position = "none"))
mse_plot <- gtable_add_cols(mse_plot, mse_plot$widths[6], pos = 6) 
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=8, l=7, b=12, r=7, name = c("a", "b"))
mse_plot <- gtable_add_cols(mse_plot, unit(2/10, "line"), pos = 6)
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=14, l=6, b=14, r=8, name = c("a", "b"))
#mse_plot$heights[19] <- unit(0, "cm")
outplot <- arrangeGrob(mse_plot,g_legend(mse_core), nrow=2,heights=c(5, 0.5), 
                       #top="Optimal Cross Temporal Results", 
                       left = textGrob("AvgRelMSE",rot = 90, vjust = 1, hjust = 0.25))

ggsave("OCT_AvgMSE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=5.5)
# TCS - ITE ----
mae_par <- mae %>% filter(reco_mode %in% c("ite", "tcs", "kah"))
mse_par <- mse %>% filter(reco_mode %in% c("ite", "tcs", "kah"))

# Ranking AvgRelMAE ----
table_mae <- mae_par  %>%
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% arrange(all_all,bts_all,uts_all)
score_mae <- table_mae %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))

name_xmae <- paste(score_mae$reco_mode, score_mae$comb)
name_xmae[name_xmae=="Base Base"] = "base"
rp <- score_mae %>% add_column(x=factor(name_xmae, rev(name_xmae), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mae <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4,"mean")),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts","mean")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mae <- ggplotGrob(core_rank_mae)
#gtable_show_layout(rank_mae)
rank_mae <- gtable_add_rows(rank_mae, rank_mae$heights[6], pos = 5) 
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

ggsave("TCS_ITE_Rank_AvgRelMAE.pdf",
       plot = rank_mae,
       path = path,
       width=6, 
       height=9)

# Plot AvgRelMAE value
num_mae <- as.numeric(factor(paste(mae_par$reco_mode, mae_par$comb)))+100
mae_dataplot <- mae_par %>% mutate(comb = paste(comb,num_mae, sep="-"),
                               k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

plotOK <- c()
for(i in 1:length(mae_dataplot$comb)){
  gr <- unlist(strsplit(as.character(mae_dataplot$comb[i]), "-"))
  if(is.na(gr[2]))
    gr[2] = "none"
  
  if(any(gr %in% c("ols", "strar1")) | gr[2] == "struc"){
    plotOK[i] <- 0
  }else{
    plotOK[i] <- 1
  }
}

ord <- mae_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mae_core <- mae_dataplot[plotOK ==1,] %>%
  mutate(comb=factor(comb, rev(ord), ordered = T), 
         reco_mode = factor(reco_mode, c("kah", "tcs", "ite"))) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  scale_x_discrete(labels = lab_prova)+
  facet_grid(c("k", "reco_mode"), labeller = as_labeller(rec_names), scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
# gtable_show_layout(mae_plot)
mae_plot <- ggplotGrob(mae_core+theme(legend.position = "none"))
mae_plot <- gtable_add_cols(mae_plot, mae_plot$widths[10], pos = 10) 
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=8, l=11, b=12, r=11, name = c("a", "b"))
mae_plot <- gtable_add_cols(mae_plot, unit(2/10, "line"), pos = 10)
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=14, l=10, b=14, r=12, name = c("a", "b"))
#mae_plot$heights[19] <- unit(0, "cm")
outplot <- arrangeGrob(mae_plot,g_legend(mae_core), nrow=2,heights=c(5, 0.5), 
                       #top="Heuristic KA, Iterative and Optimal Cross Temporal Results", 
                       left = textGrob("AvgRelMAE",rot = 90, vjust = 1, hjust = 0.25))

ggsave("TCS_ITE_AvgMAE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=7)

# Ranking AvgRelMSE ----
table_mse <- mse_par  %>%
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% arrange(all_all,bts_all,uts_all)
score_mse <- table_mse %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))

name_xmse <- paste(score_mse$reco_mode, score_mse$comb)
name_xmse[name_xmse=="Base Base"] = "base"
rp <- score_mse %>% add_column(x=factor(name_xmse, rev(name_xmse), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mse <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4,"mean")),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts","mean")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mse <- ggplotGrob(core_rank_mse)
#gtable_show_layout(rank_mse)
rank_mse <- gtable_add_rows(rank_mse, rank_mse$heights[6], pos = 5) 
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

ggsave("TCS_ITE_Rank_AvgRelMSE.pdf",
       plot = rank_mse,
       path = path,
       width=6, 
       height=9)

# Plot AvgRelMSE value
num_mse <- as.numeric(factor(paste(mse_par$reco_mode, mse_par$comb)))+100
mse_dataplot <- mse_par %>% mutate(comb = paste(comb,num_mse, sep="-"),
                                   k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

plotOK <- c()
for(i in 1:length(mse_dataplot$comb)){
  gr <- unlist(strsplit(as.character(mse_dataplot$comb[i]), "-"))
  if(is.na(gr[2]))
    gr[2] = "none"
  
  if(any(gr %in% c("ols", "strar1")) | gr[2] == "struc"){
    plotOK[i] <- 0
  }else{
    plotOK[i] <- 1
  }
}

ord <- mse_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mse_core <- mse_dataplot[plotOK ==1,] %>%
  mutate(comb=factor(comb, rev(ord), ordered = T), 
         reco_mode = factor(reco_mode, c("kah", "tcs", "ite"))) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  scale_x_discrete(labels = lab_prova)+
  facet_grid(c("k", "reco_mode"), labeller = as_labeller(rec_names), scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
# gtable_show_layout(mse_plot)
mse_plot <- ggplotGrob(mse_core+theme(legend.position = "none"))
mse_plot <- gtable_add_cols(mse_plot, mse_plot$widths[10], pos = 10) 
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=8, l=11, b=12, r=11, name = c("a", "b"))
mse_plot <- gtable_add_cols(mse_plot, unit(2/10, "line"), pos = 10)
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=14, l=10, b=14, r=12, name = c("a", "b"))
#mse_plot$heights[19] <- unit(0, "cm")
outplot <- arrangeGrob(mse_plot,g_legend(mse_core), nrow=2,heights=c(5, 0.5), 
                       #top="Heuristic KA, Iterative and Optimal Cross Temporal Results", 
                       left = textGrob("AvgRelMSE",rot = 90, vjust = 1, hjust = 0.25))

ggsave("TCS_ITE_AvgMSE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=7)

# BEST ----
mae_best <- mae %>% filter(paste(reco_mode, comb, sep="-") %in% 
                        c("Base-Base","cs-shr","t-wlsv","t-acov","t-sar1","kah-wlsv-shr",
                          "tcs-acov-shr","tcs-sar1-shr","ite-wlsv-shr","ite-acov-shr",
                          "ite-sar1-shr","oct-wlsv","oct-bdshr","oct-acov"))
mse_best <- mse %>% filter(paste(reco_mode, comb, sep="-") %in% 
                        c("Base-Base","cs-shr","t-wlsv","t-acov","t-sar1","kah-wlsv-shr",
                          "tcs-acov-shr","tcs-sar1-shr","ite-wlsv-shr","ite-acov-shr",
                          "ite-sar1-shr","oct-wlsv","oct-bdshr","oct-acov"))


# Ranking AvgRelMAE ----
table_mae <- mae_best %>% 
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% 
  arrange(all_all,bts_all,uts_all)
score_mae <- table_mae %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))
name_x <- paste(score_mae$reco_mode, score_mae$comb)

# Plot score 
name_x[name_x=="Base Base"] = "base"
rp <- score_mae %>% add_column(x=factor(name_x, rev(name_x), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mae <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4)),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mae <- ggplotGrob(core_rank_mae)
#gtable_show_layout(rank_mae)
rank_mae <- gtable_add_rows(rank_mae, rank_mae$heights[6], pos = 5) 
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mae <- gtable_add_grob(rank_mae, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

ggsave("best_Rank_AvgRelMAE.pdf",
       plot = rank_mae,
       path = path,
       width=5, 
       height=2)

# Plot AvgRelMAE value
mae_dataplot <- mae_best %>% filter(comb !="Base") %>% mutate( comb = paste(reco_mode, comb),
  k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

ord <- mae_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mae_core <- mae_dataplot %>%
  mutate(comb=factor(comb, rev(ord), ordered = T)) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  facet_grid(c("k"), labeller = "label_value", scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
#gtable_show_layout(mae_plot)
mae_plot <- ggplotGrob(mae_core+theme(legend.position = "none"))
mae_plot <- gtable_add_cols(mae_plot, mae_plot$widths[6], pos = 6) 
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=7, l=7, b=11, r=7, name = c("a", "b"))
mae_plot <- gtable_add_cols(mae_plot, unit(2/10, "line"), pos = 6)
mae_plot <- gtable_add_grob(mae_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=13, l=6, b=13, r=8, name = c("a", "b"))

outplot <- arrangeGrob(mae_plot,g_legend(mae_core), nrow=2,heights=c(5, 0.5), 
                       left = textGrob("AvgRelMAE",rot = 90, vjust = 1, hjust = 0.25))
ggsave("best_AvgMAE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=6)


# Ranking AvgRelMSE ----
table_mse <- mse_best %>% 
  mutate(k=factor(k,c("all",1,2,4), ordered = T)) %>% arrange(comb,k) %>%
  pivot_wider(names_from = k, values_from = c(all,uts,bts)) %>% 
  arrange(all_all,bts_all,uts_all)
score_mse <- table_mse %>% mutate_if(is.numeric, function(x) rank(x, ties.method = "average"))
name_x <- paste(score_mse$reco_mode, score_mse$comb)

# Plot score 
name_x[name_x=="Base Base"] = "base"
rp <- score_mse %>% add_column(x=factor(name_x, rev(name_x), ordered = T), .before = 1) %>% 
  select(-comb,-reco_mode) %>%
  pivot_longer(c(-x), names_to = "colname", values_to = "index") %>% 
  separate(colname,sep="_", into=c("cross-sectional","temporal"), fill = "right")

core_rank_mse <- rp %>% 
  mutate(temporal=factor(temporal, c("all",1,2,4)),
         `cross-sectional`=factor(`cross-sectional`, c("all","bts","uts")))%>%
  ggplot(aes(x = temporal, 
             y = x, fill = index)) +
  geom_tile(colour = "white",size = 0.5) + scale_fill_distiller(palette = "RdYlGn")+
  geom_text(aes(label = round(index, 1.5)), size=2)+
  #facet_wrap(~`cross-sectional`, nrow=1,  drop=TRUE) +
  scale_x_discrete(position = "top")+
  facet_grid(~`cross-sectional`, scales="free_x", space = "free_x", labeller = label_value) +
  theme(axis.line = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), panel.spacing = unit(0, "lines"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")

rank_mse <- ggplotGrob(core_rank_mse)
#gtable_show_layout(rank_mse)
rank_mse <- gtable_add_rows(rank_mse, rank_mse$heights[6], pos = 5) 
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("all",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=5, b=6, r=5, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("bts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=7, b=6, r=7, name = c("a", "b"))
rank_mse <- gtable_add_grob(rank_mse, 
                            list(rectGrob(gp = gpar(col = NA, fill = NA, size = .5)),
                                 textGrob("uts",
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=6, l=9, b=6, r=9, name = c("a", "b"))

ggsave("best_Rank_AvgRelMSE.pdf",
       plot = rank_mse,
       path = path,
       width=5, 
       height=2)

# Plot AvgRelMSE value
mse_dataplot <- mse_best %>% filter(comb !="Base") %>% mutate( comb = paste(reco_mode, comb),
                                                               k=factor(k, levels = c(4,2,1,"all"), ordered = T)) %>% 
  pivot_longer(c(-comb,-k,-reco_mode), names_to = "series", values_to = "index")

ord <- mse_dataplot %>% filter(k == "all", series=="all") %>% arrange(index) %>% pull(comb)

lab_prova <- function(x) substring(as.character(x), 1,
                                   nchar(as.character(x))-4)
mse_core <- mse_dataplot %>%
  mutate(comb=factor(comb, rev(ord), ordered = T)) %>% 
  ggplot()+
  geom_line(aes(x=comb,y=index, group=series, col=series, linetype=series), size=0.5)+
  geom_point(aes(x=comb,y=index, col=series, shape=series), size=2)+
  geom_hline(aes(yintercept=1), col="#b7b7b7")+
  facet_grid(c("k"), labeller = "label_value", scales = "free")+  
  theme(axis.title.y = element_blank(),axis.title.x = element_blank(),legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# Adjust the graph
#gtable_show_layout(mse_plot)
mse_plot <- ggplotGrob(mse_core+theme(legend.position = "none"))
mse_plot <- gtable_add_cols(mse_plot, mse_plot$widths[6], pos = 6) 
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("Temporal aggregation level", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=7, l=7, b=11, r=7, name = c("a", "b"))
mse_plot <- gtable_add_cols(mse_plot, unit(2/10, "line"), pos = 6)
mse_plot <- gtable_add_grob(mse_plot, 
                            list(rectGrob(gp = gpar(col = NA, fill = "gray85", size = .5)),
                                 textGrob("All levels", rot = -90,  
                                          gp = gpar(cex = .75, fontface = 'bold', col = "black"))), 
                            t=13, l=6, b=13, r=8, name = c("a", "b"))

outplot <- arrangeGrob(mse_plot,g_legend(mse_core), nrow=2,heights=c(5, 0.5), 
                       left = textGrob("AvgRelMSE",rot = 90, vjust = 1, hjust = 0.25))
ggsave("best_AvgMSE.pdf",
       plot = outplot,
       path = path,
       width=8, 
       height=6)