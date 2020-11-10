######################################################################
# 00Aus_base.R
#
# Aggregating and forecasting Australian GDP Time series by quarter,
# semester and year
#
# Input files: AusGDP_inpdata.RData
# Output files: Aus_basef.RData
#
# This code is written by T. Di Fonzo and D. Girolimetto
# Department of Statistics, University of Padua (Italy)
# email: tommaso.difonzo@unipd.it; difonzo@stat.unipd.it
######################################################################
rm(list = ls(all = TRUE))
libs <- c("tidyverse","forecast","tibbletime","zoo","lubridate")
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

load("./AusGDP_inpdata.RData")
mts <- as.numeric(time(AusGDP$Gdp))
tms <- date_decimal(mts)
#AusGDP <- AusGDP %>% add_column(ym=tms)

DF <- tibble("date" = character(),
             "Series" = character(),
             "F-method" = character(),
             "R-method" = character(),
             "Forecast Horizon" = integer(),
             "Forecasts" = double(),
             "Actual" = double(),
             "Scaling Factor" = numeric(),
             "Training window_length" = integer(),
             "Replication" = integer(),
             "Hk" = integer())

# Start first trainng (Date)
start_first_train <- c(1984,4)
# End first trainng (Date)
end_first_train <- c(1994,3)
# End last trainng (Date)
max_train <- c(2017,1)

# Length of rolling training set
train_length <- AusGDP %>% pull(.,1) %>% window(end=end_first_train) %>% length()

# Maximum time the window expands
test_length <- AusGDP %>% pull(.,1)  %>% window(start=end_first_train, end=max_train) %>% length()
#test_length <- 4 # testing the cycle

# number of variable
n <- ncol(AusGDP)
H <- 4

# Allocate object
fitmat_ARIMA <- list()
resmat_ARIMA <- list()
fitmat_ETS <- list()
resmat_ETS <- list()

Del1 <- matrix(NA, ncol = n, nrow = test_length)
Phi1 <- matrix(NA, ncol = n, nrow = test_length)
Theta1 <- matrix(NA, ncol = n, nrow = test_length)

Del2 <- matrix(NA, ncol = n, nrow = test_length)
Phi2 <- matrix(NA, ncol = n, nrow = test_length)
Theta2 <- matrix(NA, ncol = n, nrow = test_length)

Del4 <- matrix(NA, ncol = n, nrow = test_length)
Phi4 <- matrix(NA, ncol = n, nrow = test_length)
Theta4 <- matrix(NA, ncol = n, nrow = test_length)

Start <- Sys.time()
for(j in 0:(test_length-1)){

  # Training and test set for quarter data
  month_test1 <- tms[(train_length + j+1):(train_length + j+H)]
  if(j%%4==0){
    Train1 <- AusGDP[1:(train_length + j),]
  }else if((j-1)%%4==0){
    Train1 <- AusGDP[2:(train_length + j),]
  }else if((j-2)%%4==0){
    Train1 <- AusGDP[3:(train_length + j),]
  }else if((j-3)%%4==0){
    Train1 <- AusGDP[4:(train_length + j),]
  }
  #Train1 <- AusGDP[1:(train_length + j),]
  Test1 <- AusGDP[(train_length + j+1):(train_length + j+H),]

  # Allocate space for models and residuals
  Residuals_all_ARIMA1 <- matrix(NA, nrow = nrow(Train1), ncol = n)
  fit_ARIMA1 <- list()
  Residuals_all_ETS1 <- matrix(NA, nrow = nrow(Train1), ncol = n)
  fit_ETS1 <- list()

  cat("Quarter time-serie number: ")
  # Starting forecasting for each series
  for(i in 1:n){
    # take i-serie from training set
    TS <- ts(Train1[,i], frequency = 4)

    #Scaling Factor for calculating MASE
    snaive(TS)$residuals %>% abs() %>% mean(., na.rm=TRUE) -> Q

    #ARIMA
    fit_ARIMA1[[i]] <- auto.arima(TS)                                     # Model
    Forecast_ARIMA <- forecast(fit_ARIMA1[[i]], h = H)                    # Forecasts
    Residuals_all_ARIMA1[,i] <- as.vector(TS - fitted(fit_ARIMA1[[i]]))   # Residuals

    Del1[(j+1),i] <- sum(fit_ARIMA1[[i]]$model$Delta!=0)
    Phi1[(j+1),i] <- sum(fit_ARIMA1[[i]]$model$phi!=0)
    Theta1[(j+1),i] <- sum(fit_ARIMA1[[i]]$model$theta!=0)

    # Add rows line to DF tibble
    for (h in 1:H) {
      DF <- DF %>% add_row("date" = as.character(month_test1[h]),
                           "Series" = paste(colnames(AusGDP)[i]),
                           "F-method" = "ARIMA",
                           "R-method" = "Base",
                           "Forecast Horizon" = h,
                           "Forecasts" = Forecast_ARIMA$mean[h],
                           "Actual" = as.numeric(Test1[h,i]),
                           "Scaling Factor" = Q,
                           "Training window_length" = NROW(TS),
                           "Replication" = j+1,
                           "Hk" = 4)
    }

    #ETS
    fit_ETS1[[i]] <- ets(TS)                                          # Model
    Forecast_ETS <- forecast(fit_ETS1[[i]], h = H)                    # Forecasts
    Residuals_all_ETS1[,i] <- as.vector(TS - fitted(fit_ETS1[[i]]))   # Residuals

    # Add rows line to DF tibble
    for (h in 1:H) {
      DF <- DF %>% add_row("date" = as.character(month_test1[h]),
                           "Series" = paste(colnames(AusGDP)[i]),
                           "F-method" = "ETS",
                           "R-method" = "Base",
                           "Forecast Horizon" = h,
                           "Forecasts" = Forecast_ETS$mean[h],
                           "Actual" = as.numeric(Test1[h,i]),
                           "Scaling Factor" = Q,
                           "Training window_length" = NROW(TS),
                           "Replication" = j+1,
                           "Hk" = 4)
    }
    if(i%%5==0) cat(i, " ", sep="")
  }
  cat("\n")

  # Training and test set for semester data
  Train2 <- as_tibble(apply(Train1, 2, function(x) rowSums(matrix(x,ncol = 2,byrow = T))))
  # if(j%%2==0){
  #   Train2 <- as_tibble(apply(Train1, 2, function(x) rowSums(matrix(x,ncol = 2,byrow = T))))
  # }else{
  #   Train2 <- as_tibble(apply(Train1[-1,], 2, function(x) rowSums(matrix(x,ncol = 2,byrow = T))))
  # }

  Test2 <- as_tibble(apply(Test1, 2, function(x) rowSums(matrix(x,ncol = 2,byrow = T))))
  month_test2 <- tms[seq((train_length + j+1),(train_length + j+H), by =2)]

  # Allocate space for models and residuals
  Residuals_all_ARIMA2 <- matrix(NA, nrow = nrow(Train2), ncol = n)
  fit_ARIMA2 <- list()
  Residuals_all_ETS2 <- matrix(NA, nrow = nrow(Train2), ncol = n)
  fit_ETS2 <- list()

  cat("Semester time-serie number: ")
  # Starting forecasting for each series
  for(i in 1:n){
    # take i-serie from training set
    TS <- ts(Train2[,i], frequency = 2)

    #Scaling Factor for calculating MASE
    snaive(TS)$residuals %>% abs() %>% mean(., na.rm=TRUE) -> Q

    #ARIMA
    fit_ARIMA2[[i]] <- auto.arima(TS)                                   # Model
    Forecast_ARIMA <- forecast(fit_ARIMA2[[i]], h = H/2)                # Forecasts
    Residuals_all_ARIMA2[,i] <- as.vector(TS - fitted(fit_ARIMA2[[i]])) # Residuals

    Del2[(j+1),i] <- sum(fit_ARIMA2[[i]]$model$Delta!=0)
    Phi2[(j+1),i] <- sum(fit_ARIMA2[[i]]$model$phi!=0)
    Theta2[(j+1),i] <- sum(fit_ARIMA2[[i]]$model$theta!=0)

    # Add rows line to DF tibble
    for (h in 1:(H/2)) {
      DF <- DF %>% add_row("date" = as.character(month_test2[h]),
                           "Series" = paste(colnames(AusGDP)[i]),
                           "F-method" = "ARIMA",
                           "R-method" = "Base",
                           "Forecast Horizon" = h,
                           "Forecasts" = Forecast_ARIMA$mean[h],
                           "Actual" = as.numeric(Test2[h,i]),
                           "Scaling Factor" = Q,
                           "Training window_length" = NROW(TS),
                           "Replication" = j+1,
                           "Hk" = 2)
    }

    #ETS
    fit_ETS2[[i]] <- ets(TS)                                        # Model
    Forecast_ETS <- forecast(fit_ETS2[[i]], h = H/2)                # Forecasts
    Residuals_all_ETS2[,i] <- as.vector(TS - fitted(fit_ETS2[[i]])) # Residuals

    # Add rows line to DF tibble
    for (h in 1:(H/2)) {
      DF <- DF %>% add_row("date" = as.character(month_test2[h]),
                           "Series" = paste(colnames(AusGDP)[i]),
                           "F-method" = "ETS",
                           "R-method" = "Base",
                           "Forecast Horizon" = h,
                           "Forecasts" = Forecast_ETS$mean[h],
                           "Actual" = as.numeric(Test2[h,i]),
                           "Scaling Factor" = Q,
                           "Training window_length" = NROW(TS),
                           "Replication" = j+1,
                           "Hk" = 2)
    }
    if(i%%5==0) cat(i, " ", sep="")
  }
  cat("\n")

  # Training and test set for annual data
  month_test4 <- tms[seq((train_length + j+1),(train_length + j+H), by =4)]
  Train4 <- as_tibble(apply(Train1, 2, function(x) rowSums(matrix(x,ncol = 4,byrow = T))))

  # if(j%%4==0){
  #   Train4 <- as_tibble(apply(Train1, 2, function(x) rowSums(matrix(x,ncol = 4,byrow = T))))
  # }else if((j-1)%%4==0){
  #   Train4 <- as_tibble(apply(Train1[-1,], 2, function(x) rowSums(matrix(x,ncol = 4,byrow = T))))
  # }else if((j-2)%%4==0){
  #   Train4 <- as_tibble(apply(Train1[-c(1,2),], 2, function(x) rowSums(matrix(x,ncol = 4,byrow = T))))
  # }else if((j-3)%%4==0){
  #   Train4 <- as_tibble(apply(Train1[-c(1,2,3),], 2, function(x) rowSums(matrix(x,ncol = 4,byrow = T))))
  # }

  Test4 <- as_tibble(t(apply(Test1, 2, function(x) rowSums(matrix(x,ncol = 4,byrow = T)))))
  # Allocate space for models and residuals
  Residuals_all_ARIMA4 <- matrix(NA, nrow = nrow(Train4), ncol = n)
  fit_ARIMA4 <- list()
  Residuals_all_ETS4 <- matrix(NA, nrow = nrow(Train4), ncol = n)
  fit_ETS4 <- list()

  cat("Annual time-serie number: ")
  # Starting forecasting for each series
  for(i in 1:n){
    # take i-serie from training set
    TS <- ts(Train4[,i], frequency = 1)

    #Scaling Factor for calculating MASE
    snaive(TS)$residuals %>% abs() %>% mean(., na.rm=TRUE) -> Q

    #ARIMA
    fit_ARIMA4[[i]] <- auto.arima(TS)
    Forecast_ARIMA <- forecast(fit_ARIMA4[[i]], h = H/4)
    Residuals_all_ARIMA4[,i] <- as.vector(TS - fitted(fit_ARIMA4[[i]]))

    Del4[(j+1),i] <- sum(fit_ARIMA4[[i]]$model$Delta!=0)
    Phi4[(j+1),i] <- sum(fit_ARIMA4[[i]]$model$phi!=0)
    Theta4[(j+1),i] <- sum(fit_ARIMA4[[i]]$model$theta!=0)

    # Add rows line to DF tibble
    for (h in 1:(H/4)) {
      DF <- DF %>% add_row("date" = as.character(month_test4[h]),
                           "Series" = paste(colnames(AusGDP)[i]),
                           "F-method" = "ARIMA",
                           "R-method" = "Base",
                           "Forecast Horizon" = h,
                           "Forecasts" = Forecast_ARIMA$mean[h],
                           "Actual" = as.numeric(Test4[h,i]),
                           "Scaling Factor" = Q,
                           "Training window_length" = NROW(TS),
                           "Replication" = j+1,
                           "Hk" = 1)
    }

    #ETS
    fit_ETS4[[i]] <- ets(TS)
    Forecast_ETS <- forecast(fit_ETS4[[i]], h = H/4)
    Residuals_all_ETS4[,i] <- as.vector(TS - fitted(fit_ETS4[[i]]))

    # Add rows line to DF tibble
    for (h in 1:(H/4)) {
      DF <- DF %>% add_row("date" = as.character(month_test4[h]),
                           "Series" = paste(colnames(AusGDP)[i]),
                           "F-method" = "ETS",
                           "R-method" = "Base",
                           "Forecast Horizon" = h,
                           "Forecasts" = Forecast_ETS$mean[h],
                           "Actual" = as.numeric(Test4[h,i]),
                           "Scaling Factor" = Q,
                           "Training window_length" = NROW(TS),
                           "Replication" = j+1,
                           "Hk" = 1)
    }
    if(i%%5==0) cat(i, " ", sep="")
  }
  cat("\n")
  res_ARIMA <- t(rbind(Residuals_all_ARIMA4, Residuals_all_ARIMA2, Residuals_all_ARIMA1))

  res_ETS <- t(rbind(Residuals_all_ETS4, Residuals_all_ETS2, Residuals_all_ETS1))

  resmat_ARIMA[[(j+1)]] <- res_ARIMA
  resmat_ETS[[(j+1)]] <- res_ETS

  cat("Training window n.", (j+1), "(out of", test_length, ",",(j+1)/test_length*100,"%)\n")
}
End <- Sys.time()
print(End-Start)

save.image(file = "./Aus_basef.RData")
rm(list = ls(all = TRUE))
