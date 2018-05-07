#' ---
#' title: "APMC/Mandi DS Task - Analysis"
#' #author: "Raam"
#' #date: "23 April 2018"
#' output: 
#'   html_document: 
#'     fig_caption: yes
#'     toc_depth: 4
#'     keep_md: true
#' ---
#' 
#' The overall objective of the task is two-fold:
#' 
#' 1. To understand the "trend" in the modal (average) prices of different commodities in different APMCs (Agricultural produce market committee),
#' 2. To predict the modal prices of different APMC-Commodity sets.
#' 
#' This file contains the **analysis** part.
#' 
## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

#' ### Import and view data
## ---- results='hide'-----------------------------------------------------
msp_data <- read.csv('CMO_MSP_Mandi.csv')
monthly_data<-read.csv('Monthly_data_cmo.csv')
summary(msp_data)
summary(monthly_data)

# Make case-insensitive
levels(monthly_data$Commodity) <- tolower(levels(monthly_data$Commodity))
levels(msp_data$commodity) <- tolower(levels(msp_data$commodity))

#' 
#' ### Data cleaning - Test and filter outliers (Obj: 1)
#' 
#' First, in the msp_data, the value "sugarcane" in the column "Commodity" seems to be miss-written as "sugar-cane".
#' 
#' Reason: ```summary(msp_data)``` shows that there are 32 commodites each having 5 MSP across 5 years, implying that the total rows should be 32x5=160. However, there are only 155. And the remaining 5 corresponds to that of sugar-cane.
#' 
## ---- results="hide"-----------------------------------------------------
# Below code clearly shows that sugar-cane is miswritten
aggregate(commodity ~ year, data = msp_data, FUN = summary) 
# Correct it.
levels(msp_data$commodity)[levels(msp_data$commodity)=="sugar-cane"] <- "sugarcane"

#' 
#' Second, since one of the objectives of the task is to compare prices of different commodities with their corresponding MSP(Minimum Support Price), logically it makes sense to extract and keep only those observations from ```monthly_data``` whose commodities's MSPs are known (from ```msp_data```).
#' 
## ------------------------------------------------------------------------
# Find intersection
msp_commodity <- msp_data$commodity
all_commodity <- monthly_data$Commodity
comm_with_msp <- intersect(all_commodity, msp_commodity)
comm_with_msp # There are 19 commodities whose MSPs and monthly data are available

# monthly_data_msp has monthly data of those commodities whose MSPs are available
monthly_data_msp<-monthly_data[monthly_data$Commodity %in% comm_with_msp, ]
monthly_data_msp$Commodity <- factor(monthly_data_msp$Commodity)
monthly_data_msp$APMC <- factor(monthly_data_msp$APMC)

#' 
#' Third, ```monthly_data_msp``` has some abnormally large values across several columns (modal_price, min_price, max_price) which can be observed using the command below:
#' 
## ------------------------------------------------------------------------
summary(monthly_data_msp)

#' 
#' However, the prices of some of the commodities may be genuinely large. So, clean this dataframe based on the large values for **each commodity**, as done below. 
#' 
## ---- results="hide"-----------------------------------------------------
# removing outliers for each commodity
monthly_data_mspcleaned<-monthly_data_msp

for(comm in comm_with_msp) {
  modal_outliers = boxplot.stats(monthly_data_mspcleaned[monthly_data_mspcleaned$Commodity == comm, "modal_price"])$out
  monthly_data_mspcleaned[(monthly_data_mspcleaned$Commodity == comm) &
                            (monthly_data_mspcleaned$modal_price %in% modal_outliers), "modal_price"] <- NA
  
  min_outliers = boxplot.stats(monthly_data_mspcleaned[monthly_data_mspcleaned$Commodity == comm, "min_price"])$out
  monthly_data_mspcleaned[(monthly_data_mspcleaned$Commodity == comm) &
                            (monthly_data_mspcleaned$min_price %in% min_outliers), "min_price"] <- NA
 
  max_outliers = boxplot.stats(monthly_data_mspcleaned[monthly_data_mspcleaned$Commodity == comm, "max_price"])$out
  monthly_data_mspcleaned[(monthly_data_mspcleaned$Commodity == comm) &
                            (monthly_data_mspcleaned$max_price %in% max_outliers), "max_price"] <- NA 
  
}

monthly_data_mspcleaned[monthly_data_mspcleaned == 0] <- NA
summary(monthly_data_mspcleaned)
# There are very few (in hundreds) NAs in each column now, so just remove them.
monthly_data_mspcleaned<-na.omit(monthly_data_mspcleaned, cols=c("min_price", "max_price"))
# No need to remove NAs for modal_price, becoz the the above code removes all NAs.

#' 
#' Some of the commodities's prices (for instance, coconut's) are less than 100 per quintal, which don't seem quite reasonable. However, in the absence of additional data, they are left untouched.
#' 
## ------------------------------------------------------------------------
# possible anamoly - commdoties like coconut
# monthly_data_mspcleaned[monthly_data_mspcleaned$Commodity == "coconut",]

#' 
#' ```summary(monthly_data_msp)``` and ```summary(monthly_data_mspcleaned)``` shows difference, yet the max values of different prices still seem to be large. Again, in the absence of additional data, they are left untouched.
#' 
#' ### Understand price fluctuations (Obj: 2)
#' 
#' First, find out how many groups of APMC-Commodity exist. Here, I'm considering only those groups which have atleast 24 observations, because for most time series models/analysis, the recommended sequence size is at least **two** complete cycles (24 for monthly data) of the period.
#' 
#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
library("xts")
data1 <- subset(monthly_data_mspcleaned, select = c("date", "APMC", "Commodity","district_name", "modal_price")) # extract required columns
data1$date <- as.Date(as.yearmon(data1$date)) # format date type
data1<-data1[order(data1$date),] # order dataframe by date
row.names(data1) <- NULL # rest index

library("dplyr")
grouped_data1 <- data1 %>% group_by(APMC, Commodity) # group data by APMC and Commodity
grouped_data1 = grouped_data1 %>% filter(n()>24) # remove groups with less than 24 obs.
n_groups(grouped_data1) 

#' 
#' ```monthly_data_mspcleaned``` has 308 groups (can be seen using ```str(monthly_data_mspcleaned)```) while ```grouped_data1``` has 278 groups. Hence, 30 groups have observations less 24 and it was observed that many have less than 10.
#' 
#' #### Decomposition of time series - detect seasonality type (Obj: 2.1)
#' 
#' Classical decomposition is not suitable because of less observations per group (in our data, max observations per group is **27**). So I'm using ```stl()``` decomposition. Since, it supports only additive decomposition, I'm also considering the log transformation of each time series for testing if they have multiplicative component.
#' 
## ------------------------------------------------------------------------
# Functions to decompose
get_seasonality <- function(x){
    fit <- stl(ts(x, frequency = 12, start = c(2014,9)), s.window="periodic")
    return(fit$time.series[,"seasonal"])
}
get_trend <- function(x){
    fit <- stl(ts(x, frequency = 12, start = c(2014,9)), s.window="periodic")
    return(fit$time.series[,"trend"])
}
get_residual <- function(x){
    fit <- stl(ts(x, frequency = 12, start = c(2014,9)), s.window="periodic")
    return(fit$time.series[,"remainder"])
 }

#' 
#' ``` view(grouped_decomp)``` below displays the components after decomposition. It can be observed that the "seasonality" component for both additive and multiplicative decomposition is very small, **questioning the presence of seasonality** in our data. However, this could also be due to the samll sequence size of our individual time series data.
#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
# Computing decomposition of grouped data
grouped_decomp <- grouped_data1
grouped_decomp$modal_price_log <- log(grouped_decomp$modal_price) # for multiplicative decomposition

grouped_decomp <- grouped_decomp %>% mutate(trend_a = get_trend(modal_price), 
                                  seasonality_a = get_seasonality(modal_price), 
                                  residual_a = get_residual(modal_price),
                                  
                                  trend_m = exp(get_trend(modal_price_log)), 
                                  seasonality_m = exp(get_seasonality(modal_price_log)), 
                                  residual_m = exp(get_residual(modal_price_log)))

# view(grouped_decomp)

#' 
#' Next, I'm creating a new dataframe ```data1_analysis``` that summarises each group. Here, I'm deciding whether the seasonality type of each APMC-Commodity (a time series) group is additive or multiplicative using Auto-Correlation Function (ACF) on **residuals**. That is, I'm checking how much correlation between the data points still exists within the residuals. 
#' 
#' So, compute the ACF of the residuals (after removing trend and seasonality components) of a particular group twice, first assuming additive and then assuming multiplicative. The decision is made based on the difference between the two.
#' 
## ------------------------------------------------------------------------
data1_analysis <- grouped_data1[,c("district_name","APMC", "Commodity")] %>% filter(row_number() == 1)
data1_analysis$afa_a<-NA
data1_analysis$afa_m<-NA
data1_analysis$afa_diff<-NA
data1_analysis$is_season<-NA
data1_analysis$season_type<-NA

data1_analysis$APMC <- factor(data1_analysis$APMC )
data1_analysis$Commodity <- factor(data1_analysis$Commodity )
# str(data1_analysis) # It has 112 APMCs and 14 commodities and all 33 districts
data1_analysis <- as.data.frame(data1_analysis)

for(i in 1:nrow(data1_analysis)){
  res <- filter(grouped_decomp, APMC == as.character(data1_analysis[i,"APMC"]) & 
           Commodity == as.character(data1_analysis[i,"Commodity"] ))
 
  data1_analysis[i,"afa_a"] <- sum(acf(res$residual_a, plot=FALSE)$acf^2)
  data1_analysis[i,"afa_m"] <- sum(acf(res$residual_m, plot=FALSE)$acf^2)
} 

#' 
#' If the ACF of residuals in additive decomposition is greater than that in multiplicative decomposition, then the series has multiplicative components. This is because, after removing trend and seasonlity, the ACF of residuals should be very minimal. 
#' 
#' Here, I'm considering the percentage change between the two residuals to decide. It can be observed from below that the change is not at all significant for many series (almost all changes are between 1%-2%). 
#' 
## ------------------------------------------------------------------------
# Percentage change of ACF from additive to multiplicative decomposition 
# If the change is +ve, then it is additive.
data1_analysis$afa_diff <- ((data1_analysis$afa_m - data1_analysis$afa_a)/ data1_analysis$afa_a)*100 

# Decide a threshold for afa_diff for deciding whether seasonality exists - here I've chosen 1% change. 
# If the change is below 1%, there is no seasonality present in the series.
data1_analysis$is_season <- ifelse(abs(data1_analysis$afa_diff) > 1, 1, 0)

# Anyways, I'm just using the simple ACF method to quickly decide.
data1_analysis$season_type <- ifelse(data1_analysis$afa_diff > 0, "additive", "multiplicative")

str(data1_analysis)
# view(data1_analysis)

#' 
#' #### De-seasonalise time series (Obj: 2.2)
#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
# De-seasonalise based on found decomposition type
grouped_decomp$de_seasoned <- NA

for(i in 1:nrow(grouped_decomp)){
  grouped_decomp$de_seasoned[i] <- ifelse((data1_analysis[(data1_analysis$APMC == as.character(grouped_decomp$APMC[i])) 
                                               & (data1_analysis$Commodity == as.character(grouped_decomp$Commodity[i])),
                                                  "season_type"] == "additive"),
                                                  grouped_decomp$modal_price[i]-grouped_decomp$seasonality_a[i],
                                                  grouped_decomp$modal_price[i]/grouped_decomp$seasonality_m[i])
}

#' 
#' ### Compare raw and de-seasoned prices with MSPs (Obj: 3)
#' 
#' Here, I'm using the percentage increase of raw/de-seasoned prices w.r.t their corresponding MSPs. Thus, if the result is +ve, the the prices are more than MSPs and vice versa.
#' 
## ------------------------------------------------------------------------
# First, extract onyl required columns
temp1 <- subset(grouped_decomp, select = c("date", "APMC", "Commodity", "district_name", 
                                 "modal_price", "de_seasoned"))
temp1$APMC <- factor(temp1$APMC)
temp1$Commodity <- factor(temp1$Commodity)

#' 
## ------------------------------------------------------------------------
# Function to compute percentage increase of price from MSP.
get_msp_fluc <- function(comm, year, price){
  msp <- msp_data[(msp_data$commodity == as.character(comm)) & (msp_data$year == year), "msprice"]
  perc_fluc <- ((price-msp)/msp)*100
  return(round(perc_fluc,1))
}

#' 
## ---- warning=FALSE, message=FALSE---------------------------------------
# computing deviation from MSP
library("data.table")
temp1 <- temp1 %>% group_by(date, Commodity)%>% 
                         mutate(msp_fluc_raw = get_msp_fluc(Commodity, year(date), modal_price), 
                                msp_fluc_des = get_msp_fluc(Commodity, year(date), de_seasoned))


#' It can be observed from ```temp1``` that the deviation from MSP is almost the same for both raw and de-seasoned prices. Remember, we found earlier that the **seasonal components of almost all the time series in our data are too small**. Thus, I'm comparing only one of the prices (de-seasoned price) with MSP.
#' 
#' The below function implements the visualization of the highest and lowest deviation of de-seasoned prices from MSPs in each month.
#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
library("ggplot2")

plot_MSP_deviations <- function(type){
  if(type == "max"){
    plot1_df <- as.data.frame(temp1 %>% group_by(date) %>% slice(which.max(msp_fluc_des)) %>% select(date, APMC, Commodity, msp_fluc_des)) }
  else{
    df$y2014 <- -0.5
    df$y2015 <- -0.5
    df$y2016 <- -0.5

    plot1_df <- as.data.frame(temp1 %>% group_by(date) %>% slice(which.min(msp_fluc_des)) %>% select(date, APMC, Commodity, msp_fluc_des)) }

  plot1_df$APMC_Commodity <- paste(plot1_df$APMC, plot1_df$Commodity, sep=" -- ")

  for(i in 1:nrow(plot1_df)){
    df[month(plot1_df$date[i]), paste("y",year(plot1_df$date[i]), sep = "")] <- plot1_df$msp_fluc_des[i]
    df[month(plot1_df$date[i]), paste("apmc",year(plot1_df$date[i]), sep = "")] <- plot1_df$APMC_Commodity[i]
  }
  
  df1<-as.data.table(df)
  df1 <- melt(df1, id=1, measure=list(2:4, 5:7))
  df1[,variable:=factor(variable, labels=c("2014", "2015", "2016"))]
  my.order=rep(12:1,3) 
  colnames(df1)[2] <- "year"
  hjust <- ifelse(type=="max", "right", "left") # for text alignment within each bar

  ggplot(df1, aes(reorder(month, my.order),value1,fill=year)) + 
    geom_col(position="dodge") + 
    geom_text(aes(label=value2), position=position_dodge(.9), vjust=0.5, hjust=hjust, size=3.5) + coord_flip() +
    xlab("Month") +
    ylab("% increase") +
    ggtitle("Percentage increase of de-seasoned prices w.r.t their corresponding MSPs") +
    theme(plot.title = element_text(hjust = 0.5)) 
}


#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
# dataframe to plot MSP deviation
df <- data.frame(month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df$y2014 <- 0.5 # replacing 0 with 0.5 just for better visualization
df$y2015 <- 0.5
df$y2016 <- 0.5
df$apmc2014 <- ""
df$apmc2015 <- ""
df$apmc2016 <- ""

# Plot max deviations
plot_MSP_deviations(type="max")

#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
# Plot min deviations
plot_MSP_deviations(type="min")

#' 
#' ### Analyze price fluctuations (Obj: 4)
#' 
#' First, the standard deviations (in the prices) of each commodity in each year across different APMCs are computed and are considered as references. Then, the standard deviation of each APMC-Commodity group (a time series) is compared with its commodity's standard deviation (across different APMCs) to understand its fluctuation in the given period.
#' 
## ------------------------------------------------------------------------
# Compute mean and std for each commodity in each year
msp_data_new <- msp_data
msp_data_new$mean_deseasoned<-NA
msp_data_new$std_deseasoned<-NA

#' 
## ------------------------------------------------------------------------
start = which(msp_data_new$year == 2014)[1] # start row from 2014 becoz monthly data does not contain information before 2014.

for(i in start:nrow(msp_data)){
  prices <- temp1[year(temp1$date) == msp_data_new[i,"year"] &
                    as.character(temp1$Commodity) == as.character(msp_data_new[i,"commodity"]), "de_seasoned"]
  msp_data_new$mean_deseasoned[i] <- mean(prices$de_seasoned, na.rm = TRUE)
  msp_data_new$std_deseasoned[i] <- sd(prices$de_seasoned, na.rm = TRUE)
}

# temp1 has only 14 commodities for 3 years. So in msp_price_new, out of 155 rows (for 31 commodities), there are 155-14*3 = 113 NAs in the mean and std columns.
#  This can be seen using summary(msp_data_new) and sum(is.na(msp_data_new$std_deseasoned))

#' 
#' For each APMC-Commodity group, the ratio of its standard deviation in modal_prices (de-seasoned) to the standard deviation of the corresponding commodity across APMCs, over the given period (Sep 2014 - Nov 2016), gives an idea of the fluctuation of that APMC-Commodity.
#' 
## ------------------------------------------------------------------------
# select required columns
fluc_analysis <- subset(temp1, 
                        select=c("district_name", "APMC", "Commodity", "date", "de_seasoned"))
fluc_analysis <- fluc_analysis %>% group_by(APMC, Commodity, year(date)) %>% 
                              summarise(mean_price = mean(de_seasoned), 
                                        std_price = sd(de_seasoned))
colnames(fluc_analysis)[3] <- "year"
fluc_analysis<- arrange(fluc_analysis, APMC, Commodity, year)

fluc_analysis <- as.data.frame(fluc_analysis) # becuase this is a dplyr dibble.

# ratio of two sds
fluc_analysis <- fluc_analysis %>% group_by(Commodity, year) %>% 
  mutate(ratio_sds = std_price/(msp_data_new[msp_data_new$commodity==as.character(Commodity) & msp_data_new$year==year, "std_deseasoned"]))

fluc_analysis$ratio_sds = round(fluc_analysis$ratio_sds, 2) 
summary(fluc_analysis$ratio_sds) # summary of fluctuations
# filter(fluc_analysis, year==2016 & ratio_sds >= 2) # do this without year and with year to see flluctuations overall and in each year respectively

#' In 2014, two groups have ratio > 2. And, while there are more groups with ratios between 1 and 1.5 in each year, in 2015-16, there are no groups with ratios > 1.5. Thus the fluctuations in 2014 is the greatest in the given data. However, it is important to note that this could largely be due to the very low size of data in 2014 (only from Sep to Dec).
#' 
#' Then, flag sets of APMC-Commodity with the highest fluctuations in each year. I also checked that there is only one maximum. Incidentally, the fluctuations in 2014 is also the overall highest.
## ------------------------------------------------------------------------
fluc_analysis %>% group_by(year) %>% summarise(max(ratio_sds))
fluc_analysis[fluc_analysis$year == 2014 & fluc_analysis$ratio_sds == 2.33, ] # highes in 2014
fluc_analysis[fluc_analysis$year == 2015 & fluc_analysis$ratio_sds == 1.39, ] # highes in 2015
fluc_analysis[fluc_analysis$year == 2016 & fluc_analysis$ratio_sds == 1.32, ] # highes in 2016

#' 
#' Next, the the prices of APMC-Commodity group with highest flutuations in each year (and overall) is plotted over time to see the trend visually.
#' 
## ------------------------------------------------------------------------
overall_prices <- temp1[temp1$APMC == "Parli-Vaijnath" & temp1$Commodity == "pigeon pea (tur)", c("date","de_seasoned")]
prices_2014 <- temp1[temp1$APMC == "Parli-Vaijnath" & temp1$Commodity == "pigeon pea (tur)"
                     & year(temp1$date) == 2014, c("date","de_seasoned")]
prices_2015 <- temp1[temp1$APMC == "Digras" & temp1$Commodity == "pigeon pea (tur)"
                     & year(temp1$date) == 2015, c("date","de_seasoned")]
prices_2016 <- temp1[temp1$APMC == "Murum" & temp1$Commodity == "pigeon pea (tur)"
                     & year(temp1$date) == 2016, c("date","de_seasoned")]
library("scales")
overall_prices$date <- as.POSIXct(overall_prices$date)
prices_2014$date <- as.POSIXct(prices_2014$date)
prices_2015$date <- as.POSIXct(prices_2015$date)
prices_2016$date <- as.POSIXct(prices_2016$date)

ggplot()+
  geom_line(data=overall_prices, aes(x=date, y=de_seasoned, color="overall"), size=1.5) + 
  geom_line(data=prices_2014, aes(x=date, y=de_seasoned, color="2014"), size=2) +
  geom_line(data=prices_2015, aes(x=date, y=de_seasoned, color="2015"), size=2) +
  geom_line(data=prices_2016, aes(x=date, y=de_seasoned, color="2016"), size=2) + 
  scale_x_datetime(date_breaks = "3 month", labels = date_format("%m-%Y")) +
  xlab("Time period") +
  ylab("De-seasoned price") +
  ggtitle("APMC-Commodites with highest price fluctuations") +
  theme(plot.title = element_text(hjust = 0.5))

#' 
#' Now, if our data have had clear seasoning effect, fluctuations in each such season could have been easily computed. However, there are no clear seasons in our data, perhaps due to its small size. Hence, I'm manually dividing the time period into sub-periods to analyze the fluctuations in each sub-period.
#' 
#' I'm repeating the same process above to identify fluctuations in different constructed seasons. Here, I'm dividing the time period from Sep 2014 to Nov 2016 into 5 seasons as given in the code below. I'm considering the months from June to December of each year as rainy season and the months from January to May of each year as dry season. This is a simple and quick segregation to analyze the flucutation in different sub-periods.
#' 
## ------------------------------------------------------------------------
temp2<-temp1
temp2$season <- ifelse(temp2$date >= "2014-09-01" & temp2$date <= "2014-12-01", "rainy_2014", 
                ifelse(temp2$date >= "2015-01-01" & temp2$date <= "2015-05-01", "dry_2015",
                ifelse(temp2$date >= "2015-06-01" & temp2$date <= "2015-12-01", "rainy_2015",
                ifelse(temp2$date >= "2016-01-01" & temp2$date <= "2016-05-01", "dry_2016",
                ifelse(temp2$date >= "2016-06-01" & temp2$date <= "2016-12-01", "rainy_2016","no_season")))))

season_data <- temp2 %>% group_by(season, Commodity) %>% 
                        summarise(mean_season = mean(de_seasoned), std_season = sd(de_seasoned))
# the above has 14 commodities for 5 seasons, hence total 70 rows.

fluc_season <- subset(temp2, 
                        select=c("district_name", "APMC", "Commodity", "season", "de_seasoned"))
fluc_season <- fluc_season %>% group_by(APMC, Commodity, season) %>% 
  summarise(mean_price = mean(de_seasoned), 
            std_price = sd(de_seasoned))
fluc_season<- arrange(fluc_season, APMC, Commodity, season)

fluc_season <- as.data.frame(fluc_season) # becuase this is dibble,.
season_data <- as.data.frame(season_data)

fluc_season <- fluc_season %>% group_by(Commodity, season) %>% 
  mutate(ratio_sds = std_price/(season_data[season_data$Commodity==as.character(Commodity) & season_data$season==season, "std_season"]))

fluc_season$ratio_sds = round(fluc_season$ratio_sds, 2) 

fluc_season %>% group_by(season) %>% summarise(max(ratio_sds))
# APMC-Commodity with highest fluctuations in each season can be found using the command below with different values.
# fluc_season[fluc_season$season == "dry_2015"  & fluc_season$ratio_sds == 1.35, ]


#' 
## ---- include=FALSE------------------------------------------------------
# save required dataframes
save(grouped_data1,file="grouped_data1.Rdata")
save(data1_analysis,file="data1_analysis.Rdata")

