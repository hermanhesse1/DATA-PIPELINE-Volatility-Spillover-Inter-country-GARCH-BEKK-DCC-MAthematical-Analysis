#candidate number 49537

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(PerformanceAnalytics)
  library(rugarch)
  library(rmgarch)
  library(timeSeries)
  library(gridExtra)
  library(urca)
  library(moments)
  library(tseries)
  library(knitr)
  library(kableExtra)
  library(readxl)
  library(PerformanceAnalytics)
  library(zoo)
  library(mgarchBEKK)
  library(igraph)
  library(zoo)
  library(ggplot2)
  library(dplyr)
  library(rmgarch)
  library(MASS)
  library(zoo)
  library(quantmod)
  library(tseries)
  library(lmtest)
  library(vars)
})

library(tidyverse)
library(readxl)
library(PerformanceAnalytics)
library(rugarch)
library(rmgarch)
library(timeSeries)
library(gridExtra)
library(urca)
library(moments)
library(tseries)
library(knitr)
library(kableExtra)
library(readxl)
library(PerformanceAnalytics)
library(zoo)
library(mgarchBEKK)
library(igraph)
library(zoo)
library(ggplot2)
library(dplyr)
library(rmgarch)
library(MASS)
library(zoo)
library(quantmod)
library(tseries)
library(lmtest)
library(vars)


indices_df <- data.frame(
  Country = c("Germany", "Hungary", "Czech Republic", "Poland"),
  Index = c("DAX", "BUX", "PX", "WIG")
)

kable(indices_df, format = "html", caption = "European Stock Market Indices") %>%
  kable_styling(
    full_width        = FALSE,
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

conflictRules("dplyr", exclude = c("filter", "lag"))
options(xts.warn_dplyr_breaks_lag = FALSE)

# Read and clean data
data <- read_excel("BerlinBudapestBucharestPrague.xlsx")

# Remove duplicates
data_new <- data[!duplicated(data), ]

# Ensure proper date format and remove rows with invalid dates
data_new$Date <- as.Date(data_new$Date, "%Y-%m-%d")
data_new <- data_new[!is.na(data_new$Date), ]

# Convert data to zoo object
data_zoo <- read.zoo(data_new, order.by = data_new$Date)

start_date <- as.Date("2022-02-01")
end_date <- as.Date("2023-01-01")




# Assuming your data is stored in `data_zoo`
filtered_data <- window(data_zoo, start = start_date, end = end_date)
bux <- filtered_data[, "BUX", drop = FALSE]
dax <- filtered_data[, "DAX", drop = FALSE]
wig <- filtered_data[, "WIG", drop = FALSE]
px <- filtered_data[, "PX", drop = FALSE]

pdax_price = fortify.zoo(dax)
plotdax_price = pdax_price %>%
  ggplot(aes(x = Index, y = DAX)) +
  geom_line(color = "#69b3a2") +
  ggtitle("DAX Prices") +
  ylab("Price") +
  xlab("Date") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme_classic()

pbux_price = fortify.zoo(bux)
plotbux_price = pbux_price %>%
  ggplot(aes(x = Index, y = BUX)) +
  geom_line(color = "#69b3a2") +
  ggtitle("BUX Prices") +
  ylab("Price") +
  xlab("Date") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme_classic()

pwig_price = fortify.zoo(wig)
plotwig_price = pwig_price %>%
  ggplot(aes(x = Index, y = WIG)) +
  geom_line(color = "#69b3a2") +
  ggtitle("WIG Prices") +
  ylab("Price") +
  xlab("Date") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme_classic()

ppx_price = fortify.zoo(px)

plotpx_price = ppx_price %>%
  ggplot(aes(x = Index, y = PX)) +
  geom_line(color = "#69b3a2") +
  ggtitle("PX Prices") +
  ylab("Price") +
  xlab("Date") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme_classic()


calculate_log_returns <- function(index) {
  if (!is.null(index)) {
    return(CalculateReturns(index, method = "log"))
  }
  return(NULL)
}

grid.arrange(plotbux_price, plotwig_price, plotpx_price, plotdax_price, ncol = 2, nrow = 2)

conflictRules("dplyr", exclude = c("filter", "lag"))
options(xts.warn_dplyr_breaks_lag = FALSE)

dax <- filtered_data[, "DAX", drop = FALSE]
r_dax <- calculate_log_returns(dax)

bux <- filtered_data[, "BUX", drop = FALSE]
r_bux <- calculate_log_returns(bux)

wig <- filtered_data[, "WIG", drop = FALSE]
r_wig <- calculate_log_returns(wig)

px <- filtered_data[, "PX", drop = FALSE]
r_px <- calculate_log_returns(px)

# Remove NA values from returns for plotting
r_dax <- na.omit(r_dax)
r_bux <- na.omit(r_bux)
r_wig <- na.omit(r_wig)
r_px <- na.omit(r_px)

# Function to plot data
plot_returns <- function(data, title, color) {
  data_fortified <- fortify.zoo(data)
  ggplot(data_fortified, aes(x = Index, y = coredata(data))) +
    geom_line(color = color) +
    ggtitle(title) +
    ylab("Return Rate") +
    xlab("Date") +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    theme_classic()
}

# Plot each index
plot_dax <- plot_returns(r_dax, "DAX", "#69b3a2")
plot_bux <- plot_returns(r_bux, "BUX", "#69b3a2")
plot_wig <- plot_returns(r_wig, "WIG", "#69b3a2")
plot_px <- plot_returns(r_px, "PX", "#69b3a2")

grid.arrange(plot_bux, plot_wig, plot_px, plot_dax, ncol = 2, nrow = 2)

des = merge(r_wig, r_px, r_bux, r_dax)
des = des[-1,]
k=ncol(des)



options(digits=8)

SummaryStatistics = function(data){
  moments = matrix(NA, ncol=ncol(data), nrow=14)
  colnames(moments)=colnames(data)
  rownames(moments)=c("Mean","Variance","Skewness","","Kurtosis","","JB","","ERS","","Q(20)","","Q2(20)","")
  for (i in 1:ncol(data)){
    moments[1,i] = mean(data[,i])
    moments[2,i] = var(data[,i])
    skew = moments::agostino.test(data[,i])
    moments[3,i] = skew$statistic[1]
    moments[4,i] = skew$p.value
    kurt = moments::anscombe.test(data[,i])
    moments[5,i] = kurt$statistic[1]-3
    moments[6,i] = kurt$p.value
    jb = moments::jarque.test(as.numeric(data[,i]))
    moments[7,i] = jb$statistic
    moments[8,i] = jb$p.value
    ers = urca::ur.ers(data[,i],type="DF-GLS",model="constant")
    moments[9,i] = ers@teststat
    moments[10,i]= ers@testreg$coefficients[1,4]
    bt = WeightedPortTest::Weighted.Box.test(data[,i], type="Ljung-Box", lag=20)
    moments[11,i] = bt$statistic
    moments[12,i] = bt$p.value
    bt2 = WeightedPortTest::Weighted.Box.test(data[,i], type="Ljung-Box", lag=20, sqrd.res=T)
    moments[13,i] = bt2$statistic
    moments[14,i] = bt2$p.value
  }
  
  cc=c(4,6,8,10,12,14)
  moments = round(moments,6)
  moments1 = moments
  for (j in 1:k){
    for (i in 1:length(cc)){
      i = cc[i]
      if (moments[i,j]<=0.01) {
        moments1[(i-1),j] = paste(format(round(moments[(i-1),j],3),nsmall=3),"***",sep="")
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      } else if (moments[i,j]<=0.05) {
        moments1[(i-1),j] = paste(format(round(moments[(i-1),j],3),nsmall=3),"**",sep="")
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      } else if (moments[i,j]<=0.10) {
        moments1[(i-1),j] = paste(format(round(moments[(i-1),j],3),nsmall=3),"*",sep="")
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      } else {
        moments1[(i-1),j] = format(round(moments[(i-1),j],3),nsmall=3)
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      }
    }
  }
  
  for (j in 1:k){
    i = 9
    if (moments[i,j]<=-2.57) {
      moments1[i,j] = paste(format(round(moments[i,j],3),nsmall=3),"***",sep="")
    } else if (moments[i,j]<=-1.96) {
      moments1[i,j] = paste(format(round(moments[i,j],3),nsmall=3),"**",sep="")
    } else if (moments[i,j]<=-1.62) {
      moments1[i,j] = paste(format(round(moments[i,j],3),nsmall=3),"*",sep="")
    } else {
      moments1[i,j] = format(round(moments[i,j],3),nsmall=3)
    }
  }
  moments1
}

stats <- SummaryStatistics(des)


kable(stats, 
      caption = "Summary Statistics of the Return Series", 
      escape = FALSE) %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover", "condensed", "responsive"))


matrix = as.matrix(cbind(r_dax, r_bux, r_wig, r_px))
Y=abs(100*matrix)
colnames(Y)=c("DAX","BUX", "WIG", "PX")
N=nrow(Y)


# Removes NAs from dates if present

start_date <- as.Date("2022-02-01")
end_date   <- as.Date("2023-01-01")

# Create a date sequence, one day at a time
date <- seq(from = start_date, to = end_date, by = "day")

# Format each date as "dd/mm/yyyy"
date <- format(date, "%d/%m/%Y")



spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model=list(armaOrder=c(0,0), include.mean=TRUE), 
                   distribution.model="std",fixed.pars=list(omega=0))

uspec.n = multispec(replicate(4, spec)) 
multf = multifit(uspec.n, Y)
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvt')
fit1 = dccfit(spec1, Y, fit.control = list(eval.se = TRUE), fit = multf )#VAR.fit = A_var
#fit1@fit$solver$sol$pars
####TABLE: DCC-parameters estimation 
param<-fit1@mfit$matcoef

kable(param, caption = "DCC-GARCH Parameter Estimates",
      col.names = c("Estimate", "Std. Error", "t value", "Pr(> |t|)"))


cor1 = rcor(fit1)  # extracts the correlation matrix



cor11=cor1[1,,] # extract colomn  of dynamic correlation of each 



cor111<-cor11[,]
k=nrow(cor111)
T <- dim(cor1)[3]
date <- seq(from = start_date, by = "day", length.out = T)
length(date) == ncol(cor111)



par(mfrow =  c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.02, mar = c(1,1,1,1) + .02, mgp = c(0, 0.1, 0))
for (i in 1:k) {
  plot(date,cor111[i,],xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1,
       main=paste("DAX with",rownames(cor111)[i])) 
  
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,ncol(cor111))),rev(cor111[i,])),col="grey20", border="grey20")
  box()
}

plot.tab.tex <- function(data){
  
  data <- as.matrix(filtered_data)
  
  c <- ncol(data)
  r <- nrow(data)
  
  cat("\\begin{tabular}{l")
  for(j in 1:c){cat("r")}
  cat("} \n")
  
  cat("  &  ")
  for (j in 1:c){
    if ( j < c){
      cat(colnames(data)[j])
      cat("  &  ")
    } else if (j == c){
      cat(colnames(data)[j])
      cat(" \\\\ \\hline \\hline\n")}
  }
  
  for(i in 1:r){
    cat(rownames(data)[i])
    cat("  &  ")
    for (j in 1:c){
      if ( j < c){
        #cat("$")
        cat(data[i,j])
        #cat("$")
        cat("  &  ")
      } else if (j == c & !((i/2) == floor(i/2))){
        cat(data[i,j])
        cat(" \\\\ \n")
      }else if (j == c & ((i/2) == floor(i/2))){
        cat(data[i,j])
        cat(" \\\\ \n")}
    }
  }
  cat("\\end{tabular}")
}


# Removes NAs from dates if present

start_date <- as.Date("2022-02-01")
end_date   <- as.Date("2023-01-01")

# Create a date sequence, one day at a time
date <- seq(from = start_date, to = end_date, by = "day")

# Format each date as "dd/mm/yyyy"
date <- format(date, "%d/%m/%Y")

Y=abs(100*matrix) 

colnames(Y)=c("DAX","BUX","WIG","PX")
# change here for forex c("GBP","EUR","HKD","CNY","JPY","AUD", "THB", "INR","TRY")


k = ncol(Y)
################## DY 2012 - Table of  Volatility Spillover  

#####create function for  the forecast error variance decomposition
FEVD <- function(model, n.ahead=10,normalize=TRUE,standardize=TRUE) {
  if (class(model) != "varest") {
    return("The model class is not varest!")
  }
  A <- Phi(model, (n.ahead-1))
  epsilon <- residuals(model)
  Sigma <- t(epsilon)%*%epsilon / (model$obs)
  gi <- array(0, dim(A))
  sigmas <- sqrt(diag(Sigma))
  for (j in 1:dim(A)[3]) {
    gi[,,j] <- t(A[,,j]%*%Sigma%*%solve(diag(sqrt(diag(Sigma)))))
  }
  
  if (standardize==TRUE){
    girf=array(NA, c(dim(gi)[1],dim(gi)[2], (dim(gi)[3])))
    for (i in 1:dim(gi)[3]){
      girf[,,i]=((gi[,,i])%*%solve(diag(diag(gi[,,1]))))
    }
    gi = girf
  }
  
  num <- apply(gi^2,1:2,sum)
  den <- c(apply(num,1,sum))
  fevd <- t(num)/den
  if (normalize==TRUE) {
    fevd=(fevd/apply(fevd, 1, sum))
  } else {
    fevd=(fevd)
  }
  return = list(fevd=fevd, girf=gi)
}

############### create function  for Dynamics of Total, Directional, Net  spillovers
SM<-function(A){ 
  k = dim(A)[1]
  SOFM = apply(A,1:2,mean)*100
  VSI = (sum(rowSums(SOFM-diag(diag(SOFM))))/k)# or similarly (sum(colSums(SOFM-diag(diag(SOFM))))/k)
  INC = colSums(SOFM)
  TO = colSums(SOFM-diag(diag(SOFM)))
  FROM = rowSums(SOFM-diag(diag(SOFM)))
  NET = TO-FROM
  NPSO = t(SOFM)-SOFM
  
  ALL = rbind(rbind(rbind(cbind(SOFM,FROM),c(TO,sum(TO))),c(INC,NA)),c(NET,VSI))
  
  colnames(ALL) = c(rownames(A),"DFO")
  rownames(ALL) = c(rownames(A),"DTO","DIO","NDS")
  ALL = format(round(ALL,2),nsmall=2) 
  ALL[nrow(ALL)-1,ncol(ALL)] = "SI" 
  return = list(SOFM=SOFM,VSI=VSI,TO=TO,FROM=FROM,NET=NET,ALL=ALL,NPSO=INC)  
}
nlag = 2  # VAR(2)
nfore = 10 # 10-step ahead forecast
A_var = VAR(Y,p=nlag,type ="const")
A_ALL = FEVD(A_var, n.ahead=nfore)$fevd
colnames(A_ALL) = rownames(A_ALL) = colnames(Y)
A_var = SM(A_ALL)
#TABLE OF VOLATILITY SPILLOVER
#View(A_var$ALL)


t = nrow(Y)
k=ncol(Y)
space =150 
A = array(NA, c(k, k, (t-space)))
colnames(A) = rownames(A) = colnames(Y)
for (i in 1:dim(A)[3]) {
  varst = VAR(Y[i:(space+i-1),], p=nlag, type="const")
  A[,,i] = FEVD(varst, n.ahead=nfore)$fevd
}

#Create matrix for directional spillovers 
to = matrix(NA, ncol=k, nrow=(t-space))
from = matrix(NA, ncol=k, nrow=(t-space))
net = matrix(NA, ncol=k, nrow=(t-space))
npso = array(NA,c(k,k,(t-space)))
total = matrix(NA,ncol=1,nrow=(t-space))
for (i in 1:dim(A)[3]){#iterate to each Dynamics of the matrix  
  vd = SM(A[,,i])
  to[i,] = vd$TO/k
  from[i,] = vd$FROM/k
  net[i,] = vd$NET/k
  npso[,,i] = c(t(vd$SOFM)-vd$SOFM)/k
  total[i,] = vd$VSI
}


date = date[-c(1:space)]

T = total
start_date <- as.Date("2022-02-01")  # or your actual first data date
date <- seq(from = start_date, by = "day", length.out = nrow(T))


# DIRECTIONAL VOLATILITY SPILLOVERS (TO)
par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.02, mar = c(1,1,1,1) + .02, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,to[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1,
       main=paste(colnames(Y)[i],"TO all others"),ylim=c(floor(min(to)),ceiling(max(to))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(to))),rev(to[,i])),col="grey20", border="grey20")
  box()
}

par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,net[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=paste("NET",colnames(Y)[i]),ylim=c(floor(min(net)),ceiling(max(net))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(net))),rev(net[,i])),col="grey20", border="grey20")
  box()
}
# NET PAIRWISE VOLATILITY SPILLOVERS
graphics.off()
nps = array(NA,c((t-space),k/2*(k-1)))
colnames(nps) = 1:ncol(nps)
jk = 1
for (i in 1:k) {
  for (j in 1:k) {
    {if (j<=i) {next} else
      nps[,jk] = npso[i,j,]
    colnames(nps)[jk] = paste0(colnames(Y)[i],"-",colnames(Y)[j])
    jk = jk + 1
    }
  }
}
par(mfrow = c(ceiling(ncol(nps)/2),2), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
for (i in 1:ncol(nps)) {
  plot(date,nps[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=colnames(nps)[i],tck=0.02,yaxs="i",ylim=c(floor(min(nps)),ceiling(max(nps))))
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(nps))),rev(nps[,i])),col="grey20", border="grey20")
  box()
}

rec = list(r_wig, r_px, r_bux)
varname = c("r_wig","r_px", "r_bux")
z = "daxbekk"

for (i in 1:length(rec)) {
  cat("Running BEKK model for pair:", i, "\n")
  
  y <- fortify.zoo(merge(r_dax, rec[[i]], all = FALSE))
  y <- subset(y, select = -c(Index))
  y <- y[-c(1), ]
  y <- as.matrix(y)
  
  # Redirect all output to /dev/null (Mac/Linux)
  capture.output({
    suppressWarnings(suppressMessages({
      x <- BEKK(y, order = c(1, 1))
    }))
  }, file = "/dev/null")
  
  assign(paste(z, varname[i], sep = ""), x)
}




# Data for BEKK Model Diagnostics
diagnostic_data_2 <- data.frame(
  Parameter = c(
    "Number of estimated series", "Length of estimated series", "Estimation Time", "Total Time",
    "BEKK Order", "AIC", "Eigenvalue 1", "Eigenvalue 2", "Eigenvalue 3", "Eigenvalue 4",
    "Unconditional Covariance (1,1)", "Unconditional Covariance (1,2)", "Unconditional Covariance (2,2)",
    "Variance of Residuals (1)", "Mean of Residuals (1)", "Variance of Residuals (2)", "Mean of Residuals (2)"
  ),
  Value = c(
    474, 237, 0.4615, 0.4713, "1 1", -1305.46, 0.4723, 0.2218, 0.0109, 0.0063,
    0.00024392789, 0.00012491454, 0.00037960014, 0.99536367, -0.010196098, 1.0017018, -0.028750347
  )
)

# Data for C Matrix Estimates
c_matrix_data_2 <- data.frame(
  Parameter = c("C (1,1)", "C (1,2)", "C (2,2)"),
  Estimate = c(0.012634168, 0.0072716043, 0.0112361777),
  `Standard Error` = c(0.00097141881, 0.0017620287, 0.0038511722)
)

# Data for ARCH Matrix Estimates
arch_matrix_data_2 <- data.frame(
  Parameter = c("ARCH (1,1)", "ARCH (1,2)", "ARCH (2,1)", "ARCH (2,2)"),
  Estimate = c(0.044495905, 0.029331242, 0.389021419, 0.532592329),
  `Standard Error` = c(0.095825551, 0.14106316, 0.10688349, 0.14765856)
)

# Data for GARCH Matrix Estimates
garch_matrix_data_2 <- data.frame(
  Parameter = c("GARCH (1,1)", "GARCH (1,2)", "GARCH (2,1)", "GARCH (2,2)"),
  Estimate = c(-0.107896651, 0.49689354, -0.021186135, 0.10998657),
  `Standard Error` = c(0.11635894, 0.50875593, 0.067518583, 0.12568488)
)

# Generate tables using kable
kable(diagnostic_data_2, caption = "BEKK Model Diagnostics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

kable(c_matrix_data_2, caption = "C Matrix Estimates") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

kable(arch_matrix_data_2, caption = "ARCH Matrix Estimates") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

kable(garch_matrix_data_2, caption = "GARCH Matrix Estimates") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))






