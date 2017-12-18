#original 'analysis_explore.R' on personal ASUS laptop

library(stats)
library(ggplot2)
library(spectral)
library(reshape2)
library(gridExtra)
se <- function(x){
  sd(x)/sqrt(length(x))
}

#this is now really slow...
cleanData2 <- function(df){
  for(i in 5:ncol(df)){
    df[,i][df[,i]>40 | df[,i]<33] <- mean(df[,i], na.rm = T)
    for(j in 1:nrow(df)){
      if(is.na(df[j,i]) & (j < 11 | j > nrow(df)-11)){
        df[j,i] <- mean(df[,i], na.rm = T)
      }
      if(is.na(df[j,i])){
        mean_index <- c(j-10:j+10)
        df[j,i] <- mean(df[mean_index,i], na.rm = T)
      }
    }
  }
  return(df)
}
groupData <- function(df,trmt){
  meta <- df[,1:4]
  a <- df[,-c(1:4)]
  return(data.frame(meta, 'avg' = rowMeans(a), 'se' = apply(a,1,se)))
}


##################################################################################
#Preping the master dataset(s)
# 1. import master.csv
# 2. remove NLD animals and activity data, remove eday -1, add weeks variable, add minutes variable

NLD = c('01','04','06','08','09','12','16','19','20','24','25','27','28','31')

#!!!maybe add function to concatenate all dataframes, auto drop redundant 
#!!!  columns (i.e. eday etc.), adjust to lowest row count
#concatenate and pick days
catCohort <- cbind(c1[1:nrow(c2),],c2[,-c(1,2)])
catCohort <- catCohort[catCohort$eday > -1,]

scrapeACT <- function(NLD_ids, cohort){
  v <- vector()
  cohort <- cohort[,-grep('act',names(cohort))]
  for (i in NLD_ids){
    v <- c(v,grep(i,names(cohort)))
  }
  CDR_trim <- cohort[,-v]
  CDR_trim <- data.frame("minutes" = 1:nrow(CDR_trim), "week" = floor((CDR_trim$eday)/7) + 1, CDR_trim)
  NLD_trim <- cohort[,v]
  NLD_trim <- data.frame("minutes" = 1:nrow(NLD_trim), "week" = floor((CDR_trim$eday)/7) + 1, 
                         cohort[,c(1,2)], NLD_trim)
  tempDFs <- list('CDR' = CDR_trim, 'NLD' = NLD_trim)
  return(tempDFs)
}
temp_data<-scrapeACT(NLD,catCohort)

#!!!add function to scrape temperature

baseCohort <- cbind(base1,base2[,-c(1,2)])
baseCohort <- baseCohort[,-grep('act',names(baseCohort))]
baseCohort <- data.frame("minutes" = 1:nrow(baseCohort), "week" = floor((baseCohort$eday)/7) + 1, baseCohort)

# 3. separate Mva treatment from Veh treatment, 'trmt' is list of treatment IDs, 'cohort' is scraped df
#     list must be mva treatments
MvacCDR <- c('03','05','07','15','21','22','30')
MvacNLD <- c('01','04','06','16','19','24','27','31')
MvacAll <- c(MvacCDR, MvacNLD)

sepTXT <- function(trmt,cohort){
  v <- vector()
  for (i in trmt){
    v = c(v,grep(i,names(cohort)))
  }
  MvCDR <- cleanData(cbind(cohort[,1:4],cohort[,v]))
  vehCDR <- cleanData(cohort[,-v])
  
  #make long data
  mvLong <- melt(MvCDR, id = c('minutes','week','eday','zt'))
  mvLong$trmt <- 1
  vehLong <- melt(vehCDR, id = c('minutes','week','eday','zt'))
  vehLong$trmt <- 2
  dataLong <- rbind(mvLong, vehLong)
  
  #make averages data
  mvAvg <- groupData(MvCDR)
  mvAvg$trmt <- 1
  vehAvg <- groupData(vehCDR)
  vehAvg$trmt <- 2
  dataAvg <- rbind(mvAvg, vehAvg)
  
  data <- list('Mvac' = MvCDR,'Veh' = vehCDR, 'long' = dataLong, 'avg' = dataAvg)
  return(data)
}

CDRdfs2 = sepTXT(MvacCDR,temp_data$CDR)
NLDdfs = sepTXT(MvacNLD,temp_data$NLD)

basedfs <- sepTXT(MvacAll, baseCohort)

#!!!maybe turn the 'meta' stuff into factors
#!!!add plotting functions

#rough plots
ggplot(CDRdfs$avg[CDRdfs$avg$week == 1,],aes(minutes,avg))+geom_point(aes(colour=factor(trmt)))+
  geom_ribbon(aes(ymin=avg-se, ymax=avg+se, x=minutes, fill = 'band', alpha = 0.2, colour=factor(trmt)))+
  geom_vline(xintercept=(1:7)*1440)

#rough plot for individual animals. vars include: trmt, week(s)
ggplot(CDRdfs$long[CDRdfs$long$trmt == 1 & CDRdfs$long$week == 1,], aes(x=minutes,y=value))+
  geom_point(aes(colour=factor(variable)))+
  geom_smooth(method = "loess", span = 0.1, aes(colour=factor(variable)))+
  geom_vline(xintercept=((1:7)+0)*1440)

plotAnimals <- function(df,trmt,week){
  dayMod = (week - 1)*7
  ggplot(df[df$trmt == trmt & df$week == week,], aes(x=minutes,y=value))+
    geom_point(aes(colour=factor(variable)))+
    geom_smooth(method = "loess", span = 0.1, aes(colour=factor(variable)))+
    geom_vline(xintercept=((1:7)+dayMod)*1440)
}

# 4. add function to test binning

squeezeData<- function(df,bin){
  a <- df[,-c(1,2,3,4)]
  meta <- df[,c(1,2,3,4)]
  meta <- meta[bin*(1:(nrow(meta)/bin)-1),]
  v <- vector()
  b <- list()
  for(i in 1:ncol(a)){
    #lazy way to avoid NAs in the last bin
    for(j in 0:(floor(nrow(a)/bin)-1)){
      v[j] <- mean(a[,i][(1:bin)+bin*j])
    }
  b[[i]] <- v
  }
  c <- data.frame(matrix(nrow = length(b[[i]]), ncol = ncol(a)))
  for(i in 1:ncol(a)){
    c[,i] <- b[[i]]
  }
  names(c) <- names(a)
  c <- data.frame(meta,c)
  return(c)
}

CDRtrim_b10 <- squeezeData(cleanData(temp_data$CDR),10)
CDRdfs_b10 <- sepTXT(MvacCDR, CDRtrim_b10) 

NLDtrim_b10 <- squeezeData(cleanData(temp_data$NLD[,-c(10,13)]),30)
NLDdfs_b10 <- sepTXT(MvacNLD, NLDtrim_b10)

BASEtrim_b30 <- squeezeData(cleanData(baseCohort),30)
BASEdfs_b30 <- sepTXT(MvacAll, BASEtrim_b30)

#rough plots of individual mice with smoothing
ggplot(CDRdfs_b10$long[(CDRdfs_b10$long$week == 8 & CDRdfs_b10$long$trmt==2),],aes(minutes,value))+
  geom_point(aes(colour=factor(variable)))+
  #geom_ribbon(aes(ymin=avg-se, ymax=avg+se, x=minutes, fill = 'band', alpha = 0.2, colour=factor(variable)))+
  geom_smooth(method = "loess", span = 0.1, aes(colour=factor(variable)))+
  geom_vline(xintercept=((1:7)+49)*1440)

ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 5,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7)+28)*1440)

#wrap weeks
p1<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 1,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7))*1440)
p2<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 2,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7)+7)*1440)
p3<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 3,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7)+14)*1440)
p4<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 4,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:2)+21)*1440)
p5<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 5,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7)+28)*1440)
p6<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 6,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7)+35)*1440)
p7<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 7,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7)+42)*1440)
p8<-ggplot(CDRdfs_b10$avg[CDRdfs_b10$avg$week == 8,],aes(minutes,avg,colour=factor(trmt)))+geom_point()+facet_wrap(~week)+
  geom_smooth(method = "loess", span = 0.05)+
  geom_vline(xintercept=((1:7)+49)*1440)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=3)


#get rid of zt and eday and convert to longitudinal data structure
MvCDR_long <- melt(mvaCDR[1:15000,-c(1,2)],id.vars = 'minutes')
MvCDR_long <- melt(mvaCDR_b10[1:1500,-c(1,2)],id.vars = 'minutes')

ggplot(MvCDR_long, aes(minutes,value))+geom_point(aes(colour = factor(variable)))+geom_vline(xintercept=(1:10)*1440)

VehCDR_long <- melt(mvaCDR[1:15000,-c(1,2)],id.vars = 'minutes')
ggplot(MvCDR_long, aes(minutes,value))+geom_point(aes(colour = factor(variable)))+geom_vline(xintercept=(1:7)*1440)

#Maybe add function to wrap week point plots



############################################################################################
#smoothing the data
#############################################################################################

a <- data.frame(minutes=CDRdfs_b10$avg[CDRdfs_b10$avg$week==5 & CDRdfs_b10$avg$trmt == 1,"minutes"] ,
                temp=CDRdfs_b10$avg[CDRdfs_b10$avg$week==5 & CDRdfs_b10$avg$trmt == 1,"avg"])

#use smooth.spline to interpolate data, pick which spar value looks best
spline1 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.1)$y, method = "spar = 0.1")
spline2 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.2)$y, method = "spar = 0.2")
spline3 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.3)$y, method = "spar = 0.3")
spline4 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.4)$y, method = "spar = 0.4")
spline5 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.5)$y, method = "spar = 0.5")
spline6 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.6)$y, method = "spar = 0.6")
spline7 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.7)$y, method = "spar = 0.7")
spline8 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.8)$y, method = "spar = 0.8")
spline9 <- data.frame(x = a$minutes, y = smooth.spline(a$temp, spar = 0.9)$y, method = "spar = 0.9")

#maybe make something to add zt lines

ggplot(rbind(spline1, spline2, spline3, spline4, spline5, spline6, spline7, spline8, spline9), aes(x,y))+
  geom_point(data = a, aes(minutes, temp), alpha = 0.05, col = 'red')+geom_line(col='blue')+facet_wrap(~method)+
  geom_vline(xintercept=((1:7)+28)*1440)

#pick the plot with the simplest but most accurate smoothing

mva_wk5_smooth <- spline3
veh_wk5_smooth <- spline3


###################################################################################
#the Fourier transform
#much of this comes from http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
#####################################################################################

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  #DGs, 11/28/2017, modified 'Mod(plot.data[,2])' to ignore the main freq
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[-1,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}


#the first frequency is very strong...maybe ignore
#according to Nyquist you only need to observefrequencies half the observation interval, so even 40 is pretty small, but we get the picture
#use your favorite spline

plot.frequency.spectrum(fft(mva_wk5_smooth$y),xlimits = c(1,40))
plot.frequency.spectrum(fft(veh_wk5_smooth$y), xlimits = c(1,40))


#spectral density estimation, also works on data frames
spec_mva <- spectrum(mva_wk5_smooth$y)
spec_veh <- spectrum(veh_wk5_smooth$y)
spec_df <- spectrum(data.frame(mva=mva_wk5_smooth$y,veh=veh_wk5_smooth$y))



###################################################################################
#Autocorrelation function
######################################################################################
acf(spline6$y)



###################################################################################
#spectral decomposition
###################################################################################


###################################################################################
#destructive interferance

base1 <- M3_base[1:week,]
base1$temp[is.na(base1$temp)] <- median(base1$temp, na.rm=T)
base1$temp[base1$temp>40] <- median(base1$temp, na.rm=T)
base1$minutes <- as.integer(rownames(base1))
base_spline <- data.frame(x = base1$minutes, y = smooth.spline(base1$temp, spar = 0.7)$y, method = "spar = 0.6")
base_spline$Norm <- base_spline$y/max(base_spline$y)

#base 2 for proof of concept 
base2 <- M3_base[week:((week*2)-1),]
base2$temp[is.na(base2$temp)] <- median(base2$temp, na.rm=T)
base2$temp[base2$temp>40] <- median(base2$temp, na.rm=T)
base2$minutes <- as.integer(rownames(base2))
base2_spline <- data.frame(x = base2$minutes, y = smooth.spline(base2$temp, spar = 0.7)$y, method = "spar = 0.6")
base2_spline$norm <- base2_spline$y/max(base2_spline$y)

destructb2 = base_spline$Norm - base2_spline$norm
destructb2df <- data.frame(x=base2_spline$x,y=destructb2)

ggplot(data = destructb2df, aes(x,y)) + geom_line(col='blue') + geom_vline(xintercept=(1:numDays)*1440)

#week 1
M3_cdrTrim <-  M3_cdr[M3_cdr$eday > -1,]
cdr1 <- M3_cdrTrim[1:week,]
cdr1$temp[is.na(cdr1$temp)] <- median(cdr1$temp, na.rm=T)
cdr1$temp[cdr1$temp>40] <- median(cdr1$temp, na.rm=T)
cdr1$minutes <- as.integer(rownames(cdr1))
cdr_spline <- data.frame(x = cdr1$minutes, y = smooth.spline(cdr1$temp, spar = 0.6)$y, method = "spar = 0.6")
cdr_spline$norm <- cdr_spline$y/max(cdr_spline$y)

destruct1 = base_spline$Norm - cdr_spline$norm
destructdf <- data.frame(x=base_spline$x,y=destruct1)

#week 2
cdr2 <- M3_cdr[M3_cdr$eday > 6 & M3_cdr$eday < 14,]
cdr2$temp[is.na(cdr2$temp)] <- median(cdr2$temp, na.rm=T)
cdr2$temp[cdr2$temp>40] <- median(cdr2$temp, na.rm=T)
cdr2$minutes <- as.integer(rownames(cdr2))
cdr2_spline <- data.frame(x = cdr2$minutes, y = smooth.spline(cdr2$temp, spar = 0.7)$y, method = "spar = 0.6")
cdr2_spline$norm <- cdr2_spline$y/max(cdr2_spline$y)

destruct2 = base_spline$Norm - cdr2_spline$norm
destruct2df <- data.frame(x=base_spline$x,y=destruct2)

ggplot(data = destruct2df, aes(x,y)) + geom_line(col='blue') + geom_vline(xintercept=(1:numDays)*1440)

#################################################################
#calculate inflection points
#1. get smooth data (loess)
#2. calculate points where diff(diff(data))==0 ...or something
#################################################################

differentiate <- function(df){
  y = (diff(df[,2])/diff(df[,1]))
  x = 1:length(y)
  df2 <- data.frame(dx = x, dy = y)
  return(df2)
}

#a is double differnetiated data frame
findInflection <- function(a){
  t = vector()
  max_diff <- diff(range(a[,2]))
  for(i in 2:nrow(a)){
    if(a[,2][i-1] < 0 & a[,2][i] > 0){
      if(abs(a[,2][i-1])+a[,2][i] < max_diff/20){
        t = c(t, a[,1][i])
      }
    }
    if(a[,2][i-1] > 0 & a[,2][i] < 0){
      if(a[,2][i-1]+abs(a[,2][i]) < max_diff/20){
      t = c(t, a[,1][i])
      }
    }
  }
  return(t)
}
  
#example
par(mfrow=c(1,1))
par(mfrow=c(3,1), mar=c(1.8,1,1.8,1))
lo <- loess(X03_temp~minutes, data=CDRdfs$Mvac[CDRdfs$Mvac$week==1,],span = 0.2)
plot(lo$fitted~lo$x, type = 'l')
smooth_data <- data.frame(x=lo$x,y=lo$fitted)

dLo <- differentiate(smooth_data)
plot(dLo$dy~dLo$dx, type='l', ylim=c(-0.005,0.005))
abline(h=0, col='red')

ddLo <- differentiate(dLo)
plot(ddLo$dy~ddLo$dx, type='l', ylim=c(-1/20000,1/20000))
abline(h=0, col='red')
times <- findInflection(ddLo)
abline(v=times, col='blue')

#################################################################
##Singular spectrum analysis
#################################################################

#decomposition, first week Mva treated animals only, lag of 1 day (data binned every 10 min)
s1=ssa(CDRdfs_b10$avg[CDRdfs_b10$avg$week==1 & CDRdfs_b10$avg$trmt==1,5], L=50)

plot(s1)
plot(s1, type = 'vectors')
plot(s1, type = 'paired')
plot(s1, type = 'series', groups = as.list(1:10))

#random: How do you measure phase shifts b/w waveforms
plot(CDRdfs_b10$avg[CDRdfs_b10$avg$week==1 & CDRdfs_b10$avg$trmt==1,5]-
       CDRdfs_b10$avg[CDRdfs_b10$avg$week==1 & CDRdfs_b10$avg$trmt==2,5])

