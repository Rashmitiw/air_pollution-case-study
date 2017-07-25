## Has fine particle pollution in the U.S. decreased from 1999 to 2012?

#read table from 1999
# comment.char = # ignor line start with hash table 
pm99 <- read.table("RD_501_88101_1999-0.txt",comment.char = "#",header = FALSE,sep = "|",na.strings = "")

# check dimension

dim(pm99)

# top rows of table

head(pm99)

# Assign col names to the varible cnames from the text file

cnames <- readLines("RD_501_88101_1999-0.txt",1)
cnames

# Split all the names out

cnames <- strsplit(cnames,"|",fixed=TRUE)

# Assign the col names to tables

names(pm99)<- cnames[[1]]

# col names have spaces b/w them so use make.names fuc take arbitrary string and turn it into valid names

names(pm99)<- make.names(cnames[[1]])

#look out at sample.value col
x0<-pm99$Sample.Value
class(x0)
#display internal structure of x0

str(x0)

summary(x0)

# percentage of missing NA values

mean(is.na(x0)) 
# about 11 percet missing

# read 2012 data
# comment.char = # ignor line start with hash table 
pm12 <- read.table("RD_501_88101_2012-0.txt",comment.char = "#",header = FALSE,sep = "|",na.strings = "")
# check dimension

dim(pm12)

names(pm12)<- make.names(cnames[[1]])

head(pm12)

x1<-pm12$Sample.Value

str(x1)

summary(x1)

mean(is.na(x1))

# compare 2012 and 1999 data

summary(x1)

summary(x0)

# visual representation of data
# box plot
boxplot(x0,x1)

boxplot(log10(x0),log10(x1))

#look at negative values 
summary(x1)

negative <- x1<0
str(negative)

sum (negative,na.rm=TRUE)
mean(negative,na.rm=TRUE)

#date of measurement
dates<- pm12$Date

str(dates)

#convert date as date format
dates<- as.Date(as.character(dates),"%Y%m%d")

str(date)

# histogram of date

hist(dates,"month")
# negative data histogram

hist(dates[negative],"month")


#exploring change at one monitor

#subset PM data and look out all monitors in New York State

site0<- unique(subset(pm99,State.Code==36 , c(County.Code,Site.ID)))
site1<- unique(subset(pm12,State.Code==36 , c(County.Code,Site.ID)))

head(site0)

#paste county code and siteID 

site0<-paste(site0[,1],site0[,2],sep = ".")
site1<-paste(site1[,1],site1[,2],sep=".")

head(site0)
str(site0)

str(site1)

#what is the interaction between two
both <- intersect(site0,site1)
both
# 10 results
# how many observation are available each model
# new variabl county.site

pm99$county.site<-with(pm99,paste(County.Code,Site.ID,sep = "."))
pm12$county.site<-with(pm12,paste(County.Code,Site.ID,sep = "."))

#subset for newyork state

cnt0<- subset(pm99,State.Code==36 & county.site %in% both)
cnt1<- subset(pm12,State.Code==36 & county.site %in% both)

# Split dataframe by each montitor

split(cnt0,cnt0$county.site)

sapply(split(cnt0,cnt0$county.site),nrow)

# compare to 2012 cnt1
sapply(split(cnt1,cnt1$county.site),nrow)

#took count.site = 63.2008

pm12sub <-subset(pm12,State.Code==36 & County.Code==63 & Site.ID==2008)

pm99sub <-subset(pm99,State.Code==36 & County.Code==63 & Site.ID==2008)

dim(pm12sub)
dim(pm99sub)

# plot the data pm25 as functional time 
# time series
# to see pm25 gone down over 13 year period
#plot data for 2012

dates1 <-pm12sub$Date
x1sub <- pm12sub$Sample.Value

plot(dates1,x1sub) 
# convert date into proper format
dates1<- as.Date(as.character(dates1),"%Y%m%d")
str(dates1)

plot(dates1,x1sub)

#plot data for 1999

dates0 <-pm99sub$Date
x0sub <- pm99sub$Sample.Value

# convert date into proper format
dates0<- as.Date(as.character(dates0),"%Y%m%d")
str(dates0)

plot(dates0,x0sub)


#buiding a panel plot for both observation

par(mfrow=c(1,2),mar= c(4,4,2,1))
plot(dates0,x0sub,pch=20)
#median line 

abline(h=median(x0sub,na.rm=T))
plot(dates1,x1sub,pch=20)
abline(h=median(x1sub,na.rm=T))

#--look unusal ..values are going up but Y axis of both are different 
# median in 1999 - above 10 median in 2012 - under 10
#so its miseading - -- need to plot on same range

# for same range use range fucntion

rng<- range(x0sub,x1sub,na.rm=T)

#again plot

par(mfrow= c(1,2))

plot(dates0,x0sub,pch=20,ylim=rng)
abline(h=median(x0sub,na.rm=T))
plot(dates1,x1sub,pch=20,ylim = rng)
abline(h=median(x1sub,na.rm=T))


# so its conclude that in 1999 problem was worse more pm25 value and in 2012 they overcome the problem

# exploring change at the state level

# look at different state
# take avg of sample.value by state

#take mean - tapply
mn0 <- with(pm99,tapply(Sample.Value,State.Code,mean,na.rm=T))
mn1 <- with(pm12,tapply(Sample.Value,State.Code,mean,na.rm=T))

summary(mn0)
summary (mn1)

#create dataframe  of -  id of the state and avg of sample value

d0<-data.frame(state=names(mn0),mean=mn0)
d1<-data.frame(state=names(mn1),mean=mn1)

head(d0)
head(d1)

# Merge d0 and d1 

mrg<- merge(d0,d1,by="state")
dim(mrg)
head(mrg)

#plot this data frame

par(mfrow =c(1,1))
with( mrg,plot(rep(1999,52),mrg[,2],xlim =c(1998,2013)))

with( mrg,points(rep(2012,52),mrg[,3]))

# connect the dots

segments(rep(1999,52),mrg[,2],rep(2012,52 ),mrg[,3])

