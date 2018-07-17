install.packages("plyr")
install.packages("dplyr")
install.packages("Information")
install.packages("caret")
install.packages("e1071")
install.packages("rpart.plot")
install.packages("unbalanced")
install.packages("mlr")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("cowplot")
library(plyr)
library(MASS)
library(dplyr)
library(Information)
library(caret)
library(e1071)
library(rpart.plot)
library(unbalanced)
library(mlr)
library(randomForest)
library(ggplot2)
library(cowplot)
#IMPORTING DATASETS
dem=read.csv("Demographic data.csv")
cred=read.csv("Credit Bureau data.csv")
#DATA CLEANING#
#checking for duplicate ids in demographic and credit bureau dataset
str(dem)
dem=dem[!duplicated(dem$Application.ID),]
str(dem)
str(cred)
cred=cred[!duplicated(cred$Application.ID),]
str(cred)
#MERGING BOTH DATASETS TO CREATE MASTER DATASET.
master=merge(x=dem,y=cred,by="Application.ID",all=TRUE)
str(master)
summary(master)
#Since Performance tag was in both datasets hence we evaluate if these columns are identical or different.
identical(master[['Performance.Tag.y']],master[['Performance.Tag.x']])
#SINCE BOTH COLUMNS ARE IDENTICAL HENCE WE REMOVE ONE OF THE COLUMNS AND RENAME THE OTHER COLUMN
# master=select(master,-Performance.Tag.x)
master <- master[,-12]
#Removing one Performance Tag column . 
colnames(master)[colnames(master)=="Performance.Tag.y"]="Performance.Tag"
#CHECKING FOR DUPLICATES IN MASTER 
master[duplicated(master$Application.ID),]
#THERE ARE NO DUPLICATES

#NOW WE WILL HANDLE MISSING VALUES, NEGATIVE VALUES AND OUTLIERS IN R
graphics.off()
par(mar=c(2,2,2,2))
boxplot(master$Age)
#master=master[!master$Age<=0,]
table(master$Age[master$Age<=0])
master$Age[master$Age<18]<-NA
#removing blank rows with blank values in gender(only 1 row)
master=master[!master$Gender=="",]
anyNA(master$Gender)
#removing blank rows with blank values in marital status(only 5 row)
master=master[!master$Marital.Status..at.the.time.of.application.=="",]
anyNA(master$Marital.Status..at.the.time.of.application.)
anyNA(master$No.of.dependents)
master=master[!is.na(master$No.of.dependents),]#removing 3 rows
table(master$Income==-0.5)## There 81 values with -0.5 as its income HENCE REPLACING WITH MODE COULD INTRODUCE BIAS
#master$Income[master$Income==-0.5]=0.0
master$Income[master$Income==-0.5]=NA
master[master$Education=="",]
#SINCE THERE ARE LOT OF ENTRIES WITH BLANK VALUE IN EDUCATION HENCE WE WILL REPLACE THESE BLANK VALUES BY MISSING FOR FUTURE ANALYSIS
master$Education=as.character(master$Education)
master$Education[master$Education==""]="Missing"
summary(master$Profession)
#from the observation we can see that the number of candidates with SAL profession is the highest hence we replace the blank values with SAL.
master$Profession=as.character(master$Profession)
table(master$Profession=="")#THERE ARE 14 BLANK VALUES IN PROFESSION HENCE WE CAN REPLACE IT WITH MODE
master$Profession[master$Profession==""]="SAL"
#CHECK FOR BLANK AND NA VALUES IN TYPE OF RESIDENCE
anyNA(master$Type.of.residence)
table(master$Type.of.residence=="")#There are 8 blank values in   TYPE OF RESIDENCE
master[master$Type.of.residence=="",]
summary(master$Type.of.residence)#MODE IN TYPE OF RESIDENCE IS "RENTED". Hence we replace blank values with RENTED
master$Type.of.residence=as.character(master$Type.of.residence)
master$Type.of.residence[master$Type.of.residence==""]="Rented"
anyNA(master$No.of.months.in.current.residence)
master[master$No.of.months.in.current.residence=="",]
boxplot(master$No.of.months.in.current.residence)
summary(master$No.of.months.in.current.residence)
#No of months in current residence is clean
master[master$No.of.months.in.current.company=="",]
boxplot(master$No.of.months.in.current.company)
summary(master$No.of.months.in.current.company)
#no of months in current company is clean
#Now we check missing data in 90,60,30 days past due in last 6 months
master[master$No.of.times.90.DPD.or.worse.in.last.6.months=="",]
boxplot(master$No.of.times.90.DPD.or.worse.in.last.6.months)
summary(master$No.of.times.90.DPD.or.worse.in.last.6.months)
master[master$No.of.times.60.DPD.or.worse.in.last.6.months=="",]
boxplot(master$No.of.times.60.DPD.or.worse.in.last.6.months)
summary(master$No.of.times.60.DPD.or.worse.in.last.6.months)
master[master$No.of.times.30.DPD.or.worse.in.last.6.months=="",]
boxplot(master$No.of.times.30.DPD.or.worse.in.last.6.months)
summary(master$No.of.times.30.DPD.or.worse.in.last.6.months)
#Now we check missing data in 90,60,30 days past due in last 12 months
master[master$No.of.times.90.DPD.or.worse.in.last.12.months=="",]
boxplot(master$No.of.times.90.DPD.or.worse.in.last.12.months)
summary(master$No.of.times.90.DPD.or.worse.in.last.12.months)
master[master$No.of.times.60.DPD.or.worse.in.last.12.months=="",]
boxplot(master$No.of.times.60.DPD.or.worse.in.last.12.months)
summary(master$No.of.times.60.DPD.or.worse.in.last.12.months)
master[master$No.of.times.30.DPD.or.worse.in.last.12.months=="",]
boxplot(master$No.of.times.30.DPD.or.worse.in.last.12.months)
summary(master$No.of.times.30.DPD.or.worse.in.last.12.months)
#Now we check for avg credit card utilization in last 12 months
anyNA(master$Avgas.CC.Utilization.in.last.12.months)
summary(master$Avgas.CC.Utilization.in.last.12.months)
#Now we check for no of trades opened in last six months
boxplot(master$No.of.trades.opened.in.last.6.months)
summary(master$No.of.trades.opened.in.last.6.months)
master=master[!is.na(master$No.of.trades.opened.in.last.6.months),]
summary(master)
##CLEANING AND EXPLORATORY DATA ANALYSIS COMPLETED
##WOE AND IV ANALYSIS TO IMPUTE MISSING VALUES IN AVG CC UTILIZATION, PRESENCE OF OPEN HOME LOAN, OUTSTANDING BALANCE
#CONVERTING CATEGORICAL INDEPENDENT VARIABLES INTO FACTORS AND DEPENDENT VARIABLE INTO NUMERIC FOR WOE AND IV
sapply(master,class)
master$Education=as.factor(master$Education)
master$No.of.dependents=as.factor(master$No.of.dependents)
master$Profession=as.factor(master$Profession)
master$Type.of.residence=as.factor(master$Type.of.residence)
master$Presence.of.open.home.loan=as.factor(master$Presence.of.open.home.loan)
master$Presence.of.open.auto.loan=as.factor(master$Presence.of.open.auto.loan)
master$Performance.Tag=as.numeric(master$Performance.Tag)
#SPLITTING THE DATASET ALONG REJECTED AND SELECTED APPLICANTS
rejected=subset(master,is.na(master$Performance.Tag))
selected=subset(master,!is.na(master$Performance.Tag))
woe_data=selected
IV <- create_infotables(data=woe_data, y="Performance.Tag", bins=10, parallel=FALSE)
woe_data$Age=as.numeric(woe_data$Age)
woe_data$Age=cut(woe_data$Age,breaks = c(14,30,35,38,41,44,47,50,53,57,65),labels = c("[18,30]", "[31,35]", "[36,38]", "[39,41]", "[42,44]", "[45,47]", "[48,50]", "[51,53]", "[54,57]", "[58,65]"))
#SINCE NA VALUE IN IV TABLE IS IN CHARACTER HENCE WE HAVE TO CONVERT NA VALUES FROM FACTOR TO CHARACTER IN WOE_DATA$AGE TO MAP THE ORIGINAL VALUES TO THE WOE VALUES
woe_data$Age=addNA(woe_data$Age)
levels(woe_data$Age) <- c(levels(woe_data$Age), "NA")
woe_data$Age[is.na(woe_data$Age)]="NA"
woe_data$Age=mapvalues(woe_data$Age,from = IV$Tables$Age$Age,to=IV$Tables$Age$WOE)
woe_data$Gender=mapvalues(woe_data$Gender,from = IV$Tables$Gender$Gender, to=IV$Tables$Gender$WOE)
woe_data$Marital.Status..at.the.time.of.application.=mapvalues(woe_data$Marital.Status..at.the.time.of.application.,from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to=IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
woe_data$No.of.dependents=mapvalues(woe_data$No.of.dependents,from = IV$Tables$No.of.dependents$No.of.dependents, to=IV$Tables$No.of.dependents$WOE)
woe_data$Education=mapvalues(woe_data$Education,from = IV$Tables$Education$Education,to=IV$Tables$Education$WOE)
woe_data$Profession=mapvalues(woe_data$Profession,from = IV$Tables$Profession$Profession,to=IV$Tables$Profession$WOE)
woe_data$Type.of.residence=mapvalues(woe_data$Type.of.residence,from = IV$Tables$Type.of.residence$Type.of.residence,to=IV$Tables$Type.of.residence$WOE)
#SAME WILL BE PERFORMED IN INCOME AS WE DID IN AGE
woe_data$Income=as.numeric(woe_data$Income)
woe_data$Income=cut(woe_data$Income,breaks = c(-1,5,10,16,21,26,31,36,41,48,60),labels = c("[0,5]", "[6,10]", "[11,16]", "[17,21]", "[22,26]", "[27,31]", "[32,36]", "[37,41]", "[42,48]", "[49,60]"))
woe_data$Income=addNA(woe_data$Income)
levels(woe_data$Income) <- c(levels(woe_data$Income), "NA")
woe_data$Income[is.na(woe_data$Income)]="NA"
woe_data$Income=mapvalues(woe_data$Income,from = IV$Tables$Income$Income,to=IV$Tables$Income$WOE)
#CHANGING NO OF MONTHS IN CURRENT RESIDENCE FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.months.in.current.residence=as.numeric(woe_data$No.of.months.in.current.residence)
woe_data$No.of.months.in.current.residence=cut(woe_data$No.of.months.in.current.residence,breaks = c(5,9,28,49,72,97,126),labels = c("[6,9]", "[10,28]", "[29,49]", "[50,72]", "[73,97]", "[98,126]"))
woe_data$No.of.months.in.current.residence=mapvalues(woe_data$No.of.months.in.current.residence,from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence,to=IV$Tables$No.of.months.in.current.residence$WOE)
#CHANGING NO OF MONTHS IN CURRENT company FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.months.in.current.company=as.numeric(woe_data$No.of.months.in.current.company)
woe_data$No.of.months.in.current.company=cut(woe_data$No.of.months.in.current.company,breaks = c(2,5,12,19,26,33,40,47,53,61,133),labels = c("[3,5]", "[6,12]", "[13,19]", "[20,26]", "[27,33]", "[34,40]","[41,47]","[48,53]","[54,61]","[62,133]"))
woe_data$No.of.months.in.current.company=mapvalues(woe_data$No.of.months.in.current.company,from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company,to=IV$Tables$No.of.months.in.current.company$WOE)
#CHANGING NO OF times 90 days past due or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months=as.numeric(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months=cut(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months,breaks = c(-1,0,3),labels = c("[0,0]", "[1,3]"))
woe_data$No.of.times.90.DPD.or.worse.in.last.6.months=mapvalues(woe_data$No.of.times.90.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 60 days past due or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months=as.numeric(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months=cut(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months,breaks = c(-1,0,5),labels = c("[0,0]", "[1,5]"))
woe_data$No.of.times.60.DPD.or.worse.in.last.6.months=mapvalues(woe_data$No.of.times.60.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 30 days past due or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months=as.numeric(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months)
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months=cut(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
woe_data$No.of.times.30.DPD.or.worse.in.last.6.months=mapvalues(woe_data$No.of.times.30.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 90 days past due FOR 12 MONTHS or more FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months=as.numeric(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months=cut(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,5),labels = c("[0,0]", "[1,1]","[2,5]"))
woe_data$No.of.times.90.DPD.or.worse.in.last.12.months=mapvalues(woe_data$No.of.times.90.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)
#CHANGING NO OF times 60 days past due or more FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months=as.numeric(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months=cut(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
woe_data$No.of.times.60.DPD.or.worse.in.last.12.months=mapvalues(woe_data$No.of.times.60.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)
#CHANGING NO OF times 30 days past due or more  FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months=as.numeric(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months)
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months=cut(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months,breaks = c(-1,0,2,9),labels = c("[0,0]", "[1,2]","[3,9]"))
woe_data$No.of.times.30.DPD.or.worse.in.last.12.months=mapvalues(woe_data$No.of.times.30.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)
#SAME WILL BE PERFORMED IN AVERAGE CREDIT CARD UTILIZATION AS WE DID IN AGE
woe_data$Avgas.CC.Utilization.in.last.12.months=as.numeric(woe_data$Avgas.CC.Utilization.in.last.12.months)
woe_data$Avgas.CC.Utilization.in.last.12.months=cut(woe_data$Avgas.CC.Utilization.in.last.12.months,breaks = c(-1,4,6,8,11,14,21,37,51,71,113),labels = c("[0,4]", "[5,6]", "[7,8]", "[9,11]", "[12,14]", "[15,21]", "[22,37]", "[38,51]", "[52,71]", "[72,113]"))
woe_data$Avgas.CC.Utilization.in.last.12.months=addNA(woe_data$Avgas.CC.Utilization.in.last.12.months)
levels(woe_data$Avgas.CC.Utilization.in.last.12.months) <- c(levels(woe_data$Avgas.CC.Utilization.in.last.12.months), "NA")
woe_data$Avgas.CC.Utilization.in.last.12.months[is.na(woe_data$Avgas.CC.Utilization.in.last.12.months)]="NA"
woe_data$Avgas.CC.Utilization.in.last.12.months=mapvalues(woe_data$Avgas.CC.Utilization.in.last.12.months,from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months,to=IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 6 MONTHS
woe_data$No.of.trades.opened.in.last.6.months=as.numeric(woe_data$No.of.trades.opened.in.last.6.months)
woe_data$No.of.trades.opened.in.last.6.months=cut(woe_data$No.of.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,3,4,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,12]"))
woe_data$No.of.trades.opened.in.last.6.months=mapvalues(woe_data$No.of.trades.opened.in.last.6.months,from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months,to=IV$Tables$No.of.trades.opened.in.last.6.months$WOE)
#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 12 MONTHS
woe_data$No.of.trades.opened.in.last.12.months=as.numeric(woe_data$No.of.trades.opened.in.last.12.months)
woe_data$No.of.trades.opened.in.last.12.months=cut(woe_data$No.of.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,5,7,9,12,28),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,5]", "[6,7]","[8,9]","[10,12]","[13,28]"))
woe_data$No.of.trades.opened.in.last.12.months=mapvalues(woe_data$No.of.trades.opened.in.last.12.months,from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months,to=IV$Tables$No.of.trades.opened.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 6 MONTHS
woe_data$No.of.PL.trades.opened.in.last.6.months=as.numeric(woe_data$No.of.PL.trades.opened.in.last.6.months)
woe_data$No.of.PL.trades.opened.in.last.6.months=cut(woe_data$No.of.PL.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,6),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,6]"))
woe_data$No.of.PL.trades.opened.in.last.6.months=mapvalues(woe_data$No.of.PL.trades.opened.in.last.6.months,from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months,to=IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)
#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 12 MONTHS
woe_data$No.of.PL.trades.opened.in.last.12.months=as.numeric(woe_data$No.of.PL.trades.opened.in.last.12.months)
woe_data$No.of.PL.trades.opened.in.last.12.months=cut(woe_data$No.of.PL.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,4,5,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,12]"))
woe_data$No.of.PL.trades.opened.in.last.12.months=mapvalues(woe_data$No.of.PL.trades.opened.in.last.12.months,from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months,to=IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST SIX MONTHS
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=as.numeric(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=cut(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,4,10),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,4]","[5,10]"))
woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=mapvalues(woe_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)
#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST 12 MONTHS
woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=as.numeric(woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=cut(woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,3,4,5,8,20),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,8]","[9,20]"))
woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=mapvalues(woe_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)
#MAPPING WOE VALUES FOR PRESENCE OF OPEN HOME LOAN
woe_data$Presence.of.open.home.loan=addNA(woe_data$Presence.of.open.home.loan)
woe_data$Presence.of.open.home.loan=mapvalues(woe_data$Presence.of.open.home.loan,from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan,to=IV$Tables$Presence.of.open.home.loan$WOE)
#MAPPING WOE VALUES FOR OUTSTANDING BALANCE
woe_data$Outstanding.Balance=as.numeric(woe_data$Outstanding.Balance)
woe_data$Outstanding.Balance=cut(woe_data$Outstanding.Balance,breaks = c(-1,6843,25509,386809,585402,774228,972455,1357300,2960987,3282013,5218801),labels = c("[0,6843]", "[6847,25509]", "[25522,386809]", "[386813,585402]", "[585423,774228]", "[774241,972455]", "[972456,1357300]", "[1357399,2960987]", "[2960994,3282013]", "[3282027,5218801]"))
woe_data$Outstanding.Balance=addNA(woe_data$Outstanding.Balance)
levels(woe_data$Outstanding.Balance) <- c(levels(woe_data$Outstanding.Balance), "NA")
woe_data$Outstanding.Balance[is.na(woe_data$Outstanding.Balance)]="NA"
woe_data$Outstanding.Balance=mapvalues(woe_data$Outstanding.Balance,from = IV$Tables$Outstanding.Balance$Outstanding.Balance,to=IV$Tables$Outstanding.Balance$WOE)
#MAPPING WOE VALUES FOR TOTAL NO OF TRADES
woe_data$Total.No.of.Trades=as.numeric(woe_data$Total.No.of.Trades)
woe_data$Total.No.of.Trades=cut(woe_data$Total.No.of.Trades,breaks = c(-1,1,2,3,4,5,6,8,10,19,44),labels = c("[0,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]","[7,8]","[9,10]","[11,19]","[20,44]"))
woe_data$Total.No.of.Trades=mapvalues(woe_data$Total.No.of.Trades,from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades,to=IV$Tables$Total.No.of.Trades$WOE)
#MAPPING WOE VALUES FOR PRESENCE OF AUTO OPEN HOME LOANS
woe_data$Presence.of.open.auto.loan=mapvalues(woe_data$Presence.of.open.auto.loan,from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan,to=IV$Tables$Presence.of.open.auto.loan$WOE)

######################   WOE COMPLETED #############################

#CHECK FOR ANY MISSING STILL THERE
anyNA(woe_data)#FALSE. HENCE OUR DATASET DOES NOT CONTAIN ANY MISSING VALUES AND ALL VALUES ARE REPLACED BY WOE VALUES

########################  WOE AND ANALYSIS BY PLOTTING AND LOGISTIC REGRESSION ##################
####MODEL BUILDING ON DEMOGRAPHIC DATA USING LOGISTIC REGRESSION####
woe_dem=woe_data[,c(1,2,3,4,5,6,7,8,9,10,11,29)]
set.seed(3033)
in_train <- createDataPartition(woe_dem$Performance.Tag, p=0.7, list = F)
training=woe_dem[in_train,]
testing=woe_dem[-in_train,]
training[["Performance.Tag"]]=factor(training[["Performance.Tag"]])
logit_model=glm(Performance.Tag~Age+Gender+Marital.Status..at.the.time.of.application.+No.of.dependents+Income+Education+Profession+Type.of.residence+No.of.months.in.current.residence+No.of.months.in.current.company,family = "binomial",data = training)

stepAIC(logit_model)

logit_model_2 <-glm(formula = Performance.Tag ~ Income + Profession + No.of.months.in.current.residence + 
                      No.of.months.in.current.company, family = "binomial", data = training)

summary(logit_model_2)

#EVALUATING THE LOGISTIC MODEL.
pred <- predict(logit_model_2, newdata = testing, type = "response")
y_pred_num <- ifelse(pred > 0.2, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testing$Performance.Tag
mean(y_pred == y_act)
testing[["Performance.Tag"]]=factor(testing[["Performance.Tag"]])
confusionMatrix(y_pred,testing$Performance.Tag)####95.82% ACCURACY ACHIEVED. However negative Prediction value is 0% because it has displayed positives for 873 negatives.
## Hence logistic regression may not be as efficient on a new dataset.HOWEVER PREDICTIVE POWER OF DIFFERENT VARIABLES ARE NOTED.



### MODEL BUILDING ON MASTER DATA USING LOGISTIC REGRESSION #######
woe_master=woe_data[,2:29]
set.seed(3033)
in_train <- createDataPartition(woe_master$Performance.Tag, p=0.7, list = F)
training=woe_master[in_train,]
testing=woe_master[-in_train,]
training[["Performance.Tag"]]=factor(training[["Performance.Tag"]])
logit_model=glm(Performance.Tag~.,family = "binomial",data = training)

# #Performming the StepAIC fucntion for the LR model on the entire master data . 
# stepAIC(logit_model)

summary(logit_model)
#EVALUATING THE LOGISTIC MODEL.
pred <- predict(logit_model, newdata = testing, type = "response")
y_pred_num <- ifelse(pred > 0.2, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testing$Performance.Tag
mean(y_pred == y_act)
testing[["Performance.Tag"]]=factor(testing[["Performance.Tag"]])
confusionMatrix(y_pred,testing$Performance.Tag)### 95.82% ACCURACY BUT IMBALANCED CLASSIFICATION.

###### BALANCING THE DATASET TO TACKLE IMBALANCED CLASSIFICATION######

woe_master=woe_data

######MODEL BUILDING AND EVALUATION ON MASTER DATASE USING LOGISTIC REGRESSION ######
# woe_master_balanced=balanced_data[,2:29]
set.seed(3033)
in_train <- createDataPartition(woe_master$Performance.Tag, p=0.7, list = F)
training=woe_master[in_train,]
testing=woe_master[-in_train,]

data<-ubBalance(X= training[,-29], Y=as.factor(training[,29]), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
balanced_data=cbind(data$X,data$Y)
summary(balanced_data)
colnames(balanced_data)[29]="Performance.Tag"



# training[["Performance.Tag"]]=factor(training[["Performance.Tag"]])


#Considering all the Variables with IV values having Meadium and strong Predicive power . 

logit_model=glm(Performance.Tag~Avgas.CC.Utilization.in.last.12.months+No.of.trades.opened.in.last.12.months+No.of.PL.trades.opened.in.last.12.months+
                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.+Outstanding.Balance+No.of.times.30.DPD.or.worse.in.last.6.months+Total.No.of.Trades+No.of.PL.trades.opened.in.last.6.months+No.of.times.90.DPD.or.worse.in.last.12.months+No.of.times.60.DPD.or.worse.in.last.6.months+No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
                 ,family = "binomial",data =balanced_data )
logit_model
summary(logit_model)

#EVALUATING THE LOGISTIC MODEL.

# pred <- predict(logit_model, newdata = testing , type = "response")
# y_pred_num <- ifelse(pred > 0.5, 1, 0)
# y_pred <- factor(y_pred_num, levels=c(0, 1))
# y_act <- testing$Performance.Tag
# mean(y_pred == y_act)
# testing[["Performance.Tag"]]=factor(testing[["Performance.Tag"]])
# confusionMatrix(y_pred,testing$Performance.Tag)
# 
# ###CHECKING THE OPTIMAL CUTPOINTS
# 
# cutoffs <- seq(0.01,0.99,0.01)
# accuracy <- NULL
# for (i in seq(along = cutoffs)){
#   y_pred_num <- ifelse(pred > cutoffs[i], 1, 0)
#   y_pred <- factor(y_pred_num, levels=c(0, 1))
#   y_act <- testing$Performance.Tag
#   accuracy=c(accuracy,(mean(y_pred == y_act))*100)
# }
# ##And then you can visually explore the cutoff vs probability by plotting
# 
# plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
#      main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")
# 
# which.max(accuracy)
# cutoffs[which.max(accuracy)]
# 
# #####  ACCURACY=70.36199 AT CUTPOINT=0.42... HENCE LOGISTIC REGRESSION NOT SO ACCURATE.

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logit_model, newdata = testing[, -29], type = "response")

View(predictions_logit)
summary(predictions_logit)

## Model Evaluation: Logistic Regression (test data)

# Let's use the probability cutoff of 50%.

predicted_Performance.Tag <- factor(ifelse(predictions_logit >= 0.5, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==0]<-"no"
levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==1]<-"yes"


conf <- confusionMatrix(predicted_Performance.Tag, testing$Performance.Tag, positive = "yes")

conf


# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  14284   419
# yes  5795   458
# 
# Accuracy : 0.7035          
# 95% CI : (0.6972, 0.7097)
# No Information Rate : 0.9582          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0594          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.52223         
# Specificity : 0.71139         
# Pos Pred Value : 0.07324         
# Neg Pred Value : 0.97150         
# Prevalence : 0.04185         
# Detection Rate : 0.02186         
# Detection Prevalence : 0.29839         
# Balanced Accuracy : 0.61681         
# 
# 'Positive' Class : yes     

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_Performance.Tag <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_Performance.Tag, testing$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]

cutoff
#0.4554545


predicted_response <- factor(ifelse(predictions_logit >= 0.4554, "yes", "no"))

#CONFUSION MATRIX FOR THE TEST DATA
#-------------------------------------------------------
conf_final <- confusionMatrix(predicted_response, testing$Performance.Tag, positive = "yes")

acc <- conf_final$overall[1]

#Accuracy 
#0.6593338 

sens <- conf_final$byClass[1]

# Sensitivity 
# 0.5701254 

spec <- conf_final$byClass[2]

# Specificity 
# 0.6632302 



#For Entire Dataset testing

woe_master$Performance.Tag<- as.factor(woe_master$Performance.Tag)
levels(woe_master$Performance.Tag)[levels(woe_master$Performance.Tag)==0]<-"no"
levels(woe_master$Performance.Tag)[levels(woe_master$Performance.Tag)==1]<-"yes"



predictions_logit_final <- predict(logit_model, newdata = woe_master[, -29], type = "response")

View(predictions_logit_final)
summary(predictions_logit_final)

predicted_Performance.Tag <- factor(ifelse(predictions_logit_final >= 0.5, "yes", "no"))


# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_Performance.Tag, woe_master$Performance.Tag, positive = "yes")

conf

#     Confusion Matrix and Statistics
#     
#     Reference
#     Prediction    no   yes
#     no  47476  1403
#     yes 19432  1544
#     
#     Accuracy : 0.7017          
#     95% CI : (0.6983, 0.7051)
#     No Information Rate : 0.9578          
#     P-Value [Acc > NIR] : 1               
#     
#     Kappa : 0.0595          
#     Mcnemar's Test P-Value : <2e-16          
#                                        
#          Sensitivity : 0.52392         
#          Specificity : 0.70957         
#       Pos Pred Value : 0.07361         
#       Neg Pred Value : 0.97130         
#           Prevalence : 0.04219         
#       Detection Rate : 0.02210         
# Detection Prevalence : 0.30028         
#    Balanced Accuracy : 0.61675         
#                                        
#     'Positive' Class : yes 



# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_Performance.Tag <- factor(ifelse(predictions_logit_final >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_Performance.Tag, woe_master$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]

cutoff
predicted_Performance.Tag <- factor(ifelse(predictions_logit_final >= 0.4554, "yes", "no"))


# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_Performance.Tag, woe_master$Performance.Tag, positive = "yes")

conf

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  44377  1243
# yes 22531  1704
# 
# Accuracy : 0.6597          
# 95% CI : (0.6561, 0.6632)
# No Information Rate : 0.9578          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0542          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.57822         
# Specificity : 0.66325         
# Pos Pred Value : 0.07031         
# Neg Pred Value : 0.97275         
# Prevalence : 0.04219         
# Detection Rate : 0.02439         
# Detection Prevalence : 0.34693         
# Balanced Accuracy : 0.62073         
# 
# 'Positive' Class : yes  
# 


# ### MODEL BUILDING ON master data USING DECISION TREE####
# woe_master_balanced=balanced_data[,2:29]
# set.seed(3033)
# in_train <- createDataPartition(woe_master_balanced$Performance.Tag, p=0.7, list = F)
# training=woe_master_balanced[in_train,]
# testing=woe_master_balanced[-in_train,]
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# set.seed(3333)
# dtree_fit=caret::train(Performance.Tag~.,data=training,method="rpart",trControl=trctrl,tuneLength=10,parms=list(split="information"))
# y_pred=predict(dtree_fit,newdata = testing)
# confusionMatrix(y_pred,testing$Performance.Tag)#### ACCURACY OF 81.31 ACHIEVED. BETTER THAN LOGISTIC REGRESSION

#### MODEL BUILDING ON MASTER DATASET USING RANDOM FOREST ######

#Building the Random Forest model on the partioned data earlier and the balanced training data . 

fit <- randomForest(Performance.Tag ~ .,balanced_data ,ntree=500)

# predicted= predict(fit,testing[,-29],type="prob")
# 
# predicted<- as.factor(predicted)
# levels(predicted)[levels(predicted)==0]<-"no"
# levels(predicted)[levels(predicted)==1]<-"yes"
# 
# 
# confusionMatrix(predicted,testing$Performance.Tag)#### 85.75% accuracy achieved

# Predict response for test data

rf_pred <- predict(fit, testing[, -29], type = "prob")

head(rf_pred)

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, testing$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

cutoff_rf

predicted_response <- factor(ifelse(rf_pred[, 2] >= 0.32, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response, testing[, 29], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

# Accuracy 
conf_forest$overall[1]

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  12336   328
# yes  7743   549
# 
# Accuracy : 0.6149          
# 95% CI : (0.6082, 0.6215)
# No Information Rate : 0.9582          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0477          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.62600         
# Specificity : 0.61437         
# Pos Pred Value : 0.06621         
# Neg Pred Value : 0.97410         
# Prevalence : 0.04185         
# Detection Rate : 0.02620         
# Detection Prevalence : 0.39569         
# Balanced Accuracy : 0.62019         
# 
# 'Positive' Class : yes  

# Predict response for test data

rf_pred_final <- predict(fit, woe_master[, -29], type = "prob")

head(rf_pred_final)

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_final[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, woe_master$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

cutoff_rf

# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_final <- factor(ifelse(rf_pred_final[, 2] >= 0.396, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_final, woe_master[, 29], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

# Accuracy 
conf_forest$overall[1]
# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  53837   549
# yes 13071  2398
# 
# Accuracy : 0.805          
# 95% CI : (0.8021, 0.808)
# No Information Rate : 0.9578         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.204          
# Mcnemar's Test P-Value : <2e-16         
#                                          
#             Sensitivity : 0.81371        
#             Specificity : 0.80464        
#          Pos Pred Value : 0.15502        
#          Neg Pred Value : 0.98991        
#              Prevalence : 0.04219        
#          Detection Rate : 0.03433        
#    Detection Prevalence : 0.22144        
#       Balanced Accuracy : 0.80918        
#                                          
#        'Positive' Class : yes            
#                               


#Final Model Selection 

final_model <- fit




#### Tuning parameters of random forest modelling on the dataset ####

woe_master_balanced=balanced_data[,2:29]
set.seed(3033)
in_train <- createDataPartition(woe_master_balanced$Performance.Tag, p=0.7, list = F)
training=woe_master_balanced[in_train,]
testing=woe_master_balanced[-in_train,]

trainTask <- makeClassifTask(data = training,target = "Performance.Tag")
testTask <- makeClassifTask(data = testing,target = "Performance.Tag")

rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(importance = TRUE)

rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50))
rancontrol <- makeTuneControlRandom(maxit = 50L)

set_cv <- makeResampleDesc("CV",iters = 3L)

rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = acc)
####[Tune] Result: ntree=357; mtry=7; nodesize=15 : acc.test.mean=0.8557742
#### no improvement. 

####### PREDICTING THE LIKELIHOOD OF DEFAULT ON REJECTED CANDIDATES ########


##### 1. converting rejected data into woe data #########
rejected$Performance.Tag=1
rejected$Age=as.numeric(rejected$Age)
rejected$Age=cut(rejected$Age,breaks = c(14,30,35,38,41,44,47,50,53,57,65),labels = c("[18,30]", "[31,35]", "[36,38]", "[39,41]", "[42,44]", "[45,47]", "[48,50]", "[51,53]", "[54,57]", "[58,65]"))
#SINCE NA VALUE IN IV TABLE IS IN CHARACTER HENCE WE HAVE TO CONVERT NA VALUES FROM FACTOR TO CHARACTER IN rejected$AGE TO MAP THE ORIGINAL VALUES TO THE WOE VALUES
rejected$Age=addNA(rejected$Age)
levels(rejected$Age) <- c(levels(rejected$Age), "NA")
rejected$Age[is.na(rejected$Age)]="NA"
rejected$Age=mapvalues(rejected$Age,from = IV$Tables$Age$Age,to=IV$Tables$Age$WOE)
rejected$Gender=mapvalues(rejected$Gender,from = IV$Tables$Gender$Gender, to=IV$Tables$Gender$WOE)
rejected$Marital.Status..at.the.time.of.application.=mapvalues(rejected$Marital.Status..at.the.time.of.application.,from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to=IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
rejected$No.of.dependents=mapvalues(rejected$No.of.dependents,from = IV$Tables$No.of.dependents$No.of.dependents, to=IV$Tables$No.of.dependents$WOE)
rejected$Education=mapvalues(rejected$Education,from = IV$Tables$Education$Education,to=IV$Tables$Education$WOE)
rejected$Profession=mapvalues(rejected$Profession,from = IV$Tables$Profession$Profession,to=IV$Tables$Profession$WOE)
rejected$Type.of.residence=mapvalues(rejected$Type.of.residence,from = IV$Tables$Type.of.residence$Type.of.residence,to=IV$Tables$Type.of.residence$WOE)
#SAME WILL BE PERFORMED IN INCOME AS WE DID IN AGE
rejected$Income=as.numeric(rejected$Income)
rejected$Income=cut(rejected$Income,breaks = c(-1,5,10,16,21,26,31,36,41,48,60),labels = c("[0,5]", "[6,10]", "[11,16]", "[17,21]", "[22,26]", "[27,31]", "[32,36]", "[37,41]", "[42,48]", "[49,60]"))
rejected$Income=addNA(rejected$Income)
levels(rejected$Income) <- c(levels(rejected$Income), "NA")
rejected$Income[is.na(rejected$Income)]="NA"
rejected$Income=mapvalues(rejected$Income,from = IV$Tables$Income$Income,to=IV$Tables$Income$WOE)
#CHANGING NO OF MONTHS IN CURRENT RESIDENCE FROM CONTINOUS TO CATEGORICAL
rejected$No.of.months.in.current.residence=as.numeric(rejected$No.of.months.in.current.residence)
rejected$No.of.months.in.current.residence=cut(rejected$No.of.months.in.current.residence,breaks = c(5,9,28,49,72,97,126),labels = c("[6,9]", "[10,28]", "[29,49]", "[50,72]", "[73,97]", "[98,126]"))
rejected$No.of.months.in.current.residence=mapvalues(rejected$No.of.months.in.current.residence,from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence,to=IV$Tables$No.of.months.in.current.residence$WOE)
#CHANGING NO OF MONTHS IN CURRENT company FROM CONTINOUS TO CATEGORICAL
rejected$No.of.months.in.current.company=as.numeric(rejected$No.of.months.in.current.company)
rejected$No.of.months.in.current.company=cut(rejected$No.of.months.in.current.company,breaks = c(2,5,12,19,26,33,40,47,53,61,133),labels = c("[3,5]", "[6,12]", "[13,19]", "[20,26]", "[27,33]", "[34,40]","[41,47]","[48,53]","[54,61]","[62,133]"))
rejected$No.of.months.in.current.company=mapvalues(rejected$No.of.months.in.current.company,from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company,to=IV$Tables$No.of.months.in.current.company$WOE)
#CHANGING NO OF times 90 days past due or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.90.DPD.or.worse.in.last.6.months=as.numeric(rejected$No.of.times.90.DPD.or.worse.in.last.6.months)
rejected$No.of.times.90.DPD.or.worse.in.last.6.months=cut(rejected$No.of.times.90.DPD.or.worse.in.last.6.months,breaks = c(-1,0,3),labels = c("[0,0]", "[1,3]"))
rejected$No.of.times.90.DPD.or.worse.in.last.6.months=mapvalues(rejected$No.of.times.90.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 60 days past due or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.60.DPD.or.worse.in.last.6.months=as.numeric(rejected$No.of.times.60.DPD.or.worse.in.last.6.months)
rejected$No.of.times.60.DPD.or.worse.in.last.6.months=cut(rejected$No.of.times.60.DPD.or.worse.in.last.6.months,breaks = c(-1,0,5),labels = c("[0,0]", "[1,5]"))
rejected$No.of.times.60.DPD.or.worse.in.last.6.months=mapvalues(rejected$No.of.times.60.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 30 days past due or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.30.DPD.or.worse.in.last.6.months=as.numeric(rejected$No.of.times.30.DPD.or.worse.in.last.6.months)
rejected$No.of.times.30.DPD.or.worse.in.last.6.months=cut(rejected$No.of.times.30.DPD.or.worse.in.last.6.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
rejected$No.of.times.30.DPD.or.worse.in.last.6.months=mapvalues(rejected$No.of.times.30.DPD.or.worse.in.last.6.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)
#CHANGING NO OF times 90 days past due FOR 12 MONTHS or more FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.90.DPD.or.worse.in.last.12.months=as.numeric(rejected$No.of.times.90.DPD.or.worse.in.last.12.months)
rejected$No.of.times.90.DPD.or.worse.in.last.12.months=cut(rejected$No.of.times.90.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,5),labels = c("[0,0]", "[1,1]","[2,5]"))
rejected$No.of.times.90.DPD.or.worse.in.last.12.months=mapvalues(rejected$No.of.times.90.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)
#CHANGING NO OF times 60 days past due or more FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.60.DPD.or.worse.in.last.12.months=as.numeric(rejected$No.of.times.60.DPD.or.worse.in.last.12.months)
rejected$No.of.times.60.DPD.or.worse.in.last.12.months=cut(rejected$No.of.times.60.DPD.or.worse.in.last.12.months,breaks = c(-1,0,1,7),labels = c("[0,0]", "[1,1]","[2,7]"))
rejected$No.of.times.60.DPD.or.worse.in.last.12.months=mapvalues(rejected$No.of.times.60.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)
#CHANGING NO OF times 30 days past due or more  FOR 12 MONTHS FROM CONTINOUS TO CATEGORICAL
rejected$No.of.times.30.DPD.or.worse.in.last.12.months=as.numeric(rejected$No.of.times.30.DPD.or.worse.in.last.12.months)
rejected$No.of.times.30.DPD.or.worse.in.last.12.months=cut(rejected$No.of.times.30.DPD.or.worse.in.last.12.months,breaks = c(-1,0,2,9),labels = c("[0,0]", "[1,2]","[3,9]"))
rejected$No.of.times.30.DPD.or.worse.in.last.12.months=mapvalues(rejected$No.of.times.30.DPD.or.worse.in.last.12.months,from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months,to=IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)
#SAME WILL BE PERFORMED IN AVERAGE CREDIT CARD UTILIZATION AS WE DID IN AGE
rejected$Avgas.CC.Utilization.in.last.12.months=as.numeric(rejected$Avgas.CC.Utilization.in.last.12.months)
rejected$Avgas.CC.Utilization.in.last.12.months=cut(rejected$Avgas.CC.Utilization.in.last.12.months,breaks = c(-1,4,6,8,11,14,21,37,51,71,113),labels = c("[0,4]", "[5,6]", "[7,8]", "[9,11]", "[12,14]", "[15,21]", "[22,37]", "[38,51]", "[52,71]", "[72,113]"))
rejected$Avgas.CC.Utilization.in.last.12.months=addNA(rejected$Avgas.CC.Utilization.in.last.12.months)
levels(rejected$Avgas.CC.Utilization.in.last.12.months) <- c(levels(rejected$Avgas.CC.Utilization.in.last.12.months), "NA")
rejected$Avgas.CC.Utilization.in.last.12.months[is.na(rejected$Avgas.CC.Utilization.in.last.12.months)]="NA"
rejected$Avgas.CC.Utilization.in.last.12.months=mapvalues(rejected$Avgas.CC.Utilization.in.last.12.months,from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months,to=IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 6 MONTHS
rejected$No.of.trades.opened.in.last.6.months=as.numeric(rejected$No.of.trades.opened.in.last.6.months)
rejected$No.of.trades.opened.in.last.6.months=cut(rejected$No.of.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,3,4,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,12]"))
rejected$No.of.trades.opened.in.last.6.months=mapvalues(rejected$No.of.trades.opened.in.last.6.months,from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months,to=IV$Tables$No.of.trades.opened.in.last.6.months$WOE)
#MAPPING WOE VALUES FOR NO OF TRADES OPENED IN LAST 12 MONTHS
rejected$No.of.trades.opened.in.last.12.months=as.numeric(rejected$No.of.trades.opened.in.last.12.months)
rejected$No.of.trades.opened.in.last.12.months=cut(rejected$No.of.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,5,7,9,12,28),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]", "[4,5]", "[6,7]","[8,9]","[10,12]","[13,28]"))
rejected$No.of.trades.opened.in.last.12.months=mapvalues(rejected$No.of.trades.opened.in.last.12.months,from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months,to=IV$Tables$No.of.trades.opened.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 6 MONTHS
rejected$No.of.PL.trades.opened.in.last.6.months=as.numeric(rejected$No.of.PL.trades.opened.in.last.6.months)
rejected$No.of.PL.trades.opened.in.last.6.months=cut(rejected$No.of.PL.trades.opened.in.last.6.months,breaks = c(-1,0,1,2,6),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,6]"))
rejected$No.of.PL.trades.opened.in.last.6.months=mapvalues(rejected$No.of.PL.trades.opened.in.last.6.months,from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months,to=IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)
#MAPPING WOE VALUES FOR pl OF TRADES OPENED IN LAST 12 MONTHS
rejected$No.of.PL.trades.opened.in.last.12.months=as.numeric(rejected$No.of.PL.trades.opened.in.last.12.months)
rejected$No.of.PL.trades.opened.in.last.12.months=cut(rejected$No.of.PL.trades.opened.in.last.12.months,breaks = c(-1,0,1,2,3,4,5,12),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,12]"))
rejected$No.of.PL.trades.opened.in.last.12.months=mapvalues(rejected$No.of.PL.trades.opened.in.last.12.months,from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months,to=IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)
#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST SIX MONTHS
rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=as.numeric(rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=cut(rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,4,10),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,4]","[5,10]"))
rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.=mapvalues(rejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)
#MAPPING WOE VALUES FOR NO OF INQUIRIES IN LAST 12 MONTHS
rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=as.numeric(rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=cut(rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,breaks = c(-1,0,1,2,3,4,5,8,20),labels = c("[0,0]", "[1,1]", "[2,2]", "[3,3]","[4,4]","[5,5]","[6,8]","[9,20]"))
rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.=mapvalues(rejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,to=IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)
#MAPPING WOE VALUES FOR PRESENCE OF OPEN HOME LOAN
rejected$Presence.of.open.home.loan=addNA(rejected$Presence.of.open.home.loan)
rejected$Presence.of.open.home.loan=mapvalues(rejected$Presence.of.open.home.loan,from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan,to=IV$Tables$Presence.of.open.home.loan$WOE)
#MAPPING WOE VALUES FOR OUTSTANDING BALANCE
rejected$Outstanding.Balance=as.numeric(rejected$Outstanding.Balance)
rejected$Outstanding.Balance=cut(rejected$Outstanding.Balance,breaks = c(-1,6843,25509,386809,585402,774228,972455,1357300,2960987,3282013,5218801),labels = c("[0,6843]", "[6847,25509]", "[25522,386809]", "[386813,585402]", "[585423,774228]", "[774241,972455]", "[972456,1357300]", "[1357399,2960987]", "[2960994,3282013]", "[3282027,5218801]"))
rejected$Outstanding.Balance=addNA(rejected$Outstanding.Balance)
levels(rejected$Outstanding.Balance) <- c(levels(rejected$Outstanding.Balance), "NA")
rejected$Outstanding.Balance[is.na(rejected$Outstanding.Balance)]="NA"
rejected$Outstanding.Balance=mapvalues(rejected$Outstanding.Balance,from = IV$Tables$Outstanding.Balance$Outstanding.Balance,to=IV$Tables$Outstanding.Balance$WOE)
#MAPPING WOE VALUES FOR TOTAL NO OF TRADES
rejected$Total.No.of.Trades=as.numeric(rejected$Total.No.of.Trades)
rejected$Total.No.of.Trades=cut(rejected$Total.No.of.Trades,breaks = c(-1,1,2,3,4,5,6,8,10,19,44),labels = c("[0,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]","[7,8]","[9,10]","[11,19]","[20,44]"))
rejected$Total.No.of.Trades=mapvalues(rejected$Total.No.of.Trades,from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades,to=IV$Tables$Total.No.of.Trades$WOE)
#MAPPING WOE VALUES FOR PRESENCE OF AUTO OPEN HOME LOANS
rejected$Presence.of.open.auto.loan=mapvalues(rejected$Presence.of.open.auto.loan,from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan,to=IV$Tables$Presence.of.open.auto.loan$WOE)

###### 2. evaluating likelihood of default on rejected candidates ########

predicted= predict(fit,rejected[,-29])
rejected$Performance.Tag=as.factor(rejected$Performance.Tag)
mean(as.numeric(predicted)==as.numeric(rejected$Performance.Tag))

#### 78% are will be classified as defaulters.
## RESULT: SINCE THE REJECTED CANDIDATES WERE REJECTED BECAUSE OF THE RISK TO DEFAULT IN FUTURE HENCE OUR MODEL PREDICTS THAT MORE THAN 70% OF CANDIDATES WILL BE DEFAULTERS IF THEY WOULD HAVE BEEN ACCEPTED.
## There may be many other factors as well for which the candidate may be rejected other than his possibility of rejection. This has been accounted into remaining 22%.

######### COMBINING BOTH ACCEPTED AND REJECTED CANDIDATES DATASET #######
levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==0]<-"no"
levels(testing$Performance.Tag)[levels(testing$Performance.Tag)==1]<-"yes"

fin_merged_data=rbind(woe_master,rejected)
fin_merged_data$Performance.Tag=as.factor(fin_merged_data$Performance.Tag)


#####BALANCING THE FINAL MERGED DATA ######
fin_balanced=fin_merged_data
# data<-ubBalance(X= fin_balanced[,-29], Y=as.factor(fin_balanced[,29]), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
# final_balanced_data=cbind(data$X,data$Y)
# summary(final_balanced_data)
# colnames(final_balanced_data)[29]="Performance.Tag"
# 
# ##### NOW WE WILL FIND PROBABILITY OF LIKELIHOOD OF DEFAULT ########
# set.seed(3033)
# in_train <- createDataPartition(final_balanced_data$Performance.Tag, p=0.7, list = F)
# training=final_balanced_data[in_train,]
# testing=final_balanced_data[-in_train,]
# training[["Performance.Tag"]]=factor(training[["Performance.Tag"]])
# 
# final_model=glm(Performance.Tag~.,family = "binomial",data = training[,-1])
# 
# pred <- predict(final_model, newdata = testing[,-1], type = "response")
# y_pred_num <- ifelse(pred > 0.5, 1, 0)
# y_pred <- factor(y_pred_num, levels=c(0, 1))
# y_act <- testing$Performance.Tag
# mean(y_pred == y_act)
# testing[["Performance.Tag"]]=factor(testing[["Performance.Tag"]])
# confusionMatrix(y_pred,testing$Performance.Tag)
# 
# #### CUTOFF FOR WHICH ACCURACY WILL BE THE HIGHEST ######
# cutoffs <- seq(0.01,0.99,0.01)
# accuracy <- NULL
# for (i in seq(along = cutoffs)){
#   y_pred_num <- ifelse(pred > cutoffs[i], 1, 0)
#   y_pred <- factor(y_pred_num, levels=c(0, 1))
#   y_act <- testing$Performance.Tag
#   accuracy=c(accuracy,(mean(y_pred == y_act))*100)
# }
# ##And then you can visually explore the cutoff vs probability by plotting
# 
# plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
#      main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")
# 
# which.max(accuracy)
# cutoffs[which.max(accuracy)]#### accuracy of 74.72 on complete dataset


####### probability of likelihood on original unbalanced data #####

predict_unb_final=predict(final_model,newdata = fin_merged_data,type = "prob")
predict_unb_final=predict_unb_final[,2]

###### PREPARING APPLICATION SCORECARD #######
Factor=20/log(2)
Offset=400-(Factor*log(10))
score=Offset+(Factor*log((1-predict_unb_final)/predict_unb_final))
fin_merged_data$score=score

###### FINDING CUTOFF BELOW WHICH ALL WILL BE REJECTED #####

rejected_score=subset(fin_merged_data,fin_merged_data$Application.ID %in% rejected$Application.ID)
selected_score=subset(fin_merged_data,fin_merged_data$Application.ID %in% selected$Application.ID)
# > summary(rejected_score$score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 270.2   334.9   342.2   340.3   348.4   375.8 
# > summary(selected_score$score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 238.7   347.9   369.7     Inf   395.1     Inf 
cutoff_score=seq(238.7,512.7,1)
# head(sort(score,decreasing=T),10)
# 1766    13385    47461    57384     3443     5559     5566     7397    19257 
# Inf      Inf      Inf      Inf 512.8194 512.8194 512.8194 512.8194 512.8194 
# 21255 
# 512.8194 
rejected_count2=NULL
selected_count2=NULL
for(i in seq(along=cutoff_score)){
  rejected_count2=c(rejected_count2,sum(rejected_score$score<cutoff_score[i]))
  selected_count2=c(selected_count2,sum(selected_score$score>cutoff_score[i]))
}
plot(cutoff_score,rejected_count2,type = "l")
par(new = T)
plot(cutoff_score, selected_count2,type = "l",axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, 'selected_count')
# locator()#### point this locator on the graph where the lines are intersecting.Thus the optimal cutoff score is 314.
# > locator()#### point this locator on the graph where the lines are intersecting.Thus the optimal cutoff score is 314.
# $x
# [1] 348.1212
# 
# $y
# [1] 51861.51
# 
# > 


#### PLOTTING PERFORMANCE TAG WITH RESPECT TO DIFFERENT ATTRIBUTES(One's having highest predictive power) ########


## PLOTTING ALL THE INDEPENDENT ATTRIBUTES WITH DEPENDENT ATTRIBUTE TO GET A BETTER VISUALISATION.
avg_cc_plot <- ggplot(selected, aes(x=Avgas.CC.Utilization.in.last.12.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

trade_plot <- ggplot(selected, aes(x=No.of.trades.opened.in.last.12.months,fill=factor(Performance.Tag)))+ geom_bar()

pl_trade_plot <- ggplot(selected, aes(x=No.of.PL.trades.opened.in.last.12.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")


inquiries_plot <- ggplot(selected, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

balance_plot <- ggplot(selected, aes(x=Outstanding.Balance,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

dpd30_plot <- ggplot(selected, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

total_trades_plot <- ggplot(selected, aes(x=Total.No.of.Trades,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

pl_trade6_plot <- ggplot(selected, aes(x=No.of.PL.trades.opened.in.last.6.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

dpd90_plot <- ggplot(selected, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

dpd60_plot <- ggplot(selected, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

inquiries6_plot <- ggplot(selected, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

dpd30_12_plot <- ggplot(selected, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

trades_6_plot <- ggplot(selected, aes(x=No.of.trades.opened.in.last.6.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

dpd60_12_plot <- ggplot(selected, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

dpd90_6_plot <- ggplot(selected, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

residence_plot <- ggplot(selected, aes(x=No.of.months.in.current.residence,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

income_plot <- ggplot(selected, aes(x=Income,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

current_com_plot <- ggplot(selected, aes(x=No.of.months.in.current.company,fill=factor(Performance.Tag)))+ geom_bar(position="fill")

plot_grid(avg_cc_plot,trade_plot,pl_trade_plot,dpd90_plot,labels = "AUTO")
plot_grid(dpd60_12_plot,dpd90_6_plot,residence_plot,income_plot,current_com_plot,labels = "AUTO")
plot_grid(inquiries_plot,balance_plot,dpd30_plot,pl_trade6_plot,labels = "AUTO")
plot_grid(dpd60_plot,inquiries6_plot,dpd30_12_plot,trades_6_plot,labels = "AUTO")
locator()

# 
# > table(rejected_score$score<348)
# 
# FALSE  TRUE 
# 373  1052 

table(selected_score$score>348)

below_cutoff_but_selected=selected_score[selected_score$score<348,]
View(below_cutoff_but_selected)
sum(below_cutoff_but_selected$Performance.Tag=="yes")
##[1] 2447
sum(master$Performance.Tag==1)
##[1] NA
sum(selected_score$Performance.Tag=="yes")
##[1] 2947
##> 2947/69855
##[1] 0.04218739
2947-2447
##[1] 500
 500/52243
##[1] 0.00957

 #Credict loss with no model = 4.21%
 #Credit Loss with model = 0.00957%
 #Credut loss saved = 3.21%
 
 #Revenue loss
 
 
