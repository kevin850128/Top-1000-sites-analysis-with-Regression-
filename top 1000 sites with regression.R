#rawdata = read.table("C:/Users/Kevin/Desktop/top_1000_sites.tsv.txt",sep="\t",header = TRUE)
#���n�ΤW�����檺��k�AŪ�i�ӷ|��234�����

library(readr)
rawdata <- read_delim("https://raw.githubusercontent.com/johnmyleswhite/ML_for_Hackers/24e55baa672c4ece0579423fabf220fa2b907a71/05-Regression/data/top_1000_sites.tsv", delim="\t")
head(rawdata)
str(rawdata)

## missing value
for ( i in 1:length(rawdata[1,]) ){
  na <- sum(is.na(rawdata[, i]) )
  print( na )
}     #  Category:47�� , InEnglish:900�� , TLD:900�� missing value


#�B�zmissing value�A�]��Category�MTLD�o������ڤ����ߡA�]���|���ӫؼҡA�N���B�z�F
rawdata$InEnglish[is.na(rawdata$InEnglish)] = "NA" 
#������ƪ���900���bInEnglish��쳣�ONA�A�]���]�j�k�u�|�Ϋe100���h�]�A�ëD�ڭ̷Q�n��
#�ҥH�o��D�`���n�A�n��NA�]�����@��level�h�]�j�k


#�N���O����নfactor
rawdata$HasAdvertising = as.factor(rawdata$HasAdvertising)
rawdata$Category = as.factor(rawdata$Category)
rawdata$InEnglish = as.factor(rawdata$InEnglish)
rawdata$TLD = as.factor(rawdata$TLD)



library(ggplot2)
ggplot(rawdata, aes(x = PageViews, y = UniqueVisitors)) + geom_point()
ggplot(rawdata, aes(x = log(PageViews), y = log(UniqueVisitors))) + geom_point() +geom_smooth(method = 'lm', se = FALSE)

library(GGally)
rawdata$HasAdvertising  <- ifelse(rawdata$HasAdvertising == "Yes", 1,0)
rawdata$InEnglish <- ifelse(rawdata$InEnglish == "Yes", 1,0)
#��17�椣�i����
#���F�e�U����correlation plot�A�~�ݭn����W�����A�]��ggcorrŪ����factor���A��ơA�G���নdummy variable
ggcorr(rawdata[c("PageViews","UniqueVisitors","Reach","HasAdvertising","InEnglish")],label = TRUE,label_round = 2)


####�j�k#######
fullmodel = lm(log(PageViews)~log(UniqueVisitors)+HasAdvertising+InEnglish,data=rawdata)
summary(fullmodel)
fullmodelrmse = sqrt(sum((fullmodel$residuals)^2)/length(fullmodel$residuals))

modelu = lm(log(PageViews)~log(UniqueVisitors),data=rawdata)
summary(modelu)
modelurmse = sqrt(sum((modelu$residuals)^2)/length(modelu$residuals))

modelh = lm(log(PageViews)~HasAdvertising,data=rawdata)
summary(modelh)
modelhrmse = sqrt(sum((modelh$residuals)^2)/length(modelh$residuals))

modeli = lm(log(PageViews)~InEnglish,data=rawdata)
summary(modeli)
modelirmse = sqrt(sum((modeli$residuals)^2)/length(modeli$residuals))

#�u���e100��InEnglish�S�򥢭Ȫ���Ƨ@²��j�k�A��ƭn��Ū�B���i�����17��A���٬O�n�����25��
model_100english = lm(log(PageViews)~InEnglish,data=rawdata)
summary(model_100english)


#����ҫ����]
plot(fullmodel)
library(car)
ncvTest(fullmodel)#����P���ܲ��ơA�S�L
durbinWatsonTest(fullmodel)#����W�ߡA���L
shapiro.test(fullmodel$residuals)#����`�A�A�S�L






