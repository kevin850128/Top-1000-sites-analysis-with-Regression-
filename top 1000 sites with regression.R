#rawdata = read.table("C:/Users/Kevin/Desktop/top_1000_sites.tsv.txt",sep="\t",header = TRUE)
#不要用上面那行的方法，讀進來會少234筆資料

library(readr)
rawdata <- read_delim("https://raw.githubusercontent.com/johnmyleswhite/ML_for_Hackers/24e55baa672c4ece0579423fabf220fa2b907a71/05-Regression/data/top_1000_sites.tsv", delim="\t")
head(rawdata)
str(rawdata)

## missing value
for ( i in 1:length(rawdata[1,]) ){
  na <- sum(is.na(rawdata[, i]) )
  print( na )
}     #  Category:47筆 , InEnglish:900筆 , TLD:900筆 missing value


#處理missing value，因為Category和TLD這兩個欄位我不關心，也不會拿來建模，就不處理了
rawdata$InEnglish[is.na(rawdata$InEnglish)] = "NA" 
#此筆資料的後900筆在InEnglish欄位都是NA，因此跑迴歸只會用前100筆去跑，並非我們想要的
#所以這行非常重要，要把NA也當成一個level去跑迴歸


#將類別欄位轉成factor
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
#但17行不可執行
#為了畫下面的correlation plot，才需要執行上面兩行，因為ggcorr讀不懂factor型態資料，故需轉成dummy variable
ggcorr(rawdata[c("PageViews","UniqueVisitors","Reach","HasAdvertising","InEnglish")],label = TRUE,label_round = 2)


####迴歸#######
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

#只拿前100筆InEnglish沒遺失值的資料作簡單迴歸，資料要重讀且不可執行第17行，但還是要執行第25行
model_100english = lm(log(PageViews)~InEnglish,data=rawdata)
summary(model_100english)


#檢驗模型假設
plot(fullmodel)
library(car)
ncvTest(fullmodel)#檢驗同質變異數，沒過
durbinWatsonTest(fullmodel)#檢驗獨立，有過
shapiro.test(fullmodel$residuals)#檢驗常態，沒過







