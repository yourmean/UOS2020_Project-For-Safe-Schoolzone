
setwd("C:\\Users\\moonf\\Desktop\\2020.2학기\\빅데이터경진대회\\데이터")
if(!require(readxl)){install.packages("readxl"); library(readxl)}
if(!require(stringr)){install.packages("stringr"); library(stringr)}
if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(reshape2)){install.packages("reshape2"); library(reshape2)}
if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}

# 5개년 전국의 시간별 사고건수

time_trf<-read_excel("시간대별어린이(12세이하)교통사고.xls",
                     sheet="Sheet1",
                     col_names=F,
                     col_types="guess",
                     na="NA")
time_trf<-time_trf[c(1:(which.max(time_trf[,1]=="서울")-1)),]
time_trf<-as.matrix(time_trf)
time_trf<-t(time_trf)

colnames(time_trf)<-c("기준년도", "시간","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
time_trf<-time_trf[-c(1,2),]
time_trf<-as.data.frame(time_trf)
time_trf<-time_trf[-which(time_trf$시간=="합계"),]
rownames(time_trf)<-NULL
time_trf[,3:8]<-sapply(time_trf[,3:8],as.numeric)
head(time_trf)


# 5개년 전국의 스쿨존내 시간별 사고건수

time_s_trf<-read_excel("시간대별스쿨존내어린이(12세이하)교통사고.xls",
                     sheet="Sheet1",
                     col_names=F,
                     col_types="guess",
                     na="NA")
time_s_trf<-time_s_trf[c(1:(which.max(time_s_trf[,1]=="서울")-1)),]
time_s_trf<-as.matrix(time_s_trf)
time_s_trf<-t(time_s_trf)

colnames(time_s_trf)<-c("기준년도", "시간","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
time_s_trf<-time_s_trf[-c(1,2),]
time_s_trf<-as.data.frame(time_s_trf)
time_s_trf<-time_s_trf[-which(time_s_trf$시간=="합계"),]
rownames(time_s_trf)<-NULL
time_s_trf[,3:8]<-sapply(time_s_trf[,3:ncol(time_s_trf)],as.numeric)

# 5개년 전국의 요일별 어린이사고건수

week_trf<-read_excel("요일별어린이(12세이하)교통사고.xls",
                     sheet="Sheet1",
                     col_names=F,
                     col_types="guess",
                     na="NA")

week_trf<-week_trf[c(1:(which.max(week_trf[,1]=="서울")-1)),]
week_trf<-as.matrix(week_trf)
week_trf<-t(week_trf)

colnames(week_trf)<-c("기준년도","요일","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
week_trf<-week_trf[-c(1,2),]
week_trf<-as.data.frame(week_trf)
week_trf<-week_trf[-which(week_trf$요일=="합계"),]
rownames(week_trf)<-NULL
week_trf[,3:ncol(week_trf)]<-sapply(week_trf[,3:ncol(week_trf)],as.numeric)
head(week_trf)


# 5개년 전국의 스쿨존내 요일별 어린이사고건수

week_s_trf<-read_excel("요일별스쿨존내어린이(12세이하)교통사고.xls",
                     sheet="Sheet1",
                     col_names=F,
                     col_types="guess",
                     na="NA")

week_s_trf<-week_s_trf[c(1:(which.max(week_s_trf[,1]=="서울")-1)),]
week_s_trf<-as.matrix(week_s_trf)
week_s_trf<-t(week_s_trf)

colnames(week_s_trf)<-c("기준년도","요일","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
week_s_trf<-week_s_trf[-c(1,2),]
week_s_trf<-as.data.frame(week_s_trf)
week_s_trf<-week_s_trf[-which(week_s_trf$요일=="합계"),]
rownames(week_s_trf)<-NULL
week_s_trf[,3:ncol(week_s_trf)]<-sapply(week_s_trf[,3:ncol(week_s_trf)],as.numeric)
head(week_s_trf)


# # 5개년 전국의 연령별어린이(12세이하)사상자
# 
# age_trf<-read_excel("연령별어린이(12세이하)사상자.xls",
#                     sheet="Sheet1",
#                     col_names=F,
#                     col_types="guess",
#                     na="NA")
# age_trf<-age_trf[c(1:(which.max(age_trf[,1]=="서울")-1)),]
# age_trf<-as.matrix(age_trf)
# age_trf<-t(age_trf)
# 
# colnames(age_trf)<-c("기준년도","연령","사망","부상")
# age_trf<-age_trf[-c(1,2),]
# age_trf<-as.data.frame(age_trf)
# age_trf<-age_trf[-which(age_trf$연령=="합계"),]
# rownames(age_trf)<-NULL
# age_trf[,3:4]<-sapply(age_trf[,3:4],as.numeric)



# 5개년 전국의 법규위반

law_trf<-read_excel("법규위반별스쿨존내어린이(12세이하)교통사고.xls",
                     sheet="Sheet1",
                     col_names=F,
                     col_types="guess",
                     na="NA")

law_trf<-law_trf[c(1:(which.max(law_trf[,1]=="서울")-1)),]
law_trf<-law_trf[-2,]
law_trf<-as.matrix(law_trf)
law_trf<-t(law_trf)

colnames(law_trf)<-c("기준년도","법규","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
law_trf<-law_trf[-c(1,2),]
law_trf<-as.data.frame(law_trf)
law_trf<-law_trf[-which(law_trf$법규=="합계"),]
rownames(law_trf)<-NULL
law_trf[,3:ncol(law_trf)]<-sapply(law_trf[,3:ncol(law_trf)],as.numeric)

# 5개년 전국의 사고유형
# 
# type_trf<-read_excel("사고유형별어린이(12세이하)교통사고.xls",
#                     sheet="Sheet1",
#                     col_names=F,
#                     col_types="guess",
#                     na="NA")
# 
# type_trf<-type_trf[c(1:(which.max(type_trf[,1]=="서울")-1)),]
# type_trf<-as.matrix(type_trf)
# type_trf<-t(type_trf)
# 
# colnames(type_trf)<-c("기준년도","사고유형","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
# type_trf<-type_trf[-c(1,2),]
# type_trf<-as.data.frame(type_trf)
# type_trf<-type_trf[-which(type_trf$사고유형=="합계"),]
# rownames(type_trf)<-NULL
# type_trf[,3:ncol(type_trf)]<-sapply(type_trf[,3:ncol(type_trf)],as.numeric)

# 5개년 월별 어린이 교통사고 사고건수
month_trf<-read_excel("월별어린이(12세이하)교통사고.xls",
                      sheet="Sheet1",
                      col_names=F,
                      col_types="guess",
                      na="NA")
month_trf<-month_trf[c(1:(which.max(month_trf[,1]=="서울")-1)),]
month_trf<-as.matrix(month_trf)
month_trf<-t(month_trf)

colnames(month_trf)<-c("기준년도", "월","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
month_trf<-month_trf[-c(1,2),]
month_trf<-as.data.frame(month_trf)
month_trf<-month_trf[-which(month_trf$월=="합계"),]
rownames(month_trf)<-NULL
month_trf[,3:8]<-sapply(month_trf[,3:8],as.numeric)

# 5개년 월별 스쿨존내 어린이 교통사고 사고건수

month_s_trf<-read_excel("월별스쿨존내어린이(12세이하)교통사고.xls",
                      sheet="Sheet1",
                      col_names=F,
                      col_types="guess",
                      na="NA")
month_s_trf<-month_s_trf[c(1:(which.max(month_s_trf[,1]=="서울")-1)),]
month_s_trf<-as.matrix(month_s_trf)
month_s_trf<-t(month_s_trf)

colnames(month_s_trf)<-c("기준년도", "월","사고건수","사망자수","부상자수","중상자수","경상자수","부상신고자수")
month_s_trf<-month_s_trf[-c(1,2),]
month_s_trf<-as.data.frame(month_s_trf)
month_s_trf<-month_s_trf[-which(month_s_trf$월=="합계"),]
rownames(month_s_trf)<-NULL
month_s_trf[,3:8]<-sapply(month_s_trf[,3:8],as.numeric)
 




















































