rm(list = ls())
gc(reset = T)

#install.packages(c('httr','rvest'))
library(httr)
library(rvest)
setwd("C:/Users/user/Documents/SNU/2학기/탐색적자료분석및 시각화/공공데이타분석/")
service_key = "gfwgG1b5KAPvJxW7eJdsRtBg3487NmkTs5vZrIBNDxCMQ5Sfz9iWR6ORO8hqWiWlSvQhQsQimjOSarxpEJdz9A%3D%3D"

air_data = NULL
i=1
for (i in 1:100000){
url = paste0("http://openapi.onbid.co.kr/openapi/services/",
             "UtlinsttPblsalThingInquireSvc/getPublicSaleObject?",
            # "DPSL_MTD_CD=",0001,
             "pageNo=",i,
             "&numOfRows=aaa",27,
             "&serviceKey=",service_key)

url_get = GET(url)
url_xml = read_xml(url_get)
item_list = xml_nodes(url_xml, 'items item')
#tmp_item = xml_text(tmp_item)
item_list = lapply(item_list, function(x) return(xml_text(xml_children(x))))
item_dat = do.call('rbind',item_list)
item_dat = data.frame(item_dat, stringsAsFactors = F)
tmp = xml_nodes(url_xml, 'items item') 
colnames_dat = html_name(xml_children(tmp[[1]]))
colnames(item_dat) = colnames_dat
air_data = rbind(item_dat, air_data)
write.csv(air_data, paste0("gongmea_data_",i,".csv"), row.names = T)

}


setwd("C:/Users/user/Documents/SNU/2학기/탐색적자료분석및 시각화/공공데이타분석/")
getwd()
air_data <- read.csv("gongmea_data_724.csv", stringsAsFactors=FALSE ) 

### Data 
head(air_data)
tail(air_data)
air_data[1:6,]

### 중복된 항목 제거 데이터프레임[ !duplicated( 데이터프레임$기준변수) , ] 

str(air_data)

air_data_dubrm=air_data[!duplicated(air_data$CLTR_MNMT_NO),]

# 12010 개 데이타 
# 총 724 번 쿼리조 조회함 

## 날짜 분석 

dim(air_data_dubrm)
mindate=min(air_data_dubrm$PBCT_BEGN_DTM)
maxdate=max(air_data_dubrm$PBCT_BEGN_DTM)

### 종류별 건수 비고 

#install.packages("dplyr")
library(dplyr)

dim(air_data_dubrm)


# table df 로 변환 
tbl_df_real=tbl_df(air_data_dubrm)


# 결측치 제거 

replace(tbl_df_real$APSL_ASES_AVG_AMT,tbl_df_real$APSL_ASES_AVG_AMT=="-","0")-> tbl_df_real$APSL_ASES_AVG_AMT

replace(tbl_df_real$APSL_ASES_AVG_AMT,tbl_df_real$APSL_ASES_AVG_AMT=="NA","0")-> tbl_df_real$APSL_ASES_AVG_AMT

# 형변환 (각 매물의 감정가 : APSL_ASES_AVG_AMT)
tbl_df_real$APSL_ASES_AVG_AMT=as.numeric(tbl_df_real$APSL_ASES_AVG_AMT)


# 각 물건의 이름과, 감정가를 선택하여 Data Frame 으로 저장 

CTGR_FULL_NM_df=(tbl_df_real %>% select(CTGR_FULL_NM, APSL_ASES_AVG_AMT))


# 각 물건 별로 통계량 계산하기 위한 Group by 
planes=group_by(tbl_df_real,CTGR_FULL_NM)
delay <- summarise(planes, count_2 = n(), mean_v=mean(APSL_ASES_AVG_AMT), max_v=max(APSL_ASES_AVG_AMT),min_v=min(APSL_ASES_AVG_AMT) )

## 정렬
delay_sorted=arrange(delay, desc(count_2) )

### 종류별 건수 비고를 위한 통계량 분석 및 데이타 핸들링 - 물건 Categrory 나누기 
### '/' 로 나누어져 있는 물건의 Category 를 1 Group , 2Group Catefory 로 상세화 
### Category Group Parcing 

#install.packages("stringr")
library(stringr)

# / 을 기준으로 2개의 그룹으로 나눈다. 

temp=str_split(delay_sorted$CTGR_FULL_NM,"/",n = 2)
Dates <- sapply(temp, "[", 1)
Rates <- sapply(temp, "[", 2)

# 새로운 대분류 , 중분류 컬럼을 생성해서 넣어준다. (Transform 사용)

delay_sorted_split=transform(delay_sorted, group_1=sapply(temp, "[", 1), group_2=sapply(temp, "[", 2))
planes2=group_by(delay_sorted_split,group_1)
delay2 <- summarise(planes2, group2_num = n(), count_2=sum(count_2) , mean_v=mean(mean_v), max_v=max(max_v),min_v=min(min_v) )



### ggplot2 으로 시각화 
## 각 Group 별 물건 수 비교 (Top 20) 
## 물건 많은 순으로 정렬

library(ggplot2)

delay_sorted2=arrange(delay2, desc(count_2) )

p=ggplot(data=head(delay_sorted2,n=15),aes(x=reorder(group_1,-count_2), y=count_2)) 
p + geom_col(colour="grey")  + 
  xlab("물건 종류(대분류)")  + 
  ylab("물건 수") + 
  ggtitle("2017년 5월 16일 ~11월 2일 사이 공매기간인 물건 분석 ") + 
  geom_text(aes(label = group_1), size = 4,vjust = -0.3 ) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

### 각Group 별 최대 감정가 

p=ggplot(data=head(delay_sorted2,n=15),aes(x=reorder(group_1,-count_2), y=round(max_v/10000000,0))) 
p + geom_col()  + 
  xlab("물건 종류(대분류)")  + 
  ylab("Max 감정가 천만원") + 
  ggtitle("2017년 5월 16일 ~11월 2일 사이 공매기간인 물건 분석") +
  geom_text(aes(label = round(max_v/10000000,3)), size = 4,vjust = -0.3 ) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
  
### 각Group 별 평균 감정가
  
  p=ggplot(data=head(delay_sorted2,n=15),aes(x=reorder(group_1,-count_2), y=round(mean_v/10000000,0))) 
p + geom_col()  + 
  xlab("물건 종류(대분류)")  + 
  ylab("평균 감정가 천만원") + 
  ggtitle("2017년 5월 16일 ~11월 2일 사이 공매기간인 물건 분석") +
  geom_text(aes(label = round(mean_v/10000000,3)), size = 4,vjust = -0.3 ) +  
  theme(axis.text.x=element_text(angle=90, hjust=1))
  

### 관심이 제일 많은 
# 상가용및업무용건물, 토지, 주거용건물 
#
# 각 물건 별로 통계량 계산하기 위한 Group by 
# / 을 기준으로 2개의 그룹으로 나눈다. 

temp=str_split(tbl_df_real$CTGR_FULL_NM,"/",n = 2)
Dates <- sapply(temp, "[", 1)
Rates <- sapply(temp, "[", 2)

# 새로운 대분류 , 중분류 컬럼을 생성해서 넣어준다. (Transform 사용)

tbl_df_real_split=transform(tbl_df_real, group_1=sapply(temp, "[", 1), group_2=sapply(temp, "[", 2))
tbl_df_real_split=tbl_df(tbl_df_real_split)

tbl_df_real_split$group_1=as.character(tbl_df_real_split$group_1)

# remove space 
replace(tbl_df_real_split$group_1,tbl_df_real_split$group_1=="토지 ","토지")-> tbl_df_real_split$group_1
replace(tbl_df_real_split$group_1,tbl_df_real_split$group_1=="상가용및업무용건물 ","상가용및업무용건물")-> tbl_df_real_split$group_1
replace(tbl_df_real_split$group_1,tbl_df_real_split$group_1=="주거용건물 ","주거용건물")-> tbl_df_real_split$group_1
replace(tbl_df_real_split$group_1,tbl_df_real_split$group_1=="용도복합용건물 ","용도복합용건물")-> tbl_df_real_split$group_1

tbl_df_real_split$group_1
a=filter(tbl_df_real_split, group_1 == "토지" )
b=filter(tbl_df_real_split, group_1 == "상가용및업무용건물" )
c=filter(tbl_df_real_split, group_1 == "주거용건물" )
d=filter(tbl_df_real_split, group_1 == "용도복합용건물" )

tbl_df_real_split_filterd=rbind(a,b,c,d)

#####  2 그룹 분석 
#####  1그룹 토지, 상가용 및 업무용 건물, 주거용 건물, 용도 복합룔 건물


### 각 Group 별 물건 수 비교 (Top 20) 
### 물건 많은 순으로 정렬


library(ggplot2)


p=ggplot(data=tbl_df_real_split_filterd, aes(x=group_2))
p + geom_bar()  + 
  xlab("물건 종류(중분류)")  + 
  ylab("물건 수") + 
  ggtitle("2017년 5월 16일 ~11월 2일 사이 공매기간인 물건 분석 ") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) 

