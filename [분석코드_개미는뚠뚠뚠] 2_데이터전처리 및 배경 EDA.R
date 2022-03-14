# 2021 금융데이터 경진대회 : 개미는 뚠뚠뚠 팀
# [[1. 데이터 전처리 및 배경 EDA ]]
## 목차
## 1. 데이터 전처리
##  1) 패키지 불러오기
##  2) Finance데이터 PCA
##  3)  Trend 데이터 PCA
##  4) 코로나 이후 상승 종목
## 2. 배경 EDA
##  1) 데이터 전처리
##  2) 날짜별 총체결금액
##  3) 나이대별 거래횟수/ 총체결금액
##  4) 나이대별 거래소별 수량/금액
##  5)매수/매도 횟수 상위 30개
##  6) 테마별 거래 횟수 상위 30개


setwd("C:/Users/User/Desktop/분석코드_개미는뚠뚠뚠") #### 코드 제출 파일경로로 바꿔주세요
getwd()

## 1. 데이터 전처리
### 1) 패키지 불러오기
need_packages <- c("tidyverse", "data.table") #사용할 라이브러리를 불러옵니다
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())


### 2) Finance데이터 PCA
### : 시장변동성을 설명할 수 있는 지수 KOSPI와 KOSDAQ 데이터를 PCA를 사용하여 하나의 요인으로 합쳐줍니다.
###   이것은 투자자의 민감도 지수(트랜드베타지수, 금융베타지수)를 만들 때 사용될 예정입니다.

#### (1)  KOSPI(코스피) 종가 일별 데이터
kospi_price <- fread("data/kospi_price.csv",data.table = F, encoding="UTF-8")
kospi_price <- kospi_price[,1:2]
kospi_price$Date <- kospi_price$Date %>% as.Date()


#### (2)  KOSDAQ(코스닥) 종가 일별 데이터
kosdaq_price <- fread("data/kosdaq_price.csv",data.table = F, encoding="UTF-8")
kosdaq_price <- kosdaq_price[,1:2]
kosdaq_price$Date <- kosdaq_price$Date %>% as.Date()


#### (3)  PCA
finance_data <- left_join(kospi_price,kosdaq_price,by="Date")
finance_pca <- finance_data[,c(2:3)]

finance_pca <- prcomp(finance_pca, scale = TRUE)
summary(finance_pca) #### PC1의 cumluative proportion 0.9951 : 분산의 누적 합계가 90%이상이므로 PC1은 두 변수를 잘 설명한다고 할 수 있습니다.

finance_data$PC1 <- finance_pca$x[,1]
finance_data %>% head()

write.csv(finance_data,"data/finance_pca.csv")

### 3)  Trend 데이터 PCA
### : 금융트랜드를 설명할 수 있는 네이버 트랜드과 구글 트랜드 데이터를 PCA를 사용하여 하나의 요인으로 합쳐줍니다.
###  이것은 투자자의 민감도 지수(트랜드베타지수, 금융베타지수)를 만들 때 사용될 예정입니다.

#### (1) Google 트랜드 데이터
google_trend <- fread("data/google_trend.csv",data.table = F, encoding="UTF-8")
colnames(google_trend) <- c("Date","google")
google_trend$Date <- google_trend$Date %>% as.Date()
google_trend$google <- google_trend$google %>% as.numeric()

#### (2)  Naver 트랜드 데이터
naver_trend <- fread("data/naver_trend.csv",data.table = F, encoding="UTF-8")
naver_trend <- naver_trend[-c(1:6),]
colnames(naver_trend) <- c("Date","naver")
naver_trend$Date <- naver_trend$Date %>% as.Date()
naver_trend$naver <- naver_trend$naver %>% as.numeric()

#### (3) PCA
trend_data <- left_join(google_trend,naver_trend, by="Date")
trend_pca <- trend_data[,c(2:3)]

trend_pca <- prcomp(trend_pca, scale = TRUE)
summary(trend_pca) #### PC1의 cumluative proportion 0.9554 : 분산의 누적 합계가 90%이상이므로 PC1은 두 변수를 잘 설명한다고 할 수 있습니다.

trend_data$PC1 <- trend_pca$x[,1]

write.csv(trend_data,"data/trend_pca.csv")


### 4) 코로나 이후 상승 종목
### : 코로나시기에 대부분의 종목주가가 하락했음에도 불구하고, 주가가 상승한 회사를 구하였습니다.
###   투자자의 투자성향을 분석하기위해 후에 사용할 예정입니다.

#### (1) 데이터불러오기
firm1_200120 <- fread("data/200120 kospi.csv",data.table = F) #### 20년 1월 20일에 코스피 데이터
firm2_200120 <- fread("data/200120 kosdaq.csv",data.table = F) #### 20년 1월 20일에 코스닥 데이터
firm2_200120 <- firm2_200120[,-3]
firm_200120 <- rbind(firm1_200120,firm2_200120)
firm_200120 <- firm_200120[,c(1,3)]
firm1_200331 <- fread("data/200331 kospi.csv",data.table = F) #### 20년 3월 31일에 코스피 데이터
firm2_200331 <- fread("data/200331 kosdaq.csv",data.table = F) #### 20년 3월 31일에 코스피 데이터
firm2_200331 <- firm2_200331[,-3]
firm_200331 <- rbind(firm1_200331,firm2_200331)
firm_200331 <- firm_200331[,c(1,3)]

#### (2) 데이터 전처리
firm <- left_join(firm_200120,firm_200331,by="종목코드")
firm <- firm[,-4]
firm <- firm %>% rename("종목명"="종목코드")
firm <- firm %>% rename("종가1"="종가.x")
firm <- firm %>% rename("종가2"="종가.y")

firm$종가1<-firm$종가1 %>% as.numeric()
firm$종가2<-firm$종가2 %>% as.numeric()

#### (3) 주가가 상승한 데이터 필터링
firm <- firm %>% filter(종가2 - 종가1 > 0) #### 20년 3월 31일에 주가가 더 상승한 종목을 필터링 합니다.
firm %>% filter(종가2 - 종가1 > 0) %>% count() #### 총 192개의 종목이 20.1.20 ~ 20.3.31 에 주가가 상승하였습니다.

write.csv(firm,"data/firm_up.csv")




## 2. 배경 EDA
### 1) 데이터 전처리
#### (1) 패키지 불러오기
need_packages <- c("tidyverse", "ggplot2", "RColorBrewer", "data.table") #사용할 라이브러리를 불러옵니다
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())


data1 <- read.csv("data/2021금융데이터 경진대회_데이터셋_한국투자증권(원본).csv")
data1 %>% head()
data1 %>% str()


### 2) 날짜별 총체결금액
data1$주문일자 %>% unique() %>% length() #50일 : 2020.01.20~2020.03.31
date_count <- data1 %>% group_by(주문일자,매도매수구분코드) %>% summarise(count = n(), sum_n =sum(총체결수량), sum_price = sum(총체결금액))
date_count$주문일자 <- as.Date(as.character(date_count$주문일자), format = '%Y%m%d')
date_count$매도매수구분코드[date_count$매도매수구분코드==1] <- "매도"
date_count$매도매수구분코드[date_count$매도매수구분코드==2] <- "매수"

mcolor <- c("#81D8D0","#3CAEA3","#6AA2CD","#B43C8A","#383275")

g1 <- ggplot(date_count, aes(x=주문일자, y = sum_price, group =매도매수구분코드, colour = 매도매수구분코드 )) + geom_line()+   geom_point(size=2) + labs(x = "날짜", y = "금액", title = "날짜별 총체결금액") + theme_minimal() + theme(plot.title = element_text(size=20))+scale_fill_manual(values = mcolor) + scale_color_manual(values = mcolor)+ theme(legend.position = "bottom")
g1

### 3) 나이대별 거래횟수/ 총체결금액
age_count <- data1 %>% group_by(동일나이군구분코드) %>%summarise(count = n(), sum_price = sum(총체결금액))
age_count$동일나이군구분코드 <- c("20대이하", "30대", "40대", "50대이상")
mcolor <- c("#3CAEA3","#6AA2CD","#B43C8A","#383275")

g2 <- ggplot(age_count, aes(x=동일나이군구분코드, y=count, fill = 동일나이군구분코드, colour = 동일나이군구분코드)) + geom_col( alpha = 0.7) + theme_minimal() + labs(x = "나이대", y = "거래횟수", title = "나이대별 거래 횟수") + geom_text(aes(label = count), position = position_stack(0.5), size = 7)+ theme(plot.title = element_text(size=20)) + scale_fill_manual(values = mcolor) + scale_color_manual(values = mcolor)
g2

g3 <- ggplot(age_count, aes(x=동일나이군구분코드, y=sum_price, fill = 동일나이군구분코드, colour = 동일나이군구분코드)) + geom_col( alpha = 0.7) + theme_minimal() + labs(x = "나이대", y = "총체결금액", title = "나이대별 총체결금액") + geom_text(aes(label = count, color = 동일나이군구분코드), position = position_stack(0.5), size = 7)+ theme(plot.title = element_text(size=20)) + scale_fill_manual(values = mcolor) + scale_color_manual(values = mcolor)
g3

### 4) 나이대별 거래소별 수량/금액
market_count <- data1 %>% group_by(거래소구분코드, 매도매수구분코드) %>% summarise(count = n(), sum_n =sum(총체결수량), sum_price = sum(총체결금액))
market_count$거래소구분코드[market_count$거래소구분코드==2] <- "코스피"
market_count$거래소구분코드[market_count$거래소구분코드==3] <- "코스닥"
market_count$매도매수구분코드[market_count$매도매수구분코드==1] <- "매도"
market_count$매도매수구분코드[market_count$매도매수구분코드==2] <- "매수"
market_count
market_count <- market_count %>% mutate(구분 = paste0(거래소구분코드, "/",매도매수구분코드))
market_count

age_k_count <- data1 %>% group_by(동일나이군구분코드,거래소구분코드) %>% summarise(count = n(), sum_n =sum(총체결수량), sum_price = sum(총체결금액))
age_k_count$동일나이군구분코드[age_k_count$동일나이군구분코드==20] <- "20대이하"
age_k_count$동일나이군구분코드[age_k_count$동일나이군구분코드==30] <- "30대"
age_k_count$동일나이군구분코드[age_k_count$동일나이군구분코드==40] <- "40대"
age_k_count$동일나이군구분코드[age_k_count$동일나이군구분코드==50] <- "50대 이상"
age_k_count$거래소구분코드[age_k_count$거래소구분코드==2] <- "코스피"
age_k_count$거래소구분코드[age_k_count$거래소구분코드==3] <- "코스닥"

g4 <- ggplot(age_k_count, aes(x=동일나이군구분코드, y=sum_n, fill = 거래소구분코드, colour = 거래소구분코드)) + geom_bar(stat = 'identity',alpha = 0.7, position='fill') + theme_minimal() + labs(x = "나이대", y = "거래횟수", title = "나이대별 거래소별 수량") + theme(plot.title = element_text(size=20)) +scale_fill_manual(values = mcolor) + scale_color_manual(values = mcolor)
g4

g5 <- ggplot(age_k_count, aes(x=동일나이군구분코드, y=sum_price, fill = 거래소구분코드, colour = 거래소구분코드)) + geom_bar(stat = 'identity',alpha = 0.7, position='fill') + theme_minimal() + labs(x = "나이대", y = "거래횟수", title = "나이대별 거래소별 금액") + theme(plot.title = element_text(size=20))+scale_fill_manual(values = mcolor) + scale_color_manual(values = mcolor)
g5

### 5)매수/매도 횟수 상위 30개
product_count <- data1 %>% group_by(상품명, 매도매수구분코드) %>% summarise(count = n(), sum_n =sum(총체결수량), sum_price = sum(총체결금액))
product_count_1 <- product_count %>% filter(매도매수구분코드 == 1) %>% arrange(-count)
product_count_2 <- product_count %>% filter(매도매수구분코드 == 2) %>% arrange(-count)

product_count_1 <- product_count_1 %>% head(30) %>% data.frame()
product_count_2 <- product_count_2 %>% head(30) %>% data.frame()

g6 <- ggplot(product_count_1, aes(x=reorder(상품명, count), y=count, fill = count, colour = count)) + geom_bar(stat="identity", alpha = 0.7) + scale_fill_gradient("거래횟수", low = "#81D8D0", high = "Cadet Blue 4") + scale_color_gradient(guide = "none", low = "#81D8D0", high = "Cadet Blue 4")+ theme_minimal()+
  theme(axis.title=element_text(size=12), plot.title=element_text(size=20)) +labs(x = "상품명", y = "매도 횟수", title = "매도 횟수 상위 30 상품명")  + coord_flip() + geom_text(aes(x=상품명, y=count,label=count),size=4, position = position_stack(vjust = .5))
g6

g7 <- ggplot(product_count_2, aes(x=reorder(상품명, count), y=count, fill = count, colour = count)) + geom_bar(stat="identity", alpha = 0.7) + scale_fill_gradient("거래횟수", low = "#81D8D0", high = "Cadet Blue 4") + scale_color_gradient(guide = "none", low = "#81D8D0", high = "Cadet Blue 4")+ theme_minimal()+
  theme(axis.title=element_text(size=12), plot.title=element_text(size=20)) +labs(x = "상품명", y = "매수 횟수", title = "매수 횟수 상위 30 상품명")  + coord_flip() + geom_text(aes(x=상품명, y=count,label=count),size=4, position = position_stack(vjust = .5))
g7

### 6) 테마별 거래 횟수 상위 30개
sector <- fread("data/테마주 및 업종명.csv",data.table = F, encoding="UTF-8")
names(sector)[1] <- c("테마")
names(sector)[2] <- c("상품명")
names(sector)[3] <- c("단축코드")

sector_data <- left_join(data1,sector,by ="상품명")
sector_count <- sector_data %>% group_by(테마) %>% summarise(count=n()) %>% data.frame()
sector_count <- sector_count %>% arrange(desc(count))
sector_count <- sector_count[-c(0, 1),]
sector_count1 <- sector_count %>% head(30)

g8 <- ggplot(sector_count1, aes(x=reorder(테마, -count), y=count, fill = count, colour = count)) + geom_bar(stat="identity", alpha = 0.7) + scale_fill_gradient("거래횟수", low = "#81D8D0", high = "#383275") + scale_color_gradient(guide = "none", low = "#81D8D0", high = "#383275")+
  theme(panel.background = element_rect(fill = "white" , color = mcolor), text = element_text(face = "bold"), axis.text.x=element_text(angle=90), axis.title=element_text(size=15),plot.title=element_text(size=20)) +labs(x = "테마명", y = "거래횟수", title = "거래횟수 상위 30 테마")+ theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8) ) 
g8
