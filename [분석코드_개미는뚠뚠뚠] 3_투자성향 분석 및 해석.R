# 2021 금융데이터 경진대회 : 개미는 뚠뚠뚠 팀
# [[1. 데이터 전처리 2 ]]
## 목차
## 1. 데이터 전처리
##  1) 데이터 전처리
##  2) 데이터 불러오기
##  3)  파생변수 만들기
##  4) 데이터 합쳐 하나의 데이터로 만들기
## 2. FA : 요인분석
##  1) 데이터 전처리
##  2) 요인분석 : Factor Analysis
##  3) 데이터 합치기
## 3. 클러스터링 : K-means
##  1) 데이터 전처리
##  2) 데이터 불러오기
##  3)  파생변수 만들기
##  4) 데이터 합쳐 하나의 데이터로 만들기
## 4. 투자성향분석 : 클러스터링 해석
##  1) 데이터 전처리
##  2) Factor anlysis 전 후 corplot
##  3) 클러스터 전반적 EDA
##  4)  클러스터1 - 한우물개미
##  5) 클러스터2 - 큰손개미
##  6) 클러스터3 - 주린이 개미
##  7) 안정개미(클러스터4)
##  8) 클러스터5 - 한방개미


setwd("C:/Users/User/Desktop/분석코드_개미는뚠뚠뚠") #### 코드 제출 파일경로로 바꿔주세요
getwd()

### 1. 데이터 전처리
#### 1) 패키지 불러오기
need_packages <- c("tidyverse", "stat") #사용할 라이브러리를 불러옵니다
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

### 2) 데이터 불러오기
### : 사용할 데이터를 불러옵니다.
data <- read.csv("data/2021금융데이터 경진대회_데이터셋_한국투자증권(원본).csv") #### 금융데이터 경진대회에서 제공한 한국투자증권 데이터입니다.
trend <- read.csv("data/trend_pca.csv") #### '데이터 전처리1.R' 에서 만든 trend요인 데이터 입니다.
finance <- read.csv("data/finance_pca.csv") #### '데이터 전처리1.R' 에서 만든 시장 변동성 요인 데이터 입니다.
kospi200_data <- read.csv("data/kospi200.csv") #### kospi 200데이터 입니다.
firm_up_data <- read.csv("data/firm_up.csv") #### '데이터 전처리1.R' 에서 만든 코로나 이후 주가 상승 종목 데이터입니다.

### 3)  파생변수 만들기
### : 동학개미운동에 참여한 신규 고객의 투자성향을 분석하기위해 투자자별 파생변수를 만들었습니다.
###   이후 이 파생변수를 활용하여 다양한 분석을 진행할 예정입니다.

#### (1) 나이대
age_data <- data %>% select(고객구분코드, 동일나이군구분코드) %>% unique() %>% data.frame()
colnames(age_data) <- c("id", "age")

#### (2) 총거래횟수
trade_cnt_data <- data %>% group_by(고객구분코드) %>% summarise(trade_cnt = n()) %>% data.frame()
colnames(trade_cnt_data) <- c("id", "trade_cnt")

#### (3) 총거래금액
trade_sum_p_data <- data %>% group_by(고객구분코드) %>% summarise(trade_sum_p = sum(총체결금액)) %>% data.frame()
colnames(trade_sum_p_data) <- c("id", "trade_sum_p")

#### (4) 총거래 수량
trade_sum_amt_data <- data %>% group_by(고객구분코드) %>% summarise(trade_sum_amt = sum(총체결수량)) %>% data.frame()
colnames(trade_sum_amt_data)<- c("id", "trade_sum_amt")

#### (5) 거래종목개수
stck_cnt_data <- data %>% group_by(고객구분코드) %>% summarise(stck_cnt = n_distinct(상품번호)) %>% data.frame()
colnames(stck_cnt_data) <- c("id", "stck_cnt")

#### (6) 거래날짜 횟수
trade_date_cnt_data <- data %>% group_by(고객구분코드) %>% summarise(trade_date_cnt = n_distinct(주문일자)) %>% data.frame()
colnames(trade_date_cnt_data) <- c("id", "trade_date_cnt")

#### (7) 우량주 거래 비율
kospi200_list <- sprintf("%06d", kospi200_data$종목코드)
data <- data %>% mutate(stck_code = substr(상품번호, 7,12)) %>% data.frame()

trade_sum_p_data %>% head() #### 투자자별 총 거래 금액 데이터
kospi200_trade_sum_data <- data %>% filter(stck_code %in% kospi200_list) %>% group_by(고객구분코드) %>% summarise(kospi200_trade_sum = sum(총체결금액)) %>% data.frame() #### 거래종목 중 kospi200리스트에 있는 종목만 선택합니다.
colnames(kospi200_trade_sum_data) <- c("id", "kospi200_trade_sum")
kospi200_ratio_data <- left_join(trade_sum_p_data, kospi200_trade_sum_data) #### 두 데이터를 합쳐줍니다다
kospi200_ratio_data %>% head(10)
kospi200_ratio_data$kospi200_trade_sum[is.na(kospi200_ratio_data$kospi200_trade_sum)]<- 0 #### NA값을 0으로 채워줍니다.
kospi200_ratio_data %>% head()
kospi200_ratio_data <- kospi200_ratio_data %>% mutate(kospi200_ratio = kospi200_trade_sum /trade_sum_p) %>% data.frame() #### 우량주 거래금액을 총 거래금액으로 나누어줍니다.
kospi200_ratio_data<-kospi200_ratio_data %>% select(id, kospi200_ratio) %>% data.frame()


#### (8) 코로나 이후 상승한 종목의 비율
firm_up_data$종목명 %>% length()
firm_up_list <- sprintf("%06d", firm_up_data$종목명)
firm_up_trade_sum_data <- data %>% filter(stck_code %in% firm_up_list) %>% group_by(고객구분코드) %>% summarise(firm_up_trade_sum = sum(총체결금액)) %>% data.frame() #### 코로나 이후 상승한 종목만 선택합니다.
colnames(firm_up_trade_sum_data) <- c('id', 'firm_up_trade_sum')
firm_up_trade_sum_data %>% head()
firm_up_ratio_data <- left_join(trade_sum_p_data, firm_up_trade_sum_data) #### 두 데이터를 합쳐줍니다.
firm_up_ratio_data %>% head()
firm_up_ratio_data$firm_up_trade_sum[is.na(firm_up_ratio_data$firm_up_trade_sum)]<- 0 #### NA값을 0으로 채워줍니다.
firm_up_ratio_data %>% head()
firm_up_ratio_data <- firm_up_ratio_data %>% mutate(firm_up_ratio = firm_up_trade_sum /trade_sum_p) %>% data.frame() #### 코로나 이후 상승 종목의 거래금액을 총 거래금액으로 나누어줍니다.
firm_up_ratio_data <- firm_up_ratio_data %>% select(id, firm_up_ratio) %>% data.frame()


#### (9) 매수비율
net_purchase_data2 <- data %>% filter(매도매수구분코드==2) %>% group_by(고객구분코드) %>% summarise(net_purchase2 = sum(총체결금액)) #### 투자자별 매수 거래 내역만 선택합니다.
net_purchase_data1 <- data %>% filter(매도매수구분코드==1) %>% group_by(고객구분코드) %>% summarise(net_purchase1=sum(총체결금액)) #### 투자자별 매도 거래 내역만 선택합니다.
colnames(net_purchase_data2) <- c("id", "net_purchase2")
colnames(net_purchase_data1) <- c("id", "net_purchase1")
net_purchase_data <- left_join(trade_sum_p_data, net_purchase_data2)
net_purchase_data <- left_join(net_purchase_data, net_purchase_data1)

net_purchase_data$net_purchase2[is.na(net_purchase_data$net_purchase2)] <- 0
net_purchase_data$net_purchase1[is.na(net_purchase_data$net_purchase1)] <- 0 #### NA를 0으로 채워줍니다.

net_purchase_data %>% head()
purchase_ratio_data <- net_purchase_data %>% mutate(purchase_ratio = net_purchase2/(net_purchase2 +  net_purchase1)) #### 매수 금액의 합을 매도/매수금액의 합으로 나누어 투자자별 매수 비율을 구합니다.
purchase_ratio_data <- purchase_ratio_data %>% select(c(id, purchase_ratio)) %>% data.frame()


#### (10) 개인 민감도 지수 (트랜드 베타 지수)
#### : 개인이 트랜드에 얼마나 민감하게 반응하는지에 대한 변수를 생성합니다.
id_data <- data %>% select(고객구분코드) %>% unique()
id_data$trend_beta <- rep(NA, dim(id_data)[1])

data %>% str()

trend$Date <- as.Date(trend$Date)
trend_dat <- trend %>% select(c(Date, PC1))
dim(id_data)[1]

for ( i in 1:dim(id_data)[1]){
  id_trade <- data %>% filter(고객구분코드 == id_data[i,1]) %>% filter(매도매수구분코드==2) %>% select(주문일자, 총체결금액) #### 한 투자자의 거래내역만 언택합니다
  id_trade <- id_trade %>% group_by(주문일자) %>% summarise(sum_p  = sum(총체결금액)) #### 날짜별로 총거래금액을 계산합니다.
  id_trade$주문일자 <- as.Date(as.character(id_trade$주문일자), format = '%Y%m%d')
  id_trade <- id_trade %>% mutate(trade_ratio = (sum_p/sum(sum_p))*100) #### 날짜별 거래금액을 전체거래금액으로 나누어 날짜별 거래금액의 비율을 계산합니다.
  colnames(id_trade) <- c('Date', 'sum_p', 'trade_ratio')
  if (dim(id_trade)[1] >= 3){ #### 거래횟수가 3이상인 경우에만 민감도 지수를 계산합니다. 3회 미만일 경우 민감도지수를 계산하는 것이 의미있지 않기 때문입니다.
    df <- left_join(id_trade, trend_dat, by = 'Date') %>% data.frame() #### 트랜드 요인 데이터와 합칩니다.
    #model <- lm(df$trade_ratio ~ df$PC1) #### 투자자 날짜별 거래금액과 트랜드요인으로 선형회귀분석을 시행합니다.
    #id_data[i, 2]<- model$coefficients[2] #### 회귀분석모형의 기울기를 민감도 지수로 선정합니다.
    Beta <- cov(df$trade_ratio, df$PC1)/var(df$PC1) #### 회귀직선의 기울기를 더 빠르게 계산하는 공식입니다.
    id_data[i, 2] <- Beta
  } else {
    id_data[i, 2]<- NA #### 거래내역이 3회 미만인 경우 NA로 처리합니다.
  }
  
  print(i)
}

trend_beta_data <- id_data
trend_beta_data %>% head()
colnames(trend_beta_data) <- c("id", "trend_beta")
trend_beta_data$trend_beta %>% summary() #### 분포 확인을 합니다.


#### (11) 개인 민감도 지수 (시장변동성 베타 지수)
id_data <- data %>% select(고객구분코드) %>% unique()
id_data$finance_beta <- rep(NA, dim(id_data)[1])
data %>% str()

finance$Date <- as.Date(finance$Date)
finance_dat <- finance %>% select(c(Date, PC1))
dim(id_data)[1]

for ( i in 1:dim(id_data)[1]){
  id_trade <- data %>% filter(고객구분코드 == id_data[i,1]) %>% filter(매도매수구분코드==2) %>% select(주문일자, 총체결금액) #### 한 투자자의 거래내역만 언택합니다
  id_trade <- id_trade %>% group_by(주문일자) %>% summarise(sum_p  = sum(총체결금액)) #### 날짜별로 총거래금액을 계산합니다.
  id_trade$주문일자 <- as.Date(as.character(id_trade$주문일자), format = '%Y%m%d')
  id_trade <- id_trade %>% mutate(trade_ratio = (sum_p/sum(sum_p))*100) #### 날짜별 거래금액을 전체거래금액으로 나누어 날짜별 거래금액의 비율을 계산합니다.
  colnames(id_trade) <- c('Date', 'sum_p', 'trade_ratio')
  if (dim(id_trade)[1] >= 3){ #### 거래횟수가 3이상인 경우에만 민감도 지수를 계산합니다. 3회 미만일 경우 민감도지수를 계산하는 것이 의미있지 않기 때문입니다.
    df <- left_join(id_trade, finance_dat, by = 'Date') %>% data.frame() #### 시장변동성 요인 데이터와 합칩니다.
    #model <- lm(df$trade_ratio ~ df$PC1) #### 투자자 날짜별 거래금액과 시장변동성요인으로 선형회귀분석을 시행합니다.
    #id_data[i, 2]<- model$coefficients[2] #### 회귀분석모형의 기울기를 민감도 지수로 선정합니다.
    Beta <- cov(df$trade_ratio, df$PC1)/var(df$PC1) #### 회귀직선의 기울기를 더 빠르게 계산하는 공식입니다.
    id_data[i, 2] <- Beta
  } else {
    id_data[i, 2]<- NA #### 거래내역이 3회 미만인 경우 NA로 처리합니다.
  }
  
  print(i)
}


finance_beta_data <- id_data
colnames(finance_beta_data) <- c("id", "finance_beta")
finance_beta_data$finance_beta %>% summary()


### 4) 데이터 합쳐 하나의 데이터로 만들기
### : 위에서 생성한 10개의 파생변수을 고객 id를 기준으로 합쳐줍니다.
df <- left_join(age_data,trade_cnt_data, by = 'id' )
df <- left_join(df,trade_sum_p_data, by = 'id' )
df <- left_join(df,trade_sum_amt_data, by = 'id' )
df <- left_join(df,stck_cnt_data, by = 'id' )
df <- left_join(df,trade_date_cnt_data, by = 'id' )
df <- left_join(df,kospi200_ratio_data, by = 'id' )
df <- left_join(df,firm_up_ratio_data, by = 'id' )
df <- left_join(df,purchase_ratio_data, by = 'id' )
df <- left_join(df,trend_beta_data, by = 'id' )
df <- left_join(df,finance_beta_data, by = 'id' )

df %>% head(20)
df %>% dim()
write.csv(df, "data/id_feature_data.csv", row.names = F)

##################################

## 2. FA : 요인분석

### 1) 데이터 전처리
#### (1) 패키지 불러오기
need_packages <- c("tidyverse", "chemometrics", "corrplot", "stats", "psych") #사용할 라이브러리
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

#### (2) 데이터 불러오기

data <- read.csv("data/id_feature_data.csv")
data %>% head()
data %>% dim() #24989 18

#### (3) 데이터에 NA 제거
#### 본격적인 분석을 하기 전에 NA를 가지고 있는 변수를 제거합니다.
data <- na.omit(data)

#### (4)  age를 Factor변수로 변환
data$age <- as.factor(data$age)

#### (5) 데이터 상관관계 파악
data_cor <- cor(data[,-c(1, 2)])
corrplot(data_cor, method = 'circle') #### 선형상관관계가 꽤 높은 몇개의 변수들이 보입니다. 상관관계가 높은 변수들을 FA를 통해 차원을 축소하고자 합니다.


#### (6) purchase_ratio 제거
#### : 매수비율 변수는 다른 요인에 비해 독립적인 요인을 가질 것이라고 판단하여 제거합니다.
id_purchase <- data %>% select(c(id, purchase_ratio)) %>% data.frame()
data <- data %>% select(-purchase_ratio)


### 2) 요인분석 : Factor Analysis

#### (1) 구형성 검정
df <- data[,-c(1, 2)] ####id, age 변수는 수치형 변수가 아니기 때문에 제외시킨 후 검정을 실시합니다.
cortest.bartlett(cor(df)) #### 귀무가설( : 모집단의 상관행렬은 단위행렬과 동일하다)이 가설을 기각합니다. 

#### (2) 고유치 확인
eigen(cor(df))$values #### 1이 넘는 고유치가 4개이므로 요인개수를 4개로 정합니다.

#### (3) Scree plot
df_cor <- cor(df, use = "pairwise.complete.obs")

scree(df_cor, factors = FALSE) #### 4~5에서 급격하게 줄어드므로 요인개수를 4개로 정합니다. 

#### (4) FA
set.seed(1234)
df_fa <- fa(df, nfactors=4, n.obs=N, rotate = "oblimin", fm="minres")
df_fa

df_fa$RMSEA #### 0.08이하이므로 요인분석 모델이 꽤 적합하다고 할 수 있습니다.

#### (5) 시각화
colnames(df_fa$loadings) <- c("Factor1", "Factor2",  "Factor3", "Factor4")
fa.diagram(df_fa)

#### (6) 요인추출
fa_dat <- df_fa$scores

df <- cbind(data$id, fa_dat) %>% data.frame()
df <- df[,c(1, 2, 5, 3, 4)]
colnames(df) <- c("id","Factor1", "Factor2",  "Factor3", "Factor4")

df$Factor1 <- as.numeric(df$Factor1)
df$Factor2 <- as.numeric(df$Factor2)
df$Factor3 <- as.numeric(df$Factor3)
df$Factor4 <- as.numeric(df$Factor4)

df$Factor2 <- -df$Factor2 #### Finance_beta가 양수로 요인이 만들어 졌기때문에 마이너스를 붙여 의미를 맞추어 줍니다.

### 3) 데이터 합치기
data <- read.csv("data/id_feature_data.csv")
data <- data %>% select(-purchase_ratio)
df %>% dim() #### 23463  5
df <- left_join(df, id_purchase, by= 'id' ) #### 매수비율 변수 를 id를 기준으로 합쳐줍니다.
df <- left_join(df, data, by = 'id') #### 파생변수들을 id를 기준으로 합쳐줍니다.

write.csv(df, "data/factor_data.csv", row.names = FALSE)

##################################################

## 3. 클러스터링 : K-means

### 1) 데이터 전처리
#### (1) 패키지 불러오기
need_packages <- c("tidyverse", "factoextra", "chemometrics", "DMwR", "NbClust") #사용할 라이브러리
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

#### (2) 메모리 문제 해결
memory.size()
memory.limit()
memory.limit(size = 20000)

#### (3) 데이터 불러오기
data <- read.csv("data/factor_data.csv")
data %>% head()
data <- data %>% select(id, Factor1, Factor2, Factor3, Factor4, purchase_ratio)
data %>% head()

### 2) 이상치 제거 
### : 클러스터링은 거리기반 알고리즘이기에 이상치에 민감합니다. 보다 정확한 클러스터를 만들고, 해석하기위하여 마할라노비스의 거리를 사용하여 이상치를 제거하였습니다. 

#### (1) 마할라노비스 거리
set.seed(1234)
dat <- Moutlier(data[,-1],quantile = 0.99) #### 1-0.99를 이상치로 분류합니다.
dat %>% head()
dat_outlier_cut <- dat$cutoff[1] #### 이상치를 판단하는 값(cut-off value)를 추출합니다.

dat_outlier_md <- dat$md #### 마할라노비스 거리 Vector를 만듭니다.

dat_outlier_md <- dat_outlier_md %>% as.vector() %>% unlist() %>% as.numeric()

outlier_md <- c()  #### 이상치를 담을 vector를 만듭니다.

for(i in 1:length(dat_outlier_md)){
  if (dat_outlier_md[i] >= dat_outlier_cut) {      #### cut-off value보다 크면 인덱스를 outlier_md에 저장합니다
    outlier_mdi <- i
    outlier_md <- append(outlier_md, outlier_mdi)}
}

outlier_md %>% length() #### 628개의 이상치가 존재합니다.
dat.md <-data[-c(outlier_md), ] #### 이상치보다 큰 인덱스를 제거합니다
dat.md %>% head()
dat.md %>% dim()

### 3) 클러스터링 : K-means
### : 생성한 변수를 통하여 투자자의 성향을 분류하고자합니다.
#### (1) 스케일링
scale.data <- scale(dat.md[,-1])

#### (2) Scree Plot 그리기
set.seed(1169)
scale.data <- scale.data[,c(1, 3, 4, 5)] #### 여러번 클러스터링을 해본 후 클러스터링이 가장 잘 된 변수를 선택합니다.
fviz_nbclust(scale.data, kmeans, method = "wss") #### K = 5에서 급격히 기울기가 줄어들기에 K를 5로 설정합니다.

#### (3) Kmeans 클러스터링
set.seed(1270) ##1247
km <- kmeans(scale.data, 5,iter.max = 1000)

fviz_cluster(km, data = scale.data, stand=T) + theme_light()

km$cluster %>% table

### 4) 데이터 만들기
cl <- km$cluster %>% as.vector() %>% unlist() #### cluster를 추출합니다.
df <- cbind(dat.md$id, cl) %>% data.frame()
colnames(df) <- c('id', 'cluster')

fa_data <- read.csv("data/factor_data.csv")
id_cl <- df
id_cl <- left_join(id_cl, fa_data, by ='id' )

id_cl$cluster <- as.factor(id_cl$cluster)

write.csv(id_cl, "data/id_cl_data.csv", row.names = FALSE)

#################################################

## 4. 투자성향분석 : 클러스터링 해석

### 1) 데이터 전처리
#### (1) 패키지 불러오기
need_packages <- c("tidyverse", "data.table", "ggplot2", "factoextra", "corrplot", "fmsb", "extrafont", "wordcloud2", "scales") #사용할 라이브러리를 불러옵니다
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())


#### (2) 데이터 불러오기
id_cl <-  fread("data/id_cl_data.csv",data.table = F)
dat <- fread("data/2021금융데이터 경진대회_데이터셋_한국투자증권(원본).csv",data.table = F)

### 2) Factor anlysis 전 후 corplot
mcor<-cor(id_cl[,-c(1,2,3,4,5,6,8)])
col <- colorRampPalette( c(alpha("#81D8D0",0.4) , alpha("#3CAEA3",0.4),alpha("#6AA2CD",0.4),alpha("#B43C8A",0.4), alpha("#383275",0.4)))
corrplot(mcor, shade.col=NA, tl.col="black", tl.srt=45,col=col(200), addcolorlabel="no", order="AOE")

fcor<-cor(id_cl[,c(2,3,4,5,6,7)])
corrplot(fcor, shade.col=NA, tl.col="black", tl.srt=45,col=col(200), addcolorlabel="no", order="AOE")

### 3) 클러스터 전반적 EDA
#### (1) 클러스터별 레이더 차트 만들기
id_cl %>% head()
id_cl$cluster[id_cl$cluster=="4"] <- "0"
id_cl$cluster[id_cl$cluster=="3"] <- "4"
id_cl$cluster[id_cl$cluster=="0"] <- "3"
id_cl$cluster %>% table()



#### (2) 클러스터별 지수 평균 산출
id_cl$cluster<-id_cl$cluster %>% as.numeric()
c1<-id_cl[,c(2:7)] %>% filter(cluster == '1') %>% apply(2,mean)
c2<-id_cl[,c(2:7)] %>% filter(cluster == '2') %>% apply(2,mean)
c3<-id_cl[,c(2:7)] %>% filter(cluster == '3') %>% apply(2,mean)
c4<-id_cl[,c(2:7)] %>% filter(cluster == '4') %>% apply(2,mean)
c5<-id_cl[,c(2:7)] %>% filter(cluster == '5') %>% apply(2,mean)


#### (3) 레이더 차트 데이터 프레임 만들기
cdf<-rbind(c1,c2,c3,c4,c5) %>% data.frame()
cdf<-cdf[,-1]
colnames(cdf) <- c('거래참여지수', '민감도지수','분산투자지수','성장기대지수','매수비율')


#### (4) 모든 클러스터 
colors_in<-c(alpha("#81D8D0",0.4),alpha("#3CAEA3",0.4),alpha("#6AA2CD",0.4),alpha("#B43C8A",0.4),alpha("#383275",0.4))

radarchart <- radarchart(cdf, axistype=0,maxmin=F, pcol=colors_in, pfcol=colors_in,
                         plwd=5, plty=0.8, cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, vlcex=0.8)
radarchart

#### (5) 클러스터1 - 한우물개미
colors_in<-c(alpha("#81D8D0",0.4),alpha("#3CAEA3",0),alpha("#6AA2CD",0),alpha("#B43C8A",0),alpha("#383275",0))
radarchart1<-radarchart(cdf, axistype=0,maxmin=F, pcol=colors_in, pfcol=colors_in,
                        plwd=5, plty=0.8, cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, vlcex=0.8)

radarchart1

#### (6) 클러스터2 - 큰손개미
colors_in<-c(alpha("#81D8D0",0),alpha("#3CAEA3",0.4),alpha("#6AA2CD",0),alpha("#B43C8A",0),alpha("#383275",0))
radarchart2<-radarchart(cdf, axistype=0,maxmin=F, pcol=colors_in, pfcol=colors_in,
                        plwd=5, plty=0.8, cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, vlcex=0.8)

radarchart2

#### (7) 클러스터3 - 주린이개미
colors_in<-c(alpha("#81D8D0",0),alpha("#3CAEA3",0),alpha("#6AA2CD",0.4),alpha("#B43C8A",0),alpha("#383275",0))
radarchart3<-radarchart(cdf, axistype=0,maxmin=F, pcol=colors_in, pfcol=colors_in,
                        plwd=5, plty=0.8, cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, vlcex=0.8)

radarchart3

#### (8) 클러스터4 - 안정개미
colors_in<-c(alpha("#81D8D0",0),alpha("#3CAEA3",0),alpha("#6AA2CD",0),alpha("#B43C8A",0.4),alpha("#383275",0))
radarchart4<-radarchart(cdf, axistype=0,maxmin=F, pcol=colors_in, pfcol=colors_in,
                        plwd=5, plty=0.8, cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, vlcex=0.8)

radarchart4

#### (9) 클러스터5 - 한방개미
colors_in<-c(alpha("#81D8D0",0),alpha("#3CAEA3",0),alpha("#6AA2CD",0),alpha("#B43C8A",0),alpha("#383275",0.4))
radarchart5<-radarchart(cdf, axistype=0,maxmin=F, pcol=colors_in, pfcol=colors_in,
                        plwd=5, plty=0.8, cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, vlcex=0.8)

radarchart5

#### (10) 박스플랏
mcolor<-c("#B43C8A","#6AA2CD","#3CAEA3","#383275","#81D8D0")
graphdf<- id_cl[,c(1:7)]
graphdf$cluster<- graphdf$cluster %>% as.character()
graphdf$cluster[graphdf$cluster=="1"] <-"한우물개미"
graphdf$cluster[graphdf$cluster=="2"] <- "큰손개미"
graphdf$cluster[graphdf$cluster=="3"] <- "주린이개미"
graphdf$cluster[graphdf$cluster=="4"] <- "안정개미"
graphdf$cluster[graphdf$cluster=="5"] <- "한방개미"
colnames(graphdf)<-c("id","cluster","거래참여지수","민감도지수","분산투자지수","성장기대지수","매수비율")


graphdf1<-ggplot(data=graphdf,aes(x=cluster, group=cluster, y=거래참여지수))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=mcolor, colour=mcolor) + theme_light() 
graphdf1+ylim(-1,2)+labs(x="")

graphdf2<-ggplot(data=graphdf,aes(x=cluster, group=cluster, y=민감도지수))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=mcolor, colour=mcolor) + theme_light() 
graphdf2+ylim(-0.25,0.5)+labs(x="")

graphdf3<-ggplot(data=graphdf,aes(x=cluster, group=cluster, y=분산투자지수))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=mcolor, colour=mcolor) + theme_light() 
graphdf3+ylim(-1,3)+labs(x="")

graphdf4<-ggplot(data=graphdf,aes(x=cluster, group=cluster, y=성장기대지수))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=mcolor, colour=mcolor) + theme_light() 
graphdf4+labs(x="")

graphdf5<-ggplot(data=graphdf,aes(x=cluster, group=cluster, y=매수비율))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=mcolor, colour=mcolor) + theme_light() 
graphdf5+ylim(.25, 1)+labs(x="")


### 4)  클러스터1 - 한우물개미
#### (1) 매수비율
ncolor<-c("#81D8D0", alpha("#81D8D0",0.5), "#3CAEA3", alpha("#3CAEA3",0.5), "#6AA2CD", alpha("#6AA2CD", 0.5),"#B43C8A", alpha("#B43C8A",0.5),"#383275", alpha("#383275", 0.5))

graph1<-data.frame(
  cluster=c("한우물개미","한우물개미","큰손개미","큰손개미","주린이개미","주린이개미","안정개미","안정개미","한방개미","한방개미"),
  cluster1=c("1","1.1", "2", "2.1", "3","3.1", "4","4.1", "5","5.1"),
  매수비율=c(cdf$매수비율[1], 1-cdf$매수비율[1], cdf$매수비율[2], 1-cdf$매수비율[2], cdf$매수비율[3], 1-cdf$매수비율[3], cdf$매수비율[4], 1-cdf$매수비율[4], cdf$매수비율[5], 1-cdf$매수비율[5])
)

graph1<-ggplot(graph1,aes(x=cluster,y=매수비율,fill=cluster1))+
  geom_bar(stat="identity",position="fill", fill=ncolor)+ theme_light()
graph1

#### (2) 워드클라우드-우량주
dat<-dat %>% rename("id"="고객구분코드")
dat<-dat %>% rename("name"="상품명")
wc_cl<-left_join(dat[,c(1,8)], id_cl[,c(1,2)], by='id')


wcolor<-rep(c("#81D8D0","#3CAEA3","#6AA2CD","#B43C8A","#383275"),10)
wc<-wc_cl %>% filter(cluster=="1")%>% count(name, sort=TRUE)
set.seed(1234)
wc<-wc %>% wordcloud2(color=wcolor)
wc

wc<-wc_cl %>% filter(cluster=="1")%>% count(name, sort=TRUE)
set.seed(1234)
wc1<-wc[-c(1,2),]%>% filter(n>300) %>% wordcloud2(color=wcolor)
wc1


### 5) 클러스터2 - 큰손개미
#### (1) 거래금액 boxplot
mcolor<-c("#B43C8A","#6AA2CD","#3CAEA3","#383275","#81D8D0")
graph2<- id_cl[,c(2,10)] 
graph2$cluster<- graph2$cluster %>% as.character()
graph2$cluster[graph2$cluster=="1"] <- "한우물개미"
graph2$cluster[graph2$cluster=="2"] <- "큰손개미"
graph2$cluster[graph2$cluster=="3"] <- "주린이개미"
graph2$cluster[graph2$cluster=="4"] <- "안정개미"
graph2$cluster[graph2$cluster=="5"] <- "한방개미"
graph2<-graph2 %>% rename("총거래금액"="trade_sum_p")

graph2 <- ggplot(data=graph2 ,aes(x=cluster, group=cluster, y=총거래금액))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=mcolor, colour=mcolor) + theme_light() 
graph2+ylim(0,400000000)+labs(x="")


#### (2) 거래금액 비율
pcolor<-rep(c("#383275","#3CAEA3"))
id_cl2<-id_cl %>% filter(cluster=="2")
graph3<-data.frame(sum=c(others=id_cl$trade_sum_p %>% sum()-id_cl2$trade_sum_p %>% sum(),
                         큰손개미=id_cl2$trade_sum_p %>% sum()))

graph3<-ggplot(graph3, aes(x="", y=sum)) + geom_bar(stat="identity", position="fill", fill=pcolor, alpha=0.6)+
  theme_light() +coord_polar("y",start=0)
graph3+labs(x="",y="")

### 6) 클러스터3 - 주린이 개미
####(1) 분산투자
dcolor<-c("#6AA2CD","#383275")
graph6<- id_cl[,c(2,12)] %>% group_by(cluster) %>% summarise(mean=mean(stck_cnt)) %>% filter(cluster=="3")
graph7<-data.frame(cluster=0)
graph8<-id_cl[,c(2,12)] %>% filter(cluster!="3")%>% summarise(mean=mean(stck_cnt)) 
graph7<-merge(graph7, graph8)
graph6<-rbind(graph6, graph7) 
colnames(graph6)<- c("cluster", "평균종목개수")
graph6[,1]<-c("주린이개미", "others")
graph6[,2]<-round(graph6[,2],2)

graph6<-ggplot(graph6,aes(x=cluster,y=평균종목개수))+
  geom_bar(stat="identity", fill=dcolor, alpha=0.6)+ theme_light()+
  geom_text(stat = "identity", 
            aes(label=평균종목개수),
            position = position_dodge(width=1.8),
            vjust=1.5)
graph6+labs(x="")

#### (2) 민감도지수 평균
dcolor<-c("#383275", "#6AA2CD")
graph10<- id_cl[,c(2,4)] 
graph10$cluster<-graph10$cluster %>% as.character()
graph10$cluster[graph10$cluster!="3"] <- "others"
graph10$cluster[graph10$cluster=="3"] <- "주린이개미"
graph11<-graph10
graph11<-graph11 %>% group_by(cluster) %>% summarise(민감도지수평균=mean(Factor2))
graph11[,2]<-round(graph11[,2],4)
graph10<-graph10 %>% rename("민감도지수"="Factor2")



graph10 <- ggplot(data=graph10 ,aes(x=cluster, group=cluster, y=민감도지수))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=dcolor, colour=dcolor) + theme_light() 
graph10+ylim(-0.25, 0.5)+labs(x="")

graph11 <- ggplot(data=graph11 ,aes(x=cluster, group=cluster, y=민감도지수평균))+
  geom_bar(stat="identity", fill=dcolor, alpha=0.6)+ theme_light()+
  geom_text(stat = "identity", 
            aes(label=민감도지수평균),
            position = position_dodge(width=1.8),
            vjust=1.5)
graph11


### 7) 안정개미(클러스터4)
#### (1) firm_up ratio 비교
dcolor<-c("#B43C8A","#81D8D0")
graph9<- id_cl[,c(2,15)] %>% filter(cluster==c("1", "4"))
graph9$cluster<- graph9$cluster %>% as.character()


graph9$cluster[graph9$cluster=="1"] <- "한우물개미"
graph9$cluster[graph9$cluster=="4"] <- "안정개미"

graph9 <- ggplot(data=graph9 ,aes(x=cluster, group=cluster, y=firm_up_ratio))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=dcolor, colour=dcolor) + theme_light() 

graph9 + ylim(0,0.5)


#### (2)kospi200 ratio 비교
mcolor<-c("#B43C8A","#6AA2CD","#3CAEA3","#383275","#81D8D0")
graph17<- id_cl[,c(2,14)] 
graph17$cluster<- graph17$cluster %>% as.character()
graph17$cluster[graph17$cluster=="1"] <- "한우물개미"
graph17$cluster[graph17$cluster=="2"] <- "큰손개미"
graph17$cluster[graph17$cluster=="3"] <- "주린이개미"
graph17$cluster[graph17$cluster=="4"] <- "안정개미"
graph17$cluster[graph17$cluster=="5"] <- "한방개미"


graph17 <- ggplot(data=graph17 ,aes(x=cluster, group=cluster, y=kospi200_ratio))+
  geom_boxplot(outlier.shape = NA, alpha = .4, fill=mcolor, colour=mcolor) + theme_light() 
graph17 


### 8) 클러스터5 - 한방개미
#### (1) 종목 워드클라우드
wc5<-wc_cl %>%filter(cluster=="5") %>% count(name, sort=TRUE)
wc5<-wc5 %>%  wordcloud2(color=wcolor)
wc5

#### (2) 거래비율 시계열 그래프
date_cl<-left_join(dat[,c(1,2,15)], id_cl[,c(1,2)], by='id')
date_cl<-date_cl %>% rename("date"="주문일자")
date_cl<-date_cl %>% rename("price"="총체결금액")

date_cl1<-date_cl %>% filter(cluster=="5") %>% group_by(date) %>% summarise(sum=sum(price))
date_cl2<-date_cl %>% filter(cluster!="5") %>% group_by(date) %>% summarise(sum=sum(price))

date_cl1$date <- as.Date(as.character(date_cl1$date), format = '%Y%m%d')
date_cl2$date <- as.Date(as.character(date_cl2$date), format = '%Y%m%d')


date_cl$date <- as.Date(as.character(date_cl$date), format = '%Y%m%d')


date_cl1$sum <- date_cl1$sum/sum(date_cl1$sum)
date_cl2$sum <- date_cl2$sum/sum(date_cl2$sum)

colnames(date_cl1) <- c('date', '한방개미')
colnames(date_cl2) <- c('date', 'others')

date_cl <- full_join(date_cl1, date_cl2, by = 'date')
date_cl1 <- date_cl %>% gather(cluster, sum, 한방개미, others )

graph14<-ggplot(date_cl1, aes(x=date, y = sum, color = cluster)) + geom_line(size=1.5) + theme_light() +
  scale_color_brewer(palette = 3)+ theme(legend.position = "bottom")
graph14

