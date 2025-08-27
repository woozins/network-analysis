#df from toymodel.r
df

library(statnet)        # ergm 포함됨
library(ergm.count)     # count 데이터를 다룰 땐 이걸 추가로 사용
library(dplyr)
library(progress)

# 빠른 피팅을 위해 df에서 10000개 열만 추출함.
sample_idx <- sample(1:nrow(df), 10000)
df_toy <- df[sample_idx,]

# 이동 건수 집계
edges <- df_toy %>%
  group_by(전출행정구역_시도코드, 전입행정구역_시도코드) %>%
  summarise(weight = n(), .groups = 'drop')

# 노드 이름 벡터
nodes <- unique(c(edges$전출행정구역_시도코드, edges$전입행정구역_시도코드))

# 빈 네트워크 생성
net <- network.initialize(length(nodes), directed = TRUE)
network.vertex.names(net) <- nodes

#monitor progress
pb <- progress_bar$new(
  total = nrow(edges),
  format = "진행 [:bar] :percent 완료 | :current/:total"
)


# 엣지 추가
for (i in 1:nrow(edges)) {
  pb$tick()
  add.edge(net,
           tail = match(edges$전출행정구역_시도코드[i], nodes),
           head = match(edges$전입행정구역_시도코드[i], nodes),
           names.eval = "count",
           vals.eval = edges$weight[i])
}

# ERGM with count response
fit <- ergm(net ~ sum, 
            response = "count",  # count 기반 모델
            reference = ~Poisson)

summary(fit)
