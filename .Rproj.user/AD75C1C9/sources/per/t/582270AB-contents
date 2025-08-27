### 데이터불러오기 ####################################################################################################################3
rm(list = ls())

library(readr)
library(igraph)
library(readxl)
library(dplyr)

seoul_2019 <- read_csv('network/dataset/seoul_movement_2019.csv')
seoul_2020 <- read_csv('network/dataset/seoul_movement_2020.csv')
seoul_2021 <- read_csv('network/dataset/seoul_movement_2021.csv')
seoul_2022 <- read_csv('network/dataset/seoul_movement_2022.csv')
seoul_2023 <- read_csv('network/dataset/seoul_movement_2023.csv')
seoul_2024 <- read_csv('network/dataset/seoul_movement_2024.csv')

data_seoul = list(seoul_2019, seoul_2020, seoul_2021, seoul_2022, seoul_2023,  seoul_2024)

#node strength

edges_list <- data_seoul
g_weighted_list_seoul <- list()
for (edges in edges_list){
  colnames(edges) <- c('from','to','weight')
  g_weighted_list_seoul <- append(g_weighted_list_seoul, list(graph_from_data_frame(edges, directed = TRUE)))
}


# get adjacency matrix
adj_list_seoul <- list()
for (g in g_weighted_list_seoul){ 
  adj_list_seoul <- append(adj_list_seoul, list(as_adjacency_matrix(g, attr = "weight", sparse = FALSE)))
}
adj_list_seoul


#보정된 adj matrix

# pop list 필요하다 !!! 일단 오늘은 그대로 하장
pop2019 <- read_excel("network/dataset/2019_2019_주민등록 인구 기타현황(아동청소년청년 인구현황)_연간.xlsx", skip = 3)
pop2020 <- read_excel("network/dataset/2020_2020_주민등록 인구 기타현황(아동청소년청년 인구현황)_연간.xlsx", skip = 3)
pop2021 <- read_excel("network/dataset/2021_2021_주민등록 인구 기타현황(아동청소년청년 인구현황)_연간.xlsx", skip = 3)
pop2022 <- read_excel("network/dataset/2022_2022_주민등록 인구 기타현황(아동청소년청년 인구현황)_연간.xlsx", skip = 3)
pop2023 <- read_excel("network/dataset/2023_2023_주민등록 인구 기타현황(아동청소년청년 인구현황)_연간.xlsx", skip = 3)
pop2024 <- read_excel("network/dataset/2024_2024_주민등록 인구 기타현황(아동청소년청년 인구현황)_연간.xlsx", skip = 3)

pop_data <- list(pop2019, pop2020, pop2021, pop2022, pop2023, pop2024)
pop_data
pop_list <- list()
for (data in pop_data){
  temp <- data[,c(2,3)]
  colnames(temp) <- c('gu', 'pop')
  temp <- temp %>% mutate(gu = sapply(strsplit(gu, " "), function(x) x[2]), pop = as.numeric(gsub(",", "", pop)))
  pop_list <- append(pop_list, list(temp))
}


length(adj_list_seoul)

adj_adj_list_seoul <- list()
for (i in seq(1:length(adj_list_seoul))){
  adj <- adj_list_seoul[[i]]
  pop <- pop_list[[i]]
  temp <- left_join(cbind(as_tibble(adj), colnames(adj)), pop, by = c('colnames(adj)' ='gu'))
  denominator <- temp[,ncol(temp)]
  adj_adj <- as.matrix(sweep(adj, 1, denominator, '/')*100*1000) #percent/1000 단위로
  storage.mode(adj_adj) <- 'integer'
  adj_adj_list_seoul <- append(adj_adj_list_seoul, list(adj_adj))
}

# 
# #HITS algorithm
# #weighted HITS
# weighted_hits <- function(adj_matrix, max_iter = 100, tol = 1e-6) {
#   n <- nrow(adj_matrix)
#   
#   # 초기화
#   h <- rep(1, n)
#   a <- rep(1, n)
#   
#   for (iter in 1:max_iter) {
#     a_new <- t(adj_matrix) %*% h
#     h_new <- adj_matrix %*% a_new
#     
#     # 정규화
#     a_new <- a_new / sqrt(sum(a_new^2))
#     h_new <- h_new / sqrt(sum(h_new^2))
#     
#     # 수렴 조건
#     if (max(abs(h_new - h)) < tol && max(abs(a_new - a)) < tol) {
#       break
#     }
#     
#     a <- a_new
#     h <- h_new
#   }
#   
#   list(authority = as.vector(a), hub = as.vector(h))
# }
# 
# 
# result <- weighted_hits(adj)
# result.frame <- data.frame(
#   Node = V(g_weighted)$name,
#   Authority = result$authority,
#   Hub = result$hub
# )
# 
# result.frame
# result.frame[order(-result.frame$Authority),]
# result.frame[order(-result.frame$Hub),]


#모델링#################################################################################################################################

# we need adjusted adjacent matrix
# adjusted adjacent matrix for 2024
library(readxl)

#1. stochastic block model : poisson_sbm
library(blockmodels)
options(bitmapType = "cairo")  # 그래픽 장치 오류 방지

bm_list <- list()
gof_list <- list()
for (adj_adj in adj_adj_list_seoul){
  bm <- BM_poisson("SBM", adj_adj, explore_min = 4, explore_max = 10)
  bm$estimate()
  bm_list <- append(bm_list, list(bm))
}


# ICL이 계속해서 증가. Elbow point 찾기.
for (bm in bm_list){
  icls <- cbind(c(1:10), bm$ICL)
  line <- function(x){
    vec <- icls[nrow(icls), ] - icls[1,]
    grad <- vec[2]/vec[1]
    return(grad*(x-icls[1,1]) + icls[1,2])
  }
  # k 차이 순은 직선과의 거리 순
  k <- icls[,2] - line(c(1:10))  
  K <- which.max(k)
  print(K)
}
# 5개로 하자.


# get memberships
membership_list <- list()
for(i in seq_along(bm_list)){
  sbm_result <- bm_list[[i]]
  memberships <- cbind(rownames(adj_adj_list_seoul[[i]]), apply(sbm_result$memberships[[5]]$Z, 1, which.max))
  membership_list <- append(membership_list, list(memberships))
}

# # flow matrix
# bm$model_parameters[[4]]$lambda

# latent distance modeling
library(ggplot2)
library(amen)

#AME model <- assume normality
R <- 2
fit <- ame(adj, family = "nrm", R = R, plot = TRUE )  # nrm = Normal, R = latent dimension
plot(fit, plotfun = "circle", main = "Latent Space Positions")
summary(fit)

latent <- cbind(fit$U, fit$V, memberships[,2])
colnames(latent) <- c('U_x','U_y','V_x','V_y', 'membership')

ggplot(data = latent)+
  geom_point(aes(x = U_x, y = U_y, color = membership), shape = 19, size = 3)


# ldm model via stan
adj_adj
# via cmdstan
library(cmdstanr)

mat <- adj_adj_list_seoul[[6]]
mod <- cmdstan_model("network/stan/ldm_poisson.stan")
stan_data <- list(N = 25, Y = mat)
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 3,
  parallel_chains = 6,
  iter_warmup = 10000,
  iter_sampling = 50000
)

#model summary
library(posterior)
library(bayesplot)
library(Cairo)
library(ggplot2)
draws <- fit$draws()
summary <- summarise_draws(draws)
summary$mean
summary

grid <- matrix(summary$mean[51:100], ncol = 2)
grid
grid_data <- as.data.frame(cbind(grid, rownames(stan_data$Y)))
colnames(grid_data) <- c('x','y', 'node')
grid_data$x <- as.numeric(grid_data$x)
grid_data$y <- as.numeric(grid_data$y)
grid_data$membership <- as.factor(memberships[,2])
grid_data <- grid_data %>%
  mutate(membership = recode(membership, 
                      "1" = "서북권", 
                      "2" = "도심권",
                      "3" = "서남권", 
                      "4" = "동남권",
                      "5" = "동북권"))
write.csv(grid_data, 'grid_data.csv')
#plot grid

savegraph <- paste0('network/plots/ldm', format(Sys.time(), "%d-%H-%M-%S"),'.png')
CairoPNG(savegraph, width=1200, height=300 * dim(draws)[2])
ggplot(data = grid_data)+
  geom_point(aes(x =x, y = y, color = membership))
dev.off()

# traceplot
draws <- fit$draws(variables = c("z[5,2]"))
dim(draws)
savegraph <- paste0('plots/', format(Sys.time(), "%d-%H-%M-%S"),'.png')
CairoPNG(savegraph, width=1200, height=300 * dim(draws)[2])
mcmc_trace(draws)
dev.off()


print(summary,n = 100)


# SBM visualization ################################################################################3


# 시각화.
# install.packages('sf')
library(sf)
library(ggplot2)
library(readxl)
library(dplyr)
library(purrr)
library(gridExtra)
library(patchwork)
# 지도 데이터
shp_data <- 'gadm/gadm41_KOR_2.shp'
seoul_shp <- 'trashcan/tl_emd._seoul_4326.shp'
map_sf <- st_read(shp_data, quiet = TRUE)
seoul_sf <-  st_read(seoul_shp, quiet = TRUE)


# 행정구역별 위경도
file_path <- '행정구역별_위경도_좌표.xlsx'
seoul_coord <- read_excel(file_path, sheet = '서울특별시') 
seoul_coord

seoul_coord <- seoul_coord %>% mutate(node = paste(시도, 시군구)) %>% distinct(node, .keep_all = TRUE)
seoul_coord


location <- seoul_coord %>% select('위도', '경도', '시군구')
location <- location[2:nrow(location), ]

#join membership and location
membership_df_list <- lapply(membership_list, function(v) data.frame(v))
membership_df_list

membership_df <- Reduce(function(x, y) left_join(x, y, by = "X1"), membership_df_list)
colnames(membership_df) <- c('구' , 'group_2019', 'group_2020', 'group_2021', 'group_2022', 'group_2023', 'group_2024')
membership_location <- left_join(membership_df, location, by = c('구' = '시군구'))

ggplot() +
  geom_sf(data = seoul_sf, fill = "lightgray", color = 'lightgray') +
  geom_point(data = membership_location, mapping = aes(x = 경도, y = 위도, color = group_2019, size= 1))+
  geom_text(data = membership_location, mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -1.5)+
  labs(title = "2024년 서울 구 네트워크 군집 결과") +
  theme_minimal()

# 
# savegraph <- paste0('plots/membership_total', format(Sys.time(), "%d-%H-%M-%S"),'.png')
# CairoPNG(savegraph, width=1200, height=300 * 3)
# p2 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2019, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)  # vjust는 텍스트 위치 조정
# p3 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2020, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
# p4 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2021, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
# p5 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2022, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
# p6 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2023, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)   # vjust는 텍스트 위치 조정
# p7 <- ggplot(membership_location) + geom_point( mapping = aes(x = 경도, y = 위도, color = group_2024, size= 1))+geom_text(mapping = aes(x = 경도, y = 위도, label = 구), size = 2.5, vjust = -0.5)  # vjust는 텍스트 위치 조정
# grid.arrange(p2, p3, p4, p5, p6, p7, nrow = 3, ncol = 3)
# theme_minimal()
# dev.off()


write.csv(membership_df, "total.csv", row.names = FALSE)

