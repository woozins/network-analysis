library(igraph)
library(readxl)

seoul_2014 <- read_csv('dataset/seoul_movement_2014.csv')
seoul_2019 <- read_csv('dataset/seoul_movement_2019.csv')
seoul_2020 <- read_csv('dataset/seoul_movement_2020.csv')
seoul_2021 <- read_csv('dataset/seoul_movement_2021.csv')
seoul_2022 <- read_csv('dataset/seoul_movement_2022.csv')
seoul_2023 <- read_csv('dataset/seoul_movement_2023.csv')
seoul_2024 <- read_csv('dataset/seoul_movement_2024.csv')

data_seoul = list(seoul_2014, seoul_2019, seoul_2020, seoul_2021, seoul_2022, seoul_2023,  seoul_2024)

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

#HITS algorithm
#weighted HITS
weighted_hits <- function(adj_matrix, max_iter = 100, tol = 1e-6) {
  n <- nrow(adj_matrix)
  
  # 초기화
  h <- rep(1, n)
  a <- rep(1, n)
  
  for (iter in 1:max_iter) {
    a_new <- t(adj_matrix) %*% h
    h_new <- adj_matrix %*% a_new
    
    # 정규화
    a_new <- a_new / sqrt(sum(a_new^2))
    h_new <- h_new / sqrt(sum(h_new^2))
    
    # 수렴 조건
    if (max(abs(h_new - h)) < tol && max(abs(a_new - a)) < tol) {
      break
    }
    
    a <- a_new
    h <- h_new
  }
  
  list(authority = as.vector(a), hub = as.vector(h))
}


result <- weighted_hits(adj)
result.frame <- data.frame(
  Node = V(g_weighted)$name,
  Authority = result$authority,
  Hub = result$hub
)

result.frame
result.frame[order(-result.frame$Authority),]
result.frame[order(-result.frame$Hub),]

