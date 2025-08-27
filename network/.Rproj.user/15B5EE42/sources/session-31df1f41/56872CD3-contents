rm(list = ls())

library(igraph)
library(readxl)

young_2014 <- read_csv("dataset/청년_구간_이동_전입자수_2014.csv")
young_2019 <- read_csv("dataset/청년_구간_이동_전입자수_2019.csv")
young_2020 <- read_csv("dataset/청년_구간_이동_전입자수_2020.csv")
young_2021 <- read_csv("dataset/청년_구간_이동_전입자수_2021.csv")
young_2022 <- read_csv("dataset/청년_구간_이동_전입자수_2022.csv")
young_2023 <- read_csv("dataset/청년_구간_이동_전입자수_2023.csv")
young_2024 <- read_csv("dataset/청년_구간_이동_전입자수_2024.csv")

data_young = list(young_2014, young_2019, young_2020, young_2021, young_2022, young_2023,  young_2024)

#node strength

edges_list <- data_young
g_weighted_list_young <- list()

for (edges in edges_list){
colnames(edges) <- c('from','to','weight')
g_weighted_list_young <- append(g_weighted_list_young, list(graph_from_data_frame(edges, directed = TRUE)))
}


# get adjacency matrix
adj_list_young <- list()

for (g in g_weighted_list_young){ 
adj_list_young <- append(adj_list_young, list(as_adjacency_matrix(g, attr = "weight", sparse = FALSE)))
}

adj_list_young

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

