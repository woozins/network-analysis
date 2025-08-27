rm(list = ls())
source('node_analysis.R')

# we need adjusted adjacent matrix
# adjusted adjacent matrix for 2024

library(readxl)
pop <- read_excel('구별인구_2412.xlsx', skip = 3)[,c(2,4)]
colnames(pop) <- c('gu', 'pop')
temp <- left_join(cbind(as_tibble(adj), colnames(adj)), pop, by = c('colnames(adj)' ='gu'))
denominator <- temp[,ncol(temp)]
adj_adj <- as.matrix(sweep(adj, 1, denominator, '/')*100*1000) #percent/1000 단위로
storage.mode(adj_adj) <- 'integer'
adj_adj

#1. stochastic block model : poisson_sbm
library(blockmodels)
library(sbm)
bm <- BM_poisson("SBM", adj_adj, explore_min = 4, explore_max = 10)
bm$estimate()
bm$memberships

# ICL이 계속해서 증가. Elbow point 찾기.

icls <- cbind(c(1:10), bm$ICL)
line <- function(x){
  vec <- icls[nrow(icls), ] - icls[1,]
  grad <- vec[2]/vec[1]
  return(grad*(x-icls[1,1]) + icls[1,2])
}

plot(icls)
lines(line(c(1:10)))
# k 차이 순은 직선과의 거리 순
k <- icls[,2] - line(c(1:10))  
K <- which.max(k)

# elbow point 따른 최적의 클러스터 수 : 5

sbm_result <- bm$memberships
memberships <- cbind(V(g_weighted)$name, apply(bm$memberships[[5]]$Z, 1, which.max))
memberships

#2. for 2019 data
bm_2019 <- BM_poisson("SBM", adj_2019, explore_min = 1, explore_max = 10)
bm_2019$estimate()



icls <- cbind(c(1:10), bm_2019$ICL)
line <- function(x){
  vec <- icls[nrow(icls), ] - icls[1,]
  grad <- vec[2]/vec[1]
  return(grad*(x-icls[1,1]) + icls[1,2])
}

plot(icls)
lines(line(c(1:10)))
# k 차이 순은 직선과의 거리 순
k <- icls[,2] - line(c(1:10))  
which.max(k)

# elbow point 따른 최적의 클러스터 수 : 4

sbm_result_2019 <- bm_2019$memberships
memberships_2019 <- cbind(V(g_weighted_2019)$name, apply(bm_2019$memberships[[4]]$Z, 1, which.max))
memberships_2019


# flow matrix

bm$model_parameters[[4]]$lambda
bm_2019$model_parameters[[4]]$lambda



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
mod <- cmdstan_model("stan/ldm_poisson.stan")
stan_data <- list(N = 25, Y = adj_adj)
stan_data$Y
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 3,
  parallel_chains = 6,
  iter_warmup = 10000,
  iter_sampling = 90000
)

#model summary
library(posterior)
library(bayesplot)
library(Cairo)
library(ggplot2)
draws <- fit$draws()
summary <- summarise_draws(draws)
summary
grid <- matrix(summary$mean[2:51], ncol = 2)
grid_data <- as.data.frame(cbind(grid, rownames(stan_data$Y)))
colnames(grid_data) <- c('x','y', 'node')
grid_data$x <- as.numeric(grid_data$x)
grid_data$y <- as.numeric(grid_data$y)
grid_data$membership <- as.factor(memberships[,2])

#plot grid

savegraph <- paste0('plots/grid', format(Sys.time(), "%d-%H-%M-%S"),'.png')
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






