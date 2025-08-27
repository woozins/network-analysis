data {
  int<lower=1> N;
  array[N, N] int<lower=0> Y;
}

parameters {
  array[N - 1] vector[2] z_free;
  real beta0;
}

transformed parameters {
  array[N] vector[2] z;

  z[1] = to_vector([0, 0]);             // ← 여기 수정!
  for (i in 2:N)
    z[i] = z_free[i - 1];
}

model {
  for (i in 1:(N - 1))
    z_free[i] ~ normal(0, 1);
    beta0 ~ normal(0, 1);

  for (i in 1:N)
    for (j in 1:N)
      if (i != j)
        Y[i,j] ~ poisson_log(beta0 - distance(z[i], z[j]));
}
