data {
  int<lower=0> N; // Number of games (observations)
  int<lower=0> J; // Number of teams
  vector[N] y; // Outcome (Goal difference)
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma);
}

