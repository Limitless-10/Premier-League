data {
  int<lower=0> N; // Number of games (observations)
  int<lower=0> J; // Number of teams
  matrix[N,J] X; // Predictor matrix (which two teams are playing)
  
  matrix[N,2] y; // Outcome (Goals for and against)
}

parameters {
  vector[J] attack_score;
  vector[J] defense_score;
  
  real<lower=0> sigma;
}

transformed parameters {
  vector[J*2] total_score;
  
  // Combine attack_score and defense_score into total_score
  total_score = append_row(attack_score, defense_score);
}

model {
  y ~ normal(mu, sigma);
  
``
}

