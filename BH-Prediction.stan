data {
  int<lower=1> ngames;
  int<lower=1> nteams;
  int<lower=1, upper=nteams> hometeam[ngames];
  int<lower=1, upper=nteams> awayteam[ngames];
  int<lower=0> y1[ngames];
  int<lower=0> y2[ngames];
}

parameters {
  real home;              // home advantage
  real mu_att;            // mean for attack
  real mu_def;            // mean for defense
  real<lower=0> tau_att;  // precision for attack
  real<lower=0> tau_def;  // precision for defense

  vector[nteams] att_star; // raw attack effects
  vector[nteams] def_star; // raw defense effects
}

transformed parameters {
  vector[nteams] att; // final attack effects with sum-to-zero constraint
  vector[nteams] def; // final defense effects with sum-to-zero constraint

  // Enforce the "sum-to-zero" (actually mean=zero) by subtracting the mean:
  {
    real mean_att_star = mean(att_star);
    real mean_def_star = mean(def_star);
    for (t in 1:nteams) {
      att[t] = att_star[t] - mean_att_star;
      def[t] = def_star[t] - mean_def_star;
    }
  }
}

model {
  // ------------------- PRIORS ------------------- 
  // In WinBUGS: dnorm(0, 0.0001) means Normal(0, precision=0.0001),
  // which translates to Normal(0, sd = 1/sqrt(0.0001) = 100).
  home   ~ normal(0, 5);
  mu_att ~ normal(0, 5);
  mu_def ~ normal(0, 5);

  // In WinBUGS: dgamma(0.01, 0.01) => shape=0.01, rate=0.01 => mean=1, very wide
  tau_att ~ gamma(3, 0.27); 
  tau_def ~ gamma(3, 0.27);

  // Attack/defense random effects
  // WinBUGS: att.star[t] ~ dnorm(mu.att, tau.att)
  // Stan:    normal(mu_att, sd=1/sqrt(tau_att))
  att_star ~ normal(mu_att, 1 / sqrt(tau_att));
  def_star ~ normal(mu_def, 1 / sqrt(tau_def));

  // ------------------- LIKELIHOOD -------------------
  for (g in 1:ngames) {
    // log(theta[g,1]) = home + att[hometeam[g]] + def[awayteam[g]]
    // log(theta[g,2]) = att[awayteam[g]] + def[hometeam[g]]
    real theta1 = exp(home + att[hometeam[g]] + def[awayteam[g]]);
    real theta2 = exp(att[awayteam[g]] + def[hometeam[g]]);

    // y1[g], y2[g] ~ Poisson(...) 
    y1[g] ~ poisson(theta1);
    y2[g] ~ poisson(theta2);
  }
}

generated quantities {
  // Posterior predictive draws for ynew
  int ynew[ngames, 2];
  for (g in 1:ngames) {
    real theta1 = exp(home + att[hometeam[g]] + def[awayteam[g]]);
    real theta2 = exp(att[awayteam[g]] + def[hometeam[g]]);

    // simulate new goals from Poisson
    ynew[g,1] = poisson_rng(theta1);
    ynew[g,2] = poisson_rng(theta2);
  }
  
  int yrep1[ngames] = ynew[,1];
}
