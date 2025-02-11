// functions
functions {
  // defines the log survival
  vector log_S (vector t, real shape, vector scale){
    vector[num_elements(t)] log_S;
    for (i in 1:num_elements(t)){
      log_S[i] = weibull_lccdf(t[i]|shape, scale[i]);
    }
    return log_S;
  }
  
  //defines the log hazard
  vector log_h (vector t,real shape, vector scale){
    vector[num_elements(t)] log_h ;
    vector[num_elements(t)] ls ;
    ls = log_S(t,shape, scale) ;
    for (i in 1:num_elements(t)){
      log_h[i] = weibull_lpdf(t[i]|shape,scale[i])-ls[i];
    }
    return log_h;
  }
  
  //defines the log likelihood for right censored data
  real surv_weibull_lpdf( vector t,vector d,
  real shape,vector scale){
    vector[num_elements(t)] log_lik;
    real prob;
    log_lik = d .* log_h(t,shape,scale)+log_S(t,shape,scale);
    prob = sum(log_lik);
    return prob;
  }
}
// data
data {
  int<lower=0> N;
  matrix[N, 4] y; // goal time/ censor time for both team (t1,c1,t2,c2)
}
// transformed data
transformed data {
  // matrix[N, 2] event;
  // 
  // for (i in 1:N) {
  //   event[i, 1] = y[i, 1]; 
  //   event[i, 2] = y[i, 3]; 
  // }
}
// parameters
parameters {
  // vector[5] theta;
  real<lower=0> sigma;
  real<lower=0> gamma_weight; // shape parameter
  vector[2] att;
  vector[2] def;
  real mu;
  real home;
}
// transformed parameters 
transformed parameters {
  vector[2] lambda;
  
  lambda[1] = exp(mu + home + att[1] + def[2]);
  lambda[2] = exp(mu + att[2] + def[1]);
}
// model
model {
  // priors
  // sigma ~ cauchy(0,5); // prior for sigma
  home ~ normal(0,100); // prior for home parameter
  mu ~ normal(0,100); // prior for home parameter
  att ~ normal(0,100); // prior for home parameter
  def ~ normal(0,100); // prior for home parameter
  gamma_weight ~ gamma(0.001, 0.001);
  
  
  // likelihood
  target += surv_weibull_lpdf(y[,1]|y[,2],gamma_weight,rep_vector(lambda[1], N)); //model for data, team 1 (home)
  target += surv_weibull_lpdf(y[,3]|y[,4],gamma_weight,rep_vector(lambda[2], N)); //model for data, team 2 (away)
}

