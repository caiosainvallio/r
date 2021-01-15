###########################################################################################
############ Highest Posterior Density Credible Interval for Mean Difference ##############
###########################################################################################
# (by Luiz Hespanhol and Caio Sain Vallio)

# Gibbs sampler for unknown mean and variance ---------------------------------------------
update_mu = function(n, ybar, sig2, mu_0, sig2_0) {
  sig2_1 = 1.0 / (n / sig2 + 1.0 / sig2_0)
  mu_1 = sig2_1 * (n * ybar / sig2 + mu_0 / sig2_0)
  rnorm(n=1, mean=mu_1, sd=sqrt(sig2_1))
}

update_sig2 = function(n, y, mu, nu_0, beta_0) {
  nu_1 = nu_0 + n / 2.0
  sumsq = sum( (y - mu)^2 ) # vectorized
  beta_1 = beta_0 + sumsq / 2.0
  out_gamma = rgamma(n=1, shape=nu_1, rate=beta_1) # rate for gamma is shape for inv-gamma
  1.0 / out_gamma # reciprocal of a gamma random variable is distributed inv-gamma
}

gibbs = function(y, n_iter, init, prior) {
  ybar = mean(y)
  n = length(y)
  
  ## initialize
  mu_out = numeric(n_iter)
  sig2_out = numeric(n_iter)
  
  mu_now = init$mu
  
  ## Gibbs sampler
  for (i in 1:n_iter) {
    sig2_now = update_sig2(n=n, y=y, mu=mu_now, nu_0=prior$nu_0, beta_0=prior$beta_0)
    mu_now = update_mu(n=n, ybar=ybar, sig2=sig2_now, mu_0=prior$mu_0, sig2_0=prior$sig2_0)
    
    sig2_out[i] = sig2_now
    mu_out[i] = mu_now
  }
  
  cbind(mu=mu_out, sig2=sig2_out)
}


hpdinterval_mean_diff <- function(x, y) {
  
  ########## Setting up the problem ##########
  # group x
  xbar = mean(x)
  nx = length(x)
  
  # group y
  ybar = mean(y)
  ny = length(y)
  
  ## prior for x and y
  prior = list()
  prior$mu_0 = 0.0
  prior$sig2_0 = 1e6
  prior$n_0 = 0.0 # prior effective sample size for sig2
  prior$s2_0 = 1.0 # prior point estimate for sig2
  prior$nu_0 = prior$n_0 / 2.0 # prior parameter for inverse-gamma
  prior$beta_0 = prior$n_0 * prior$s2_0 / 2.0 # prior parameter for inverse-gamma
  
  # Initialising and run the sampler
  set.seed(13579)
  
  # posterior group x
  init = list()
  init$mu1 = min(x)
  init$mu2 = quantile(y, probs = 0.25)[1][[1]]
  init$mu3 = mean(x)
  init$mu4 = quantile(x, probs = 0.75)[1][[1]]
  init$mu5 = max(x)
  
  postx1 = gibbs(y=x, n_iter=25000, init=init[1], prior=prior)
  postx1 = as.data.frame(postx1[5001:25000,])
  
  postx2 = gibbs(y=x, n_iter=25000, init=init[2], prior=prior)
  postx2 = as.data.frame(postx2[5001:25000,])
  
  postx3 = gibbs(y=x, n_iter=25000, init=init[3], prior=prior)
  postx3 = as.data.frame(postx3[5001:25000,])
  
  postx4 = gibbs(y=x, n_iter=25000, init=init[4], prior=prior)
  postx4 = as.data.frame(postx4[5001:25000,])
  
  postx5 = gibbs(y=x, n_iter=25000, init=init[5], prior=prior)
  postx5 = as.data.frame(postx5[5001:25000,])
  
  # posterior group y
  init = list()
  init$mu1 = min(y)
  init$mu2 = quantile(y, probs = 0.25)[1][[1]]
  init$mu3 = mean(y)
  init$mu4 = quantile(y, probs = 0.75)[1][[1]]
  init$mu5 = max(y)
  
  posty1 = gibbs(y=y, n_iter=25000, init=init[1], prior=prior)
  posty1 = as.data.frame(posty1[5001:25000,])
  
  posty2 = gibbs(y=y, n_iter=25000, init=init[2], prior=prior)
  posty2 = as.data.frame(posty2[5001:25000,])
  
  posty3 = gibbs(y=y, n_iter=25000, init=init[3], prior=prior)
  posty3 = as.data.frame(posty3[5001:25000,])
  
  posty4 = gibbs(y=y, n_iter=25000, init=init[4], prior=prior)
  posty4 = as.data.frame(posty4[5001:25000,])
  
  posty5 = gibbs(y=y, n_iter=25000, init=init[5], prior=prior)
  posty5 = as.data.frame(posty5[5001:25000,])
  
  # diff between group x and y
  post1 = posty1 - postx1
  post2 = posty2 - postx2
  post3 = posty3 - postx3
  post4 = posty4 - postx4
  post5 = posty5 - postx5
  
  
  post = mcmc.list(
    as.mcmc(post1), 
    as.mcmc(post2), 
    as.mcmc(post3), 
    as.mcmc(post4), 
    as.mcmc(post5)
  )
  
  ########## Summarising the results ########## 
  hpdinterval <- rbind(post1,post2,post3,post4,post5)
  hpdinterval <- data.frame(
    HPDinterval(mcmc.list(as.mcmc(hpdinterval)),
                prob = 0.95)
  )
  
  return(hpdinterval) 
}
###########################################################################################
###########################################################################################