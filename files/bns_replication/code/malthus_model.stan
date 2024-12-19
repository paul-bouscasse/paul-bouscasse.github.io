// STAN FILE
// final version

functions {
  vector fd(vector x) { // first difference
    return x[2:rows(x)] - x[1:(rows(x)-1)];
  }
  vector lag(vector x) { // lag
    return append_row(0, x[1:(rows(x)-1)]);
  }
  vector m1(vector x) { // minus value at time 1
    return x - x[1];
  }
  vector p1(vector x) { // plus value at time 1
    return x + x[1];
  }
  vector bar(vector x) { // average of current and previous time periods
    return (x[2:rows(x)] + x[1:(rows(x)-1)])/2;
  }
}

data {

  int<lower=0, upper=1> include_full; // boolean for full model
  int<lower=0, upper=1> include_slope; // boolean for wheter slope is given
  int<lower=0, upper=1> include_apar; // boolean for wheter alpha is a parameter
  int<lower=0, upper=1> include_K; // capital boolean
  int<lower=0, upper=1> include_Z; // boolean for falling alpha
  int<lower=0, upper=1> include_hme; // measurement error for days worked
  int<lower=0, upper=1> include_check; // check w, k, mlq
  int nal; // dimension of alpha
  int T; // number of periods
  int nBx1; // number of breaks for asymmetric population shock
  int Bx1[nBx1+2]; // breaks for asymmetric shock
  int nBx2; // number of breaks for symmetric population shock
  int Bx2[nBx2+2]; // breaks for symmetric population shock
  int nBe; // number of productivity breaks
  int dim_b;
  real cntr_b;
  int Be[nBe+2,dim_b]; // productivity breaks
  int wme; // wage measurement error dummy
  int nw; // number of wage series
  int nme; // dimension of measurement error
  int cntr; // concentration parameter for Dirichlet
  int<lower=0> T_trn_n; // number of trend observations for population
  int ii_trn_n[T_trn_n]; // coordinates of trend observations for population
  int<lower=0> T_mis_n; // number of missing observations for population
  int ii_mis_n[T_mis_n]; // coordinates of missing observations for population
  int ii_obs_n[T-T_trn_n-T_mis_n]; // coordinates of observations for population
  int<lower=0> T_mis_w; // number of missing observations for wage
  int<lower=0> T_mis_h; // number of missing observations for days
  int<lower=0> T_mis_r1; // number of missing observations for r1
  int<lower=0> T_mis_r2; // number of missing observations for r2 
  int<lower=0> T_drft; // number of years where alpha is modified
  int ii_obs_r1[T-T_mis_r1]; // coordinates of observations for interest rate
  int ii_obs_r2[T-T_mis_r2]; // coordinates of observations for interest rate
  int ii_mis_w[T_mis_w]; // coordinates of missing observations for wage
  int ii_obs_w[T-T_mis_w]; // coordinates of observations for wage
  int ii_mis_h[T_mis_h]; // coordinates of missing observations for days
  int ii_obs_h[T-T_mis_h]; // coordinates of observations for days
  int ii_alpha[T-T_drft]; // years when alpha is not modified
  int ii_drft[T_drft]; // years when alpha is modified
  int ii_drft_m1[T_drft]; // years when alpha is modified lagged
  vector[T_trn_n] n_trn; // population trend data (after 1540)
  vector[T-T_trn_n-T_mis_n] n_obs; // population observations data (before 1530)
  matrix[T-T_mis_w,nw] w_obs; // wage data
  vector[T-T_mis_h] h_obs; // days worked data
  vector<lower=0,upper=.2>[T-T_mis_r1] r_obs1; // interest rate data
  vector<lower=0,upper=.2>[T-T_mis_r2] r_obs2; // interest rate data
  vector[T_drft] rent_obs; // rents data
  vector[T_drft] k_obs; // capital data
  real E_b; // prior mean for population level parameter (psi)
  real<lower=0> std_b; // prior std for population level parameter (psi)
  vector[1] slope0; // values for slope if externally calibrated 
  vector[2] p_prior; // bounds for uniform prior for probability of a plage p
  vector[2] muXI_prior; // bounds for uniform prior for mean of plague (mu_XI)
  vector[2] IG_prior; // shape and rate parameter for prior of variance of shocks
  vector[2] s2e1_prior; // shape and rate parameter for prior of variance of eps_1
  vector[2] s2e2_prior; // shape and rate parameter for prior of variance of eps_2
  int t_1350; // coordinate of 1350 (for IRF)
  int T_irf; // length of IRF
  int t_1750; // coordinate of 1350 (for IRF2)
  int T_irf2; // length of IRF
  vector[2] alphapp; // factors shares
}

parameters {
  vector<lower=0, upper=slope0[1]>[(1-include_apar)*include_K ? 1:0] beta0; // beta
  simplex[include_apar ? (nal+1):1] alpha_smplx; // (alpha,beta,1-alpha-beta)
  vector<lower=-2, upper=2>[include_full] gamma; // malthusian parameter (see population supply equation)
  vector[nw+2*include_Z] phi; // intercept of the labor demand curve
  real psi; // pins down level of population before 1530
  vector[include_full] omega; // growth rate of population (sort of, see population supply equation)
  vector[nBe+1] mu_a; // growth rate of productivity
  vector<lower=0>[(T_mis_h>0)] s2h; // variance of days worked
  vector<lower=0>[nBe+1] s2e1; // variance of permanent productivity shocks
  vector<lower=0>[nBe+1] s2e2; // variance of temporary productivity shocks
  vector<lower=0, upper=1>[include_full ? T:0] XI1; // plague shocks in level
  vector<lower=p_prior[1], upper=p_prior[2]>[include_full ? (nBx1+1):0] p; // probability of plage shocks
  vector<lower=muXI_prior[1], upper=muXI_prior[2]>[include_full] mu_XI; // mean of plage shocks
  vector<lower=0>[include_full] nu_XI; // pseudo sample size of plage shocks
  vector<lower=0>[include_full] s2x2; // variance of symmetric population shocks
  vector<lower=0>[nme] s2i; // variance of population measurement errors
  vector<lower=0>[nme] inv_nu; // inverse of number of degrees of freedom of various measurement errors
  vector[nw-1] mu_w; // normalization of wage data series
  vector[T-1] dmlq; // atilde from t=2 to t=T (a_1 normalized to 0)
  vector[T_trn_n] n_trn_err; // true population trend (taking measurement error into account)
  vector[T_mis_n] n_miss; // population when data is missing
  vector[T-T_trn_n-T_mis_n] n_obs_err; // true population observations (taking measurement error into account)
  vector[T_mis_w] w_miss; // wage when data is missing
  vector[T_mis_h] h_miss; // days when data is missing
  vector[include_hme ? (T-T_mis_h):0] h_obs_err; // true days (taking measurement error into account)
  vector[(wme) ? (T-T_mis_w):T] w_err; // true wage (taking measurement error into account)
  vector<lower=0, upper=.2>[include_K ? 1:0] delta; // depreciation of capital
  vector<lower=0, upper=.2>[include_K ? T:0] r; // true underlying rate
  vector<lower=0, upper=.2>[include_K ? 1:0] r0; // initial interest rate
  simplex[3] drft[include_Z ? T_drft-1:0]; // (alpha_t, beta_t, 1-alpha_t-beta_t)
  vector<lower=0>[dim_b] lambda; // draws from Gamma distribution to get Dirichlet with lambda[j]/sum(lambda)
}

transformed parameters {
  
  // Declarations
  vector[nal] alpha;
  vector[(T_mis_h>0)] sh; // std of days worked
  vector[nBe+1] se1; // std of permanent productivity shocks
  vector[nBe+1] se2; // std of temporary productivity shocks
  vector[include_full*2] betap; // parameters of the distribution of the plage shocks
  vector[include_full] sx2; // std of symmetric population shocks
  vector[nme] si; // std of measurement errors
  vector[nme] nu; // degrees of freedom of measurement errors
  vector[include_full*T] xi1; // plague shocks in logs
  vector[T] n; // population
  vector[T] h; // days
  vector[T] w; // wage
  vector[include_K ? T:0] k0; // capital
  vector[include_K ? T:0] lr; // true underlying rate
  vector[T] mlq; // Malmquist index with period 0
  vector[T] atilde; // permanent component of productivity
  vector[T] lbdm; // labor demand
  vector[T] alphat; // alpha_t
  vector[include_K ? T:0] betat; // beta_t
  vector[dim_b] piv; // probability vector for breaks
  
  // Alpha
  if (include_apar) {
    alpha = alpha_smplx[1:nal];
  } else {
    if (include_K) {
      alpha[1] = slope0[1]*(1 - beta0[1]);
      alpha[2] = beta0[1];
    } else {
      alpha[1] = slope0[1];
      alpha[2] = 0;
    }
  }
  
  // Definitions
  if (T_mis_h>0) sh = sqrt(s2h); // variance to std
  se1 = sqrt(s2e1);
  se2 = sqrt(s2e2);
  if (include_full) {
    betap[1] = mu_XI[1]*nu_XI[1]; // re-parametrization of beta distribution (BDa2, p. 110)
    betap[2] = (1-mu_XI[1])*nu_XI[1];
    sx2 = sqrt(s2x2);
    xi1 = log(XI1);
  }
  si = sqrt(s2i);
  for (j in 1:nme) { // re-parametrization of student distribution (BDa2, p. 443)
    nu[j] = 1/inv_nu[j];
  }
  
  // Drift
  alphat[ii_alpha] = rep_vector(alpha[1],T-T_drft);
  if (T_drft>0) alphat[ii_drft] = append_row(rep_vector(alpha[1],1),to_vector(drft[,1]));
  
  // Interest rate
  if (include_K) {
    lr[1] = r0[1];
    lr[2:T] = r[1:(T-1)];
    betat[ii_alpha] = rep_vector(alpha[2],T-T_drft);
    if (T_drft>0) betat[ii_drft] = append_row(rep_vector(alpha[2],1),to_vector(drft[,2]));
  }
  
  // Wage
  w[ii_mis_w] = w_miss;
  if (wme==0) w[ii_obs_w] = w_obs[,1];
  else w[ii_obs_w] = w_err;
  
  // Population (see slide 21 for explanation)
  n[ii_trn_n] = psi + n_trn + n_trn_err;
  n[ii_mis_n] = n_miss;
  n[ii_obs_n] = n_obs + n_obs_err;
  
  // Days
  h[ii_mis_h] = h_miss;
  h[ii_obs_h] = h_obs;
  if (include_hme) h[ii_obs_h] += h_obs_err;
  
  // Malmquist index
  mlq[1] = 0;
  for (t in 2:T) {
    mlq[t] = dmlq[t-1] + mlq[t-1];
  }
  
  // Capital
  if (include_K) k0 = (w - phi[1]) + (n+h) - log(r + delta[1]) + log(betat) - log(1-alphat-betat);
  
  // Productivity
  atilde[ii_alpha] = mlq[ii_alpha];
  if (T_drft>0) {
    int t2 = 0;
    vector[T_drft] da;
    da = dmlq[ii_drft_m1] - (fd(betat)[ii_drft_m1]).*bar(k0)[ii_drft_m1] + (fd(alphat+betat)[ii_drft_m1]).*bar(n+h)[ii_drft_m1];
    for (t in ii_drft) {
      t2 = t2 + 1;
      atilde[t] = atilde[t-1] + da[t2];
    }
  }
  
  // Labor demand
  if (include_K) {
    lbdm = log(1-alphat-betat) + ((betat).*log(betat) + atilde - ((alphat).*(n+h) + (betat).*log(r + delta[1])))./(1-betat);
  } else {
    lbdm = log(1-alphat) + atilde - (alphat).*(n+h);
  }
  
  // Dirichlet
  piv = lambda/sum(lambda);
  
}

model{

  //////////////////////////////////////////////////////////////////////////////
  // PRIORS
  //////////////////////////////////////////////////////////////////////////////
  
  // demand block
  if (include_apar) {
    target += dirichlet_lpdf(alpha_smplx | rep_vector(1,nal+1));
  } else {
    target += uniform_lpdf(alpha[2] | max([0,1-1/slope0[1]]), 1);
  }
  target += normal_lpdf(phi | 0, 100);
  target += normal_lpdf(mu_a | 0, 1);
  target += inv_gamma_lpdf(s2e1 | s2e1_prior[1], s2e1_prior[2]);
  target += inv_gamma_lpdf(s2e2 | s2e2_prior[1], s2e2_prior[2]);
  
  // population and days
  target += normal_lpdf(psi | E_b, std_b);
  if (T_mis_h>0) target += inv_gamma_lpdf(s2h | IG_prior[1], IG_prior[2]);
  
  // capital part
  if (include_K) {
    target += uniform_lpdf(r0 | 0, 0.2);
    target += normal_lpdf(r | lr, .01); // interest rate assumed random walk: prior
    if (min(r) < 0 || max(r) > .2) {
        target += negative_infinity();
    } else {
      for (i in 1:T) {
        target += -log_diff_exp(normal_lcdf(0.2 | lr[i], .01), normal_lcdf(0 | lr[i], .01));
      }
    }
    target += normal_lpdf(delta[1] | .1, .05);
    if (delta[1] < 0 || delta[1] > .2) target += negative_infinity();
    else target += -log_diff_exp(normal_lcdf(0.2 | .1, .05), normal_lcdf(0 | .1, .05));
  }
  if (T_drft>0) {
    if (include_apar) target += dirichlet_lpdf(drft[1,] | cntr*to_vector(alpha_smplx));
    else {
      target += dirichlet_lpdf(drft[1,] | cntr*append_row(alpha,1-sum(alpha)));
    }
    for (t in 2:(T_drft-1))
      target += dirichlet_lpdf(drft[t,] | cntr*to_vector(drft[t-1,]));
  }
  target += gamma_lpdf(lambda | cntr_b, 1);
  
  // measurement error
  target += uniform_lpdf(inv_nu | 0, 1);
  target += inv_gamma_lpdf(s2i | IG_prior[1], IG_prior[2]);
  if (T_mis_h>0) target += normal_lpdf(h[2:T] | h[1:(T-1)], sh[1]); // days worked assumed random walk: useful when data is missing
  target += student_t_lpdf(n_trn_err | nu[1], 0, si[1]); // population trend measurement error
  target += student_t_lpdf(n_obs_err | nu[2], 0, si[2]); // population observations measurement error
  if (include_hme) target += student_t_lpdf(h_obs_err | nu[3], 0, si[3]); // hour measurement error
  
  // supply block
  if (include_full) {
    target += uniform_lpdf(gamma | -2, 2);
    target += normal_lpdf(omega | 0, 1);
    target += uniform_lpdf(p | p_prior[1], p_prior[2]);
    target += uniform_lpdf(mu_XI | muXI_prior[1], muXI_prior[2]);
    target += pareto_lpdf(nu_XI | 1, 1.5);
    target += inv_gamma_lpdf(s2x2 | IG_prior[1], IG_prior[2]);
  }
  
  //////////////////////////////////////////////////////////////////////////////
  // LIKELIHOOD
  //////////////////////////////////////////////////////////////////////////////
  
  // mixture modelling
  {
    vector[dim_b] lps = log(piv);
    for (j in 1:dim_b) {
      real lps0 = 0;
      for (i in 1:(nBe+1)) { // looping over productivity breaks
        if (Be[i+1,j]>Be[i,j]) {
          
          // Malmquist process
          lps0 += normal_lpdf(mlq[max(2,Be[i,j]):(Be[i+1,j]-1)] | mu_a[i] + mlq[max(1,Be[i,j]-1):(Be[i+1,j]-2)], se1[i]);
          
          // Labor demand
          if (include_K==0) lps0 += normal_lpdf(w[Be[i,j]:(Be[i+1,j]-1)] | phi[1] + lbdm[Be[i,j]:(Be[i+1,j]-1)], se2[i]);
          else lps0 += normal_lpdf(w[Be[i,j]:(Be[i+1,j]-1)] | phi[1] + lbdm[Be[i,j]:(Be[i+1,j]-1)], (se2[i])./(1-betat[Be[i,j]:(Be[i+1,j]-1)]));
          
        }
      }
      lps[j] += lps0;
    }
    target += log_sum_exp(lps);
  }
  
  // wage measurement error
  if (wme==1) {
    for (i in 1:nw) target += student_t_lpdf(w_obs[,i] | nu[2+include_hme+1], phi[i] - phi[1] + w[ii_obs_w], si[2+include_hme+1]);
  }
  
  // interest rate
  if (include_K) {
    target += student_t_lpdf(r_obs1 | nu[nme-2*include_Z], r[ii_obs_r1], si[nme-2*include_Z]);
    target += student_t_lpdf(r_obs2 | nu[nme-2*include_Z], r[ii_obs_r2], si[nme-2*include_Z]);
  }
  if (include_full) {
    for (j in 1:(nBx1+1)) { // looping over population breaks
      target += beta_lpdf(XI1[Bx1[j]:(Bx1[j+1]-1)] | betap[1], betap[2]);  // plagues
      for (k in 1:(nBx2+1)) {
        if (min(Bx1[j+1]-1,Bx2[k+1]-1)>=max(2,max(Bx1[j],Bx2[k]))) {
          for (t in max(2,max(Bx1[j],Bx2[k])):min(Bx1[j+1]-1,Bx2[k+1]-1)) {
            target += log_mix(p[j], // population supply equation: mixture of plague and no plague case
                              normal_lpdf(n[t] | omega[1] + gamma[1]*(w[t-1] + h[t-1]) + n[t-1] + xi1[t], sx2[1]), // plague with probability p[j]
                              normal_lpdf(n[t] | omega[1] + gamma[1]*(w[t-1] + h[t-1]) + n[t-1], sx2[1])); // plague with probability 1-p[j]
          }
        }
      }
    }
  }
  if (include_Z) {
    target += student_t_lpdf(rent_obs | nu[nme-1], phi[nw+1] + w[ii_drft] + (n+h)[ii_drft] + log(alphat[ii_drft]) - log(1-alphat[ii_drft]-betat[ii_drft]), si[nme-1]);
      // land demand
    target += student_t_lpdf(k_obs | nu[nme], phi[nw+2] + w[ii_drft] + (n+h)[ii_drft] - log(r[ii_drft] + delta[1]) + log(betat[ii_drft]) - log(1-alphat[ii_drft]-betat[ii_drft]), si[nme]);
      // capital demand
  }
  
}

generated quantities {
  
  vector[T] e1;
  vector[T] e2; // temporary productivity shocks
  vector[include_full ? T:0] xi; // population shocks (xi_1 + xi_2)
  vector[T] iota; // population measurement errors
  vector[T] N; // population in level (millions)
  vector[T] H; // days in level
  vector[include_full ? T_irf2:0] n_irf2; // log population for IRF
  vector[include_full ? T_irf2:0] N_irf2; // level population for IRF
  vector[include_full ? T_irf2:0] w_irf2; // wage for IRF
  vector[include_full ? T_irf2:0] h_irf2; // days for IRF
  real slope; //slope of the labor demand curve
  vector[include_K ? T:0] k; // log capital normalized
  vector[include_K ? T:0] rents; // log rents
  vector[dim_b] pi0;
  vector[include_full ? T:0] w_ss; // steady state wage
  vector[include_full ? T:0] WoW_ss; // wage over steady state wage ratio
  vector[include_full ? T:0] WoW_ss2; // wage over steady state wage ratio without plage
  vector[include_check ? T:0] w_check; // check that can recover w from labor demand (should be same as w)
  vector[include_check*include_K ? T:0] k_check; // check that can recover k from labor demand (should be same as k)
  vector[include_check ? T:0] mlq_check; // check that can recover Malmquist from production function (should be same as mlq)
  
  // Population and days
  N = exp(n - 6*log(10));
  H = exp(h);
  
  // productivity shocks and steady state wage
  e1 = rep_vector(0,T);
  if (include_full) w_ss = -h - rep_vector(omega[1]/gamma[1],T);
  for (j in 1:dim_b) {
    for (i in 1:(nBe+1)) { // looping over productivity breaks
      if (Be[i+1,j]>Be[i,j]) { // case with 2 breaks: Be[2,j]=Be[3,j]
        e1[max(2,Be[i,j]):(Be[i+1,j]-1)] += piv[j]*(mlq[max(2,Be[i,j]):(Be[i+1,j]-1)] - mu_a[i] - mlq[max(1,Be[i,j]-1):(Be[i+1,j]-2)]);
        if (include_full) w_ss[max(2,Be[i,j]):(Be[i+1,j]-1)] += piv[j]*mu_a[i]./alphat[max(2,Be[i,j]):(Be[i+1,j]-1)]/gamma[1];
      }
    }
  }
  if (include_K==0) e2 = w - phi[1] - lbdm;
  else e2 = (1-betat).*(w  - phi[1] - lbdm);
  
  // measurement errors
  iota[ii_trn_n] = -n_trn_err; // population measurement error
  for (t in 1:T_mis_n) {
    iota[ii_mis_n[t]] = 0;
  }
  iota[ii_obs_n] = -n_obs_err;
  
  // Slope of labor demand curve
  slope = alpha[1]/(1 - alpha[2]);
  
  // Capital and rents
  if (include_K) {
    k = m1(k0);
    rents = m1(w + log(alphat) - log(1-alphat-betat) + (n + h));
  }
  
  // population block
  if (include_full) {
    
    // steady state wage
    WoW_ss2 = exp(w - w_ss);
    for (j in 1:(nBx1+1)) {
      w_ss[Bx1[j]:(Bx1[j+1]-1)] += -p[j]*(digamma(betap[1]) - digamma(betap[1] + betap[2]))/gamma[1];
    }
    WoW_ss = exp(w - w_ss);
    
    // population shocks recovered from population supply equation
    xi[1] = 0;
    for (t in 2:T) {
      xi[t] = n[t] - omega[1] - gamma[1]*(w[t-1] + h[t-1]) - n[t-1];
    }
    
    // IRF2 (see slide 21-22 of the appendix)
    n_irf2[1] = n[t_1750];
    w_irf2 = w[t_1750:T];
    h_irf2 = h[t_1750:T];
    n_irf2[2] = omega[1] + gamma[1]*(w[t_1750-1] + h[t_1750-1]) + n_irf2[1];
    for (t in 3:T_irf2) {
      n_irf2[t] = omega[1] + gamma[1]*(w_irf2[t-1] + h_irf2[t-1]) + n_irf2[t-1];
    }
    N_irf2 = exp(n_irf2 - 6*log(10));
    
  }
  
  // Malmquist check
  if (include_check) {
    mlq_check[1] = 0;
    {
      vector[T-1] dmlq_check;
      if (include_K) {
        w_check = phi[1] + log(1 - alphat - betat) + atilde + e2 + (betat).*k0 - (alphat+betat).*(n+h);
        k_check = log(betat)./(1-betat) + (atilde+e2)./(1-betat) + (1-alphat-betat)./(1-betat).*(n+h) - log(r+delta[1])./(1-betat);
        dmlq_check = fd(w - log(1 - alphat - betat)) - bar(betat).*fd(k) + bar(alphat+betat).*fd(n+h);
      } else {
        w_check = phi[1] + log(1 - alphat) + atilde + e2 - (alphat).*(n+h);
        dmlq_check = fd(w - log(1 - alphat)) + bar(alphat).*fd(n+h);
      }
      for (t in 2:T) {
        mlq_check[t] = mlq_check[t-1] + dmlq_check[t-1];
      }
      mlq_check -= e2 - e2[1];
    }
  }
  
}
