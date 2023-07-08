#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator()(){
   DATA_VECTOR(y);     // Fecundity observations.
   DATA_VECTOR(x);     // Size observations.
   DATA_IVECTOR(tow);  // Size observations.
  
   // Parameters:
   PARAMETER(beta);                  // Log-scale slope parameter.
   PARAMETER(log_alpha_primiparous); // Log-scale intercept parameter for primiparous females.
   PARAMETER(log_alpha_multiparous); // Log-scale intercept parameter for mutliparous females
   PARAMETER(log_sigma_primiparous); // Log-scale error parameter for primiparous females.
   PARAMETER(log_sigma_multiparous); // Log-scale error parameter for multiparous females.
   PARAMETER(log_sigma_outlier);     // Log-scale error parameter for outlier observations.
   PARAMETER(logit_p_multiparous);   // Logit-scale proportion parameter.
   PARAMETER(logit_p_outlier);       // Logit-scale outlier proportion parameter.
  
   PARAMETER_VECTOR(tow_effect);
   PARAMETER(log_sigma_tow_effect);
  
    
   // Derived qunatities and transformed parameters:
   int n = y.size();
   Type sigma_primiparous = exp(log_sigma_primiparous);  // Regression error for primiparous females.
   Type sigma_multiparous = exp(log_sigma_multiparous);  // Regression error for multiparous females.
   Type sigma_outlier     = exp(log_sigma_outlier);      // Regression error for observation outliers.
   Type p_multiparous     = exp(logit_p_multiparous) / (1 + exp(logit_p_multiparous)); // Multiparous proportion.
   Type p_outlier         = exp(logit_p_outlier) / (1 + exp(logit_p_outlier));         // Outlier proportion.
   Type v = 0;  // Log-likelihood variable.
  
   // Tow rando effect:
   v -= sum(dnorm(tow_effect, 0, exp(log_sigma_tow_effect), true));

   // Model likelihood:
   for (int i = 0; i < n; i++){
      // Regression means:
      Type mu_primiparous = log_alpha_primiparous + tow_effect[tow[i]] + beta * log(x[i]);
      Type mu_multiparous = log_alpha_multiparous + beta * log(x[i]);
     
      // Mixture likelihood:
      v -= log((1-p_multiparous) * ((1-p_outlier) * dnorm(log(y[i]), mu_primiparous, sigma_primiparous, false) +
                                       p_outlier  * dnorm(log(y[i]), mu_primiparous, sigma_outlier, false)) + 
                   p_multiparous * ((1-p_outlier) * dnorm(log(y[i]), mu_multiparous, sigma_multiparous, false) +
                                       p_outlier  * dnorm(log(y[i]), mu_multiparous, sigma_outlier, false)));
   }
  
   // Export variables:
   REPORT(tow_effect);
   REPORT(sigma_primiparous);
   REPORT(sigma_multiparous);
   REPORT(sigma_outlier);
   REPORT(p_multiparous);
  
   return v;
}

