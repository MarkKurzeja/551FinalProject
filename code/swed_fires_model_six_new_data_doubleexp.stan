/* 
This is the model that uses vectorization, non-centered parameterization
"Matts Method" as well as regularization to estimate the coefficients for the 
years, municipalities, and the betas.
*/
data {
	int<lower = 1> nobs; // Number of observations / rows
	int<lower = 1> n_muni; // Number of municipalities
	matrix[nobs, n_muni] munis; // factor matrix of the municipalities
	int<lower = 1> n_preds; // Number of the predictors
	matrix[nobs, n_preds] preds; // Predictors themselves
	int<lower = 1> n_years; // The number of years we have collected
	matrix[nobs, n_years] years; // Year factor matrix
	int fires[nobs]; // Get a matrix of the fire outputs
}
parameters {
	real beta_null; // model intercept
	vector[n_preds] betas; // model slopes
	
	vector[n_muni] beta_muni_tilde; // muni mixed-intercepts temp param
	real<lower = 0> beta_muni_sd; // muni mixed-sd
	
	vector[n_years] beta_year_tilde; // year mixed-intercepts temp param
	real<lower=0> beta_year_sd; // year mixed-sd
}
transformed parameters {
	vector[n_years] beta_year; // year mixed-intercepts
	vector[n_muni] beta_muni; // muni mixed-intercepts
	
	// Use matts trick (non-centered reparam) to get the results that we want
	// http://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
	// Now beta_year is basically a double exponential
	// See page 528 of Stan-reference 2.17.0 to see this parameterization derivation
	beta_year = 0 + beta_year_sd * beta_year_tilde;
	beta_muni = 0 + beta_muni_sd * beta_muni_tilde;
	
}
model {
	// Means for each muni-year combination
	vector[nobs] lambda;
	
	// Prior on the global intercept
	beta_null ~ normal(0, 1);
	
	// Prior on the predictor terms
	betas ~ double_exponential(0, 0.5);
	
	// Non-centered double exponential for beta_muni
	beta_muni_sd ~ exponential(0.5);
	beta_muni_tilde ~ normal(0,1); 
	
	// Non-centered double exponential for beta_year
	beta_year_sd ~ exponential(0.5);
	beta_year_tilde ~ normal(0,1); // This is a divergence hack....
	
	// Update the mean for each of the predictors
	lambda = rep_vector(beta_null, nobs) + 
		munis * beta_muni + 
		years * beta_year + 
		preds * betas;

	// Update the posterior
	fires ~ poisson_log(to_array_1d(lambda));
}
