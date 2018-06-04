#simglm vignette

fixed <- ~ 1 + act + diff + numCourse + act:numCourse
fixed_param <- c(2, 4, 1, 3.5, 2)
cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'), 
                  var_type = c("single", "single", "single"),
                  opts = list(list(mean = 0, sd = 4),
                              list(mean = 0, sd = 3),
                              list(mean = 0, sd = 3)))
n <- 150
error_var <- 3
with_err_gen = 'rnorm'
temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, 
                       cov_param = cov_param,
                       n = n, error_var = error_var, 
                       with_err_gen = with_err_gen, 
                       data_str = "single")


#-----------------------------------------------------------------------
# with factor, categorical, or ordinal variable
fixed <- ~ 1 + act_o + diff.o + numCourse_f + act_o:numCourse_f
fixed_param <- c(0.8, 1, 0.2, 0.1, 0, 0.15, 0.2, 0.5, 0.02, -0.6, -0.1)
cov_param <- NULL
fact_vars <- list(numlevels = c(36, 8, 5), 
                  var_type = c('single', 'single', "single"),
                  opts = list(list(replace = TRUE), list(replace = TRUE),
                              list(replace = TRUE)))
n <- 150
error_var <- 3
with_err_gen = 'rnorm'
temp_single_o <- sim_reg(fixed = fixed, fixed_param = fixed_param, 
                         cov_param = cov_param, n = n, error_var = error_var,
                         with_err_gen = with_err_gen, data_str = "single", 
                         fact_vars = fact_vars)
#-----------------------------------------------------------------------


#power
fixed <- ~ 1 + act + diff + numCourse + act:numCourse
fixed_param <- c(0.5, 1.1, 0.6, 0.9, 1.1)
cov_param <- list(dist_fun = c('rnorm', 'rnorm', 'rnorm'),
                  var_type = c("single", "single", "single"),
                  opts = list(list(mean = 0, sd = 2),
                              list(mean = 0, sd = 2),
                              list(mean = 0, sd = 1)))
n <- 150
error_var <- 20
with_err_gen <- 'rnorm'
pow_param <- c('(Intercept)', 'act', 'diff', 'numCourse')
alpha <- .01
pow_dist <- "z"
pow_tail <- 2
replicates <- 100
power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, 
                     cov_param = cov_param, n = n, error_var = error_var,
                     with_err_gen = with_err_gen, data_str = "single",
                     pow_param = pow_param, alpha = alpha,
                     pow_dist = pow_dist, pow_tail = pow_tail, 
                     replicates = replicates)
#--------------------------------------------------------------------#
# Examples below that cauase this error when run on a mac:
#  Error in sample.int(length(x), size, replace, prob) : cannot take a sample larger than the population when 'replace = FALSE'
#--------------------------------------------------------------------#
#example with sim_reg
fixed <- ~ 1 + act_o + income + grad_degree_f
fixed_param <- c(3.2, 0.4, 0.02, 0.6)
random <- ~ 1 
random_param <- list(random_var = 10, rand_gen = 'rnorm')
cov_param <- list(dist_fun = 'rnorm',
                  var_type = 'level1')
n <- 50
p <- 5
error_var <- 5
with_err_gen <- 'rnorm'
data_str <- 'cross'
fact_vars <- list(numlevels = c(36, 2), 
                  var_type = c('level1', 'level1'))

sim_reg(fixed = fixed, fixed_param = fixed_param, random = random,
        random_param = random_param, cov_param = cov_param, k = NULL, n = n, p = p,
        error_var = error_var, with_err_gen = with_err_gen, 
        data_str = data_str, fact_vars = fact_vars)

#--------------------------------------------------------------------#
#another example from https://brandonlebeau.org/2017/07/24/simglm-jsm/: 
# this one does NOT work
fixed <- ~ 1 + act_o + income + grad_degree_f
fixed_param <- c(0.5, 0.4, 0.02, 0.6)
random <- ~ 1 
random_param <- list(random_var = 10, rand_gen = 'rnorm')
cov_param <- list(dist_fun = 'rnorm',
                  var_type = 'level1')
n <- 50
p <- 5
data_str <- 'cross'
fact_vars <- list(numlevels = c(36, 2), 
                  var_type = c('level1', 'level1'))

sim_glm(fixed = fixed, fixed_param = fixed_param, random = random,
        random_param = random_param, cov_param = cov_param, k = NULL, n = n, p = p,
        data_str = data_str, fact_vars = fact_vars, outcome_type = 'logistic')
#--------------------------------------------------------------------#
#our poisson regression
fixed <- ~ 1 + sum_pLi + Group.o + sum_pLi:Group.o #one sided formula that gives the names of the variables to be included in the simulated data. The intercept is directly shown in the formulation here, but can also be omitted
#To add a factor, categorical, or ordinal variable, just append one of the following to the end of the variable name in the fixed object: “.f”, “.c”, “.o”, “_f“,”_c“, or”_o“. These indicate the variable is a discrete variable of some sort. By default, all variables with a”.f" or “.c” will be treated as factors and expanded using dummy coding. If “.o” is used in the variable names, the variable will be discrete but treated as continuous in the simulation (i.e. only a single fixed effect variable).

fixed_param <- c(-1.5, 0.3, 0.1, 0.1) #regression weights for the fixed effects, this must be the same length as fixed (or one larger if the 1 is not explicitly stated in the fixed object).

cov_param <- list(dist_fun = c('rnorm'),
                  var_type = c("single"),
                  opts = list(list(mean = 0, sd = 2)))
#mean, standard deviation, and type of variable from the fixed object (must by “single” for single level regression). The cov_param object must contain all variables except the intercept and any interactions.


fact_vars <- list(numlevels = 2,var_type='single')# fact_vars is a list that contains numlevels and var_type.
#cor_vars<-0.3
#--------------------------------------------------------------------#
myn <- 130 #sample size
error_var <- 5 #error variance
with_err_gen = 'rnorm' #distribution of residuals

pow_param <- c('(Intercept)', 'sum_pLi', 'Group.o','sum_pLi:Group.o')
alpha <- .01
pow_dist <- "z"
pow_tail <- 2
replicates <- 100


power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, 
                         cov_param = cov_param, 
                         n = myn, data_str = "single", 
                         outcome_type = 'logistic', fact_vars = fact_vars,
                         pow_param = pow_param, alpha = alpha,
                         pow_dist = pow_dist, pow_tail = pow_tail, 
                         replicates = replicates, raw_power = FALSE)
#---------------------------------------------------------------------------#
