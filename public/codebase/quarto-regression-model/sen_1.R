# https://carloscinelli.com/sensemakr/

library(sensemakr)
data("darfur")

darfur -> df

sapply(df, class)

names(df)

df <- df[, -13]

# runs regression model
model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
              pastvoted + hhsize_darfur + female, data = df)

summary(model)

sensitivity <- sensemakr(model = model, 
                         treatment = "directlyharmed",
                         benchmark_covariates = "female",
                         kd = 1:3)
summary(sensitivity)

plot(sensitivity)
ovb_minimal_reporting(sensitivity)

# https://www.youtube.com/watch?v=3wNxZcvRdPI&ab_channel=BuildSci
# https://www.youtube.com/watch?v=wzTpoINJyBQ&ab_channel=ParallelComputingandScientificMachineLearning
# https://www.youtube.com/watch?v=vBuWB9WuFhA&ab_channel=JefKarelCaers





library(sensobol)
?sensobol





library(tidymodels)
library(sensitivity)

# Example data
set.seed(123)
x1 = runif(100)
x2 = runif(100)
x3 = runif(100)
y = 3 * x1 + 2 * x2 + x3 + rnorm(100)
data <- data.frame(x1, x2, x3, y)

# Split data into training and testing sets
set.seed(234)
data_split <- initial_split(data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create a linear regression model using tidymodels
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

lm_fit <- lm_spec %>%
  fit(y ~ x1 + x2 + x3, data = train_data)

# Define a model function
model_function <- function(x) {
  new_data <- data.frame(x1 = x[, 1], x2 = x[, 2], x3 = x[, 3])
  predict(lm_fit, new_data)$`.pred`
}

# Perform Sobol sensitivity analysis
set.seed(345)
X_index1 = sample(x=1:100, size = 50, replace = FALSE)
X_index2 = c(1:length(data$x1))[-X_index1]

sobol_results <- sobol(model = model_function, 
                       X1 = data[X_index1, -4], 
                       X2 = data[X_index2, -4], 
                       nboot = 1000, order = 2)

sobol_results

#########################

## Load the package:
library(sensobol)

## Define the base sample size and the parameters
N <- 2 ^ 8
params <- paste("X", 1:3, sep = "")

## Create sample matrix to compute first and total-order indices:
mat <- sobol_matrices(N = N, params = params)

## Compute the model output (using the Ishigami test function):
Y <- ishigami_Fun(mat)

## Compute and bootstrap the Sobol' indices:
ind <- sobol_indices(Y = Y, N = N, params = params)








