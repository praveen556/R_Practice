n <- 50
bdays <- sample(1:365, n, replace = TRUE ) # Assign 365 days to vector with replace function
any(duplicated(bdays)) # Checking vector whether any birthdays are duplicated in 50 people. This will return TRUE/FALSE

# Monte Carlo simulation with B=10000 replicates

B <- 10000

results <- replicate(B, {
  bdays <- sample(1:365, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)