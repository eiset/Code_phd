# Study b):
# 1. Sample size needed for detecting a prevalence of 16 in the Middle Eastern
# refugee population.

p = 0.16
e = 0.05  # margin of error
(n = qnorm(1-0.05/2)^2*p*(1-p)/e^2)
#[1] 195.9144


# 2. Sample size needed for detecting a prevalence of 25 in the Middle Eastern
# refugee population.

p = 0.25
e = 0.05  # margin of error
(n = qnorm(1-0.05/2)^2*p*(1-p)/e^2)
#[1] 288.1094


# The same calculations are the basis of the sample size calculation for study c)
