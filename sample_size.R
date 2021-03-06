# Sample size calculations given under study b) and c). 

# The formula used is given in, amongst other, "Epidemiology: Study Design and Data Analysis" by Mark Woodward.

# Study b):
# 1. Sample size needed for detecting a prevalence of 15% in the Middle Eastern
# refugee population.

p = 0.15
e = 0.05  # margin of error
(n = qnorm(1-0.05 / 2)^2 * p * (1-p) / e^2)
#[1] 195.9144

# Normal approximation is assumed. To formally check this n and n * (1-p) must both be greater than 5.
# n = 195.9144 and n * (1 - 0.15) = 166.5272.


# 2. Sample size needed for detecting a prevalence of 20 in the Middle Eastern
# refugee population.

p = 0.20
e = 0.05  # margin of error
(n = qnorm(1-0.05 / 2)^2 * p * (1-p) / e^2)
#[1] 245.8534


# The same calculations are the basis for the sample size calculation for study c)
