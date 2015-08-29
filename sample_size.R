### Sample size calculations given under study b) and c)

# Study b):
# 1. Sample size needed for detecting a prevalence of 16 in the Middle Eastern
# refugee population.

p = 0.14
e = 0.05  # margin of error
(n = qnorm(1-0.05 / 2)^2 * p * (1-p) / e^2)
#[1] 185.0047


# 2. Sample size needed for detecting a prevalence of 25 in the Middle Eastern
# refugee population.

p = 0.25
e = 0.05  # margin of error
(n = qnorm(1-0.05 / 2)^2 * p * (1-p) / e^2)
#[1] 288.1094


# The same calculations are the basis for the sample size calculation for study c)

# Normal approximation is assumed. To formally check this n and n * (1-p) must both be greater than 5.
# n = 185.0047 and n * (1-0.14) = 159.104.

