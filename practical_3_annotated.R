################################################################################
# PRACTICAL 3: CONTINUOUS DISTRIBUTIONS, SAMPLING AND STANDARD ERRORS
################################################################################

# This practical uses R to explore continuous probability distributions,
# repeated sampling, standard errors and confidence intervals.
# It uses base R, so no additional packages or data files are required.


################################################################################
# PART 1. CONTINUOUS PROBABILITIES
################################################################################

# X is the number of minutes after 9:00 when a patient arrives.
# We assume X is uniformly distributed between 0 and 10 minutes.
# A uniform distribution gives equal probabilities to intervals of equal length.

# dunif(x, min, max) calculates the probability DENSITY at x.
# Use dunif() when you need a density height or density values for a graph.
# x = 4 is the point being evaluated; min and max define the distribution.
# The density is 1 / (10 - 0) = 0.1 per minute.
# This is NOT a 10% probability of arriving exactly at minute 4:
# for a continuous variable, the probability at one exact point is zero.

arrival_density_at_four <- dunif(x = 4, min = 0, max = 10)
arrival_density_at_four

# Calculate the probability of arriving between minutes 2 and 5.
# For this uniform distribution, probability = interval width * density:
# (5 - 2) * 0.1 = 0.3, or 30%.

probability_two_to_five_area <- (5 - 2) * arrival_density_at_four
probability_two_to_five_area

# Create values from 0 to 10 in steps of 0.1 for plotting.
# The colon operator 0:100 creates integers from 0 through 100;
# dividing by 10 changes them to 0.0, 0.1, ..., 10.0.

arrival_times <- (0:100) / 10

# Calculate the density at each value in arrival_times.
# We use dunif() here because the graph needs density heights.
# The density is 0.1 at every point in this uniform distribution.

arrival_densities <- dunif(x = arrival_times, min = 0, max = 10)

# Plot time on the horizontal axis and density on the vertical axis.
# type = "l" draws a line; xlab and ylab label the axes;
# main sets the title; ylim sets the vertical-axis range from 0 to 0.12.
# Interpretation: the flat line shows the density is constant at 0.1.
# Probability over an interval is the area under this line.

plot(arrival_times, arrival_densities, type = "l",
     xlab = "Minutes after 9:00", ylab = "Density",
     main = "Uniform arrival-time model", ylim = c(0, 0.12))

# punif(q, min, max) calculates cumulative probability P(X <= q).
# Cumulative probability means the probability up to and including a cutoff.
# Use punif() when you want the probability that X is at or below q.
# q = 4 is the cutoff time; min and max define the uniform distribution.
# P(X <= 4) = 4 / 10 = 0.4, or 40%.

probability_by_four <- punif(q = 4, min = 0, max = 10)
probability_by_four

# Calculate the probability of arriving between minutes 2 and 5
# by subtracting cumulative probabilities:
# P(2 <= X <= 5) = P(X <= 5) - P(X <= 2) = 0.5 - 0.2 = 0.3.
# This gives the same 30% result as the area calculation above.
# For continuous distributions, including/excluding an endpoint does not
# change the probability because any single exact value has probability zero.

probability_two_to_five <- punif(q = 5, min = 0, max = 10) -
  punif(q = 2, min = 0, max = 10)
probability_two_to_five


# EXERCISE 1
# a. Find P(6 < X < 9) and compare it with P(2 < X < 5).
#    Both intervals are 3 minutes wide, so the uniform model predicts
#    that they have the same probability: 3 / 10 = 0.3 (30%).
# b. Find the probability of arriving after minute 8.
#    lower.tail = FALSE asks for the probability ABOVE q, P(X > q),
#    instead of the usual cumulative probability P(X <= q).
# c. A density of 0.1 at X = 4 is not a 10% probability at that exact point.
#    Probability is area: an interval of width w has probability w * 0.1.

probability_six_to_nine <- punif(q = 9, min = 0, max = 10) -
  punif(q = 6, min = 0, max = 10)
probability_six_to_nine

probability_after_eight <- punif(q = 8, min = 0, max = 10,
                                 lower.tail = FALSE)
probability_after_eight

# Interpretation:
# P(6 < X < 9) = 0.3 (30%), the same as P(2 < X < 5).
# P(X > 8) = 0.2 (20%), because 2 of the 10 minutes remain after minute 8.


################################################################################
# PART 2. THE NORMAL DISTRIBUTION
################################################################################

# Birthweight is modelled with a Normal distribution:
# mean = 3.4 kg and standard deviation (SD) = 0.6 kg.
# In R, sd means standard deviation, not variance.

birthweight_mean <- 3.4
birthweight_sd <- 0.6

# dnorm(x, mean, sd) calculates Normal density heights.
# Use dnorm() to get density values, for example to draw a density curve.
# x is the birthweight value; mean is the distribution centre;
# sd is the spread of the distribution.

birthweight_values <- (10:58) / 10
birthweight_densities <- dnorm(x = birthweight_values,
                               mean = birthweight_mean, sd = birthweight_sd)

# Plot the Normal density curve.
# The curve is bell-shaped and centred around the mean of 3.4 kg.

plot(birthweight_values, birthweight_densities, type = "l",
     xlab = "Birthweight (kg)", ylab = "Density",
     main = "Normal birthweight model")

# pnorm(q, mean, sd) calculates cumulative probability P(X <= q),
# which is the area to the LEFT of q under the Normal density curve.
# Here q is a vector, so R calculates cumulative probabilities for every
# birthweight value in birthweight_values.

birthweight_cumulative <- pnorm(q = birthweight_values,
                                mean = birthweight_mean, sd = birthweight_sd)

# Plot the cumulative probabilities.
# The vertical axis is cumulative probability, so label it accordingly.
# The curve increases from near 0 to near 1.

plot(birthweight_values, birthweight_cumulative, type = "l",
     xlab = "Birthweight (kg)", ylab = "Cumulative probability",
     main = "Normal birthweight cumulative distribution")

# To find P(X >= 4.5), calculate the area to the RIGHT of 4.5.
# pnorm() gives the area to the left, so subtract it from 1.
# The result is about 0.0334, or 3.34%.

probability_at_least_four_point_five <- 1 - pnorm(
  q = 4.5, mean = birthweight_mean, sd = birthweight_sd)

probability_at_least_four_point_five
100 * probability_at_least_four_point_five

# Standardise 4.5 kg to a z-score: (value - mean) / SD.
# The z-score tells us how many standard deviations the value is above
# or below the mean. Here it is about 1.83 SD above the mean.
# Using a standard Normal distribution (mean 0, SD 1) gives the same tail area.

birthweight_z <- (4.5 - birthweight_mean) / birthweight_sd
birthweight_z
1 - pnorm(q = birthweight_z, mean = 0, sd = 1)

# EXERCISE 2
# a. pnorm(q = 2.8, ...) finds P(X < 2.8). To find P(2.8 < X < 4.0),
#    subtract P(X <= 2.8) from P(X <= 4.0).
# b. Standardise both boundaries with z = (value - mean) / SD and use
#    pnorm() with mean = 0 and sd = 1. The interval probability should match.
# c. Increasing SD from 0.6 to 0.9 makes the distribution more spread out.
#    Since 4.5 kg is above the unchanged mean, more probability lies in the
#    upper tail, so P(X >= 4.5) becomes larger.

probability_below_two_point_eight <- pnorm(
  q = 2.8, mean = birthweight_mean, sd = birthweight_sd)

probability_two_point_eight_to_four <- pnorm(
  q = 4, mean = birthweight_mean, sd = birthweight_sd) -
  pnorm(q = 2.8, mean = birthweight_mean, sd = birthweight_sd)

# Standardise the lower and upper boundaries for part (b).
lower_z <- (2.8 - birthweight_mean) / birthweight_sd
upper_z <- (4.0 - birthweight_mean) / birthweight_sd
lower_z
upper_z
pnorm(q = upper_z, mean = 0, sd = 1) -
  pnorm(q = lower_z, mean = 0, sd = 1)

# Calculate P(X >= 4.5) for a Normal distribution with the same mean
# but a wider SD of 0.9 kg. Use 1 - pnorm() for the upper-tail probability.

probability_above_cutoff_wider <- 1 - pnorm(
  q = 4.5, mean = birthweight_mean, sd = 0.9)
probability_above_cutoff_wider

# Interpretation:
# The wider distribution has a larger probability above 4.5 kg because
# it spreads more of its area away from the mean into the tails.


################################################################################
# PART 3. REPEATED SAMPLES AND THE CENTRAL LIMIT THEOREM (CLT)
################################################################################

# We simulate patient arrival times from the uniform distribution.
# A simulated observation is one random value drawn from the model.

# set.seed() makes the random sequence reproducible.
# Running the code again from this point produces the same random draws.
# The seed is for reproducibility; it is not a parameter of the distribution.

set.seed(303)

# runif(n, min, max) generates n random values from a uniform distribution.
# Here n = 4 means one sample contains four arrival times.
# mean() calculates the average of those four arrival times.

one_arrival_sample <- runif(n = 4, min = 0, max = 10)
one_arrival_sample
mean(one_arrival_sample)

# replicate(5, expression) repeats an expression five times.
# Each repetition generates four arrivals and calculates their sample mean.
# Therefore, the result contains five sample means, each based on four arrivals.

five_sample_means <- replicate(5, mean(runif(n = 4, min = 0, max = 10)))
five_sample_means

# Repeat the sampling procedure 1,000 times.
# n = 4 inside runif() is the number of observations in each sample.
# replicate(1000, ...) is the number of separate samples generated.
# These are two different counts.

means_four <- replicate(1000, mean(runif(n = 4, min = 0, max = 10)))

# Draw a histogram of the 1,000 sample means.
# breaks controls the histogram's bin boundaries.
# The histogram shows the sampling distribution of means for samples of four.

hist(means_four, breaks = (0:20) / 2,
     main = "Means of samples of four arrivals",
     xlab = "Sample mean arrival time (minutes)")

# Calculate the average and SD of the 1,000 sample means.
mean(means_four)
sd(means_four)

# Interpretation:
# The mean of the sample means should be close to the population mean, 5.
# Their SD describes how much sample means vary from one sample to another.
# This SD estimates the standard error (SE) of the sample mean for n = 4.


# EXERCISE 3
# a. With 16 arrivals per sample, the sampling distribution should still be
#    centred near 5 minutes, but it should be narrower because larger samples
#    generally produce more stable means. Its shape should look more Normal.
# b. Generate 1,000 sample means, each based on 16 arrivals.
# c. Compare the SDs. For a uniform population, increasing n from 4 to 16
#    should halve the SE because SE is proportional to 1 / sqrt(n).
# d. replicate(1000, ...) controls how many sample means are generated;
#    runif(n = 16, ...) controls the number of observations in each sample.

means_sixteen <- replicate(1000, mean(runif(n = 16, min = 0, max = 10)))
mean(means_sixteen)
sd(means_sixteen)

# Plot the sampling distribution for samples of sixteen arrivals.
hist(means_sixteen, breaks = (0:20) / 2,
     main = "Means of samples of sixteen arrivals",
     xlab = "Sample mean arrival time (minutes)")

# Calculate the ratio of the SDs for sample sizes 16 and 4.
# The expected ratio is about 0.5: the SE for n = 16 is about half that for n = 4.

sd_ratio <- sd(means_sixteen) / sd(means_four)
sd_ratio

# Interpretation:
# A ratio near 0.5 supports the prediction that larger samples have less
# variability in their means.


################################################################################
# PART 4. ESTIMATING STANDARD ERRORS
################################################################################

# In a real study, we usually have one sample rather than thousands.
# The estimated SE of a sample mean is sample SD / sqrt(sample size).
# SD describes variation among individuals; SE describes uncertainty
# or variation in the sample mean across repeated samples.

# Worked example: preterm-infant birthweights.

preterm_n <- 98
preterm_mean <- 1.31
preterm_sd <- 0.42

# Calculate the estimated SE of the sample mean.
# sqrt(preterm_n) is the square root of the sample size.

preterm_se <- preterm_sd / sqrt(preterm_n)

preterm_sd
preterm_se

# Interpretation:
# SD = 0.42 kg describes variation between individual birthweights.
# SE is about 0.0424 kg and describes the sampling variability of the mean.


# EXERCISE 4
# The estimated SE is SD / sqrt(n).
# Study A: SE = 20 / sqrt(100) = 2 mmHg.
# Study B: SE = 50 / sqrt(400) = 2.5 mmHg.
# Study A's mean is estimated more precisely because its SE is smaller.
# Comparing sample sizes alone would be misleading because the SDs differ.

study_a_se <- 20 / sqrt(100)
study_a_se

study_b_se <- 50 / sqrt(400)
study_b_se

# Interpretation:
# Although Study B has more participants, its much larger SD means its SE
# is larger. Precision depends on both sample size and variability.


# A PROPORTION
# A response proportion is the number responding divided by the total sample.
# For a proportion p, the estimated SE is sqrt(p * (1 - p) / n).
# Here p = 0.46 and n = 124, based on the lecture's acupuncture example.

acupuncture_n <- 124
acupuncture_p <- 0.46
acupuncture_se <- sqrt(acupuncture_p * (1 - acupuncture_p) / acupuncture_n)
acupuncture_se

# Interpretation:
# The SE is approximately 0.0448, equivalent to 4.48 percentage points.
# It describes how much the estimated response proportion would vary
# across repeated samples of the same size under similar conditions.


# EXERCISE 5
# 44 of 80 patients responded.
# The sample proportion is 44 / 80 = 0.55, or 55%.
# Use the proportion SE formula to estimate its sampling variability.

response_n <- 80
response_count <- 44
response_p <- response_count / response_n
response_se <- sqrt(response_p * (1 - response_p) / response_n)

# Show the estimated proportion and SE as percentages / percentage points.
100 * response_p
100 * response_se

# Interpretation:
# The SE describes variation in the sample response proportion across
# repeated samples; it does not describe an individual patient's outcome.


# AN EVENT RATE
# For the Poisson example, D is the total number of admissions and T is
# the number of days observed.
# Estimated daily rate = D / T.
# Estimated SE of the rate = sqrt(D) / T.

admission_count <- 100
admission_days <- 25
admission_rate <- admission_count / admission_days
admission_se <- sqrt(admission_count) / admission_days

admission_rate
admission_se

# Interpretation:
# The estimated rate is 4 admissions per day and its SE is 0.4 per day.


# EXERCISE 6
# The second unit records 36 admissions over 9 days.
# Calculate its rate and SE using the same formulas.
# Compare rates, not total counts, to decide which unit has the higher
# estimated daily rate. Compare SEs to judge precision.

other_admission_count <- 36
other_admission_days <- 9
other_admission_rate <- other_admission_count / other_admission_days
other_admission_se <- sqrt(other_admission_count) / other_admission_days

other_admission_rate
other_admission_se

# Interpretation:
# The second unit's rate is 36 / 9 = 4 admissions per day, the same as
# the first unit. Its SE is 6 / 9 = 0.667 admissions per day, which is
# larger than 0.4, so its daily rate estimate is less precise.


################################################################################
# PART 5. CONFIDENCE INTERVALS
################################################################################

# For an approximately Normal sampling distribution, an approximate 95%
# confidence interval is estimate +/- 1.96 * SE.
# The interval gives a plausible range for the population parameter.
# Keep the full precision of the SE during calculations; round only for display.

preterm_lower <- preterm_mean - 1.96 * preterm_se
preterm_upper <- preterm_mean + 1.96 * preterm_se

c(preterm_lower, preterm_upper)
round(c(preterm_lower, preterm_upper), 2)

# Interpretation:
# The approximate 95% confidence interval for the population mean birthweight
# is 1.23 to 1.39 kg. It is an interval for the population MEAN, not for
# the birthweights of 95% of individual preterm infants.


# EXERCISE 7
# Calculate an approximate 95% confidence interval for the population
# response proportion: estimate +/- 1.96 * SE.
# Multiply by 100 to report the limits as percentages to one decimal place.

response_lower <- response_p - 1.96 * response_se
response_upper <- response_p + 1.96 * response_se

c(response_lower, response_upper)
round(100 * c(response_lower, response_upper), 1)

# Interpretation:
# The interval estimates the population response proportion.
# It does not predict the outcome for an individual patient, whose outcome
# is response or no response.


# EXERCISE 8
# Calculate approximate 95% confidence intervals for both daily admission
# rates using estimate +/- 1.96 * SE, then compare their centres and widths.

admission_lower <- admission_rate - 1.96 * admission_se
admission_upper <- admission_rate + 1.96 * admission_se
other_admission_lower <- other_admission_rate - 1.96 * other_admission_se
other_admission_upper <- other_admission_rate + 1.96 * other_admission_se

# Display the first and second unit's confidence intervals.
c(admission_lower, admission_upper)
c(other_admission_lower, other_admission_upper)

# Interpretation:
# Both estimated rates are 4 admissions per day, so the intervals have
# the same centre. The second unit's interval is wider because its SE is larger.


# EXERCISE 9: INTERPRETATION
# a. The interval 1.23 to 1.39 kg estimates the population MEAN birthweight.
#    It does not say that 95% of individual infants weigh within this range.
# b. A 95% confidence level means that, over many repeated samples,
#    approximately 95% of intervals calculated by this method would contain
#    the true population parameter.
# c. One sample provides an estimate and a sample SD. These can be used to
#    estimate the SE and construct a confidence interval for the parameter.

# Written answers:
# Use the explanations above to distinguish a confidence interval for a
# population parameter from the range of individual observations.


################################################################################
# OPTIONAL EXTENSION: CHANGING THE CONFIDENCE LEVEL
################################################################################

# pnorm() converts a value into the probability to its left.
# qnorm() does the reverse: given a cumulative probability p, it returns
# the Normal value with that probability to its left.
#
# For a central 95% interval, 2.5% is in each tail, so 97.5% is to the left
# of the upper boundary. qnorm(0.975) gives a multiplier of about 1.96.

multiplier_95 <- qnorm(p = 0.975, mean = 0, sd = 1)
multiplier_95


# EXERCISE 10
# A central 99% interval leaves 0.5% in each tail, so 99.5% is to the left
# of the upper boundary. qnorm(0.995) gives a larger multiplier than 1.96.
# Use this multiplier in estimate +/- multiplier * SE to calculate the 99% CI.

multiplier_99 <- qnorm(p = 0.995, mean = 0, sd = 1)
multiplier_99

preterm_lower_99 <- preterm_mean - multiplier_99 * preterm_se
preterm_upper_99 <- preterm_mean + multiplier_99 * preterm_se

c(preterm_lower_99, preterm_upper_99)
round(c(preterm_lower_99, preterm_upper_99), 2)

# Interpretation:
# The 99% confidence interval is wider than the 95% interval because a higher
# confidence level requires a larger multiplier and therefore a wider range.
