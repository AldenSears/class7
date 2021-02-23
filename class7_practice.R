# EBIO338/358: Analysis and Visualization of Biological Data
# Class 7: Describing distributions

# Generate vector of samples from a normal distribution
samples <- rnorm(1000, 0, 1) # What are these number?

# Remember we can describe the range and interquartile range
range(samples)
summary(samples)[c(1,6)]
summary(samples)[c(2,5)]

# Can as usual visualize the data using a histogram
hist(samples)
hist(samples, freq=FALSE)

# Calculate the standard deviation by hand
(sum((samples - mean(samples))^2) / length(samples))^0.5

# Get R to calculate the standard deviation
sd(samples)


# Exploring the flip book...

set.seed(11) # What does set.seed() do?
for(i in 1:10){ # What does for() do?
  
  # Get random samples from normal distribution
  samples <- rnorm(i ^ 3, mean = 0, sd = 1) # what does rnorm() do? What are the arguments?
  
  # Make a histogram
  hist(samples, 
       breaks = seq(-5, 5, by = 0.5),
       probability = T,
       main = paste("n =", length(samples), "\n", 
                    "mean =", round(mean(samples), 3), "\n", 
                    "standard deviation =", round(sd(samples), 3)),
       ylim = c(0,1))
  
  # Theoretical expectations for this distribution
  xx <- seq(-5,5, by = 0.1)
  lines(xx, dnorm(xx, mean = 0, sd = 1), col = "blue", lwd = 2) # How does dnorm relate to rnorm?
  
  Sys.sleep(1) # What does Sys.sleep() do?
}


# Now change this around to make individual histograms for different
# parameter values

my_n <- 10000 # Use a big number to start
my_mean <- 0 # Stick to values between -3 and 3
my_sd <- 1 # Stick to values between 0.2 and 1.5

# Get random samples from normal distribution
samples <- rnorm(my_n, mean = my_mean, sd = my_sd)

# Make a histogram
hist(samples, 
     probability = T,
     breaks = seq(-10, 10, by = 0.2),
     main = paste("n =", length(samples), "\n", 
                  "mean =", round(mean(samples), 3), "\n", 
                  "standard deviation =", round(sd(samples), 3)),
     ylim = c(0,1))

# Theoretical expectations for this distribution
xx <- seq(my_mean-(my_sd*4), my_mean+(my_sd*4), by = 0.1)
lines(xx, dnorm(xx, mean = my_mean, sd = my_sd), col = "blue", lwd = 2) 

# How does sd change the shape of the distribution?
# Does mean change the shape of the distribution?



# Now examine another distribution - the Poisson distribution.

my_n <- 10000
my_lambda <- 1 # Stick to values between 1 and 6

# Get random samples from normal distribution
samples <- rpois(my_n, my_lambda)

# Make a histogram
hist(samples, 
     probability = T,
     breaks = seq(0, 20, by = 0.2),
     main = paste("n =", length(samples), "\n", 
                  "mean =", round(mean(samples), 3)),
     ylim = c(0,2))

# Theoretical expectations for this distribution
xx <- seq(0, 20, by = 1)
lines(xx, dpois(xx, lambda = my_lambda), col = "blue", lwd = 2) 

# What is the range of the samples you're getting? Are they ever negative?
# How would you describe the skew of the distribution? Does this depend on lambda?
# Why isn't there a second parameter (like standard deviation for the normal distribution)?
# What sort of data would you be collecting if you got values like this?





# Practice describing distributions with some real world examples

# Load the stream flow data from problem set 2
data <- read.csv(file="streamflowdata.csv")

# Plot a histogram of maximum stream flow (max) using the default settings
# Describe the shape of the distribution

# Now plot a histogram of max using as many breaks as there are elements in the vector
# Does your description of the shape of the distribution change when using a finer resolution of breaks?

# What is the interquartile range of the vector c(53, 39, 39, 33, 69, 30, 25, 67, 130, 94, 40)?

# An administrator entering salary data into a company spreadsheet accidentally put an extra 0 in the boss's salary isting it as 
# $2,000,000 instead of $200,000. Explain how this error will affect these summary statistics for the company payroll
# measures of center: median and mean
# measures of spread: range, IQR, and standard deviation

# Now make up some salary data to correspond with the above and show how the error affects the summary statistics quantitatively

# A small warehouse employs a supervisor at $1200 a week, an inventory manager at $700 a week, six stock workers at $400 a week
# and four drivers at $500 a week
# a) Find the mean and median wage
# b) How many employees earn more than the mean wage?
# c) Which measure of center best describes a typical wage at this company, the mean or the median? Why?
# d) Which measure of spread would best describe the payroll, the range, the IQR, or the standard deviation? Why?

# A town's January high temperatures averate 36 degrees F with a standard deviation of 10 degrees, 
# while in July the mean high temp is 74 degrees and the standard deviation is 8 degrees.
# In which month is it more unusual to have a day with a high temperature of 55 degrees? Explain

# An incoming freshman took her college's placement exams in French and math. In French, she scored
# 82 and in math, 86. The overall results on the French exam had a mean of 72 and a standard deviation 
# of 8, while the mean math score was 68 with a standard deviation of 12. On which exam did she do 
# better compared with the other freshman?
