# Load file
hitdata <- scan("hit.txt")

# Mean
h_mean <- mean(hitdata)

# Median
h_median <- median(hitdata)

# Max
h_max<- max(hitdata)

# Min
h_min <- min(hitdata)

# Boxplot
boxplot(hitdata)

# variation
h_var = var(hitdata)

# Standard Deviation
h_cv = sd(hitdata)/h_mean

# Normalization
z <- (hitdata-h_mean/sd(hitdata))

# Mean, Median, Max, Min in Vector
h_rm = c(h_mean,h_median,h_max,h_min)


# 4 Values Remove
hitdata2 <- hitdata[!(hitdata %in% h_rm)]

# Mean removed 4 values
h2_mean = mean(hitdata2)

#Modified skewness and kurtosis
mod_dist.shape = function(x)
{
  n <- length(z)
  result <- c(NA,NA)
  if (n >= 4)
  {
    z <- (x-mean(x))/sd(x)
    aj_skew <- n/(n-1)/(n-2)*sum(z)^3
    aj_kurt <- n*(n+1)/(n-1)/(n-2)/(n-3)*sum(z)^4-3*(n-1)^2/(n-2)/(n-3)
    result <- c(aj_skew,aj_kurt)
  }
  return(result)
}

mod_dist.shape(hitdata)