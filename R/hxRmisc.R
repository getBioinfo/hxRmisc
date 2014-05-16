#####>>>>> BEGIN: Jeffreys’ substitution posterior for the median >>>>>
#### Desc: Jeffreys’ Substitution Posterior for the Median: 
##         A Nice Trick to Non-parametrically Estimate the Median
##### Pro: Compared with Wilcoxon signed-rank test I think Jeffreys’ 
##         substitution posterior is a good alternative when you are 
##         interested in the median but otherwise want to make as few 
##         assumptions as possible.
##### Con: However, I don’t want to generally recommend Jeffreys’ 
##         substitution posterior, as it doesn’t make strong assumptions 
##         almost any other model based on reasonable assumptions will 
##         be more powerful and will make better use of the data.
##### URL: http://www.sumsar.net/blog/2014/05/jeffreys-substitution-posterior/

# A more numerically stable way of calculating log( sum( exp( x ))) Source:
# http://r.789695.n4.nabble.com/logsumexp-function-in-R-td3310119.html
logsumexp <- function(x) {
  xmax <- which.max(x)
  log1p(sum(exp(x[-xmax] - x[xmax]))) + x[xmax]
}

# Silently returns samples from Jeffreys’ substitution posterior given a
# vector of data (x). Also produces a histogram of the posterior and prints
# out a 95% quantile credible interval. Assumes a non-informative uniform
# [-Inf, Inf] prior over the median.
jeffreys_median <- function(x, n_samples = 10000, draw_plot = TRUE) {
  x <- sort(x)
  n <- length(x)
  loglike <- 1 - lfactorial(1:(n - 1)) - lfactorial((n - 1):1)
  interval_loglike <- loglike + log(diff(x))
  interval_prob <- exp(interval_loglike - logsumexp(interval_loglike))
  samp_inter <- sample.int(n - 1, n_samples, replace = TRUE, prob = interval_prob)
  s <- runif(n_samples, x[samp_inter], x[samp_inter + 1])
  cat("\n  Jeffreys’ Substitution Posterior for the median\n\n")
  cat("median\n  ", median(x), "\n")
  cat("95% CI\n  ", quantile(s, c(0.025, 0.975)), "\n")
  if (draw_plot) {
    hist(s, 30, freq = FALSE, main = "Posterior of the median", xlab = "median")
    lines(quantile(s, c(0.025, 0.975)), c(0, 0), col = "red", lwd = 5)
    legend("topright", legend = "95% CI", col = "red", lwd = 5)
  }
  invisible(s)
}

#####<<<<< END: Jeffreys’ substitution posterior for the median <<<<<




#####>>>>> BEGIN: fast read of data table >>>>>
#### Desc: JQuickly reading very large tables as dataframes in R 
##         A Nice Trick to Non-parametrically Estimate the Median
##### Pro: Similar to read.table but faster and more convenient.
##         Can read millions of rows in seconds
##### Con: N/A
##### URL: http://fabiomarroni.wordpress.com/2014/05/02/reading-larga-data-tables-in-r/
#### Note: Use as a wrapper for reminding fread function, 
##         Not really used for new function
##         http://stackoverflow.com/questions/8165837/how-to-pass-a-function-and-its-arguments-through-a-wrapper-function-in-r-simila

require(data.table)
read.bigdata <- function(fread, ...) {
  fread(...)
}

#####<<<<< END: fast read of data table <<<<<




#####>>>>> BEGIN: >>>>>
#### Desc: 
##         
##### Pro: 
##         
##### Con: 
##         
##### URL: 
#### Note:  
##         



#####<<<<< END: <<<<<





################### Template ##################
#####>>>>> BEGIN: >>>>>
#### Desc: 
##         
##### Pro: 
##         
##### Con: 
##         
##### URL: 
#### Note:  
##         



#####<<<<< END: <<<<<
###############################################
