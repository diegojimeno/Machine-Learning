#Analysis of variance
#The mathematical intuition on ANOVA is whether the significance of the differences among the sample means (variability of the 
#sampling mean density function) is statistically significant. The variance is depicted as the sum of the variability of the data 
#and the variability of the sampling mode, if the former is higher there is much evidence to appove the null Hypo, otherwise the difference
#among the means is significant and we then reject H0.

#It has to be taken into account that as the number of samples rises (N) is more easily to commit type I errors as ther is much more 
#probability of sampling a value that is more extreme that the sampling mean (1 standard deviations from the mean)


group1 <- c(2,3,7,2,6)
group2 <- c(10,8,7,5,10)
group3 <- c(10,13,14,13,15)

joint <- data.frame(cbind(group1, group2, group3))
data.set.aov <- stack(joint)

aov.sol <- aov(values ~ ind,  data = data.set.aov)

