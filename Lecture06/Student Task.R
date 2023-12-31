model1 <- lm(officers~crimes, data = crime)
model2 <- lm(officers~crimes+pcinc+pop, data = crime)
anova(model1, model2)

# P value is smaller than significance level --> reject que null hypothesis
# Conclusion: adding the population and capital income will increase