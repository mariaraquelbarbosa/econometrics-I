# Interpretation of Coefficients I
earnfit <- lm(EARNINGS~S, data=EAWE21.simple)
earnfit

# Efficiency / Precision
summary(earnfit)

# Plot
plot(EAWE21.simple$S, EAWE21.simple$EARNINGS)
abline(earnfit)
