# example of likert of satisfaction scale
# -5 very dissatisfied
# 0 neutral
# 5 very satisfied 

# Sequence for curve
x <- seq(-5, 5, length=200)
y <- dnorm(x)

# Plot normal curve
plot(x, y, type="l", lwd=1, ylab="density(at = x)", xlab="x")

#-------------

# dnorm
#?dnorm

#-------------

# dnorm(2)
segments(x0 = 2, y0 = 0, x1 = 2, y1 = dnorm(2), col = "red", lwd = 2)
arrows(3, 0.15, 2, dnorm(2), col="red", length=0.1)
text(3, 0.17, paste0("density(2) = ", round(dnorm(2), 3)), col="red")

#-------------

# 97% probability
lower <- qnorm(0)
upper <- qnorm(0.977)
idx <- x >= lower & x <= upper

# Polygon shading only between lower and upper bounds
polygon(c(lower, x[idx], upper), c(0, y[idx], 0),
        col="lightgrey", border=NA)

# pnorm(2)
arrows(2, 0.35, 0.5, 0.25, col="black", length=0.1)
text(2, 0.37, paste0("cdf(2) = ", round(pnorm(2), 3)), col="black")

#-------------

# qnorm(0.977)
arrows(3.8, 0.05, 2, 0, col="blue", length=0.1)
text(3.8, 0.07, paste0("quantile(0.977) = ", round(qnorm(0.977), 0)), col="blue")

#-------------

# Random samples with jitter
set.seed(123)
sample <- rnorm(50)
points(sample, jitter(rep(0, 50), amount=0.01), 
       col="darkgreen", pch=19, cex=0.6)

# rnorm(50)
arrows(-3, 0.1, -2, 0.01, col="darkgreen", length=0.1)
text(-3, 0.12, "generate(50)", col="darkgreen")

