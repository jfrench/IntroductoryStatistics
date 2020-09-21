draw_cr <- function(type = "two-tailed", df, cv, lowerx = -5, upperx = 5) {
  x <- seq(lowerx, upperx, len = 1000)
  dx <- dt(x, df = df)
  plot(x, dx, xlab = "test value", ylab = "density", type = "l")
  abline(h = 0)

  if(type == "left-tailed") {
    cvx <- seq(lowerx, cv, len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "grey")
  }else if (type == "right-tailed") {
    cvx <- seq(cv, upperx, len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "grey")
  }else {
    cvx <- seq(lowerx, -abs(cv), len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "grey")
    cvx <- seq(abs(cv), upperx, len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "grey")
  }
  
  legend("topright", c("RR", "non-RR"), fill = c("grey", "white"))
}

# draw left-tailed rejection region for z distribution
draw_cr("left-tailed", df = 1e99, cv = -1.96)
# draw right-tailed rejection region for z distribution
draw_cr("right-tailed", df = 1e99, cv = 1.96)
# draw two-tailed rejection region for z distribution
draw_cr("two-tailed", df = 1e99, cv = 2.326)
# draw two-tailed rejection region for distribution with 6 df
draw_cr("two-tailed", df = 6, cv = 1.68)
