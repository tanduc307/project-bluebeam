chart.Correlation_check <- function(R, histogram = TRUE, method = c(
                                      "pearson", "kendall",
                                      "spearman"
                                    ), cex_text = 3, cex_sign = 3, ...) {
  library("PerformanceAnalytics")
  x <- checkData(R, method = "matrix")
  if (missing(method)) {
    method <- method[1]
  }
  cormeth <- method
  panel.cor <- function(
      x, y, digits = 2, prefix = "", use = "pairwise.complete.obs",
      method = cormeth, cex_text_ok = cex_text, cex_sign_ok = cex_sign, ...) {
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")

    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value,
      corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c(
        "***",
        "**", "*", ".", " "
      )
    )
    text(0.5, 0.5, txt, cex = cex_text_ok)
    text(0.8, 0.8, Signif, cex = cex_sign_ok, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel <- function(x, ... = NULL) {
    par(new = TRUE)
    hist(x,
      col = "light gray", probability = TRUE, axes = FALSE,
      main = "", breaks = "FD"
    )
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) {
    pairs(x,
      gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor,
      diag.panel = hist.panel
    )
  } else {
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor)
  }
}