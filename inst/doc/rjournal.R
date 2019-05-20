## ----setup, include=FALSE------------------------------------------------
hook_output <- knitr::knit_hooks$get('output')
knitr::knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x <- knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    x <- ifelse(nchar(x) > n, paste(strwrap(x, width = n), "..."), x)
    x <- paste(x, collapse = '\n')
  }
  hook_output(x, options)
})
knitr::opts_chunk$set(message=FALSE, warning=FALSE, linewidth=80)

## ----propagation, echo=FALSE, out.width='50%', fig.cap='Illustration of linearity in an interval $\\pm$ one standard deviation around the mean.\\label{propagation}', fig.align='center'----
knitr::include_graphics("includes/rjournal-propagation.pdf")

## ------------------------------------------------------------------------
library(errors)

x <- 1:5 + rnorm(5, sd = 0.01)
y <- 1:5 + rnorm(5, sd = 0.02)
errors(x) <- 0.01
errors(y) <- 0.02
x; y
(z <- x / y)

## ------------------------------------------------------------------------
str(x)

## ------------------------------------------------------------------------
correl(x, x) # one, cannot be changed
correl(x, y) # NULL, not defined yet
correl(x, y) <- runif(length(x), -1, 1)
correl(x, y)
covar(x, y)

## ------------------------------------------------------------------------
z # previous computation without correlations
(z_correl <- x / y)

## ------------------------------------------------------------------------
# the elementary charge
e <- set_errors(1.6021766208e-19, 0.0000000098e-19)
print(e, digits = 2)

## ------------------------------------------------------------------------
options(errors.notation = "plus-minus")
print(e, digits = 2)
options(errors.notation = "parenthesis")

## ------------------------------------------------------------------------
iris.e <- iris
iris.e[1:4] <- lapply(iris.e[1:4], function(x) set_errors(x, x*0.02))

head(iris.e, 3)

## ---- eval=FALSE---------------------------------------------------------
#  plot(iris.e[["Sepal.Length"]], iris.e[["Sepal.Width"]], col=iris.e[["Species"]])
#  legend(6.2, 4.4, unique(iris.e[["Species"]]), col=1:length(iris.e[["Species"]]), pch=1)
#  
#  library(ggplot2)
#  
#  ggplot(iris.e, aes(Sepal.Length, Sepal.Width, color=Species)) +
#    geom_point() + theme_bw() + theme(legend.position=c(0.6, 0.8)) +
#    geom_errorbar(aes(ymin=errors_min(Sepal.Width), ymax=errors_max(Sepal.Width))) +
#    geom_errorbarh(aes(xmin=errors_min(Sepal.Length), xmax=errors_max(Sepal.Length)))

## ----plot, echo=FALSE, out.width='100%', fig.cap='Base plot with error bars (left) and \\pkg{ggplot2}\'s version (right).\\label{plot}'----
knitr::include_graphics("includes/rjournal-plot.pdf")

## ------------------------------------------------------------------------
V   <- mean(set_errors(GUM.H.2$V))
I   <- mean(set_errors(GUM.H.2$I))
phi <- mean(set_errors(GUM.H.2$phi))

correl(V, I)   <- with(GUM.H.2, cor(V, I))
correl(V, phi) <- with(GUM.H.2, cor(V, phi))
correl(I, phi) <- with(GUM.H.2, cor(I, phi))

print(R <- (V / I) * cos(phi), digits = 2, notation = "plus-minus")
print(X <- (V / I) * sin(phi), digits = 3, notation = "plus-minus")
print(Z <- (V / I), digits = 3, notation = "plus-minus")

correl(R, X); correl(R, Z); correl(X, Z)

## ------------------------------------------------------------------------
fit <- lm(bk ~ I(tk - 20), data = GUM.H.3)

y1 <- set_errors(coef(fit)[1], sqrt(vcov(fit)[1, 1]))
y2 <- set_errors(coef(fit)[2], sqrt(vcov(fit)[2, 2]))
covar(y1, y2) <- vcov(fit)[1, 2]

print(b.30 <- y1 + y2 * set_errors(30 - 20), digits = 2, notation = "plus-minus")

## ---- echo=FALSE---------------------------------------------------------
df <- data.frame(
    c("Row subsetting", "Row ordering", "Column transformation", "Row aggregation", "Column joining", "(Un)Pivoting"),
    c("\\code{[}, \\code{[[}, \\code{subset}", "\\code{order} + \\code{[}", "\\code{transform}, \\code{within}", "\\code{tapply}, \\code{by}, \\code{aggregate}", "\\code{merge}", "\\code{reshape}"),
    c("Full", "Full", "Full", "with \\code{simplify=FALSE}", "Full", "Full")
)

knitr::kable(
  df, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "", escape=FALSE,
  col.names = c("Operation", "R base function(s)", "Compatibility"),
  caption = "Compatibility of \\pkg{errors} and R base data wrangling functions.\\label{tab:compat}"
)

## ------------------------------------------------------------------------
unlist <- function(x) if (is.list(x)) do.call(c, x) else x
iris.e.agg <- aggregate(. ~ Species, data = iris.e, mean, simplify=FALSE)
as.data.frame(lapply(iris.e.agg, unlist), col.names=colnames(iris.e.agg))

