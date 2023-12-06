## ----setup, include=FALSE-----------------------------------------------------
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

# from https://stackoverflow.com/a/47298632/6788081
ref <- function(useName) {
  if(!exists(".refctr")) .refctr <- c(`_` = 0)
  if(any(names(.refctr) == useName)) return(.refctr[useName])
  type <- strsplit(useName, ":")[[1]][1]
  nObj <- sum(grepl(type, names(.refctr)))
  useNum <- nObj + 1
  newrefctr <- c(.refctr, useNum)
  names(newrefctr)[length(.refctr) + 1] <- useName
  assign(".refctr", newrefctr, envir=.rmdenvir)
  return(useNum)
}
.rmdenvir = environment()
.refctr <- c(`_` = 0)

label <- function(x) {
  if (!knitr::is_html_output())
    return("")
  if (grepl("fig", x))
    return(paste0("Figure ", ref(x), ": "))
  return(paste0("Table ", ref(x), ": "))
}

## ----echo=FALSE, eval=knitr::is_html_output(), results='asis'-----------------
#  cat('<div style="display:none">',
#      '$\\newcommand{\\E}{\\mathbb{E}}$',
#      '</div>', sep="\n")

## ----propagation, echo=FALSE, out.width='50%', fig.cap=paste0(label("fig:propagation"), "Illustration of linearity in an interval $\\pm$ one standard deviation around the mean."), fig.height=3.7, fig.width=4.5, fig.align='center'----
library(ggplot2)
df <- data.frame(x=-5:5)
p1 <- ggplot(df, aes(x)) + theme_void() +
  stat_function(fun=dnorm, alpha=0.4) +
  geom_area(stat="function", fun=dnorm, fill="black", xlim=c(-1, 1), alpha=0.4) +
  scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0))
p2 <- p1 + coord_flip()
f <- function(x) (x-5)^3/100+1.25
f1 <- function(x) eval(D(as.list(f)[[2]], "x"))
ggplot(df, aes(x)) +
  theme(axis.line=element_line(), panel.grid=element_blank(), panel.background=element_blank(),
        axis.text=element_text(size=12), axis.title=element_blank()) +
  scale_x_continuous(expand=c(0, 0), breaks=0, labels=expression(mu[X[n]])) +
  scale_y_continuous(limits=c(-5, 5), expand=c(0, 0), breaks=0, labels=expression(mu[Y])) +
  stat_function(fun=f, n=1000) +
  geom_abline(slope=f1(0), intercept=0, linetype="dashed") +
  annotation_custom(ggplotGrob(p1), ymin=-5, ymax=-3.3, xmin=-2.5, xmax=2.5) +
  annotation_custom(ggplotGrob(p2), xmin=-5, xmax=-3.8, ymin=-2, ymax=2) +
  annotate("segment", x=0, xend=0, y=-5, yend=0, alpha=0.6) +
  annotate("segment", y=0, yend=0, x=-5, xend=0, alpha=0.6) +
  annotate("segment", x=0.5, xend=0.5, y=-5, yend=0.5*f1(0), linetype="dashed", alpha=0.6) +
  annotate("segment", x=-0.5, xend=-0.5, y=-5, yend=-0.5*f1(0), linetype="dashed", alpha=0.6) +
  annotate("segment", y=0.5*f1(0), yend=0.5*f1(0), x=-5, xend=0.5, linetype="dashed", alpha=0.6) +
  annotate("segment", y=-0.5*f1(0), yend=-0.5*f1(0), x=-5, xend=-0.5, linetype="dashed", alpha=0.6) +
  annotate("text", label="f(X[n])", parse=TRUE, x=4, y=1.6) +
  labs(y="Y", x=expression(X[n]))

## -----------------------------------------------------------------------------
library(errors)

x <- 1:5 + rnorm(5, sd = 0.01)
y <- 1:5 + rnorm(5, sd = 0.02)
errors(x) <- 0.01
errors(y) <- 0.02
x; y
(z <- x / y)

## -----------------------------------------------------------------------------
str(x)

## -----------------------------------------------------------------------------
correl(x, x) # one, cannot be changed
correl(x, y) # NULL, not defined yet
correl(x, y) <- runif(length(x), -1, 1)
correl(x, y)
covar(x, y)

## -----------------------------------------------------------------------------
z # previous computation without correlations
(z_correl <- x / y)

## -----------------------------------------------------------------------------
# the elementary charge
e <- set_errors(1.6021766208e-19, 0.0000000098e-19)
print(e, digits = 2)

## -----------------------------------------------------------------------------
options(errors.notation = "plus-minus")
print(e, digits = 2)
options(errors.notation = "parenthesis")

## -----------------------------------------------------------------------------
iris.e <- iris
iris.e[1:4] <- lapply(iris.e[1:4], function(x) set_errors(x, x*0.02))

head(iris.e)

## ----eval=FALSE---------------------------------------------------------------
#  plot(Sepal.Width ~ Sepal.Length, iris.e, col=Species)
#  legend(6.2, 4.4, unique(iris.e[["Species"]]),
#         col=1:length(iris.e[["Species"]]), pch=1)
#  
#  library(ggplot2)
#  
#  ggplot(iris.e) + aes(Sepal.Length, Sepal.Width, color=Species) +
#    geom_point() + geom_errors() + theme_bw() + theme(legend.position=c(0.6, 0.8))

## ----plot, echo=FALSE, out.width='100%', fig.height=3.5, fig.width=10, fig.cap=paste0(label("fig:plot"), "Base plot with error bars (left) and ggplot2's version (right).")----
par(mfrow=c(1, 2), mar=c(2.4, 2, 0.4, 1))

plot(Sepal.Width ~ Sepal.Length, iris.e, col=Species)
legend(6.2, 4.4, unique(iris.e[["Species"]]),
       col=1:length(iris.e[["Species"]]), pch=1)

library(ggplot2)

p <- ggplot(iris.e) + aes(Sepal.Length, Sepal.Width, color=Species) +
  geom_point() + geom_errors() + theme_bw() + theme(legend.position=c(0.6, 0.8))

plot.new()
fig <- par("fig")
figurevp <- grid::viewport(
  x = unit(fig[1], "npc"), y = unit(fig[3], "npc"), just = c("left", "bottom"),
  width = unit(fig[2] - fig[1], "npc"), height = unit(fig[4] - fig[3], "npc"))
grid::pushViewport(figurevp)
vp1 <- grid::plotViewport(c(0, 0, 0, 0))
print(p, vp = vp1)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
fit <- lm(bk ~ I(tk - 20), data = GUM.H.3)

y1 <- set_errors(coef(fit)[1], sqrt(vcov(fit)[1, 1]))
y2 <- set_errors(coef(fit)[2], sqrt(vcov(fit)[2, 2]))
covar(y1, y2) <- vcov(fit)[1, 2]

print(b.30 <- y1 + y2 * set_errors(30 - 20), digits = 2, notation = "plus-minus")

## ----echo=FALSE---------------------------------------------------------------
df <- data.frame(
    c("Row subsetting", "Row ordering", "Column transformation", "Row aggregation", "Column joining", "(Un)Pivoting"),
    c("`[`, `[[`, `subset`", "`order` + `[`", "`transform`, `within`", "`tapply`, `by`, `aggregate`", "`merge`", "`reshape`"),
    c("Full", "Full", "Full", "with `simplify=FALSE`", "Full", "Full")
)

knitr::kable(
  df, booktabs=knitr::is_latex_output(), row.names=FALSE, linesep="", escape=FALSE,
  col.names = c("Operation", "R base function(s)", "Compatibility"),
  caption = paste(label("tab:compat"), "Compatibility of errors and R base data wrangling functions.")
)

## -----------------------------------------------------------------------------
unlist <- function(x) if (is.list(x)) do.call(c, x) else x
iris.e.agg <- aggregate(. ~ Species, data = iris.e, mean, simplify=FALSE)
as.data.frame(lapply(iris.e.agg, unlist), col.names=colnames(iris.e.agg))

## ----echo=FALSE, eval=knitr::is_html_output(), results='asis'-----------------
#  cat('# References')

