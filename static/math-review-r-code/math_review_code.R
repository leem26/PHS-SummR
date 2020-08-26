##########################################
# Load packages necessary for the script #

library(tidyverse)
library(plotly)

################################
# Slide 6: Create matrix 'dat' #

set.seed(6789)
n <- 10
dat <- data.frame(
  age = round(runif(n, 22, 45)), height = round(rnorm(n, 66, 4)), likes_dogs = rbinom(n, 1, .53)
)
dat$yob <- 2020 - dat$age
dat <- as.matrix(dat)

dat

###########################################
# Slides 7 & 8: Examine dimensions of dat #

# both dimensions
dim(dat)
# number of rows
nrow(dat)
# number of columns
ncol(dat)
# Extract everyone's age (column vector)
dat[,1]
# Find out if participant 2 likes dogs (scalar)
dat[2,3]


######################################
# Slide 9: Generate vectors and plot #

# Vectors
x1 = c(25, 67, 10.5)
x1
x2 = c(38, 66.5, 7)
x2

# To create plot (uses 'plotly)
dat3d <- tibble(
  y = c(25, 38, 24, 39),
  z = c(67, 66.5, 60, 70),
  x = c(10.5, 7, 6, 11),
  color = c("red", "blue", "clear", "clear"),
  size = c(1, 1, 0, 0)
)
plot_ly(dat3d, x = ~x, z = ~z, y = ~y, color = ~color, colors = c("red", "white", "blue"), size = c(4, 4, 0, 0), type = "scatter3d", mode = "markers", width = 500, height = 500) %>%
  layout(
    scene = list(
      yaxis = list(title = "age"),
      xaxis = list(title = "shoe size"),
      zaxis = list(title = "height"),
      camera = list(
        up = list(x = 0, y = 0, z = 1),
        eye = list(x = 2.5, y = 0.1, z = 0.1),
        center = list(x = 0, y = 0, z = 0)
      )
    ),
    showlegend = FALSE
  )


#################################
# Slide 10: Transpose x1 and x2 #

t(x1)
t(x2)

X = rbind(t(x1), t(x2))
X

##################################
# Slide 13: Transpose matrix dat #

t(dat)

dat_t <- t(dat)
dat[3,4] # participant 3's year of birth
dat_t[4,3]

##################################################
# Slides 19 & 20: Matrix x Matrix multiplication #

X <- matrix(c(6,3,1,6,2,3), ncol=3)
X

tX = t(X)
tX

Z = tX%*%X
Z

tX[1,]%*%X[,1]

# NOTE!!! Order of matrices in the multiplication matters!
W = X%*%tX
W

#########################################################
# Slide 24: Inverse of a matrix and linear independence #

mat_a <- matrix(c(2, 6, 1, 8), ncol = 2)
mat_a

solve(mat_a)

mat_a_inv <- solve(mat_a)
mat_a_inv %*% mat_a

#########################################################
# Slide 25: Inverse of a matrix and linear independence #
mat_b <- matrix(c(2, 6, 1, 3), ncol = 2)
mat_b

solve(mat_b)

#########################################################
# Slide 26: Inverse of a matrix and linear independence #

set.seed(6789)
newdat <- tibble(
  age = c(30, 31, 25, 35, 42, 27),
  shoesize = round(runif(6, 6, 12)),
  height = round(age + shoesize * 3),
  yob =  age * 12
)

fit_age <- lm(height ~ age, data = newdat)
fit_age_shoe <- lm(height ~ age + shoesize, data = newdat)
fit_age_year <- lm(height ~ age + yob, data = newdat)

newdat <- newdat %>%
  mutate(
    p_age = fitted(fit_age),
    p_age_shoe = fitted(fit_age_shoe),
    p_age_year = fitted(fit_age_year)
  )

# predict over sensible grid of values
ages <- unique(newdat$age)
shoes <- unique(newdat$shoesize)
d <- with(newdat, expand.grid(age = ages, shoesize = shoes))
vals_shoes <- predict(fit_age_shoe, newdata = d)

# form matrix and give to plotly
m_shoes <- matrix(vals_shoes, nrow = length(unique(d$age)), ncol = length(unique(d$shoesize)))

plot_ly(newdat, x = ~age, y = ~p_age, type = "scatter", mode = "markers", width = 220, height = 220) %>%
  add_lines(x = ~age, y = ~p_age, color = I("lightgrey")) %>%
  layout(
    xaxis = list(title = "age in years"),
    yaxis = list(title = "predicted height"),
    showlegend = FALSE,
    autosize = FALSE
  )



#########################################################
# Slide 27: Inverse of a matrix and linear independence #
plot_ly(newdat, x = ~shoesize, z = ~p_age_shoe, y = ~age, type = "scatter3d", mode = "markers", width = 400, height = 400) %>%
  add_surface(x = ~shoes, y = ~ages, z = ~m_shoes, colorscale = list(c(0, 1), c("lightgrey", "lightgrey"))) %>%
  layout(
    scene = list(
      yaxis = list(title = "age in years"),
      xaxis = list(title = "shoe size"),
      zaxis = list(title = "predicted height"),
      camera = list(
        up = list(x = 0, y = 0, z = 1),
        eye = list(x = 2.5, y = 0.01, z = 0.2),
        center = list(x = 0, y = 0, z = 0.2)
      )
    ),
    showlegend = FALSE
  ) %>% hide_colorbar()


#########################################################
# Slide 28: Inverse of a matrix and linear independence #

plot_ly(newdat, x = ~yob, z = ~p_age_year, y = ~age, type = "scatter3d", mode = "markers", width = 400, height = 400) %>%
  add_lines(x = ~yob, z = ~p_age_year, y = ~age, color = I("lightgrey")) %>%
  layout(
    scene = list(
      yaxis = list(title = "age in years"),
      xaxis = list(title = "age in months"),
      zaxis = list(title = "height"),
      camera = list(
        up = list(x = 0, y = 0, z = 1),
        eye = list(x = 2.5, y = 0.01, z = 0.2),
        center = list(x = 0, y = 0, z = 0.2)
      )
    ),
    showlegend = FALSE
  )


########################
# Slide 32: Logarithms #

curve(log(x), xlim = c(0, 4), ylab = "log(x)", xlab = "x", main = "log(x)")
abline(h = 0, lty = "dashed")
points(1, 0, col = "red")

############################
# Slide 33: Exponentiation #

curve(exp(x), xlim = c(-4, 4), ylab = "exp(x)", xlab = "x", main = "exp(x)")
abline(h = 0, lty = "dashed")
points(0, 1, col = "red")

############################
# Slide 37: Logit function #

curve(log(x/(1-x)), xlim = c(0, 1), ylab = "logit(x)", xlab = "x", main = "logit(x)")
abline(h = 0, lty = "dashed")
points(0.5, 0, col = "red")

###############################
# Slides 41 & 42: Derivatives #

curve(dexp(x, rate = 3), xlim = c(0, 2), ylab = "g(x)")
curve(dchisq(x, 4), xlim = c(1, 4), ylab = "g(x)")

#######################
# Slide 43: Integrals #

par(mar = c(2.1, 4.1, 0.1, 2.1))
func <- function(x) 2*x^3 + 3*x^2 + 4
curve(func, xlim = c(-2, 3), ylab = "f(x)")
coord_x <- c(-2, seq(-2, 2, length.out = 1000), 2)
coord_y <- c(0, func(seq(-2, 2, length.out = 1000)), 0)
polygon(coord_x, coord_y, col = "skyblue")


#######################
# Slide 44: Integrals #

par(mar = c(2.1, 4.1, 0.1, 2.1))
func <- function(x) 2*x^3 + 3*x^2 + 4
curve(func, xlim = c(-2, 3), ylab = "f(x)")
coord_x <- c(1, seq(1, 3, length.out = 1000), 3)
coord_y <- c(0, func(seq(1, 3, length.out = 1000)), 0)
polygon(coord_x, coord_y, col = "purple")

##########################################
# Slides 45 & 46: Integrals data example #

set.seed(6789)
n <- 1000
dat <- data.frame(
  age = round(runif(n, 22, 45)), height = round(rnorm(n, 66, 4)), likes_dogs = rbinom(n, 1, .53)
)
dat$yob <- 2020 - dat$age

summary(dat)

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)
library(png)
library(downloader)
library(grDevices)

ggplot(dat, aes(x=height)) + 
  geom_histogram(aes(y = stat(density)), binwidth = 1, fill="white", color="red") +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(dat$height), sd = sd(dat$height)), 
    lwd = 0.5, 
    col = 'red'
  ) +
  ggtitle("PDF") -> p1

ggplot(dat, aes(x=height)) + 
  stat_ecdf(geom = "step", color="red") +
  ggtitle("CDF") -> p2

grid.arrange(p1, p2, ncol = 2)
