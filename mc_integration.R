library(plot3D)

set.seed(100)  # sets state for rng
n <- 10000
rand_x <- runif(n,min=0,max=1)  # generates n unif rn between 0 and 1 
rand_y <- runif(n,min=0,max=1)  # and again


# approximating polynomial integral
my_fun <- function(x,y){
  return(x^2 + 3*x*y - y^4)
}

function_vals <- my_fun(rand_x,rand_y)  # function vals
integral_approx <- mean(function_vals)  # integral approximation by calculating mean


# approximating trig integral
trig_fun  <- function(x,y){
  denominator <- 1 + sin(x)^2 + sin(y)^2
  return(1/denominator)
}

trig_vals <- trig_fun(rand_x,rand_y)  # function vals
trig_int_approx <- mean(trig_vals)  # integral approximation by calculating mean


# plotting a 3d surface
x_vals <- seq(0,1,0.001)  # vector of x vals
y_vals <- seq(0,1,0.001)  # vector of y vals
m = mesh(x_vals,y_vals)  # creating a x-y mesh
x <- m$x  # these lines...
y<- m$y  # use the mesh object m created above
z <- trig_fun(x,y)  # function vals

# full colored image
surf3D(x, y, z, colvar = z, colkey = TRUE, shade = 0.5,
       box = TRUE, theta = 60, bty="b2", contour=TRUE)
