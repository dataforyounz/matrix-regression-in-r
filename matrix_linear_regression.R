
library( tidyverse )

##
## Routine to compute regression coefficients using matrix operations
##
## Use the 'cars' data set. Simple dataset that contains two variables
## The data give the speed of cars and the distances taken to stop. 
## These data were recorded in the 1920s.

n_obs <- nrow( cars )

## Visualize the data -----------------------------------------------------------
# Overlay the regression line which we will estimate

p1 <- cars %>%
      ggplot( aes(x = speed, y = dist) ) +
      geom_point(shape = "circle", size = 2L, colour = "#BD5E0D") +
      geom_smooth( method = "lm", se = F, col = "dodgerblue4") +
      labs( x = "Speed (mph)", 
            y = "Distance (feet)", 
            title = "Stopping distance as a function of speed"
           ) +
      theme_bw() + 
      theme( axis.text = element_text(size = 20),
             axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),
             axis.title = element_text( size = 14)
           )

ggsave( p1, file = "img/cars_plot@2x.png", units = "cm", width = 12, height = 7 )

## Create matrices and vectors --------------------------------------------------

# 1. Assign dependent variable to the response vector
y_vec <- cars$dist

# 2. Create a design matrix with the single independent variable
x_mat <- with( cars, model.matrix( dist ~ speed ) )

# 3. Pre-compute matrices for the normal equation 

# Step 3.1
A  <- t( x_mat ) %*% x_mat

# Step 3.2
A_inv <- solve( A )

# Step 3.3
B <- t( x_mat ) %*% y_vec

# 4. Estimate the parameter values using the normal equation
b_vec <- A_inv %*% B

# Also show the alternative method
# The slope parameter
b_1 <- with(cars, cov(speed, dist) / var(speed) )

# The intercept parameter
b_0 <- with(cars, mean(dist) - mean(speed) * b_1 )

c("intercept" = b_0, "slope" = b_1)

# 5. Compute the fitted values
y_hat <- x_mat %*% b_vec

# 6. Compute vector of residuals
e_vec <- y_hat - y_vec

## Visualize the fitted values 
# Update plot to include predicted values
p2 <- p1 +
  geom_point( aes(x = speed, y = y_hat, col = "Predicted") ) +
  labs( col = "" ) +
  theme( legend.position = "bottom") +
  scale_color_manual( values = "dodgerblue1" )

ggsave( p2, file = "img/cars_plot_fitted@2x.png", units = "cm", width = 12, height = 7 )


# 7. Compute the residual standard error 
sigma_sq <- sum( ( y_hat - y_vec ) ^ 2 ) / (n_obs - 2)

# 8. Compute the standard errors for the coefficients
sigma <- sqrt( sigma_sq ) 
b_se  <- sqrt( diag( sigma_sq * A_inv ) )

# 9. Compute t-values
t_vals <- b_vec / b_se

# 10. Compute p-values
p_vals <- round( pt( abs(t_vals), df = n_obs - 2, lower.tail = F ) * 2, 4)

# Output -----------------------------------------------------------------------
out <- data.frame( cbind( b_vec, b_se, t_vals, p_vals) )
names( out ) <- c("coefs", "std_err", "t_value", "p_value")

print( out, digits = 4 )

