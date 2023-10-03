#' Linear Regression Model
#'
#' A reference class for multiple linear regression models.
#'
#' @format An object of class \code{"linreg"}.
#' @field formula A formula object specifying the regression model.
#' @field data A data frame containing the data.
#' @field X The design matrix of independent variables.
#' @field y The dependent variable.
#' @field lm_model The fitted linear regression model.
#'
#' @section Methods:
#' \describe{
#'   \method{print}{linreg}: Print coefficients and coefficient names.
#'   \method{plot}{linreg}: Plot residuals vs. fitted values and standardized residuals vs. fitted values.
#'   \method{resid}{linreg}: Return the vector of residuals.
#'   \method{pred}{linreg}: Return the predicted values.
#'   \method{coef}{linreg}: Return the coefficients as a named vector.
#'   \method{summary}{linreg}: Return a summary of the linear regression model.
#' }
#'
#' @examples
#' # Create a linreg object
#' lin_reg_model <- linreg(formula = Petal.Length ~ Species, data = iris)
#' # Print coefficients
#' print(lin_reg_model)
#' # Plot residuals
#' plot(lin_reg_model)
#' # Get residuals
#' residuals <- resid(lin_reg_model)
#' # Get predicted values
#' predictions <- pred(lin_reg_model)
#' # Get coefficients
#' coefficients <- coef(lin_reg_model)
#' # Summary of the model
#' summary(lin_reg_model)
#' @export


# Define the Reference Class
linreg <- setRefClass(
  "linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    X = "matrix",
    y = "ANY",
    lm_model = "lm"
  ),
  methods = list(
    initialize = function(formula, data) {
      # Store the formula and data in the object
      formula <<- as.formula(formula)
      data <<- data
      
      # Create the design matrix (X) from the formula and data
      X <<- model.matrix(formula, data = data)
      
      # Extract the dependent variable (y) from the formula using all.vars
      y <<- all.vars(formula[[1]])
      
      # Fit the linear regression model
      lm_model <<- lm(formula, data = data)
      
      # Assign the values to the object's fields
      .self$formula <- formula
      .self$data <- data
      .self$X <- X
      .self$y <- data[[y]]
      .self$lm_model <- lm_model
    },
    
    print = function() {
      cat("Coefficients:\n")
      print(coef(.self$lm_model))
    },
    
    plot = function() {
      
      plot_data <- data.frame(Fitted = .self$lm_mode$fitted.values, Residuals = residuals(.self))
      
      # Residuals vs. Fitted Values
      p1 <- ggplot(data = plot_data,
                   aes(x = Fitted, y = Residuals)) +
        geom_point() +
        geom_smooth(method = "loess", se = FALSE) +
        labs(title = "Residuals vs. Fitted Values",
             x = "Fitted Values",
             y = "Residuals")
      
      # Standardized Residuals vs. Fitted Values
      p2 <- ggplot(data = plot_data,
                   aes(x = Fitted, y = Standardized_Residuals)) +
        geom_point() +
        geom_smooth(method = "loess", se = FALSE) +
        labs(title = "Standardized Residuals vs. Fitted Values",
             x = "Fitted Values",
             y = "Standardized Residuals")
      
      # Return a list of plots
      return(list(Residuals_vs_Fitted = p1, Standardized_Residuals_vs_Fitted = p2))
    },
    
    resid = function() {
      return(residuals(.self$lm_model))
    },
    
    pred = function() {
      return(.self$lm_model$fitted.values)
    },
    
    coef = function() {
      return(coef(.self$lm_model))
    },
    
    summary = function() {
      cat("Call:\n")
      print(.self$formula)
      cat("\n")
      
      cat("Residuals:\n")
      summary_res <- summary(.self$lm_model)$coefficients[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")]
      colnames(summary_res) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      print(summary_res)
      
      cat("\n")
      cat("Residual standard error:", summary(.self$lm_model)$sigma, "on", summary(.self$lm_model)$df[2], "degrees of freedom\n")
      cat("Multiple R-squared:", summary(.self$lm_model)$r.squared, "\n")
      cat("Adjusted R-squared:", summary(.self$lm_model)$adj.r.squared, "\n")
    }
  )
)
  

# Example usage:
# Create a linreg object
lin_reg_model <- linreg(formula = Petal.Length ~ Species, data = iris)

str(lin_reg_model)
