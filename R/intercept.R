#' Add intercept
#'
#' \code{step_intercept} adds an intercept if there isn't already one.
#'   
#' @param recipe A recipe object. The step will be added to the sequence of 
#'   operations for this recipe.
#' @param ... Name of the intercept column. By default it is \code{Intercept}.
#' @param role By default predictor.
#' @param trained A logical to indicate if the quantities for preprocessing 
#'   have been estimated.
#' @return An updated version of \code{recipe} with the
#'   new step added to the sequence of existing steps (if any). 
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @examples
#' rec <- recipe(Sepal.Width ~ Sepal.Length + Petal.Length, data = iris)
#'
#' intercept_trans <- rec  %>%
#'   step_intercept()
#'
#' intercept_obj <- prepare(intercept_trans, retain = TRUE)
#' 
#' head(juice(intercept_obj))

#' @export
step_intercept <- function(recipe, ..., role = "predictor", trained = FALSE) {

  dots   <- quos(...)
  n_dots <- dots_n(...)

  if(n_dots == 0) {
    term <- "Intercept"
  } else {
    term <- quo_name(dots[[1]])
  }

  add_step(
    recipe,
    step_intercept_new(
      terms   = term,
      role    = role,
      trained = trained)
  )
}

step_intercept_new <-
  function(terms = NULL, role = "predictor", trained = FALSE) {
    step(
      subclass = "intercept",
      terms    = terms,
      role     = role,
      trained  = trained

    )
  }


#' @export
prepare.step_intercept <- function(x, training, info = NULL, ...) {

  step_intercept_new(
    terms   = x$terms,
    role    = x$role,
    trained = TRUE
  )

}

#' @export
bake.step_intercept <- function(object, newdata, ...) {

  if(has_intercept(newdata)) {
    newdata
  } else {
    einser    <- rep(1, dim(newdata)[1])
    interc    <- tibble(!!object$terms := einser)
    newdata   <- bind_cols(interc, newdata)
    newdata
  }

}


print.step_intercept <- function(x, width = max(20, options()$width - 29), ...) {
  cat("Add Intercept named ", sep = "")
  if (x$trained) {
    cat(x$terms)
  } else
    cat(x$terms)
  if (x$trained)
    cat(" [trained]\n")
  else
    cat("\n")
  invisible(x)
}


##################################

# Helpers


all_equal_one <- function(x) {

  purrr::every(x, function(x) {x == 1})

}


all_equal_false <- function(x) {

  purrr::every(x, function(x) {x == FALSE})

}


has_intercept <- function(df) {

  tmp01 <- summarise_all(df, all_equal_one) %>% t() %>% `[`(,1) %>% unname()
  tmp02 <- all_equal_false(tmp01)
  tmp02 == FALSE

}
