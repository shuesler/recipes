#' Logarithmic Transformation
#'
#' \code{step_log} creates a \emph{specification} of a recipe step that will
#'   log transform data.
#'
#' @inheritParams step_center
#' @inherit step_center return
#' @param role Not used by this step since no new variables are created.
#' @param base A numeric value for the base.
#' @param vars A character string of variable names that will be (eventually)
#'   populated by the \code{terms} argument.
#' @keywords datagen
#' @concept preprocessing transformation_methods
#' @export
#' @examples
#' set.seed(313)
#' examples <- matrix(exp(rnorm(40)), ncol = 2)
#' examples <- as.data.frame(examples)
#'
#' rec <- recipe(~ V1 + V2, data = examples)
#'
#' log_trans <- rec  %>%
#'   step_log(all_predictors())
#'
#' log_obj <- prepare(log_trans, training = examples)
#'
#' transformed_te <- bake(log_obj, examples)
#' plot(examples$V1, transformed_te$V1)
#' @seealso \code{\link{step_logit}} \code{\link{step_invlogit}}
#'   \code{\link{step_hyperbolic}}  \code{\link{step_sqrt}}
#'   \code{\link{recipe}} \code{\link{prepare.recipe}}
#'   \code{\link{bake.recipe}}

step_log <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           base = exp(1),
           vars = NULL) {
    add_step(
      recipe,
      step_log_new(
        terms = check_ellipses(...),
        role = role,
        trained = trained,
        base = base,
        vars = vars
      )
    )
  }

step_log_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           base = NULL,
           vars = NULL) {
    step(
      subclass = "log",
      terms = terms,
      role = role,
      trained = trained,
      base = base,
      vars = vars
    )
  }

#' @export
prepare.step_log <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  step_log_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    base = x$base,
    vars = col_names
  )
}

#' @export
bake.step_log <- function(object, newdata, ...) {
  col_names <- object$vars
  for (i in seq_along(col_names))
    newdata[, col_names[i]] <-
      log(getElement(newdata, col_names[i]), base = object$base)
  as_tibble(newdata)
}

print.step_log <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Log transformation on ", sep = "")
    if (x$trained) {
      cat(format_ch_vec(x$vars, width = width))
    } else
      cat(format_selectors(x$terms, wdth = width))
    if (x$trained)
      cat(" [trained]\n")
    else
      cat("\n")
    invisible(x)
  }
