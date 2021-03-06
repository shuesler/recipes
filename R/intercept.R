#' Add intercept (or constant) column
#'
#' \code{step_intercept} creates a \emph{specification} of a recipe step that
#'   will add an intercept or constant term in the first column of a data
#'   matrix. \code{step_intercept} has defaults to \emph{predictor} role so
#'   that it is by default called in the bake step. Be careful to avoid
#'   unintentional transformations when calling steps with
#'   \code{all_predictors}.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... Argument ignored; included for consistency with other step
#'   specification functions.
#' @param role Defaults to "predictor"
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated. Again included for consistency.
#' @param name Character name for new added column
#' @param value A numeric constant to fill the intercept column. Defaults to 1.
#'
#' @return An updated version of \code{recipe} with the
#'   new step added to the sequence of existing steps (if any).
#' @export
#'
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#' rec_trans <- recipe(HHV ~ ., data = biomass_tr[, -(1:2)]) %>%
#'   step_intercept(value = 2)
#'
#' rec_obj <- prepare(rec_trans, training = biomass_tr)
#'
#' with_intercept <- bake(rec_obj, biomass_te)
#' with_intercept
#'
#' @seealso \code{\link{recipe}} \code{\link{prepare.recipe}}
#'   \code{\link{bake.recipe}}
step_intercept <- function(recipe, ..., role = "predictor",
                           trained = FALSE, name = "intercept", value = 1) {
  if (length(list(...)) > 0)
    warning("Selectors are not used for this step.", call. = FALSE)
  if (!is.numeric(value))
    stop("Intercept value must be numeric.", call. = FALSE)
  if (!is.character(name) | length(name) != 1)
    stop("Intercept/constant column name must be a character value.", call. = FALSE)
  add_step(
    recipe,
    step_intercept_new(
      role = role,
      trained = trained,
      name = name,
      value = value))
}

step_intercept_new <- function(role = "predictor", trained = FALSE,
                               name = "intercept", value = 1) {
  step(
    subclass = "intercept",
    role = role,
    trained = trained,
    name = name,
    value = value
  )
}

prepare.step_intercept <- function(x, training, info = NULL, ...) {
  x$trained <- TRUE
  x
}

#' @importFrom tibble add_column
bake.step_intercept <- function(object, newdata, ...) {
  tibble::add_column(newdata, !!object$name := object$value, .before = TRUE)
}
