Formula <- function(object) {
  stopifnot(inherits(object, "formula"))
  if(!inherits(object, "Formula")) class(object) <- c("Formula", "formula")
  object
}

as.Formula <- function(x, ...) UseMethod("as.Formula")

as.Formula.default <- function(x, ...) {
  if(!inherits(x, "formula")) x <- as.formula(x)
  Formula(x)
}

as.Formula.formula <- function(x, ...) {

  z <- list(...)
  if(length(z) < 1) return(Formula(x)) else z <- z[[1]]
  #Z# instead of just using the first argument in `...',
  #Z# use all (vectorized or recursively)

  if (length(x) == 3) {
    y <- x[[2]]
    rhs <- x[[3]]
  } else{
    y <- NULL
    rhs <- x[[2]]
  }
  zz <- if (length(z) == 3) z[[3]] else z[[2]]
  #Z# should we throw a warning if there is a left-hand side?
  
  #Z# avoid deparsing and parsing again
  rval <- paste(if(!is.null(y)) paste(deparse(y), collapse = ""),
    " ~ ", paste(deparse(rhs), collapse = ""),
    "|", paste(deparse(zz), collapse = ""))

  as.Formula(rval)
}

is.Formula <- function(object)
  inherits(object, "Formula")

formula.Formula <- function(x,
  part = c("first", "second", "both"), response = NULL, ...)
{
  xs <- structure(x, class = "formula")
  part <- match.arg(part)
  has_response <- attr(terms(xs), "response") == 1L
  # terms.Formula calls formula.Formula, so we use a copy of class "formula"
  if (has_response){
    y <- x[[2]]
    rhs <- x[[3]]
    if (is.null(response)) response <- TRUE
  }
  else{
    if (is.null(response)) response <- FALSE
    if (response) stop("one sided formula")
    y <- NULL
    rhs <- x[[2]]
  }
  if (length(x) == 1){
    firstpart <- rhs
    secondpart <- NULL
    if (part %in% c("second", "both")) stop("one part formula")
  }
  if (length(x) == 2) {
    firstpart <- rhs[[2]]
    secondpart <- rhs[[3]]
    rhs[[1]] <- as.name("+")
  }
  switch(part,
    "first" = {
      if(response) do.call("~", list(y, firstpart))
        else do.call("~", list(firstpart))
     },
    "second" = {
      if(response) do.call("~", list(y, secondpart))
        else do.call("~", list(secondpart))
     },
    "both" = {
      if(response) do.call("~", list(y, rhs))
        else do.call("~", list(rhs))
     })
}

terms.Formula <- function(x, ..., part = "first", response = NULL) {
  if(is.null(response))
    response <- attr(terms(structure(x, class = "formula")), "response") == 1L
  form <- formula(x, part = part, response = response)
  terms(form, ...)
}

model.frame.Formula <- function(formula, ..., part = NULL, response = NULL) {
  if (is.null(response)) response <- attr(terms(formula), "response") == 1L
  if (is.null(part)) part <- ifelse(length(formula) == 2L, "both", "first")
  form <- formula(formula, part = part, response = response)
  model.frame(form, ...)
}

model.matrix.Formula <- function(object, ..., part = "first") {
  form <- formula(object, part = part)
  model.matrix(form, ...)
  #Z# Should we call model.frame(form, ...) first to avoid missingness problems?
}
  

update.Formula <- function(object, new,...) {
  old <- object
  if (!is.Formula(old)) old <- Formula(old)
  if (!is.Formula(new)) new <- Formula(new)

  old.first <- formula(old, part = "first", response = FALSE)
  old.second <- formula(old, part = "second", response = FALSE)

  new.first <- formula(new, part = "first", response = FALSE)
  new.second <- formula(new, part = "second", response = FALSE)

  new.first <- update(old.first, new.first)
  new.second <- update(old.second, new.second)
  
  #Z# avoid deparsing and parsing again
  result <- paste(paste(deparse(old[[2]]), collapse = ""),
    "~", paste(deparse(new.first[[2]]), collapse = ""),
    "|", paste(deparse(new.second[[2]]), collapse = ""))
  as.Formula(result)
}


length.Formula <- function(x) {
  class(x) <- "formula"
  if (length(x) == 2) {
    rhs <- x[[2]]
  } else {
    if (length(x) == 3) {
      rhs <- x[[3]]
    } else {
      stop("invalid formula")
    }
  }
  if (length(rhs) > 1 && rhs[[1]] == "|") {
    lform <- 2
  } else {
    lform <- 1
  }
  lform
}


has.intercept <- function(object, ...) {
  UseMethod("has.intercept")
}

has.intercept.formula <- function(object, ...){
  attr(terms(object), "intercept") == 1L
}

has.intercept.Formula <- function(object, part = "first", ...) {
  formula <- formula(object, part = part)
  attr(terms(formula), "intercept") == 1L
}
