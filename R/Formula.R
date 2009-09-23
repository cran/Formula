Formula <- function(object) {

  stopifnot(inherits(object, "formula"))

  object_split <- split_formula(object)

  structure(object, lhs = object_split$lhs, rhs = object_split$rhs,
    class = c("Formula", "formula"))
}

as.Formula <- function(x, ...) UseMethod("as.Formula")

as.Formula.default <- function(x, ...) {
  if(!inherits(x, "formula")) x <- as.formula(x)
  Formula(x)
}

as.Formula.Formula <- function(x, ...) {
  x
}

as.Formula.formula <- function(x, ...) {

  ## combine all arguments to formula list
  x <- c(list(x), list(...))
  x <- lapply(x, as.formula)  
  
  ## split all 
  x_split <- lapply(x, split_formula)
  x_lhs <- do.call("c", lapply(x_split, "[[", "lhs"))
  x_rhs <- do.call("c", lapply(x_split, "[[", "rhs"))

  ## recombine
  x_all <- paste_formula(x_lhs, x_rhs)
  
  ## create formula
  ## (we have everything to do this by hand, but for encapsulating code
  ## call Formula() again...which splits again)
  Formula(x_all)
}

is.Formula <- function(object)
  inherits(object, "Formula")

formula.Formula <- function(x, lhs = NULL, rhs = NULL, collapse = FALSE,
  update = FALSE, drop = TRUE, ...)
{
  ## available parts
  lpart <- 1:length(attr(x, "lhs"))
  rpart <- 1:length(attr(x, "rhs"))

  ## default: keep all parts
  lhs <- if(is.null(lhs)) lpart else lpart[lhs]
  rhs <- if(is.null(rhs)) rpart else rpart[rhs]
  if(any(is.na(lhs))) {
    lhs <- as.vector(na.omit(lhs))
    if(length(lhs) < 1) lhs <- 0
    warning("subscript out of bounds, not all 'lhs' available")
  }
  if(any(is.na(rhs))) {
    rhs <- as.vector(na.omit(rhs))
    if(length(rhs) < 1) rhs <- 0
    warning("subscript out of bounds, not all 'rhs' available")
  }  

  ## collapse: keep parts separated by "|" or collapse with "+"
  collapse <- rep(as.logical(collapse), length.out = 2)

  rval <- paste_formula(attr(x, "lhs")[lhs], attr(x, "rhs")[rhs],
    lsep = ifelse(collapse[1], "+", "|"),
    rsep = ifelse(collapse[2], "+", "|"))

  ## omit potentially redundant terms
  if(all(collapse) & update) rval <- update(rval, if(length(rval) > 2) . ~ . else ~ .)

  ## reconvert to Formula if desired
  if(!drop) rval <- Formula(rval)

  return(rval)
}

terms.Formula <- function(x, ..., lhs = NULL, rhs = NULL) {

  ## simplify a Formula to a formula that can be processed with
  ## terms/model.frame etc.
  simplify_to_formula <- function(Formula, lhs = NULL, rhs = NULL) {

    ## get desired subset as formula and Formula
    form <- formula(Formula, lhs = lhs, rhs = rhs)
    Form <- Formula(form)

    ## convenience functions for checking extended features
    is_lhs_extended <- function(Formula) {
      ## check for multiple parts
      if(length(attr(Formula, "lhs")) > 1) {
        return(TRUE)
      } else {
      ## and multiple responses
        if(length(attr(Formula, "lhs")) < 1) return(FALSE)
        return(length(attr(terms(paste_formula(NULL,
	  attr(Formula, "lhs"), rsep = "+")), "term.labels")) > 1)
      }
    }

    is_rhs_extended <- function(Formula) {
      ## check for muliple parts
      length(attr(Formula, "rhs")) > 1
    }

    ## simplify (if necessary)
    ext_lhs <- is_lhs_extended(Form)
    if(ext_lhs | is_rhs_extended(Form)) {
      form <- if(ext_lhs) {
        if(length(attr(Form, "rhs")) == 1 & identical(attr(Form, "rhs")[[1]], 0)) {
          paste_formula(NULL, attr(Form, "lhs"), rsep = "+")    
        } else {
	  paste_formula(NULL, c(attr(Form, "lhs"), attr(Form, "rhs")), rsep = "+")
	}
      } else {
        paste_formula(attr(Form, "lhs"), attr(Form, "rhs"), rsep = "+")    
      }
    }
  
    return(form)
  }

  ## simplify and then call traditional terms()
  form <- simplify_to_formula(x, lhs = lhs, rhs = rhs)
  terms(form, ...)
}

model.frame.Formula <- function(formula, data = NULL, ...,
  lhs = NULL, rhs = NULL)
{
  model.frame(terms(formula, lhs = lhs, rhs = rhs, data = data), data = data, ...)
}

model.matrix.Formula <- function(object, data = environment(object), ...,
  lhs = NULL, rhs = 1)
{
  form <- formula(object, lhs = lhs, rhs = rhs, collapse = c(FALSE, TRUE))
  mt <- delete.response(terms(form, data = data))
  model.matrix(mt, data = data, ...)
}

## as model.response() is not generic, we do this:
model.part <- function(object, ...)
  UseMethod("model.part")

model.part.formula <- function(formula, data, ..., drop = FALSE) {
  formula <- Formula(formula)
  NextMethod()
}

model.part.Formula <- function(object, data, lhs = 0, rhs = 0, drop = FALSE, ...) {

  ## *hs = NULL: keep all parts
  if(is.null(lhs)) lhs <- 1:length(attr(object, "lhs"))
  if(is.null(rhs)) rhs <- 1:length(attr(object, "rhs"))

  if(isTRUE(all.equal(as.numeric(lhs), rep(0, length(lhs)))) &
     isTRUE(all.equal(as.numeric(rhs), rep(0, length(rhs)))))
    stop("Either some 'lhs' or 'rhs' has to be selected.")

  ## construct auxiliary terms object
  mt <- terms(object, lhs = lhs, rhs = rhs, data = data)

  ## subset model frame
  ix <- attr(mt, "variables")[-1]
  if(is.null(ix)) {
    ix <- 0
  } else {
    ix <- sapply(ix, deparse)
    if(!all(ix %in% names(data))) stop(
      paste("'data' does not seem to be an appropriate 'model.frame':",
      paste(paste("'", ix[!(ix %in% names(data))], "'", sep = ""), collapse = ", "),
      "not found")
    )
  }
  rval <- data[, ix, drop = drop]
  if(!is.data.frame(rval)) names(rval) <- rownames(data)
  return(rval)
}

update.Formula <- function(object, new,...) {

  new <- Formula(new)
  
  ## extract all building blocks
  o_lhs <- attr(object, "lhs")
  o_rhs <- attr(object, "rhs")
  n_lhs <- attr(new, "lhs")
  n_rhs <- attr(new, "rhs")
  lhs <- rep(list(NULL), length = max(length(o_lhs), length(n_lhs)))
  rhs <- rep(list(NULL), length = max(length(o_rhs), length(n_rhs)))

  ## convenience function for updating components
  update_components <- function(x, y) {
    xf <- yf <- ~ .
    xf[[2]] <- x
    yf[[2]] <- y
    update(xf, yf)[[2]]
  }
  
  for(i in 1:length(lhs)) {
    lhs[[i]] <- if(length(o_lhs) < i) n_lhs[[i]]
      else if(length(n_lhs) < i) o_lhs[[i]]
      else update_components(o_lhs[[i]], n_lhs[[i]])
  }

  for(i in 1:length(rhs)) {
    rhs[[i]] <- if(length(o_rhs) < i) n_rhs[[i]]
      else if(length(n_rhs) < i) o_rhs[[i]]
      else update_components(o_rhs[[i]], n_rhs[[i]])
  }

  ## recombine
  rval <- paste_formula(lhs, rhs)
  
  ## create formula
  ## (we have everything to do this by hand, but for encapsulating code
  ## call Formula() again...which splits again)
  Formula(rval)  
}

length.Formula <- function(x) {
  ## NOTE: return length of both sides, not only rhs
  c(length(attr(x, "lhs")), length(attr(x, "rhs")))
}

print.Formula <- function(x, ...) {
  ## we could avoid calling formula() by computing on the internal
  ## structure attr(x, "rhs") <- attr(x, "lhs") <- NULL
  ## but this is probably cleaner...
  print(formula(x))
  invisible(x)
}



## convenience tools #################################################

## split formulas
split_formula <- function(f) {

  stopifnot(inherits(f, "formula"))

  rhs <- if(length(f) > 2) f[[3]] else f[[2]]
  lhs <- if(length(f) > 2) f[[2]] else NULL

  extract_parts <- function(x, sep = "|") {
    if(is.null(x)) return(NULL)
    
    rval <- list()
    if(length(x) > 1 && x[[1]] == sep) {
      while(length(x) > 1 && x[[1]] == sep) {
        rval <- c(x[[3]], rval)
        x <- x[[2]]
      }
    }
    return(c(x, rval))
  }

  list(lhs = extract_parts(lhs), rhs = extract_parts(rhs))
}

## reassemble formulas
paste_formula <- function(lhs, rhs, lsep = "|", rsep = "|") {

  ## combine (parts of) formulas
  c_formula <- function(f1, f2, sep = "~") {

    stopifnot(length(sep) == 1, nchar(sep) == 1,
      sep %in% c("~", "+", "|", "&"))

    if(sep == "~") {
      rval <- . ~ .
      rval[[3]] <- f2    
      rval[[2]] <- f1
    } else {
      rval <- as.formula(paste(". ~ .", sep, "."))
      rval[[3]][[3]] <- f2
      rval[[3]][[2]] <- f1
      rval <- rval[[3]]
    }
  
    return(rval)
  }

  stopifnot(all(nchar(lsep) == 1), all(lsep %in% c("+", "|", "&")))
  stopifnot(all(nchar(rsep) == 1), all(rsep %in% c("+", "|", "&")))
  
  if(length(lhs) > 1) lsep <- rep(lsep, length.out = length(lhs) - 1)
  if(length(rhs) > 1) rsep <- rep(rsep, length.out = length(rhs) - 1)

  if(is.null(lhs)) lhs <- list()
  if(is.null(rhs)) rhs <- list()
  
  if(!is.list(lhs)) lhs <- list(lhs)
  if(!is.list(rhs)) rhs <- list(rhs)

  lval <- if(length(lhs) > 0) lhs[[1]] else NULL
  if(length(lhs) > 1) {
    for(i in 2:length(lhs)) lval <- c_formula(lval, lhs[[i]], sep = lsep[[i-1]])
  }
  rval <- if(length(rhs) > 0) rhs[[1]] else 0 ## FIXME: Is there something better?
  if(length(rhs) > 1) {
    for(i in 2:length(rhs)) rval <- c_formula(rval, rhs[[i]], sep = rsep[[i-1]])
  }

  c_formula(lval, rval, sep = "~")
}

