\name{Formula}
\alias{Formula}
\alias{is.Formula}
\alias{formula.Formula}
\alias{terms.Formula}
\alias{update.Formula}
\alias{model.matrix.Formula}
\alias{model.frame.Formula}
\alias{length.Formula}
\alias{as.Formula}
\alias{as.Formula.default}
\alias{as.Formula.formula}
\title{Extended Formulas}

\description{
  The new class \code{Formula} extends the base class \code{\link[stats]{formula}}.
  Currently, the only extenseion are two-part formulas, further extensions are
  planned.
}

\usage{
Formula(object)

as.Formula(x, \dots)
\method{as.Formula}{default}(x, \dots)
is.Formula(object)

\method{formula}{Formula}(x, part = c("first", "second", "both"), response = NULL, \dots)
\method{terms}{Formula}(x, \dots, part = "first", response = NULL)
\method{update}{Formula}(object, new, \dots)
\method{model.matrix}{Formula}(object, \dots, part = "first")
\method{model.frame}{Formula}(formula, \dots, part = "first", response = NULL)
\method{length}{Formula}(x)
}

\arguments{
  \item{object, x, formula}{an object. For \code{Formula} it needs to be a
    \code{formula} object.}
  \item{part}{character specifying the part of the right hand side of the formula to use:
    \code{"first"}, \code{"second"} or \code{"both"}.}
  \item{response}{logical. Should the response be include in the formula?}
  \item{new}{a formula giving a template which specifies how to update.}
  \item{\dots}{further arguments.}
}

\value{
  \code{Formula} returns an object of class \code{Formula}
  which inherits from \code{formula}.
}

\details{  
  \code{Formula} objects extend the basic \code{formula} objects.
  Currently, the only extension implemented are two-part formulas
  of type: \code{y ~ x1 + x2 | z1 + z2 + z3}. However, \pkg{Formula}
  is under development and features on the wishlist for future
  versions include multi-part formulas
  (\code{y ~ x1 + x2 | u1 + u2 + u3 | v1 + v2}) and multiple response
  formulas (\code{y1 + y2 ~ x1 + x2 + x3}). This might result in
  changes of the interface of some methods in future versions.
  
  Currently, a \code{Formula} is a formula for which the right hand
  side has two parts separated by a pipe sign. The \code{Formula}
  function coerce a formula to a \code{Formula} object just
  by modifying the class attribute.
  
  \code{is.Formula} check whether the argument of the function is
  \code{Formula} object.
  
  \code{as.Formula} is a generic for coercing to \code{Formula},
  the default method first coerces to \code{formula} and then calls
  \code{Formula}.
  
  Several methods are provided: They all use the \code{formula} method
  for creating a standard formula first. The returned formula is a
  one-sided formula if \code{response = FALSE} and a two-sided formula
  if \code{response = TRUE}. The right-hand side depends on the
  \code{part} argument: the first part (\code{"first"}), the second part
  \code{"second"} or both (\code{"both"}).
  
  Hence, the two arguments (\code{response} and \code{part}) are used in the
  \code{terms}, \code{model.frame} and \code{model.matrix} methods.
}

\examples{
data("airquality", package = "datasets")
f <- Ozone ~ Solar.R + Wind | Temp

# create a Formula object from the formula
f2 <- Formula(f)
class(f2)

# examples of updates of the two-parts formula
update(f2,. ~ . - Wind + Day | . + Month)
update(f2,. ~ . | . + Month)
update(f2,. ~ . - Wind + Day | .)

# get the first, second and both parts, with or without the response
formula(f2, part = "first", response = TRUE)
formula(f2, part = "second", response = FALSE)
formula(f2, part = "both")

# model.frame for the first part and for both parts
head(model.frame(f2, part = "first", data = airquality))
head(model.frame(f2, part = "both", data = airquality))

# model.matrix for the first and the second part
head(model.matrix(f2, data = airquality, part = "first"))
head(model.matrix(f2, data = airquality, part = "second"))

# note that the sample is not the same due to missing values
mf <- model.frame(f2, part = "both", data = airquality)
head(model.matrix(f2, data = mf, part = "first"))
head(model.matrix(f2, data = mf, part = "second"))
}

\keyword{classes}
