\name{Formula}

\alias{Formula}
\alias{formula.Formula}
\alias{as.Formula}
\alias{as.Formula.default}
\alias{as.Formula.formula}
\alias{as.Formula.Formula}
\alias{is.Formula}
\alias{print.Formula}
\alias{update.Formula}
\alias{length.Formula}
\alias{all.equal.Formula}
\alias{str.Formula}

\title{Extended Formulas: Multiple Responses and Multiple Regressor Parts}

\description{
  The new class \code{Formula} extends the base class
  \code{\link[stats]{formula}} by allowing for multiple responses
  and multiple parts of regressors.
}

\usage{
Formula(object)

\method{formula}{Formula}(x, lhs = NULL, rhs = NULL,
  collapse = FALSE, update = FALSE, drop = TRUE, \dots)

as.Formula(x, \dots)
is.Formula(object)
}

\arguments{
  \item{object, x}{an object. For \code{Formula} it needs to be a
    \code{formula} object.}
  \item{lhs, rhs}{indexes specifying which elements of the left- and
    right-hand side, respectively, should be employed. \code{NULL}
    corresponds to all parts, \code{0} to none.}
  \item{collapse}{logical. Should multiple parts (if any) be collapsed
    to a single part (essentially by replacing the \code{|} operator
    by \code{+})? \code{collapse} can be a vector of length 2,
    corresponding for different handling of left- and right-hand side
    respectively.}
  \item{update}{logical. Only used if \code{all(collapse)}. Should the
    resulting formula be updated to remove possibly redundant terms
    occuring in multiple terms?}
  \item{drop}{logical. Should the \code{Formula} class be dropped?
    If \code{TRUE} (the default) a \code{formula} is returned, if
    \code{FALSE} the corresponding \code{Formula} is returned.}
  \item{\dots}{further arguments.}
}

\details{  
  \code{Formula} objects extend the basic \code{formula} objects.
  These extensions include multi-part formulas such as
  \code{y ~ x1 + x2 | u1 + u2 + u3 | v1 + v2}, multiple response
  formulas \code{y1 + y2 ~ x1 + x2 + x3}, multi-part responses
  such as \code{y1 | y2 + y3 ~ x}, and combinations of these.
  
  The \code{Formula} creates a \code{Formula} object from a \code{formula}
  which can have the \code{|} operator on the left- and/or right-hand
  side (LHS and/or RHS). Essentially, it stores the original \code{formula}
  along with attribute lists containing the decomposed parts for the LHS
  and RHS, respectively.

  The main motivation for providing the \code{Formula} class is to be
  able to conveniently compute model frames and model matrices or extract
  selected responses based on an extended formula language. This functionality
  is provided by methods to the generics \code{\link[stats]{model.frame}},
  and \code{\link[stats]{model.matrix}}. For details and examples, see
  their manual page: \code{\link{model.frame.Formula}}.

  In addition to these workhorses, a few further methods and functions are provided.
  By default, the \code{formula()} method switches back to the original
  \code{formula}. Additionally, it allows selection of subsets of the
  LHS and/or RHS (via \code{lhs}, and \code{rhs}) and collapsing
  multiple parts on the LHS and/or RHS into a single part (via \code{collapse}).  
  
  \code{is.Formula} checks whether the argument inherits from the
  \code{Formula} class.
  
  \code{as.Formula} is a generic for coercing to \code{Formula}, the
  default method first coerces to \code{formula} and then calls
  \code{Formula}. The default and \code{formula} method also take an
  optional \code{env} argument, specifying the environment of the resulting
  \code{Formula}. In the latter case, this defaults to the environment
  of the \code{formula} supplied.
  
  Methods to further standard generics \code{\link[base]{print}},
  \code{\link[stats]{update}}, and \code{\link[base]{length}} are provided
  for \code{Formula} objects. The latter reports the number of parts on
  the LHS and RHS, respectively.
}

\value{
  \code{Formula} returns an object of class \code{Formula}
  which inherits from \code{formula}. It is the original \code{formula}
  plus two attributes \code{"lhs"} and \code{"rhs"} that contain the
  parts of the decomposed left- and right-hand side, respectively.
}

\references{
Zeileis A, Croissant Y (2010). Extended Model Formulas in R: Multiple Parts and Multiple Responses.
  \emph{Journal of Statistical Software}, \bold{34}(1), 1--13.
  \url{http://www.jstatsoft.org/v34/i01/}.
}

\seealso{\code{\link{model.frame.Formula}}}

\examples{
## create a simple Formula with one response and two regressor parts
f1 <- y ~ x1 + x2 | z1 + z2 + z3
F1 <- Formula(f1)
class(F1)
length(F1)

## switch back to original formula
formula(F1)

## create formula with various transformations
formula(F1, rhs = 1)
formula(F1, collapse = TRUE)
formula(F1, lhs = 0, rhs = 2)

## put it together from its parts
as.Formula(y ~ x1 + x2, ~ z1 + z2 + z3)

## update the formula
update(F1, . ~ . + I(x1^2) | . - z2 - z3)
update(F1, . | y2 + y3 ~ .)

# create a multi-response multi-part formula
f2 <- y1 | y2 + y3 ~ x1 + I(x2^2) | 0 + log(x1) | x3 / x4
F2 <- Formula(f2)
length(F2)

## obtain various subsets using standard indexing
## no lhs, first/seconde rhs
formula(F2, lhs = 0, rhs = 1:2)
formula(F2, lhs = 0, rhs = -3)
formula(F2, lhs = 0, rhs = c(TRUE, TRUE, FALSE))
## first lhs, third rhs
formula(F2, lhs = c(TRUE, FALSE), rhs = 3)
}

\keyword{classes}
