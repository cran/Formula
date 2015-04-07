\name{model.frame.Formula}

\alias{terms.Formula}
\alias{model.matrix.Formula}
\alias{model.frame.Formula}
\alias{model.part}
\alias{model.part.formula}
\alias{model.part.Formula}

\title{Model Frame/Matrix/Response Construction for Extended Formulas}

\description{
  Computation of model frames, model matrices, and model responses for
  extended formulas of class \code{Formula}.
}

\usage{
\method{model.frame}{Formula}(formula, data = NULL, \dots,
  lhs = NULL, rhs = NULL, dot = "separate")
\method{model.matrix}{Formula}(object, data = environment(object), \dots,
  lhs = NULL, rhs = 1, dot = "separate")
\method{terms}{Formula}(x, \dots,
  lhs = NULL, rhs = NULL, dot = "separate")

model.part(object, \dots)
\method{model.part}{Formula}(object, data, lhs = 0, rhs = 0,
  drop = FALSE, terms = FALSE, dot = NULL, \dots)
}

\arguments{
  \item{formula, object, x}{an object of class \code{Formula}.}
  \item{data}{a data.frame, list or environment containing the variables in
    \code{formula}. For \code{model.part} it needs to be the \code{model.frame}.}
  \item{lhs, rhs}{indexes specifying which elements of the left- and
    right-hand side, respectively, should be employed. \code{NULL}
    corresponds to all parts, \code{0} to none. At least one \code{lhs} or
    one \code{rhs} has to be specified.}
  \item{dot}{character specifying how to process formula parts with a dot
    (\code{.}) on the right-hand side. This can either be \code{"separate"}
    so that each formula part is expanded separately or \code{"sequential"}
    so that the parts are expanded sequentially conditional on all prior parts.}
  \item{drop}{logical. Should the \code{data.frame} be dropped for single
    column data frames?}
  \item{terms}{logical. Should the \code{"terms"} attribute (corresponding to
    the \code{model.part} extracted) be added?}
  \item{\dots}{further arguments passed to the respective
    \code{formula} methods.}
}

\details{
  All three model computations leverage the corresponding standard methods.
  Additionally, they allow specification of the part(s) of the left- and right-hand side
  (LHS and RHS) that should be included in the computation.

  The idea underlying all three model computations is to extract a suitable
  \code{formula} from the more general \code{Formula} and then calling
  the standard \code{\link[stats]{model.frame}}, \code{\link[stats]{model.matrix}},
  and \code{\link[stats]{terms}} methods.
  
  More specifically, if the \code{Formula} has multiple parts on the RHS,
  they are collapsed, essentially replacing \code{|} by \code{+}. If there
  is only a single response on the LHS, then it is kept on the LHS.
  Otherwise all parts of the formula are collapsed on the RHS (because \code{formula}
  objects can not have multiple responses). Hence, for multi-response \code{Formula}
  objects, the (non-generic) \code{\link[stats:model.extract]{model.response}} does
  not give the correct results. To avoid confusion a new generic \code{model.part} 
  with suitable \code{formula} method is provided which can always
  be used instead of \code{model.response}. Note, however, that it has a different
  syntax: It requires the \code{Formula} object in addition to the readily
  processed \code{model.frame} supplied in data
  (and optionally the \code{lhs}). Also, it returns either a \code{data.frame} with
  multiple columns or a single column (dropping the \code{data.frame} property)
  depending on whether multiple responses are employed or not.
  
  If the the formula contains one or more dots (\code{.}), some care has to be
  taken to process these correctly, especially if the LHS contains transformartions
  (such as \code{log}, \code{sqrt}, \code{cbind}, \code{Surv}, etc.). Calling the
  \code{terms} method with the original data (untransformed, if any) resolves
  all dots (by default separately for each part, otherwise sequentially) and also
  includes the original and updated formula as part of the terms. When calling
  \code{model.part} either the original untransformed data should be provided
  along with a \code{dot} specification or the transformed \code{model.frame}
  from the same formula without another \code{dot} specification (in which
  case the \code{dot} is inferred from the \code{terms} of the \code{model.frame}).
}

\references{
Zeileis A, Croissant Y (2010). Extended Model Formulas in R: Multiple Parts and Multiple Responses.
  \emph{Journal of Statistical Software}, \bold{34}(1), 1--13.
  \url{http://www.jstatsoft.org/v34/i01/}.
}

\seealso{\code{\link{Formula}},  \code{\link[stats]{model.frame}},
  \code{\link[stats]{model.matrix}}, \code{\link[stats]{terms}},
  \code{\link[stats:model.extract]{model.response}}}

\examples{
## artificial example data
set.seed(1090)
dat <- as.data.frame(matrix(round(runif(21), digits = 2), ncol = 7))
colnames(dat) <- c("y1", "y2", "y3", "x1", "x2", "x3", "x4")
for(i in c(2, 6:7)) dat[[i]] <- factor(dat[[i]] > 0.5, labels = c("a", "b"))
dat$y2[1] <- NA
dat

######################################
## single response and two-part RHS ##
######################################

## single response with two-part RHS
F1 <- Formula(log(y1) ~ x1 + x2 | I(x1^2))
length(F1)

## set up model frame
mf1 <- model.frame(F1, data = dat)
mf1

## extract single response
model.part(F1, data = mf1, lhs = 1, drop = TRUE)
model.response(mf1)
## model.response() works as usual

## extract model matrices
model.matrix(F1, data = mf1, rhs = 1)
model.matrix(F1, data = mf1, rhs = 2)

#########################################
## multiple responses and multiple RHS ##
#########################################

## set up Formula
F2 <- Formula(y1 + y2 | log(y3) ~ x1 + I(x2^2) | 0 + log(x1) | x3 / x4)
length(F2)

## set up full model frame
mf2 <- model.frame(F2, data = dat)
mf2

## extract responses
model.part(F2, data = mf2, lhs = 1)
model.part(F2, data = mf2, lhs = 2)
## model.response(mf2) does not give correct results!

## extract model matrices
model.matrix(F2, data = mf2, rhs = 1)
model.matrix(F2, data = mf2, rhs = 2)
model.matrix(F2, data = mf2, rhs = 3)

#######################
## Formulas with '.' ##
#######################

## set up Formula with a single '.'
F3 <- Formula(y1 | y2 ~ .)
mf3 <- model.frame(F3, data = dat)
## without y1 or y2
model.matrix(F3, data = mf3)
## without y1 but with y2
model.matrix(F3, data = mf3, lhs = 1)
## without y2 but with y1
model.matrix(F3, data = mf3, lhs = 2)

## set up Formula with multiple '.'
F3 <- Formula(y1 | y2 | log(y3) ~ . - x3 - x4 | .)
## process both '.' separately (default)
mf3 <- model.frame(F3, data = dat, dot = "separate")
## only x1-x2
model.part(F3, data = mf3, rhs = 1)
## all x1-x4
model.part(F3, data = mf3, rhs = 2)
## process the '.' sequentially, i.e., the second RHS conditional on the first
mf3 <- model.frame(F3, data = dat, dot = "sequential")
## only x1-x2
model.part(F3, data = mf3, rhs = 1)
## only x3-x4
model.part(F3, data = mf3, rhs = 2)

##############################
## Process multiple offsets ##
##############################

## set up Formula
F4 <- Formula(y1 ~ x3 + offset(x1) | x4 + offset(log(x2)))
mf4 <- model.frame(F4, data = dat)
## model.part can be applied as above and includes offset!
model.part(F4, data = mf4, rhs = 1)
## additionally, the corresponding corresponding terms can be included
model.part(F4, data = mf4, rhs = 1, terms = TRUE)
## hence model.offset() can be applied to extract offsets
model.offset(model.part(F4, data = mf4, rhs = 1, terms = TRUE))
model.offset(model.part(F4, data = mf4, rhs = 2, terms = TRUE))
}

\keyword{models}
