% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d.t.R
\name{d.t}
\alias{d.f}
\alias{d.t}
\alias{rbl}
\title{data.table shortened alternatives}
\usage{
d.t(..., keep.rownames = FALSE, check.names = FALSE, key = NULL)

d.f(..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
  fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors())

rbl(l, use.names = fill, fill = FALSE, idcol = NULL)
}
\arguments{
\item{...}{ Just as \code{\dots} in \code{\link{data.frame}}. Usual recycling rules are applied to vectors of different lengths to create a list of equal length vectors.

}

\item{keep.rownames}{ If \code{\dots} is a \code{matrix} or \code{data.frame}, \code{TRUE} will retain the rownames of that object in a column named \code{rn}.

}

\item{check.names}{ Just as \code{check.names} in \code{\link{data.frame}}.

}

\item{key}{ Character vector of one or more column names which is passed to \code{\link{setkey}}. It may be a single comma separated string such as \code{key="x,y,z"}, or a vector of names such as \code{key=c("x","y","z")}.

}

\item{row.names}{\code{NULL} or a single integer or character string
    specifying a column to be used as row names, or a character or
    integer vector giving the row names for the data frame.}

\item{check.rows}{if \code{TRUE} then the rows are checked for
    consistency of length and names.}

\item{fix.empty.names}{logical indicating if arguments which are
    \dQuote{unnamed} (in the sense of not being formally called as
    \code{someName = arg}) get an automatically constructed name or
    rather name \code{""}.  Needs to be set to \code{FALSE} even when
    \code{check.names} is false if \code{""} names should be kept.}

\item{stringsAsFactors}{logical: should character vectors be converted
    to factors?  The \sQuote{factory-fresh} default is \code{TRUE}, but
    this can be changed by setting \code{\link{options}(stringsAsFactors
      = FALSE)}.}

\item{l}{ A list containing \code{data.table}, \code{data.frame} or \code{list} objects. At least one of the inputs should have column names set. \code{\dots} is the same but you pass the objects by name separately. }

\item{use.names}{If \code{TRUE} items will be bound by matching column names. By default \code{FALSE} for \code{rbindlist} (for backwards compatibility) and \code{TRUE} for \code{rbind} (consistency with base). Columns with duplicate names are bound in the order of occurrence, similar to base. When TRUE, at least one item of the input list has to have non-null column names.}

\item{fill}{If \code{TRUE} fills missing columns with NAs. By default \code{FALSE}. When \code{TRUE}, \code{use.names} has to be \code{TRUE}, and all items of the input list has to have non-null column names. }

\item{idcol}{Generates an index column. Default (\code{NULL}) is not to. If \code{idcol=TRUE} then the column is auto named \code{.id}. Alternatively the column name can be directly provided, e.g., \code{idcol = "id"}.

  If input is a named list, ids are generated using them, else using integer vector from \code{1} to length of input list. See \code{examples}.}
}
\value{
Functions return the same objects as the original functions.
}
\description{
Shortented versions of data.table's functions \code{data.table}, \code{rbindlist}, and base's \code{data.frame}.
}
\section{TODO}{

\itemize{
   \item Improve and/or potentally merge function into other files.
}
}
\examples{
# Normal example #
dt <- d.t(x = c(1:3), y = letters[1:3], key = 'y')
key(dt)
# Without argument name #
d.t(c(1:3))

d.f(c(1:3)) # Normal data.frame
rbl(list(d.t(a = 1, b = 1), d.t(a = 2, b = 2)))
}
\seealso{
\href{https://cran.r-project.org/web/packages/data.table/index.html}{data.table}
}

