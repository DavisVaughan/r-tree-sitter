% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\name{is_parser}
\alias{is_parser}
\title{Is \code{x} a parser?}
\usage{
is_parser(x)
}
\arguments{
\item{x}{\verb{[object]}

An object.}
}
\value{
\code{TRUE} if \code{x} is a \code{tree_sitter_parser}, otherwise \code{FALSE}.
}
\description{
Checks if \code{x} is a \code{tree_sitter_parser} or not.
}
\examples{
\dontshow{if (rlang::is_installed("treesitter.r")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
language <- treesitter.r::language()
parser <- parser(language)

is_parser(parser)

is_parser(1)
\dontshow{\}) # examplesIf}
}
