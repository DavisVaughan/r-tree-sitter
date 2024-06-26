% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/language.R
\name{language_symbol_count}
\alias{language_symbol_count}
\title{Language symbol count}
\usage{
language_symbol_count(x)
}
\arguments{
\item{x}{\verb{[tree_sitter_language]}

A tree-sitter language object.}
}
\value{
A single double value.
}
\description{
Get the number of symbols contained within a language.
}
\examples{
\dontshow{if (rlang::is_installed("treesitter.r")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
language <- treesitter.r::language()
language_symbol_count(language)
\dontshow{\}) # examplesIf}
}
