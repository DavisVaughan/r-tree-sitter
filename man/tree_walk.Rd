% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree.R
\name{tree_walk}
\alias{tree_walk}
\title{Generate a \code{TreeCursor} iterator}
\usage{
tree_walk(x)
}
\arguments{
\item{x}{\verb{[tree_sitter_tree]}

A tree.}
}
\value{
A \code{TreeCursor} object.
}
\description{
\code{tree_walk()} creates a \link{TreeCursor} starting at the root node. You can
use it to "walk" the tree more efficiently than using \code{\link[=node_child]{node_child()}} and
other similar node functions.
}
\examples{
\dontshow{if (rlang::is_installed("treesitter.r")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)

cursor <- tree_walk(tree)

cursor$goto_first_child()
cursor$goto_first_child()
cursor$node()
cursor$goto_next_sibling()
cursor$node()
\dontshow{\}) # examplesIf}
}
