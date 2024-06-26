% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R
\name{node_walk}
\alias{node_walk}
\title{Generate a \code{TreeCursor} iterator}
\usage{
node_walk(x)
}
\arguments{
\item{x}{\verb{[tree_sitter_node]}

A node.}
}
\value{
A \code{TreeCursor} object.
}
\description{
\code{node_walk()} creates a \link{TreeCursor} starting at the current node. You can
use it to "walk" the tree more efficiently than using \code{\link[=node_child]{node_child()}} and
other similar node functions.
}
\examples{
\dontshow{if (rlang::is_installed("treesitter.r")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
language <- treesitter.r::language()
parser <- parser(language)

text <- "1 + foo"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

cursor <- node_walk(node)

cursor$goto_first_child()
cursor$goto_first_child()
cursor$node()
cursor$goto_next_sibling()
cursor$node()
\dontshow{\}) # examplesIf}
}
