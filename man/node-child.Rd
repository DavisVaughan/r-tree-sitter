% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R
\name{node-child}
\alias{node-child}
\alias{node_child}
\alias{node_named_child}
\title{Get a node's child by index}
\usage{
node_child(x, i)

node_named_child(x, i)
}
\arguments{
\item{x}{\verb{[tree_sitter_node]}

A node.}

\item{i}{\verb{[integer(1)]}

The index of the child to return.}
}
\value{
The \code{i}th child node of \code{x} or \code{NULL} if there is no child at that index.
}
\description{
These functions return the \code{i}th child of \code{x}.
\itemize{
\item \code{node_child()} considers both named and anonymous children.
\item \code{node_named_child()} considers only named children.
}
}
\examples{
\dontshow{if (rlang::is_installed("treesitter.r")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Starts with `program` node for the whole document
node

# Navigate to first child
node <- node_child(node, 1)
node

# Note how the named variant skips the anonymous operator node
node_child(node, 2)
node_named_child(node, 2)

# OOB indices return `NULL`
node_child(node, 5)
\dontshow{\}) # examplesIf}
}
