% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.R
\name{node-location}
\alias{node-location}
\alias{node_start_byte}
\alias{node_end_byte}
\alias{node_start_point}
\alias{node_end_point}
\alias{node_range}
\title{Node byte and point accessors}
\usage{
node_start_byte(x)

node_end_byte(x)

node_start_point(x)

node_end_point(x)

node_range(x)
}
\arguments{
\item{x}{\verb{[tree_sitter_node]}

A node.}
}
\value{
\itemize{
\item \code{node_start_byte()} and \code{node_end_byte()} return a single numeric value.
\item \code{node_start_point()} and \code{node_end_point()} return single points.
\item \code{node_range()} returns a range.
}
}
\description{
These functions return information about the location of \code{x} in the document.
The byte, row, and column locations are all 0-indexed.
\itemize{
\item \code{node_start_byte()} returns the start byte.
\item \code{node_end_byte()} returns the end byte.
\item \code{node_start_point()} returns the start point, containing a row and column
location within the document. Use accessors like \code{\link[=point_row]{point_row()}} to extract
the row and column positions.
\item \code{node_end_point()} returns the end point, containing a row and column
location within the document. Use accessors like \code{\link[=point_row]{point_row()}} to extract
the row and column positions.
\item \code{node_range()} returns a range object that contains all of the above
information. Use accessors like \code{\link[=range_start_point]{range_start_point()}} to extract
individual pieces from the range.
}
}
\examples{
\dontshow{if (rlang::is_installed("treesitter.r")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
language <- treesitter.r::language()
parser <- parser(language)

text <- "fn <- function() { 1 + 1 }"
tree <- parser_parse(parser, text)
node <- tree_root_node(tree)

# Navigate to first child
node <- node_child(node, 1)

# Navigate to function definition node
node <- node_child(node, 3)
node

node_start_byte(node)
node_end_byte(node)

node_start_point(node)
node_end_point(node)

node_range(node)
\dontshow{\}) # examplesIf}
}
