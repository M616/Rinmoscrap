% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_for_new_items.R
\name{check_for_new_items}
\alias{check_for_new_items}
\title{Check for new items in the 'url' variable and add 'is_new_listing' column}
\usage{
check_for_new_items(
  current_data,
  previous_data,
  new_column_name = "is_new_listing"
)
}
\arguments{
\item{current_data}{The current dataset that contains the 'url' variable.}

\item{previous_data}{A dataset with the same structure and 'url' variable.}

\item{new_column_name}{The name of the new column indicating whether each record is a new item (default: 'is_new_listing').}
}
\value{
The updated dataset with the new 'is_new_listing' column.
}
\description{
This function compares the current dataset with previous datasets, checks for new items in the 'url' variable,
and adds a new boolean column 'is_new_listing' indicating whether each record is a new item.
}
