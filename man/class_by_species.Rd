% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_by_species.R
\name{class_by_species}
\alias{class_by_species}
\title{Classification by species and year}
\usage{
class_by_species(all_curves, plot = T)
}
\arguments{
\item{all_curves}{Output data frame from community().}

\item{plot}{If T, outputs a plot.}
}
\value{
An edited version of the data frame output from community() designed for plotting classification results by species and year.
}
\description{
Calculate and plot ToPE and ToEE classification results for all species and year combinations within a dataset.
}
\examples{
# Set seed
set.seed(123)

# Create test dataset 1
year = seq(1,30,1)
event = round(rnorm(30, 100, 5))- seq(1,30,1)
env = (rnorm(30, 15, 0.5))
species = 1
sp1 = data.frame(year, species, event, env)

# Create test dataset 2
year = seq(1,30,1)
event = round(rnorm(30, 100, 5))
env = (rnorm(30, 15, 0.5)) + seq(0.1,3,0.1)
species = 2
sp2 = data.frame(year, species, event, env)

# combine
dataset = as.data.frame(rbind(sp1, sp2))

# Calculate empirical time of phenological emergence (ToPE)
com_res = community(dataset)

# Run and plot classification by species and year
class_by_species(com_res)
}
