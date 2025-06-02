\name{priority_plots_data}
\docType{data}
\alias{priority_plots}
\title{NEON plot sampling priority number}
\description{
  NEON plots have a priority number in the event that not all plots are able to be sampled. The lower the number the higher the priority. For optimal comparability across years, users may wish to only include data from the highest priority plots. This dataset includes the priority number as well as the NEON plotType.
}
\usage{priority_plots}
\format{A dataframe containing NEON plotID, specificModuleSamplingPriority, and plotType.}
\source{NEON}
\references{
  "UniquePlotIDsSamplingModulesPriorityLists" dataframe from NEON-OS-spatial-data GitHub repository
}
\keyword{datasets}