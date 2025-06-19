\name{plantIntTrop_data}
\docType{data}
\alias{plantIntTrop}
\title{TaxonIDs that are introduced or with tropical distribution}
\description{
  Comprehensive dataset of plant characteristics downloaded from https://plants.usda.gov/, and then filtered to taxonIDs with PLANTS.Floristic.Area or Native.Status populated. Simplified NativeStatus to binary nat (native) or intr (introduced), and PLANTS.Floristic.Area to binary trop (tropical) or temp (temperate), and then filtered to taxonIDs that were introduced, tropical, or both.
}
\usage{plantIntTrop}
\format{A dataframe containing taxonID, nativeStatus, and tropical fields.}
\source{Download circa 2020}
\references{
  https://plants.usda.gov/
}
\keyword{datasets}