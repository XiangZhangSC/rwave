% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_ph_well.R
\name{sim_ph_well}
\alias{sim_ph_well}
\title{Simulate the true pH emission of a single well}
\usage{
sim_ph_well(ocr, glycolysis, tick.num = 15)
}
\arguments{
\item{ocr}{a vector of true biological oxygen consumption rates, pmol O2 / min}

\item{glycolysis}{a vector of true glycolysis rates, mpH / min}

\item{tick.num}{the number of ticks within each measurement period}
}
\description{
sim_ph_well simulates the pH emission fluoresence of a single well
}
