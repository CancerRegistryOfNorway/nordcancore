% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{global_nordcan_settings}
\alias{global_nordcan_settings}
\alias{set_global_nordcan_settings}
\alias{get_global_nordcan_settings}
\title{NORDCAN Settings}
\usage{
set_global_nordcan_settings(
  work_dir,
  participant_name,
  first_year_incidence,
  first_year_mortality,
  first_year_region,
  last_year_incidence,
  last_year_mortality,
  last_year_survival
)

get_global_nordcan_settings()
}
\arguments{
\item{work_dir}{\verb{[character]} (mandatory, no default)

root directory for any and all (temporarily) stored NORDCAN data; if it
does not exist it is attempted to be created}

\item{participant_name}{\verb{[character]} (mandatory, no default)

name of NORDCAN participant; e.g. "Denmark" or "Sweden"}

\item{first_year_incidence}{\verb{[integer]} (mandatory, no default)

first year for which to compute the cancer record count statistics;
e.g. \code{1953L}}

\item{first_year_mortality}{\verb{[integer]} (mandatory, no default)

first year for the cancer death count statistics; e.g. \code{1953L}}

\item{first_year_region}{\verb{[integer]} (mandatory, no default)}

\item{last_year_incidence}{\verb{[integer]} (mandatory, no default)

Last year for which to compute the cancer record count statistics;
e.g. \code{2019L}}

\item{last_year_mortality}{\verb{[integer]} (mandatory, no default)

Last year for the cancer death count statistics; e.g. \code{2019L}}

\item{last_year_survival}{\verb{[integer]} (mandatory, no default)

Last year for survival analysis; e.g. \code{2019L}}
}
\description{
Set and get settings for NORDCAN software.
}
\details{
Some first years for statistics are set using \code{set_global_nordcan_settings}.
However, the first year for survival is fixed to
\code{nordcancore::global_settings_env[["last_year_survival"]] - 29L}. Likewise the first
year for prevalence is fixed to \code{first_year_incidence + 20L}.
}
