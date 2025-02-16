% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process-policy.R
\name{reconcile_policy_with_gs}
\alias{reconcile_policy_with_gs}
\title{Reconcile Policy File with Gradescope Data}
\usage{
reconcile_policy_with_gs(policy, gs, verbose = FALSE)
}
\arguments{
\item{policy}{R list of a valid policy file}

\item{gs}{Gradescope data}

\item{verbose}{Whether or not to print messages; if FALSE, still throws error if no assignments found in gs}
}
\value{
A policy list
}
\description{
This function drops any assignments present in the policy file that are not
in the Gradescope data, making sure that both the policy file and Gradescope
data are compatible. This function also sets any default values not explicitly
specified in the policy file but required for grading.
}
\examples{
reconcile_policy_with_gs(policy = policy_demo, gs = gs_demo, verbose = TRUE)


}
