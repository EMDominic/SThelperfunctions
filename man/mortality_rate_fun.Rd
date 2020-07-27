\name{mortality_rate_fun}
\alias{mortality_rate_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Mortality rate
%%  ~~function to do ... ~~
}
\description{
This function that takes in weekly or monthly aggreagated causes of deaths and population estimates
then it computes mortality rates per 100 000 by country level and provincial level.
This function takes output from the aggreagate_fun().
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mortality_rate_fun(df,
                   population_est,
                   scale.aggregate = c("Province", "Countrywide"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Aggregated dataframe
}
\item{population_est}{
Mid year population estimates. If by_province is TRUE, then provide provincial estimates else countrywide estimates.
}
\item{by_province}{
Whether to aggregate by province or country level
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## The function is currently defined as

mortality_rate_fun <- function(df,
                               population_est,
                               scale.aggregate = c("Province", "Countrywide")) {

  joint_df <- if (scale.aggregate == "Province")
  {left_join(df, population_est, by = c("DeathProv" = "Province", "Year"))}
  else {left_join(df, population_est,by = "Year")}

  joint_df \%>\%
    mutate(All_deaths_rate = (All_deaths/Pop_estimate)*100000,
           AllRes_deaths_rate = (AllRes_deaths/Pop_estimate)*100000,
           AllResU5_deaths_rate = (AllResU5_deaths/Pop_estimateU5)*100000,
           AllResG5_deaths_rate = (AllResG5_deaths/Pop_estimateG5)*100000,
           PI_deaths_rate = (PI_deaths/Pop_estimate)*100000,
           PIU5_deaths_rate = (PIU5_deaths/Pop_estimateU5)*100000,
           PIG5_deaths_rate = (PIG5_deaths/Pop_estimateG5)*100000,
           POther_deaths_rate = (POther_deaths/Pop_estimate)*100000,
           Flu_deaths_rate = (Flu_deaths/Pop_estimate)*100000,
           RSV_deaths_rate = (RSV_deaths/Pop_estimateU5)*100000)  \%>\%
    select(-c(All_deaths, AllRes_deaths, AllResU5_deaths, AllResG5_deaths, PI_deaths,
              PIU5_deaths, PIG5_deaths,
              POther_deaths, Flu_deaths, RSV_deaths, Year, Pop_estimate, Pop_estimateU5,
              Pop_estimateG5))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
