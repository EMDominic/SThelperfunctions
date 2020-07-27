\name{detrending_fun}
\alias{detrending_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Data detrending
%%  ~~function to do ... ~~
}
\description{Detrending aggregated time series data using natural cubic splines.
This function takes output from the mortality_rate_fun().
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
detrending_fun(df,
               time.aggregate = c("WOD", "MOD"),
               dof = 4,
               scale.aggregate = c("Province", "Countrywide"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ Aggregated mortality rates
%%     ~~Describe \code{df} here~~
}
  \item{time.aggregate}{ Whether to aggregate weekly ("WOD") or monthly ("MOD")
%%     ~~Describe \code{time.aggregate} here~~
}
 \item{dof}{ Degrees of freedom for the spline
%%     ~~Describe \code{time.aggregate} here~~
}
  \item{by_province}{
  Whether to aggregate by province or country level
%%     ~~Describe \code{by_province} here~~
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

detrending_fun <- function(df,
                           time.aggregate = c("WOD", "MOD"),
                           dof = 4,
                           scale.aggregate = c("Province", "Countrywide")) {

  time.aggregate = rlang::arg_match(time.aggregate)
  time.cycle <- if_else(time.aggregate == "WOD", 52.17, 12)
  quo.time.aggregate = rlang::sym(time.aggregate)

  scale.aggregate = rlang::arg_match(scale.aggregate)

  det.data <- if (scale.aggregate == "Province")
  {group_by(df, DeathProv) \%>\%
      mutate(t = seq(from = 1997, to = 2016.999, by = 1/time.cycle)) \%>\%
      ungroup() \%>\%
      gather(key = "death",
             value = "rates",
             All_deaths_rate:RSV_deaths_rate,
             na.rm = T) \%>\%
      group_by(DeathProv, death) \%>\%
      nest()
  }
  else {mutate(df, t = seq(from = 1997.000, to = 2016.999, by = 1/time.cycle)) %>%
      gather(key = "death",
             value = "rates",
             All_deaths_rate:RSV_deaths_rate,
             na.rm = T) \%>\%
      group_by(death) \%>\%
      nest()}

  det.data \%>\%
    mutate(fit = map(data, ~ lm(rates ~ ns(t,dof),data = .)),
           trend = map(fit, predict)) \%>\%
    unnest(c(data,trend)) \%>\%
    select(-fit) \%>\%
    mutate(detrended = rates - trend,
           mean_death_rate = mean(rates),
           detrended_plus_mean = detrended + mean_death_rate) \%>\%
    ungroup()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
