\name{aggregate_fun}
\alias{aggregate_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Aggregating data
%%  ~~function to do ... ~~
}
\description{
Function that takes in daily causes of death and aggregates it by week (WOD) or months (MOD) and by country or by province. The arg_match function ensures the user can choose the only two available options for the time aggregation and returns an informative error message. This functions outputs an aggreated dataframe.
}
\usage{
aggregate_fun(df,
              time.aggregate = c("WOD", "MOD"),
              scale.aggregate = c("Province", "Countrywide"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
dataframe
}
\item{time.aggregate}{
Whether to aggregate weekly ("WOD") or monthly ("MOD")
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

aggregate_fun <- function(df,
                          time.aggregate = c("WOD", "MOD"),
                          scale.aggregate = c("Province", "Countrywide")) {

  time.aggregate = rlang::arg_match(time.aggregate)
  quo.time.aggregate = rlang::sym(time.aggregate)
  scale.aggregate = rlang::arg_match(scale.aggregate)

  group_df <- if (scale.aggregate == "Province")
  {group_by(df, !! quo.time.aggregate, DeathProv)}
  else {group_by(df, !! quo.time.aggregate)}

  group_df \%>\%
    summarise(All_deaths = sum(All),
              AllRes_deaths = sum(AllRes), AllResU5_deaths = sum(AllResU5),
              AllResG5_deaths = sum(AllResG5), PI_deaths = sum(PI),
              PIU5_deaths = sum(PIU5), PIG5_deaths = sum(PIG5),
              POther_deaths = sum(POther),
              Flu_deaths = sum(Flu), RSV_deaths = sum(RSV)) \%>\%
    mutate(Year = as.numeric(format(!! quo.time.aggregate, "\%Y")))\%>\%
    ungroup()
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
