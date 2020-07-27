\name{plot_provincial_detrended}
\alias{plot_provincial_detrended}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting Provincial Detrended Data
%%  ~~function to do ... ~~
}
\description{ This function plots provincial detrended time series data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_provincial_detrended(df,
                          COD)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ detrended data set
%%     ~~Describe \code{x} here~~
}
  \item{COD}{ cause of death category. The different options are "All_deaths_rate","AllRes_deaths_rate", "PI_deaths_rate",
  "POther_deaths_rate", "Flu_deaths_rate" or "RSV_deaths_rate".
%%     ~~Describe \code{x} here~~
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
plot_provincial_detrended <- function(df,
                                      COD){
  df \%>\%
    filter(death == COD) \%>\%
    ggplot()+
    geom_line(aes(x = t, y = rates), col = "black")+
    geom_line(aes(x = t, y = trend),col = "red", size = 1)+
    geom_line(aes(x = t, y = detrended), col = "blue")+
    facet_wrap(~DeathProv, nrow = 3, ncol = 3, scales = "free_y") +
    theme_bw()+
    xlab("") +
    ylab("")+
    ggtitle(paste(gsub("_", " ", COD),"per 100 000"))+
    theme(plot.caption = element_text(size = 8, hjust = 0))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
