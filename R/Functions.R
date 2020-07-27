
aggregate_fun <- function(df,
                          time.aggregate = c("WOD", "MOD"),
                          scale.aggregate = c("Province", "Countrywide")) {

  time.aggregate = rlang::arg_match(time.aggregate)
  quo.time.aggregate = rlang::sym(time.aggregate)
  scale.aggregate = rlang::arg_match(scale.aggregate)

  group_df <- if (scale.aggregate == "Province")
  {group_by(df, !! quo.time.aggregate, DeathProv)}
  else {group_by(df, !! quo.time.aggregate)}

  group_df %>%
    summarise(All_deaths = sum(All),
              AllRes_deaths = sum(AllRes), AllResU5_deaths = sum(AllResU5),
              AllResG5_deaths = sum(AllResG5), PI_deaths = sum(PI),
              PIU5_deaths = sum(PIU5), PIG5_deaths = sum(PIG5),
              POther_deaths = sum(POther),
              Flu_deaths = sum(Flu), RSV_deaths = sum(RSV)) %>%
    mutate(Year = as.numeric(format(!! quo.time.aggregate, "%Y")))%>%
    ungroup()
}


mortality_rate_fun <- function(df,
                               population_est,
                               scale.aggregate = c("Province", "Countrywide")) {

  scale.aggregate = rlang::arg_match(scale.aggregate)

  joint_df <- if (scale.aggregate == "Province")
  {left_join(df, population_est, by = c("DeathProv" = "Province", "Year"))}
  else {left_join(df, population_est,by = "Year")}

  joint_df %>%
    mutate(All_deaths_rate = (All_deaths/Pop_estimate)*100000,
           AllRes_deaths_rate = (AllRes_deaths/Pop_estimate)*100000,
           AllResU5_deaths_rate = (AllResU5_deaths/Pop_estimateU5)*100000,
           AllResG5_deaths_rate = (AllResG5_deaths/Pop_estimateG5)*100000,
           PI_deaths_rate = (PI_deaths/Pop_estimate)*100000,
           PIU5_deaths_rate = (PIU5_deaths/Pop_estimateU5)*100000,
           PIG5_deaths_rate = (PIG5_deaths/Pop_estimateG5)*100000,
           POther_deaths_rate = (POther_deaths/Pop_estimate)*100000,
           Flu_deaths_rate = (Flu_deaths/Pop_estimate)*100000,
           RSV_deaths_rate = (RSV_deaths/Pop_estimateU5)*100000) %>%
    select(-c(All_deaths, AllRes_deaths, AllResU5_deaths, AllResG5_deaths, PI_deaths,
              PIU5_deaths, PIG5_deaths,
              POther_deaths, Flu_deaths, RSV_deaths, Year, Pop_estimate, Pop_estimateU5,
              Pop_estimateG5))
}


detrending_fun <- function(df,
                           time.aggregate = c("WOD", "MOD"),
                           dof = 4,
                           scale.aggregate = c("Province", "Countrywide")) {

  time.aggregate = rlang::arg_match(time.aggregate)
  time.cycle <- if_else(time.aggregate == "WOD", 52.17, 12)
  quo.time.aggregate = rlang::sym(time.aggregate)

  scale.aggregate = rlang::arg_match(scale.aggregate)

  det.data <- if (scale.aggregate == "Province")
  {group_by(df, DeathProv) %>%
      mutate(t = seq(from = 1997, to = 2016.999, by = 1/time.cycle)) %>%
      ungroup() %>%
      gather(key = "death",
             value = "rates",
             All_deaths_rate:RSV_deaths_rate,
             na.rm = T) %>%
      group_by(DeathProv, death) %>%
      nest()
  }
  else {mutate(df, t = seq(from = 1997.000, to = 2016.999, by = 1/time.cycle)) %>%
      gather(key = "death",
             value = "rates",
             All_deaths_rate:RSV_deaths_rate,
             na.rm = T) %>%
      group_by(death) %>%
      nest()}

  det.data %>%
    mutate(fit = map(data, ~ lm(rates ~ ns(t,dof),data = .)),
           trend = map(fit, predict)) %>%
    unnest(c(data,trend)) %>%
    select(-fit) %>%
    mutate(detrended = rates - trend,
           mean_death_rate = mean(rates),
           detrended_plus_mean = detrended + mean_death_rate) %>%
    ungroup()
}


plot_provincial_detrended <- function(df,
                                      COD){
  df %>%
    filter(death == COD) %>%
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
