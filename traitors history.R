##
##  History of decisions in the actual Traitors game shows
##
##  Sourced from 
##  US traitors: https://en.wikipedia.org/wiki/The_Traitors_(American_TV_series)
##  UK traitors: https://en.wikipedia.org/wiki/The_Traitors_(British_TV_series)
##
##  Author: Ryan McCorvie
##  Copyright 2025
##


series_history <- \( season, pp_hist, tt_hist, prize )
{
  tibble(
    season = season,
    `num players`  = pp_hist,
    `num traitors` = tt_hist,
    episode = 1:length(pp_hist),
    prize = prize
  )
}

uk3_pp <- c( 22, 20, 18, 17, 17, 16, 14 )
uk3_tt <- c(  3,  3,  3,  2,  2,  2,  2 )
uk3_prize <- 100000
uk3_history <- series_history( "UK season 3", uk3_pp, uk3_tt, uk3_prize )


us3_pp <- c( 20, 21, 19  )
us3_tt <- c(  3,  4,  4 )
us3_prize <- 250000
us3_history <- series_history( "US season 3", us3_pp, us3_tt, us3_prize )
