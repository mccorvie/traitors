##
##  Calculation library for odds relate to the Traitors reality game show
##  Also related to mafia / warewolf, etc
##
##  Author: Ryan McCorvie
##  Copyright 2025
##

library( tidyverse)
source( "traitors history.R")


##
## Probability that traitors win
##

mafia_win_boundary <- \( traitors_rules = T )
{
  p_twin <- matrix( NA, nrow = PP, ncol = TT+1)

  for( pp in 1:PP )
    p_twin[pp,1] = 0
  
  for( pp in 1:PP )
  {
    majority <- floor(pp/2)+1
    if( majority > TT ) break
    for( tt in majority:min(pp,TT))
      p_twin[ pp,tt+1 ] = 1
  }

  if( traitors_rules ) 
    p_twin[ 2,2 ] = 1 # equal votes -> traitors win
  else  
    p_twin[ 2,2 ] = 0.5 # equal votes -> coin flip
  
  p_twin  
}

# This corresponds to the mechanics of the finale:
# For the final 4 or 5, randomly eliminate without murders

traitors_finale_boundary <- \( p_twin )
{
  for( pp in 1:5)
    for( tt in 1:min(pp,TT) )
    {
      if( !is.na(p_twin[pp,tt+1])) next
      p_twin[pp,tt+1] = tt/pp * p_twin[pp-1,tt] + (pp-tt)/pp * p_twin[pp-1,tt+1] 
    }
  
  p_twin
}

traitors_win_prob <- \( p_twin )
{
  for( pp in 1:PP)
    for( tt in 1:min(pp,TT) )
    {
      if( !is.na(p_twin[pp,tt+1])) next
      p_twin[pp,tt+1] = tt/pp * p_twin[pp-2,tt] + (pp-tt)/pp * p_twin[pp-2,tt+1] 
    }
  p_twin
}



##
##  Probability a specific traitor wins 
##


one_traitor_win_boundary <- \()
{
  p_1t <- matrix( NA, nrow = PP, ncol = TT+1)
  
  for( pp in 1:PP )
    p_1t[pp,1] = 0
  
  for( pp in 1:PP )
  {
    majority <- floor(pp/2)+1
    if( majority > TT ) break
    for( tt in majority:min(pp,TT))
      p_1t[ pp,tt+1 ] = 1/tt
  }
  
  # 2 payers, 1 traitor
  p_1t[ 2,2 ] = 1

  p_1t  
}

# dynamic pogramming / backward induction solution to probability a particular traitor wins

one_traitor_win_prob_dp <- \( p_1t )
{
  for( pp in 1:PP)
    for( tt in 1:min(pp,TT) )
    {
      if( !is.na(p_1t[pp,tt+1])) next
      # Live states:
      # (a) another traitor banished 
      # (b) faithful banished
      # Terminal state (no next state):
      # (c) I'm banished
      probs     <- c( tt-1,          pp-tt ) / pp
      nextstate <- c( p_1t[pp-2,tt], p_1t[pp-2, tt+1] )
      p_1t[pp,tt+1] = sum( probs * nextstate )
    }
  p_1t
}


#  Shortcut dividing traitors win prob by # traitors

one_traitor_win_prob <- \()
{
  # pmin is hack to avoid NaN in the case # traitors = 0
  n_tt <- rep(0:TT,PP) |> pmax( 0.1 ) |> matrix( nrow=TT+1, ncol=PP) |> t()
  
  traitors_win <- mafia_win_boundary() |> traitors_win_prob()
  traitors_win / n_tt
}


one_faithful_win_boundary <- \()
{
  p_1f <- matrix( NA, nrow = PP, ncol = TT+1)
  
  for( pp in 1:PP )
    p_1f[pp,1] = 1/pp
  
  for( pp in 1:PP )
  {
    majority <- floor(pp/2)+1
    if( majority > TT ) break
    for( tt in majority:min(pp,TT))
      p_1f[ pp,tt+1 ] = 0
  }

  # 2 payers, 1 traitor
  p_1f[ 2,2 ] = 0 
  
  p_1f  
}

# dynamic pogramming / backward induction solution to probability a particular traitor wins

one_faithful_win_prob_dp <- \( p_1f )
{
  for( pp in 1:PP)
    for( tt in 1:min(pp,TT) )
    {
      if( !is.na(p_1f[pp,tt+1])) next
      # Live states:
      # (a) A traitor is banished, not murdered
      # (b) Another faithful banished, not murdered
      # Terminal states (no next state)
      # (c) I'm banished 
      # (d) another faithful banished, I'm murdered
      # (e) a traitor is banished, I'm murdered
      ff <- pp-tt
      prob_no_banish <- c( tt / pp, (ff-1)/pp ) 
      prob_no_murder <- c( (ff-1)/ff, (ff-2)/(ff-1) )
      prob           <- prob_no_banish * prob_no_murder
      next_state     <- c( p_1f[pp-2,tt], p_1f[pp-2,tt+1])
      p_1f[pp,tt+1]  <- sum( prob * next_state )
    }
  p_1f
}


one_faithful_win_prob <- \()
{
  n_pp <- rep(1:PP,TT+1) |> matrix( nrow=PP, ncol=TT+1)
  n_tt <- rep(0:TT,PP)   |> matrix( nrow=TT+1, ncol=PP) |> t()
  
  traitors_win <- mafia_win_boundary() |> traitors_win_prob()
  
  (1-traitors_win) / (n_pp-n_tt)
}

##
##  Utility functions
##

tibblize <- \( MM, var_name = "probability" ) 
{
  crossing( "num traitors" = 0:TT, "num players" = 1:PP) |> 
    mutate( !!var_name := as.vector(MM)) |> 
    filter( `num traitors` < `num players`)
}

blend_evenodd <- \(MM)
{
  MM[2:(nrow(MM)-1),] <- sqrt( MM[1:(nrow(MM)-2),] * MM[2:(nrow(MM)-1),])
  MM
}


##
##  Perform calculations
##

# game parameters 
TT <- 6   # Number of traitors
PP <- 30  # Number of players


history_tibble <- \( series_history_tt )
{
  traitors_win    <- mafia_win_boundary() |> traitors_win_prob()
  traitors_win_tt <- traitors_win |> tibblize( "traitor win" ) |> mutate( `faithful win` = 1-`traitor win`)
  #one_traitor_win  <- one_traitor_win_boundary() |> one_traitor_win_prob_dp() 
  #one_faithful_win <- one_faithful_win_boundary() |> one_faithful_win_prob_dp() 
  
  one_traitor_win  <- one_traitor_win_prob() 
  one_faithful_win <- one_faithful_win_prob() 
  
  one_traitor_win_tt  <- one_traitor_win  |> tibblize( "prob 1t" ) 
  one_faithful_win_tt <- one_faithful_win |> tibblize( "prob 1f" )
  
  series_history_tt |> left_join( traitors_win_tt) |> 
    left_join( one_traitor_win_tt ) |> 
    left_join( one_faithful_win_tt ) |> 
    mutate( 
      `expected traitor prize` = `prob 1t` * prize,
      `expected faithful prize` = `prob 1f` *  prize 
    )
}

mafia_win    <- mafia_win_boundary( F ) |> traitors_win_prob()
mafia_win1   <- mafia_win_boundary( T ) |> traitors_win_prob()
traitors_win <- mafia_win_boundary( T ) |> traitors_finale_boundary() |> traitors_win_prob()


prob_increase <-  (traitors_win - mafia_win) |> tibblize( var_name = "win prob increase")
mafia_increase

ggplot( mafia_increase, aes( y=`num traitors`, x=`num players`)) + 
  geom_tile( aes( fill = `win prob increase`)) +
  scale_fill_gradient2(low = "#2E5A87", mid = 'white', high = "#A3123A", midpoint=0,labels = scales::percent_format(accuracy=1)) +
  theme_minimal()


traitors_win_tt <- traitors_win |> tibblize( "traitor win" )


ggplot( traitors_win_tt, aes( y=`num traitors`, x=`num players`)) + 
  geom_tile( aes( fill = `traitor win`)) +
  scale_fill_gradient2(low = "#2E5A87", mid = 'white', high = "#A3123A", midpoint=0.5,labels = scales::percent_format(accuracy=1)) +
  theme_minimal()

us3_probabilty_tt <- history_tibble( us3_history )
uk3_probabilty_tt <- history_tibble( uk3_history )

uk3_probabilty_tt

ggplot( uk3_probabilty_tt, aes( x=episode)) + 
  geom_line( color = "#2E5A87", aes(y=`faithful win`)) + 
  geom_point( color = "#2E5A87", aes(y=`faithful win`)) + 
  geom_line( color = "#A3123A", aes( y=`traitor win`)) +
  geom_point( color = "#A3123A", aes( y=`traitor win`)) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  theme_minimal()

us3_probabilty_tt

ggplot( us3_probabilty_tt, aes( x=episode)) + 
  geom_line( color = "#2E5A87", aes(y=`expected faithful prize`)) + 
  geom_point( color = "#2E5A87", aes(y=`expected faithful prize`)) + 
  geom_line( color = "#A3123A", aes( y=`expected traitor prize`)) +
  geom_point( color = "#A3123A", aes( y=`expected traitor prize`)) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal()

# TO DO
# analyze seduce option
# nicer graphs


