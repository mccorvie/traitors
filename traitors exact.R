library( tidyverse)
library( RColorBrewer)
library(ggsci)



#
# Probability that traitors win
#

traitors_win_boundary <- \( mafia = F )
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
  
  # 2 payers, 1 traitor
  if( mafia )
    p_twin[ 2,2 ] = 0.5 # equal votes -> coin flip
  else
    p_twin[ 2,2 ] = 1   # traitor survives to end, wins it all

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


one_traitor_win_prob <- \( p_1t )
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

one_faithful_win_prob <- \( p_1f )
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

# game parameters 
TT <- 6   # Number of traitors
PP <- 30  # Number of players


traitors_win     <- traitors_win_boundary() |> traitors_win_prob()
one_traitor_win  <- one_traitor_win_boundary() |> one_traitor_win_prob() 
one_faithful_win <- one_faithful_win_boundary() |> one_faithful_win_prob() 

one_faithful_win

tt1 <- traitors_win |> tibblize() |> mutate( mode = "traitors win")

ggplot( tt1, aes( y=`num traitors`, x=`num players`)) + 
  geom_tile( aes( fill = probability)) +
  scale_fill_gradient2(low = "#2E5A87", mid = 'white', high = "#A3123A", midpoint=0.5) +
  theme_minimal()

prize <- 100000
tt2 <- one_traitor_win  |> tibblize() |> mutate( mode = "one traitor wins")
tt3 <- one_faithful_win |> tibblize() |> mutate( mode = "one faithful wins")
plotme <-  bind_rows( tt2, tt3) |> mutate( `expected payout` = prize * probability )

ggplot( tt2, aes( y=`num traitors`, x=`num players`)) + 
  geom_tile( aes( fill = probability)) +
  scale_fill_gradient2(low = "#2E5A87", mid = 'white', high = "#A3123A", midpoint=0) +
  theme_minimal()

t(t(one_traitor_win) * (0:TT)) + one_faithful_win*(1:PP)

n_pp <- rep(1:PP,TT+1) |> matrix( nrow=PP, ncol=TT+1)
n_tt <- rep(0:TT,PP) |> matrix( nrow=TT+1, ncol=PP) |> t()

(n_pp-n_tt)*one_faithful_win + n_tt * one_traitor_win

n_tt * one_traitor_win - traitors_win

matrix(1,3,3) * 1:3
#
#  Probability a given traitor / faithful wins 
#




#
#  Expected winnings for a given traitor / faithful
#


plotme <- plotme |> filter( `num players` %% 100== 0)

ggplot( plotme, aes( y=`num traitors`, x=`num players`)) + 
  geom_tile( aes( fill = probability)) +
  scale_fill_gradient2(low = "#2E5A87", mid = 'white', high = "#A3123A", midpoint=0.5) +
  theme_minimal()

# Probability that I as mafia player survives
