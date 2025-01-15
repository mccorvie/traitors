library( tidyverse)




game_hist <- tibble( run = numeric(), t = numeric(), R = numeric(), M = numeric())
n_run <- 1000
m_w <- 0

for( run in 1:n_run)
{
  R <- 1000
  M <- 10
  flips <- runif( R)
  Rs <- numeric()
  Ms <- numeric()
  
  for( flip in flips )
  {
    #cat( M, " ", R, "\n")
    if( flip <= M/R ) 
      M <- M-1
    R <- R-2
    Rs <- c( Rs, R )
    Ms <- c( Ms, M )
    
    #if( R == 0 ) break
    if( M == 0 ) break
    if( M >= R/2 ) break
  }
  if( M>0 )
    m_w <- m_w+1
  game_hist <- game_hist |> bind_rows( tibble( run= run, t = 1:length(Rs ), R = Rs, M = Ms ))
}

m_w
m_w/n_run
ggplot( game_hist, aes(x=t, y=M , group=run) ) + geom_line( aes(color=run))


# random permutation of murder list
# birth / death
# martingale
# aumann agreement theorem with sigma-algebra of information