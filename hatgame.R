##hatgame ?
#saralamba@gmail.com

require("igraph")

#generate network
# This model is very simple, 
# every possible edge is created with the same constant probability.
# probability is caculated from R0/npop
randomNetwork <- function(npop,R0){
  g <- sample_gnp(npop, R0/npop)
  return(g)
}


# this will return the state vector
addInfectedNode <- function(g,npop=NULL,indx=NULL){
  #sensitive =0; infected = 1;
  if(is.null(npop)){
    npop <- length(ego_size(g))
  }
  if(is.null(indx)){
    indx <- sample.int(npop,1)
  }
  st <- rep(0,npop)
  st[indx] <- 1;
  return(st)
}

#check if its neighbors are infected
is.neighbors.infected <- function(g, indx, state){
  nb <- neighbors(g, indx)
  nb.state <- state[nb]
  if(sum(nb.state==1) > 0)
    return(TRUE)
  else
    return(FALSE)
}


# update the state based on its neighbors
updateState <- function(g, state){
  npop <- length(ego_size(g))
  if(npop != length(state)){
    stop("length(state) must be equal npop")
  }
  nstate <- state
  for(i in 1:npop){
    if(state[i]==0 && is.neighbors.infected(g,i,state)){
      nstate[i] <- 1
    }
    if(state[i]==1){
      nstate[i] <- 2
    }
  }
  return(nstate)
}

#give the color for each state
stateColors <- function(state){
  #sensitive = "green", infected = "red", recovered = "blue"
  state.cols <- rep(NA,length(state))
  for(i in 1:length(state)){
    if(state[i]==0){
      state.cols[i] <- "green"
    }
    if(state[i]==1){
      state.cols[i] <- "red"
    }
    if(state[i]==2){
      state.cols[i] <- "blue"
    }
  }
  return(state.cols)
}

#for vertex.label.color
stateLabelColors <- function(state){
  #sensitive = "green", infected = "red", recovered = "blue"
  state.lab.cols <- rep(NA,length(state))
  for(i in 1:length(state)){
    if(state[i]==0){
      state.lab.cols[i] <- "black"
    }
    if(state[i]==1){
      state.lab.cols[i] <- "black"
    }
    if(state[i]==2){
      state.lab.cols[i] <- "white"
    }
  }
  return(state.lab.cols)
}


#plot contact network
plotNetwork <- function(g, state, layout=NULL){
  state.cols <- stateColors(state)
  state.lab.cols <- stateLabelColors(state)

  if(!is.null(layout)){
    plot(g, vertex.color=state.cols, layout=layout, vertex.size=15, vertex.label.color=state.lab.cols,vertex.label.cex = 1.5,edge.color="red",edge.width=4,vertex.shape="circle")
  }
  else
  {
    plot(g, vertex.color=state.cols,vertex.size= 15, vertex.label.color=state.lab.cols,vertex.label.cex = 1.5,edge.color="red",edge.width=4,vertex.shape="circle")
  }
}

#count each state
stateCount <- function(state){
  s <- sum(state==0)
  i <- sum(state==1)
  r <- sum(state==2)
  
  return(c(s,i,r))
}

#plotting the infected cases
# plot the graph only when n.points > 2
plotInfected <- function(df){
  n.points <- nrow(df)
  if(n.points > 2){
    plot(df, xlab = "time", ylab = "infected cases", 
         type="o", pch=4, cex = 1.5, col="black", lwd=2,
         cex.lab=1.5, cex.axis=1.25)
  }else{
    NULL
  }
  
}

