library(ggplot2)
library(gganimate)
library(gapminder)
N <- 20 # effective population size  
n_gen<-20
n_reps<-15

# fill up the probility transition matrix
P <- matrix( ncol = (2*N+1),nrow= (2*N+1))

for(i in 0:(2*N)){
  for(j in 0:(2*N)){
    # add to matrix for examples
    P[i+1, j+1] <- dbinom(j, 2*N, (i / (2*N))) 
  }
}



print(P)
# simulation_freq<-c(N)
gen<-1:(2*N+1)

freq<-N
# print(N)
simulation_points<- data.frame(
  generation=c(rep(1,N)),
  level=c(1:N),
  color=c(rep("green",N))
)
temp<- data.frame(
  generation=c(rep(1,N)),
  level=c((N+1):(2*N)),
  color=c(rep("red",N))
)
simulation_points<-rbind(simulation_points,temp)
print(simulation_points)
for(j in 2:n_gen)
{
  freq<-sample(gen,size=1,prob = P[freq,])
  # simulation_freq<-c(simulation_freq,freq)
  freq=freq-1
  if(freq>=1)
  {
    temp<- data.frame(
      generation=c(rep(j,freq)),
      level=c(1:(freq)),
      color=c(rep("green",freq))
    )
    simulation_points<-rbind(simulation_points,temp)
  }
  b<-2*N-freq+1
  if(b>=1)
  {
    temp<- data.frame(
      generation=c(rep(j,b)),
      level=c(freq:(freq+b-1)),
      color=c(rep("red",b))
    )
    simulation_points<-rbind(simulation_points,temp)
  }
  freq=freq+1
}
# x<-c(rep(2*N,n_gen))-simulation_freq
# print(x)
# print(simulation_points)
# # print(df)
# p <- ggplot(simulation_points) + geom_point(aes(x=generation,y=level,color=color))+
#   labs(x = "Generation", y = "Allele")
don= ggplot(simulation_points, aes(x=generation, y=level, color=color),show.legend = FALSE) +
  geom_point(aes(x=generation, y=level, color=color),show.legend = FALSE)

don = don + transition_time(generation) +
  shadow_wake(wake_length = 0.1)+enter_appear()
  # scale_color_viridis(discrete = TRUE) +
  
# p.animation = P +  shadow_wake(wake_length = 0.1)
# animate(don, width=1600, height=400)
# gganimate(p.animation)
  
animate(don, fps = 30, duration = 3,res = 100)
don
# anim_save("./nations.mp4")
# print(f)