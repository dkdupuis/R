#burst
burstFilter <- function(start=6, gravWinLen = 5, winLen = 14, expon = -3.2)
{
    #start = 6
    #gravWinLen = 5
    #winLen = 14
    #expon = -3.2
    
    burstWin <- c((start:(start+gravWinLen-1))^(expon)/(sum((start:(start+gravWinLen-1))^(expon))),rep(0,winLen-gravWinLen))-1/(winLen)
    return(burstWin)
}


#trend/level
trendFilter <- function()
{
#    start = 20
#    gravWinLen = 14
#    winLen = 14
#    expon = -1.5
    
#    trendWin <- c((start:(start+gravWinLen-1))^(expon)/(sum((start:(start+gravWinLen-1))^(expon))),rep(0,winLen-gravWinLen))-1/(winLen)
    trendWin <- c(rep(.1,3),rep(.085,3),rep(0,2),rep(-.085,3),rep(-.1,3))
    return(trendWin)
}

edgeFilter <- function()
{
   return(c(1,-1))
}

edgeFilter3Days <- function()
{
    return(c(1/3,1/3,1/3,-.5,-.5))
}

edgeAndSecondDerivFilter <- function()
{
    return(c(1.125,-1.5,.375))
}

#quartz()
#plot(1:14,burstWin,type = "l")
#plot(1:14,trendWin)