#"Computer Methods for Mathematical Computations",
# by Forsythe, Malcolm and Moler, published by Prentice-Hall in 1977.
# G. Forsythe, M. Malcolm, and C. Moler. Computer Methods for
# Mathematical Computations. Prentice Hall, Inc., 1977.
# http://www.mathworks.com/products/matlab/demos.html?
#  file=/products/demos/shipping/matlab/census.html
time = seq(1900,2000,by=10)
pop = c(75.995,91.972,105.711,123.203,131.669,
     150.697,179.323,203.212,226.505,249.633,281.422)
plot(time,pop,xlim=c(1900,2020),ylim=c(0,400))
sct = (time-1950)/50
orders = c(1:4,8,9)
polynoms = lapply(orders,
   function(i) lm(pop~poly(sct,i,raw=TRUE)))
ptime = 1900:2020
invisible(mapply(function(m,i) lines(ptime,predict(m,newdata=data.frame(sct=(ptime-1950)/50)),
             lty=i),polynoms,1:length(orders)))
             
pfun <-  function(z) {
     predict(polynoms[[5]],
           newdata=data.frame(sct=rep((z-1950)/50,20)))[20]
}
u1 = uniroot(pfun, interval=c(1990,2030))$root
yr = round(u1)
fracyr = u1 %% 1
month = floor(fracyr*12)
t2 = (fracyr-jdate/365)*365*24
hrs = floor(t2*24)
mins = (t2-hrs/24)*24*60
@
#\caption{The dangers of polynomial fitting: doomsday (according
#to extrapolation of an 8th-order polynomial fit) arrives on \ldots}
#\end{figure}
