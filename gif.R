################################################################################
# fix sigma = 0.5, let gamma to 0.
################################################################################

rm(list = ls())
require(animation)
require(tidyverse)
require(latex2exp)

sig <- 0.5
saveGIF({ 
for (gam in c(seq(0.9, 0.001, -0.005),0)) {
  a1 <- sqrt(gam)*(1-sig)/(1+sig)
  b1 <- sqrt(1+sig-gam*sig)/(1+sig)
  x1 <- c(seq(a1-2*b1,2*b1+a1,0.05 ),2*b1+a1)
  
  repeat{
    temp <- 4*b1^2 -(x1-a1)^2; temp[(abs(temp) < 10e-5)] <- 0
    gam <- gam + 0.0001
    cat(gam, "\r")
    if(length(temp[temp >= 0]) == length(temp)) {
      break
    }
  }
  y1 <- (1+sig) *sqrt(temp)/(2*pi)/((sqrt(gam)*x1+1 )*(1- sig*sqrt(gam)*x1) )

  
  p <- data.frame(x=x1, mu=y1) %>% ggplot(aes(x, mu)) +
    geom_line(color="red") + coord_fixed(ratio=1, xlim =c(-3,3), ylim =c(0,3)) +
    labs(x=TeX('$x$'), y=expression(tilde(h)[paste(gamma,",",sigma)](x) ) ,title=TeX("$\\gamma = a,\\sigma = b$", user_defined=list("a"=round(gam,2),"b"=sig) )  )  +
    theme_bw() +
    theme(plot.title=element_text(family="Arial-Black", size=rel(1.2), hjust=0.5))
  print(p)
}
},'gamto0.gif',interval = 0.01 )

################################################################################
# fix gam = 0.5, let sig to 0.
################################################################################

rm(list = ls())
require(animation)
require(tidyverse)
require(latex2exp)

gam <- 0.5
saveGIF({ 
  for (sig in c(seq(1, 0.001, -0.005),0)) {
    a1 <- sqrt(gam)*(1-sig)/(1+sig)
    b1 <- sqrt(1+sig-gam*sig)/(1+sig)
    x1 <- c(seq(a1-2*b1,2*b1+a1,0.05 ),2*b1+a1)
    
    repeat{
      temp <- 4*b1^2 -(x1-a1)^2; temp[(abs(temp) < 10e-5)] <- 0
      sig <- sig + 0.0001
      cat(sig, "\r")
      if(length(temp[temp >= 0]) == length(temp)) {
        break
      }
    }
    y1 <- (1+sig) *sqrt(temp)/(2*pi)/((sqrt(gam)*x1+1 )*(1- sig*sqrt(gam)*x1) )
    
    
    p <- data.frame(x=x1, mu=y1) %>% ggplot(aes(x, mu)) +
      geom_line(color="red") + coord_fixed(ratio=1, xlim =c(-3,3), ylim =c(0,3)) +
      labs(x=TeX('$x$'), y=expression(tilde(h)[paste(gamma,",",sigma)](x) ) ,title=TeX("$\\gamma = a,\\sigma = b$", user_defined=list("a"=gam,"b"=round(sig,2)) )  )  +
      theme_bw() +
      theme(plot.title=element_text(family="Arial-Black", size=rel(1.2), hjust=0.5))
    print(p)
  }
},'sigto0.gif',interval = 0.01 )
