library(ggplot2)
library(gridExtra)
# source("mlxmcmc1.R")
# source("model.R")


ggplotmlx <- function(...) {
  ggplot(...) + theme_bw() + theme(plot.background = element_rect(fill=rgb(1,1,1))) 
}


shinyServer(function(input, output) {
  
  d <- read.csv(file="pkdata.txt", header=TRUE, sep="\t")
  d.time <- d$time
  n <- length(d$time)
  
  c.time <- seq(0,100,by=0.25)
  
  pk.prior <- list( name = c('ka','V','Cl'),
                    reference = c(1, 0.1, 0.01),
                    sd = c(0.2, 0.2, 0.2)*0.1)
  
  
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(d))
  )
  
  
  r0 <- reactive({
    muphi <- log(c(input$ka,input$V,input$Cl))
    chol.omega <- c(input$oka,input$oV,input$oCl)
    n.simul <- 1000   
    PSI = matrix(0,nrow=n.simul,ncol=length(muphi))
    
    for (k in (1:n.simul)){
      phi <- muphi + chol.omega*rnorm(3)
      PSI[k,] <- exp(phi)
    }
    PSI <- data.frame(PSI)
    names(PSI) <- c('ka','V','Cl')
    r=list(P=PSI, p=exp(muphi))
    return(r)
  })
  
  r1 <- reactive({
    keep    <- d[vals$keeprows, , drop = FALSE]
    l0 <- log(c(0.1,1,0.04,2))
    pk.nlm1 <- nlm(fmin1, l0, keep$y, keep$time, hessian="true")
    muphi <- pk.nlm1$estimate[1:3]
    solve.omega <- solve(pk.nlm1$hessian)*input$sigma^2
    chol.omega <- chol(solve.omega[1:3,1:3])
    n.simul <- 1000   
    PSI = matrix(0,nrow=n.simul,ncol=length(muphi))    
    for (k in (1:n.simul)){
      phi <- muphi + chol.omega%*%rnorm(3)
      PSI[k,] <- t(exp(phi))
    }
    PSI <- data.frame(PSI)
    names(PSI) <- c('ka','V','Cl')
    r=list(P=PSI, p=exp(muphi), C=solve.omega[1:3,1:3])
    return(r)
  })
  
  r2 <- reactive({
    input$run
    keeprows <- vals$keeprows
    niter <- isolate(input$niter)
    pk.control <- list(niter=niter, nkernel=c(2,1))
    
    keep    <- d[keeprows, , drop = FALSE]
    exclude <- d[!keeprows, , drop = FALSE]
    
    pk.prior$reference <- (c(input$ka,input$V,input$Cl))
    pk.prior$sd <- (c(input$oka,input$oV,input$oCl))
    sigma <- (input$sigma)
    if (length(which(pk.prior$sd>0))>0){
      res <- mlxmcmc1(model="pk_1cpt",
                      data=keep,
                      xi=c(sigma,0),
                      prior=pk.prior,
                      control=pk.control)
      
      pmap <- mlxoptim(model="pk_1cpt",
                       data=keep,
                       xi=c(sigma,0),
                       prior=pk.prior)
    }else{
      res <- matrix(rep(pk.prior$reference,niter),nrow=niter,byrow=TRUE)
      pmap <- pk.prior$reference
    }
    p.table <- data.frame(res)
    names(p.table) <- pk.prior$name
    r=list(P=p.table, p=pmap)
    return(r)
  })
  
  
  output$plot1 <- renderPlot({
    pr <- 1 -input$predint/100
    probs <- c(pr/2, 1-pr/2)
    
    r <- r0()
    S <- do.call("pkv_1cpt",list(r$P,c.time))
    s <- apply(S,2,function(x) quantile(x,probs=probs))
    bd0 <- data.frame(time=c.time,s1=s[1,],s2=s[2,])
    s <- do.call("pk_1cpt",list(r$p,c.time))
    C0 <- data.frame(time=c.time,C=s)
    
    r <- r1()
    S <- do.call("pkv_1cpt",list(r$P,c.time))
    s <- apply(S,2,function(x) quantile(x,probs=probs))
    bd1 <- data.frame(time=c.time,s1=s[1,],s2=s[2,])
    s <- do.call("pk_1cpt",list(r$p,c.time))
    C1 <- data.frame(time=c.time,C=s)
    
    r <- r2()
    r$P <- isolate(r$P[(input$nburn+1):input$niter,])
    S <- do.call("pkv_1cpt",list(r$P,c.time))
    s <- apply(S,2,function(x) quantile(x,probs=probs))
    bd2 <- data.frame(time=c.time,s1=s[1,],s2=s[2,])
    s <- do.call("pk_1cpt",list(r$p,c.time))
    C2 <- data.frame(time=c.time,C=s)
    
    keep    <- d[vals$keeprows, , drop = FALSE]
    exclude <- d[!vals$keeprows, , drop = FALSE]
    
    vc=c(rgb(1,0.,0.),rgb(1,0.8,0.3),rgb(0.3,0.3,0.7),rgb(0.3,0.7,0.3),rgb(0.7,0.3,0.3))
    names(vc)=letters[1:5]
    lc <- c("used data", "unused data", "prior median",
            "LS estimate", "posterior median")
    vdisp <- rep(FALSE,5)
    
    pl <-   ggplotmlx()  
    if (input$disp_prior){
      if (input$disp_pint0)
        pl <- pl + geom_ribbon(data=bd0,aes(x=time,ymin=s1,ymax=s2), fill=rgb(0.8,0.8,0.9)) 
      pl <- pl + geom_line(data=C0, aes(x=time, y=C, colour="c")) 
      vdisp[3] <- TRUE}
    
    if (input$disp_cond){
      if (input$disp_pint1)
        pl <- pl + geom_ribbon(data=bd1,aes(x=time,ymin=s1,ymax=s2), fill=rgb(0.8,0.9,0.8)) 
      pl <- pl + geom_line(data=C1, aes(x=time, y=C, colour="d")) 
      vdisp[4] <- TRUE}
    
    if (input$disp_pmedian){
      if (input$disp_pint2)
        pl <- pl + geom_ribbon(data=bd2,aes(x=time,ymin=s1,ymax=s2), fill=rgb(0.9,0.8,0.8)) 
      pl <- pl + geom_line(data=C2, aes(x=time, y=C, colour="e")) 
      vdisp[5] <- TRUE}
    
    if (input$disp_data){
      pl <- pl + geom_point(data=keep, aes(time, y, colour="a"), size=3)
      vdisp[1] <- TRUE}
    if (input$disp_exclude){
      pl <- pl + geom_point(data=exclude, aes(time, y, colour="b") )
      vdisp[2] <- TRUE}
    
    if (any(vdisp)){
      pl <- pl + scale_colour_manual(values=vc[vdisp], labels=lc[vdisp])
      pl <- pl + theme(legend.position=c(0.8,0.85), legend.justification=c(0,1), legend.title=element_blank())
    }
    return(pl) 
  })
  
  observeEvent(input$N, {
    res = rep(FALSE, n)
    L <- round(n/as.numeric(input$N))
    i <- seq(floor(L/2),n,by=L)   
    res[i] <- TRUE
    vals$keeprows <- res
  })
  
  observeEvent(input$plot1_click, {
    res <- nearPoints(d, input$plot1_click, yvar="y", allRows=TRUE)
    if (input$add=="1")
      vals$keeprows[res$selected_] <- TRUE
    else
      vals$keeprows[res$selected_] <- FALSE
    #     vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$plot1_brush, {
    res <- brushedPoints(d, input$plot1_brush, yvar="y", allRows=TRUE)
    if (input$add=="1")
      vals$keeprows[res$selected_] <- TRUE
    else
      vals$keeprows[res$selected_] <- FALSE
    #     vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  observeEvent(input$exclude_reset, {
    res = rep(FALSE, n)
    L <- round(n/as.numeric(input$N))
    i <- seq(floor(L/2),n,by=L)   
    res[i] <- TRUE
    vals$keeprows <- res
  })
  
  output$plot2 <- renderPlot({
    pk.p <- log(c(input$ka,input$V,input$Cl))
    pk.sd <- c(input$oka,input$oV,input$oCl)
    pk.names <- c('ka','V','Cl')
    p2 <- r2()$P
    p2 <- isolate(p2[(input$nburn+1):input$niter,])
    p0 <- r0()$P
    p <- rbind(p0,p2)
    x1 <- sapply(p,min)
    x2 <- sapply(p,max)
    n <- 200
    pl <- list()
    for (k in (1:3)){
      plk <- ggplotmlx()
      
      plk <- plk  + ylab("") + xlab(pk.names[k])
      if (input$dens_post){
        dk <- density(p2[[k]],adjust=2)
        dk <- data.frame(x=dk$x,y=dk$y)
        plk <- plk  + geom_line(data=dk,aes(x,y,colour="posterior"))
        #       plk <- plk  + geom_line(aes(x,y),colour=rgb(0.7,0.3,0.3))
      }
      if (input$dens_prior){
        xk <- seq(x1[k],x2[k],length.out=n)
        dlk <- dlnorm(xk,meanlog=pk.p[k],sdlog=pk.sd[k])
        dp <- data.frame(x=xk,y=dlk)
        plk <- plk + geom_line(data=dp, aes(x=x,y=y,colour="prior"))
        #         plk <- plk + geom_line(data=dp, aes(x=x,y=y), colour=rgb(0.3,0.3,0.7))
      }
      if (k==1)
        plk <- plk + theme(legend.position=c(0.9,0.6),legend.title=element_blank())
      else
        plk <- plk + theme(legend.position="none") 
      #       plk <- plk + scale_colour_discrete(name  ="distribution", labels=c("Prior", "Posterior"))
      pl[[k]] <- plk
      
    }
    grid.arrange(pl[[1]],pl[[2]],pl[[3]])
  })
  
  output$plot3 <- renderPlot({
    p <- r2()$P
    pk.prior$sd <- c(input$oka,input$oV,input$oCl)
    ind.eta = which(pk.prior$sd>0)
    nb.etas <- length(ind.eta)
    niter <- dim(p)[1]
    p$iteration <- (1:niter)
    #     p1 <- qplot((1:niter),ka,data=p,colour="blue")
    p1 <- ggplotmlx(data=p) + geom_line(aes(x=iteration,y=ka))
    p2 <- ggplotmlx(data=p) + geom_line(aes(x=iteration,y=V))
    p3 <- ggplotmlx(data=p) + geom_line(aes(x=iteration,y=Cl))
    grid.arrange(p1,p2,p3)
  })
  
  output$tablep <- renderTable({ 
    r <- matrix(ncol=3,nrow=3)
    r[,1]=r1()$p
    r[,2]=r0()$p
    r[,3]=r2()$p
    r <- data.frame(r)
    names(r) <- c('LS','prior','MAP')
    rownames(r) <- c('ka','V','Cl')
    return(r)
  },include.colnames=TRUE)
  
  output$tableC1 <- renderTable({ 
    r <- r1()$C
    colnames(r) <- c('ka','V','Cl')
    rownames(r) <- c('ka','V','Cl')
    return(r)
  },include.colnames=TRUE,digits=4)
  
  output$tableC2 <- renderTable({ 
    t2 <- r2()$P
    r <- log(t2[(input$nburn+1):length(t2[[1]]),])
    vr <- var(r)
    return(vr)
  },include.colnames=TRUE,digits=4)
  
  output$tableC0 <- renderTable({ 
    r <- matrix(ncol=3,nrow=3)
    r[1,1]=input$oka^2
    r[2,2]=input$oV^2
    r[3,3]=input$oCl^2
    r <- data.frame(r)
    names(r) <- c('ka','V','Cl')
    rownames(r) <- c('ka','V','Cl')
    return(r)
  },include.colnames=TRUE,digits=4)
  
})


mlxmcmc1 <- function(model=NULL,data=NULL,xi=NULL,prior=NULL,control=NULL)
{
  if (is.null(control))
    control=list()
  
  if (is.null(control$niter))
    control$niter = 100
  
  if (is.null(control$nkernel))
    control$nkernel = c(2,2)
  
  if (is.null(control$stepsize.rw))
    control$stepsize.rw=0.4
  
  if (is.null(control$proba.mcmc))
    control$proba.mcmc=0.4
  
  if (is.null(control$nb.max))
    control$nb.max=3
  
  t.obs <- data$time
  y.obs <- data$y
  p0=exp(prior$reference)
  y.pred <- do.call(model,list(p0,t.obs))
  
  ind.eta = which(prior$sd>0)
  nb.etas <- length(ind.eta)
  if (nb.etas>1){
    Omega.eta <- diag(prior$sd[ind.eta]^2)
    chol.omega <- try(chol(Omega.eta))
    solve.omega<- try(solve(Omega.eta))
    domega <- diag(Omega.eta)*0.3;
  }else{
    Omega.eta <- prior$sd[ind.eta]^2
    chol.omega <- prior$sd[ind.eta]
    solve.omega<- 1/Omega.eta  
    domega <- Omega.eta*0.3;
  }
  
  
  
  #   cat("Running Metropolis-Hastings algorithm\n")
  #   print(date())
  
  mean.phi <- log(prior$reference)
  psi <- prior$reference
  phi <- log(psi)
  f <- do.call(model,list(psi,t.obs))
  g <- error.model(f,xi)
  U.y <- sum(0.5 * ((y.obs - f)/g)^2 + log(g))
  eta <- phi[ind.eta] - mean.phi[ind.eta]
  phic <- phi  
  
  PSI = matrix(0,nrow=control$niter+1,ncol=length(psi))
  PSI[1,]=psi
  
  for (kiter in 1:control$niter)
  {
    #print(kiter)
    if (control$nkernel[1] > 0) 
    {
      for (u in 1:control$nkernel[1]) 
      {
        etac <- as.vector(chol.omega%*%rnorm(nb.etas))
        phic[ind.eta] <- mean.phi[ind.eta] + etac
        psic <- exp(phic)
        f <- do.call(model,list(psic,t.obs))
        g <- error.model(f,xi)
        Uc.y <- sum(0.5 * ((y.obs - f)/g)^2 + log(g))
        deltu <- Uc.y - U.y
        if(deltu < (-1) * log(runif(1))) 
        {
          eta <- etac
          phi <- phic
          U.y <- Uc.y 
        }
      }
    }
    
    if (control$nkernel[2] > 0) 
    {
      nb.max <- min(nb.etas,control$nb.max)
      nbc2<-nt2 <- replicate(nb.etas,0)
      U.eta <- 0.5 * eta %*% solve.omega %*% eta
      for (u in 1:control$nkernel[2]) 
      {
        for (nrs2 in 1:nb.max)
        {
          for (j in 1:nb.etas)
          {
            jr <-  sample(c(1:nb.etas), nrs2)
            jr <- jr -jr[1] + j
            vk2 <- jr%%nb.etas + 1
            etac <- eta
            etac[vk2] <- eta[vk2] + rnorm(nrs2)*domega[vk2]
            phic[ind.eta] <- mean.phi[ind.eta] + etac
            psic <- exp(phic)
            f <- do.call(model,list(psic,t.obs))
            g <- error.model(f,xi)
            Uc.y <- sum(0.5 * ((y.obs - f)/g)^2 + log(g))
            Uc.eta <- 0.5 * etac %*% solve.omega %*% etac
            deltu <- Uc.y - U.y + Uc.eta - U.eta
            if(deltu < (-1) * log(runif(1))) 
            {
              eta <- etac
              U.y <- Uc.y
              U.eta <- Uc.eta
              phi[ind.eta]=phic[ind.eta]
              nbc2[vk2] <- nbc2[vk2]+1
            }       
            nt2[vk2] <- nt2[vk2] + 1
          }
        }
      }
      domega <- domega*(1 + control$stepsize.rw*(nbc2/nt2 - control$proba.mcmc))
    }
    PSI[kiter+1,] <- exp(phi)
  }
  return(PSI)
}


#--------------------------------------------
mlxoptim <- function(model=NULL,data=NULL,xi=NULL,prior=NULL,initial=NULL)
{
  errpred <- function(phi,model,y,t,muphi,ind.eta,xi,solve.omega){
    psi <- exp(muphi)
    psi[ind.eta] <- exp(phi)
    eta <- phi-muphi[ind.eta]
    f <- do.call(model,list(psi,t.obs))
    g <- error.model(f,xi)
    U.y <- sum(0.5 * ((y.obs - f)/g)^2 + log(g))
    U.eta <- 0.5 * eta %*% solve.omega %*% eta
    e <- U.y + U.eta 
    return(e)}
  
  t.obs <- data[,1]
  y.obs <- data[,2]
  ind.eta = which(prior$sd>0)
  nb.etas <- length(ind.eta)
  if (nb.etas>1){
    Omega.eta <- diag(prior$sd[ind.eta]^2)
    solve.omega<- try(solve(Omega.eta))
  }else{
    Omega.eta <- prior$sd[ind.eta]^2
    solve.omega<- 1/Omega.eta  
  }
  l0 <-log(prior$reference[ind.eta]) 
  muphi=log(prior$reference)
  if (nb.etas>1){
    r <- optim(l0,errpred,model=model,y=y.obs,t=t.obs,muphi=muphi,ind.eta=ind.eta,xi=xi,solve.omega=solve.omega,hessian=TRUE)
  }else{
    r <- optim(l0,errpred,model=model,y=y.obs,t=t.obs,muphi=muphi,ind.eta=ind.eta,xi=xi,solve.omega=solve.omega,
               method="Brent",lower=-5, upper=5)
  }
  pest <- exp(r$par)
  return(pest)
}

pk_1cpt <- function(x,t) {
  ka <- x[1]
  V<- x[2] 
  k <- x[3]/V
  C <- 100*ka/(V*(ka-k))*(exp(-k*t)-exp(-ka*t))
  return(C)
}

pkv_1cpt <- function(x,time) {
  ka <- x[,1]
  V<- x[,2] 
  k <- x[,3]/V
  C <- 100*ka/(V*(k-ka))*(exp(-ka%*%t(time))-exp(-k%*%t(time)))
  return(C)
}

error.model <- function(f,xi){
  g <- xi[1] + xi[2]*f
  return(g) 
}

fmin1 <- function(x,y,t){
  z=exp(x)
  f=pk_1cpt(z[1:3],t)
  g=z[4]
  e=sum( ((y-f)/g)^2 + log(g^2))
  return(e)
}