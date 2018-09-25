testAPI <- function (a = 2, b = 10, c = 4){

  a+b+c+1

}

fev1_projection <- function(fev1_0, int_effect, tio="No"){
  
  
  x<-c(0:11)
  
  beta_0<-2.7594
  beta_t<--0.04314
  beta_t2<--0.00093
  v_0<-0.3988
  cov1<--0.00048
  v_t<-0.000941
  v_e<-0.01724
  
  tioBefore <- 0.015
  tioAfter <- 0.022
  
  
  fev1_avg <- c()
  vari <- c()
  obs <- fev1_0
  
  for (i in 1:11){
    
    t1 <- i
    beta_t_x <- 0;
    beta_x_p <- 0;
    beta_x <- int_effect
    
    
    unconditional_mu <- c(
      mu_f=beta_0 + beta_x + beta_t_x + beta_t*t1 + beta_t2*t1*t1,
      mu_0=beta_0 + beta_x_p
    )
    
    v_t_f<- v_0 + t1^2*v_t + 2*t1*cov1
    v_t_0<- v_0 + v_e
    cov_f_0<- v_0 + t1*cov1
    cov_mat<- rbind(c(v_t_f,cov_f_0),c(cov_f_0,v_t_0))
    
    sigma_11<-as.matrix(cov_mat[1,1])
    sigma_12<-as.matrix(t(cov_mat[1,-1]))
    sigma_21<-as.matrix(cov_mat[-1,1])
    sigma_22<-as.matrix(cov_mat[-1,-1])
    
    fev1_avg[i]<-unconditional_mu[1] + sigma_12%*%solve(sigma_22)%*%(obs-unconditional_mu[-1])
    vari[i]<-sigma_11 - sigma_12%*%solve(sigma_22)%*%sigma_21
    if(tio=="Yes"){
      fev1_avg[i] <- fev1_avg[i] + tioBefore*i
    }
  }
  
  
  fev1_avg<-c(fev1_0, fev1_avg)
  
  vari<-c(0,vari)
  
  fev1_up<-fev1_avg+1.96*sqrt(vari)
  fev1_low<-fev1_avg-1.96*sqrt(vari)
  
  df<-data.frame(x, y=fev1_avg, vari, fev1_low, fev1_up)
  names(df) <- c("Time", "FEV1", "vari", "FEV1_lower", "FEV1_upper")
  
  cv1<-sqrt(vari[2:12])/(fev1_avg[2:12]-fev1_0)
  aa1<-rbind(fev1_avg[2:12], fev1_up[2:12], fev1_low[2:12], round(abs(cv1)*100,0))
  
  n_mean1<-(fev1_avg[12]-fev1_0)/11*1000
  n_sd1<-((fev1_avg[12]-fev1_0)/11-(fev1_low[12]-fev1_0)/11)/1.96*1000
  bb1<-data.frame(round(pnorm(-40, n_mean1, n_sd1)*100,0))
  
  df_aa1 <- list("df"=df, "aa1"=aa1, "bb1"=bb1, "options"=1)
  print(df_aa1)
  return(df_aa1)
 
   
  
}


#export
model<-function(setting,input)
{
  n_sim<-setting$n_sim
  time_horizon<-setting$time_horizon
  
  x<-0
  
  for(i in 1:n_sim)
  {
    for(j in 1:time_horizon)
    {
      x<-x+input$mean_age*input$p_female
    }
  }
  
  return(list(x=x))
}



#export
get_default_input<-function(scenario="")
{
  out<-list(mean_age=40, p_female=0.5)
  
  return(out)
}

#export
get_default_setting<-function(scenario="")
{
  out<-list(time_horizon=10,n_sim=100)
  
  return(out)
}
