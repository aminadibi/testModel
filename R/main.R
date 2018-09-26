
get_version<-function()
{
  return("1.2")
}




#' @export
model_run<-function(setting,input)
{
  n_sim<-setting$n_sim
  time_horizon<-setting$time_horizon
  
  x<-0
  
  for(i in 1:n_sim)
  {
    for(j in 1:time_horizon)
    {
      x<-x+input$mean_age*input$p_female
      temp<-rnorm(n_sim*time_horizon,0,1)
    }
  }
  
  return(list(x=x))
}



#' @export
get_default_input<-function(scenario="")
{
  out<-list(
    mean_age=40,
    p_female=0.5,
    scenario=scenario
  )
  
  return(out)
}

#' @export
get_default_setting<-function(scenario="")
{
  out<-list(
    time_horizon=10,
    n_sim=100,
    countries=c("Iran","Canada"),
    codes=c(1,2,3)
  )
  
  return(out)
}


