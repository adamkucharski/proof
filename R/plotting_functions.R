# Model code to accompany "Proof"
# Author: AJ Kucharski (2022-)


# Helper functions for plotting -------------------------------------------

# Confidence interval as values
c.dist <- function(x,sigF=2){
  bp1=signif(c(median(x),quantile(x,0.025),quantile(x,0.975)),sigF)
  paste(bp1[1]," (95% CrI: ",bp1[2],"-",bp1[3],")",sep="")
}

# Confidence interval as text
c.text <- function(x,sigF=3){
  y <- signif(x,sigF)
  paste(y[1]," (",y[2],"-",y[3],")",sep="")
}

# Plot data and calculate binom CI from values
plot_CI <- function(dates,xx,nn,colA="black",cex_in=1) {
  
  for(ii in 1:length(nn)){
    test_r <- binom.test(xx[ii],nn[ii])
    CI1 <- as.numeric(test_r$conf.int)[1]
    CI2 <- as.numeric(test_r$conf.int)[2]
    points(dates[ii],xx[ii]/nn[ii],col=colA,pch=19,cex=cex_in); 
    lines(c(dates[ii],dates[ii]),c(CI1,CI2),col=colA)
  }
  
}

# Plot data and CI from pre-calculated values
plot_3CI <- function(dates,xx,lower_x,upper_x,colA="black",cex_in=1) {
  set.seed(10)
  
  for(ii in 1:length(xx)){
    noise_x <- runif(1,-0.3,0.3)
    CI1 <- lower_x[ii]
    CI2 <- upper_x[ii]
    points(dates[ii]+noise_x,xx[ii],col=colA,pch=19,cex=cex_in)
    lines(c(dates[ii],dates[ii])+noise_x,c(CI1,CI2),col=colA)
  }
  
}


# Chapter 2 - calculus ----------------------------------------------------

# Plot Koch snowflake 
C2_Koch_snowflake <- function(){

  source("R/set_plot.R")
  par(mfrow=c(1,4),mar=c(3,3,1,1),las=1)
  koch(n = 0, plot = TRUE, res = 500)
  koch(n = 1, plot = TRUE, res = 500)
  koch(n = 2, plot = TRUE, res = 500)
  koch(n = 3, plot = TRUE, res = 500)
  dev.copy(pdf,paste0("plots/C2_koch.pdf",sep=""),width=11,height=3)
  dev.off()
  
}

# Plot Weierstrass function and close up
C2_Weierstrass <- function(){
  
  # Define the function
  weierstrass <- function(x, a, b, n_terms = 500) {
    sum(sapply(0:n_terms, function(n){a^n * cos(b^n*pi*x)}))
  }
  
  # Set parameters for the function
  a <- 0.5
  b <- 3
  
  # Generate x values and corresponding Weierstrass function values
  x_vals <- seq(-2, 2, by=0.001)
  y_vals <- sapply(x_vals, function(x) weierstrass(x, a, b))
  
  # Plot the function
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3,3,1,1),las=1)
  plot(x_vals, y_vals, type="l", main="", xlab="x", ylab="f(x)",bty="l")

  dev.copy(pdf,paste0("plots/C2_weier_1.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
  # Generate subset of values
  x_vals <- seq(1.31, 1.36, by=0.00001)
  y_vals <- sapply(x_vals, function(x) weierstrass(x, a, b))
  
  # Plot the function
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3,3,1,1),las=1)
  
  plot(x_vals, y_vals, type="l", main="", xlab="x", ylab="f(x)",bty="l",lwd=2)
  dev.copy(pdf,paste0("plots/C2_weier_2.pdf",sep=""),width=width.narrow,height=height.square)
  dev.off()
  
  
  
}

# Chapter 3 - Jury theorem ------------------------------------------------

C3_Condorcet_jury <- function() {
  
  # Set up estimates
  p_correct_decision <- function(nn,pp){

    # Calculation majority required for odd and even numbers
    if((nn %% 2) == 0){
      maj_n <- nn/2
      norm_p <- 1-dbinom(nn/2,nn,pp) # normalise by probability a majority reached
      
    }else{
      maj_n <- ceiling(nn/2)
      norm_p <- 1
    }
    
    # Calculate correct majority decision
    probability_wrong <-  pbinom(maj_n-1,nn,pp)/norm_p
    
    (1 - probability_wrong)
    
  }
  
  # Calculate probability correct overall for different numbers of jurors
  xx <- 1:20
  est_correct_p1 <- sapply(xx,function(x){p_correct_decision(x,0.8)})
  est_correct_p2 <- sapply(xx,function(x){p_correct_decision(x,0.6)})
  est_correct_n1 <- sapply(xx,function(x){p_correct_decision(x,0.4)})
  est_correct_n2 <- sapply(xx,function(x){p_correct_decision(x,0.2)})
  c.list <- c(0,0.25,0.5,0.75)
  text_x <- 10
    
  # Plot probability
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3,4,1,1),las=1)
  
  plot(xx,est_correct_p1,type="l",
       ylim=c(0,1),yaxs="i",xaxt="n",yaxt="n",ylab="",
       xlab="",main="",col=rgb(c.list[1],c.list[1],c.list[1]),lwd=2,bty="l")
  lines(xx,est_correct_p2,lwd=2,col=rgb(c.list[2],c.list[2],c.list[2]))
  lines(xx,est_correct_n1,lwd=2,col=rgb(c.list[3],c.list[3],c.list[3]))
  lines(xx,est_correct_n2,lwd=2,col=rgb(c.list[4],c.list[4],c.list[4]))
  axis(2, at=seq(0,1,0.1),labels = paste0(seq(0,100,10),"%"),col = "black") 
  axis(1, at=seq(0,max(xx),5),labels = seq(0,max(xx),5),col = "black") 
  title(ylab="probability majority decision is correct", line=3, cex.lab=1)
  title(xlab="number of jurors", line=2, cex.lab=1)
  
  graphics::text(x=text_x,y=0.95,labels="80% probability individual correct",cex=0.8,adj=0.05, col=rgb(c.list[1],c.list[1],c.list[1]))
  graphics::text(x=text_x,y=0.68,labels="60% probability individual correct",cex=0.8,adj=0.05,col=rgb(c.list[2],c.list[2],c.list[2]))
  graphics::text(x=text_x,y=0.3,labels="40% probability individual correct",cex=0.8,adj=0.05,col=rgb(c.list[3],c.list[3],c.list[3]))
  graphics::text(x=text_x,y=0.05,labels="20% probability individual correct",cex=0.8,adj=0.05,col=rgb(c.list[3],c.list[3],c.list[3]))
  
  
  dev.copy(pdf,paste("plots/C3_condorcet.pdf",sep=""),width=width.main,height=height.square)
  dev.off()
  
}

# Chapter 5 ---------------------------------------------------------------

# Delta analysis
C5_Delta <- function(){
  
  # Load datasets
  india_red_list <- as.Date("2021-04-23")
  
  phe_data_1 <- readxl::read_xlsx("data/covid_variants/Variants_of_Concern_Technical_Briefing_10_Data_England-1.xlsx",sheet="Fig10") |> 
                  rename(Indicator=`Travel Indicator`, Cases=`Number of VUI-21APR-01 cases`)
  phe_data_2 <- readxl::read_xlsx("data/covid_variants/Variants_of_Concern_Technical_Briefing_10_Data_England-1.xlsx",sheet="Fig12") |> 
                  rename(Indicator=`Travel Indicator`, Cases=`Number of VOC-21APR-02 cases`)

  phe_data_1_t <- phe_data_1 |> group_by(Date) |> summarise(n=sum(Cases))
  phe_data_1_u <- phe_data_1 |> filter(Indicator=="Not travel-associated" | Indicator=="Under investigation" )  |> group_by(Date) |> summarise(n=sum(Cases))
  phe_data_1_n <- phe_data_1 |> filter(Indicator=="Not travel-associated" )
  
  phe_data_2_t <- phe_data_2 |> group_by(Date) |> summarise(n=sum(Cases))
  phe_data_2_u <- phe_data_2 |> filter(Indicator=="Not travel-associated" | Indicator=="Under investigation" )  |> group_by(Date) |> summarise(n=sum(Cases))
  phe_data_2_n <- phe_data_2 |> filter(Indicator=="Not travel-associated" )

  # Import outbreakinfo dataset on variants
  variants_india <- read_tsv("data/covid_variants/outbreakinfo_mutation_report_data_2021-05-07.tsv")
  
  # Import covidregionaldata dataset on India cases
  data_india <- read_csv("data/covid_variants/all_india_12May_read.csv")
  data_india_may <- data_india |> filter(date>"2021-03-01" & date<="2021-05-07") # up to 7th May

  x_range <- c(as.Date("2021-03-01"),as.Date("2021-05-05")) # set range
  
  # Plot India cases
  source("R/set_plot.R")
  par(mfrow=c(1,3),mar=c(2.5,4,1,1),las=1)
  
  grey1 <- 0.1
  grey2 <- 0.5
  grey3 <- 0.9
  grey3t <- 0.7
  col.list <- list(rgb(grey1,grey1,grey1),rgb(grey2,grey2,grey2),rgb(grey3,grey3,grey3),
                   rgb(grey3t,grey3t,grey3t))
  
  # Plot UK cases
  plot(data_india_may$date,data_india_may$cases_new/1e3,type="l",
       xlim=x_range,ylim=c(0,5e2),yaxs="i",xaxt="n",yaxt="n",ylab="",
       xlab="",main="Cases in India",bty="l",lwd=2)
  lines(c(india_red_list,india_red_list),c(0,1e7),col="grey",lty=2)
  axis(2, at=seq(0,500,100),labels = seq(0,500,100),col = "black") 
  axis(1, at=as.Date(c("2021-03-01","2021-04-01","2021-05-01")),labels = c("March","April","May"),col = "black") 
  title(ylab="daily reported cases (thousands)", line=3, cex.lab=1)

  # Plot B.617.1
  y_max <- 18
  plot(as.Date(phe_data_1_t$Date),phe_data_1_t$n,type="l",
       xlim=x_range,ylim=c(0,y_max),yaxs="i",xaxt="n",yaxt="n",ylab="",
       xlab="",main="B.1.617.1 cases in England",col="white",bty="l")
  lines(c(india_red_list,india_red_list),c(0,1e7),col="grey",lty=2)
  axis(2, at=seq(0,y_max,2),labels = seq(0,y_max,2),col = "black") 
  axis(1, at=as.Date(c("2021-03-01","2021-04-01","2021-05-01")),labels = c("March","April","May"),col = "black") 
  title(ylab="daily reported cases", line=2, cex.lab=1)
  
  plot_lines(as.Date(phe_data_1_t$Date),phe_data_1_t$n,col=col.list[[1]],widthf=1)
  plot_lines(as.Date(phe_data_1_u$Date),phe_data_1_u$n,col=col.list[[2]],widthf=1)
  plot_lines(as.Date(phe_data_1_n$Date),phe_data_1_n$Cases,col=col.list[[3]],widthf=1)
  
  graphics::text(x=x_range[1],y=16,labels="Traveler or contact of traveler",cex=0.9,adj=0,col=col.list[[1]])
  graphics::text(x=x_range[1],y=14.5,labels="Under investigation",cex=0.9,adj=0,col=col.list[[2]])
  graphics::text(x=x_range[1],y=13,labels="Not travel-related",cex=0.9,adj=0,col=col.list[[4]])
  
  # Plot B.617.2
  y_max <- 60
  plot(as.Date(phe_data_2_t$Date),phe_data_2_t$n,type="l",
       xlim=x_range,ylim=c(0,y_max),yaxs="i",xaxt="n",yaxt="n",ylab="",
       xlab="",main="B.1.617.2 cases in England",col="white",bty="l")
  lines(c(india_red_list,india_red_list),c(0,1e7),col="grey",lty=2)
  axis(2, at=seq(0,y_max,5),labels = seq(0,y_max,5),col = "black") 
  axis(1, at=as.Date(c("2021-03-01","2021-04-01","2021-05-01")),labels = c("March","April","May"),col = "black") 
  title(ylab="daily reported cases", line=2, cex.lab=1)
  
  plot_lines(as.Date(phe_data_2_t$Date),phe_data_2_t$n,col=col.list[[1]],widthf=1)
  plot_lines(as.Date(phe_data_2_u$Date),phe_data_2_u$n,col=col.list[[2]],widthf=1)
  plot_lines(as.Date(phe_data_2_n$Date),phe_data_2_n$Cases,col=col.list[[3]],widthf=1)

  dev.copy(pdf,paste("plots/C5_COVID.pdf",sep=""),width=7.5,height=2)
  dev.off()
  
}

# Estimative probability plot
# Credit: https://github.com/zonination/perceptions

C5_estimative <- function(){
  
  source("R/percept.R")
  
}


# Chapter 6  --------------------------------------------------------------

# Probability of p-value < 0.05 when multiple hypotheses tested
C6_multiple_tests <- function() {

  # Set up estimates
  p_significant <- function(nn,pp=0.05){
    1-(1-pp)^nn
  }
  
  xx <- 0:30

  # Plot probability
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3,4,1,1),las=1)
  
  plot(xx,p_significant(xx),type="l",
       ylim=c(0,1),yaxs="i",xaxs="i",xaxt="n",yaxt="n",ylab="",
       xlab="",main="",col="black",lwd=2,bty="l")
  
  axis(2, at=seq(0,1,0.1),labels = paste0(seq(0,100,10),"%"),col = "black") 
  axis(1, at=seq(0,max(xx),5),labels = seq(0,max(xx),5),col = "black") 
  title(ylab=expression("probability of getting "*italic(p)*"-value below 5%"), 
        line=3, cex.lab=0.8)
  
  title(xlab="hypotheses tested", line=2, cex.lab=1)

  dev.copy(pdf,paste("plots/C6_pvalue.pdf",sep=""),width=width.narrow,height=height.main)
  dev.off()
  
}

# Changes in effect sizes before-and-after pre-registration

C6_registration <- function() {

  # Load data
  data_in <- read.csv("data/extracted_numerical_values.csv")
  
  # Plot probability
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3,4,1,1),las=1)
  
  x_date <- data_in$pub_year
  
  # Remove outlier (as in paper)
  data_in <- data_in[-7,]
  #data_in <- data_in |> filter(data_in$pub_year<2000)
  
  plot(data_in$pub_year,-1+0*data_in$Value,type="l",
       ylim=c(0,1.8),xlim=c(1974,2015),yaxs="i",xaxs="i",xaxt="n",yaxt="n",ylab="",
       xlab="",main="",col="black",lwd=2,bty="l")
  
  lines(c(1970,2020),c(1,1),lty=1,col="light gray")
  lines(c(2000,2000),c(-1,3),lty=2)
  plot_3CI(data_in$pub_year,data_in$Value,data_in$Lower_CI,data_in$Upper_CI,cex=0.8)
  
  axis(2, at=seq(0,2,0.2),labels = paste0(c(seq(-100,0,20),seq(20,100,20)),"%"),col = "black") 
  axis(1, at=seq(min(x_date),max(x_date),5),labels = seq(min(x_date),max(x_date),5),col = "black") 
  title(ylab="difference in risk", line=3, cex.lab=1)
  title(xlab="date of publication", line=2, cex.lab=1)
  
  graphics::arrows(x0=1975,x1=1975,y0=0.5,y1=0.1,length=0.1,lwd=2,col="dark grey")
  graphics::text(x=1976,y=0.2,labels="more beneficial",adj=0,col="dark grey")
  
  graphics::arrows(x0=1975,x1=1975,y0=1.3,y1=1.7,length=0.1,lwd=2,col="dark grey")
  graphics::text(x=1976,y=1.6,labels="more harmful",adj=0,col="dark grey")
  
  graphics::text(labels="Preregistration required",x=2000.2,y=1.72,adj=0) #\u00ad
  
  dev.copy(pdf,paste("plots/C6_trials.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
}


