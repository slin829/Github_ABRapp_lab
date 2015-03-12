ampfit <- function(freq, ID = "NULL", folder ,system){
  library(ggplot2)
  availf = c("clicks", "4k","8k", "12k","16k","20k","24k","28k")
  final_f=paste(availf, "_f", sep="")
  if(freq %in% final_f){
    stop("NB:Do not read from final ABR -- kanamycin treatment will skew the standardised data")
  }
  if(!freq %in% availf){
    stop("Frequency not available")
  }
  
  if(ID=="NULL"){
    ID=NULL
  }
  var = dir(paste0("./ABR_standards/", system))
  var= as.vector(var)
  #var<- var[var!="A1101"]
  
  Data=vector()
  for (v in var){
    if(!(paste(freq,".csv",sep="")) %in% dir(paste("./ABR_standards/", system, "/",v, sep=""))){
      next
    }
    int_data <- read.csv(paste("./ABR_standards/", system,"/",v,"/",freq,".csv",sep=""), header=TRUE)
    int_data <- cbind(int_data$Sub..ID, int_data$Level.dB., int_data$T1.ms., int_data$V1.uv., int_data$T2.ms., int_data$V2.uv.)
    int_data <- as.data.frame(int_data) 
    colnames(int_data) <- c("Sub_ID", "Sound_level", "Peak_lat", "Peak_amp", "Trough_lat", "Trough_amp")
    Data <- rbind(Data, int_data)
  }
  amp_diff <- Data[,"Peak_amp"]-Data[,"Trough_amp"]
  Data <- cbind(Data, amp_diff)
  Data <- na.omit(Data)
  
  if(!is.null(ID)){
    newdata <- read.csv(paste("./ABR_ID/", folder, "/", ID,"/",freq,".csv", sep=""))
    test_amp_diff <- newdata$V1.uv. - newdata$V2.uv.
    MyTest <- data.frame(newdata$Level.dB., test_amp_diff)
  }
    
  MyMod<- lm(Data$amp_diff~ poly(Data$Sound_level,2), data= Data)
  MyPreds <-data.frame(Data, predict(MyMod, interval="predict", level=0.95))
  
  my_plot<- ggplot(MyPreds, aes(Sound_level, amp_diff)) +
    geom_smooth(method="lm", formula=y~ poly(x,2),  aes(fill="confidence"), alpha=0.3) +
    geom_ribbon(aes(ymin=lwr, ymax=upr, fill="prediction"), data=MyPreds, alpha=0.2) +
    scale_fill_manual('Interval', values = c('green', 'blue')) +
    labs(title= freq) +
       theme(axis.title=element_text(size=20, face="bold"),
          axis.text=element_text(size=20,face="bold"),
          legend.text=element_text(size=15, face="bold"),
          legend.title=element_text(size=15),
          legend.position = "right",
          plot.title= element_text(size=20, face="bold"))
  if(freq=="clicks"){
    my_plot <- my_plot +
      scale_y_continuous(limits=c(-5,8),name="Amplitude (uV)") +
      scale_x_continuous(limits=c(0,90), breaks=c(seq(0, 90, by=20)),name="Sound level (dB)") 
  }else{
    my_plot<- my_plot +
      scale_y_continuous(limits=c(-1,3),name="Amplitude (uV)") +
      scale_x_continuous(limits=c(0,90), breaks=c(seq(0, 90, by=20)),name="Sound level (dB)") 
  }
  
  if(!is.null(ID)){
    my_plot <- my_plot + geom_point(size=3) +
      geom_point(data= MyTest,aes(newdata.Level.dB., test_amp_diff), size=3, colour="red")
  }
  
  my_plot
  
}
