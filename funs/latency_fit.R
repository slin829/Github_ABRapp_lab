#
# latfit 
# function(freq, ID, dataFolder,refFolder)
# creates 95% confidence and prediction interval for wave I latency vs sound intensity base on the system (e.g. TDT system used and animal model)
# ID = plots animal ID of interest in red
# dataFolder = data folder within ABR_ID (for ID plot)
# refFolder = reference folder within ABR standards (for interval calc)
# points = T/F toggles the reference data points on/off

latfit <- function(freq, ID="NULL", dataFolder, refFolder,  ymin= 0, ymax= 8, points="TRUE"){
  library(ggplot2)
  availFreq = c("clicks", "4k","8k", "12k","16k","20k","24k","28k")
  final_f=paste(availFreq, "_f", sep="")
  if(freq %in% final_f){
    stop("NB:Do not read from final ABR -- kanamycin treatment will skew the standardised data")
  }
  if(!freq %in% availFreq){
    stop("Frequency not available")
  }
  if(ID=="NULL"){
    ID=NULL
  }
  
  refID = dir(paste0("./ABR_standards/", refFolder)) #animal IDs for confidence and prediction interval calculation
  
  # dataframe to combine all IDs in refID 
  # for confidence and prediction interval calculation
  Data=vector()
  for (id in refID){
    if(!(paste(freq,".csv",sep="")) %in% dir(paste("./ABR_standards/", refFolder, "/",id, sep=""))){
      next
    }
    int_data <- read.csv(paste("./ABR_standards/", refFolder, "/",id,"/",freq,".csv",sep=""), header=TRUE)
    int_data <- cbind(int_data$Sub..ID, int_data$Level.dB., int_data$T1.ms., int_data$V1.uv., int_data$T2.ms., int_data$V2.uv.)
    int_data <- as.data.frame(int_data) 
    colnames(int_data) <- c("Sub_ID", "Sound_level", "Peak_lat", "Peak_amp", "Trough_lat", "Trough_amp")
    Data <- rbind(Data, int_data)
  }
  Data <- na.omit(Data)
  ##
    
  # Models for interval prediction (polynomial 2)
  # interval prediction method may be 1. "predict or 2. "confidence
  MyMod<- lm(Data$Peak_lat~ poly(Data$Sound_level,2), data= Data)
  MyPreds <-data.frame(Data, predict(MyMod, interval="predict", level=0.95))
  
  # define plot
  my_plot<- ggplot(MyPreds, aes(Sound_level, Peak_lat)) +
      geom_smooth(method="lm", formula=y~ poly(x,2),  aes(fill="confidence"), alpha=0.3) +# include points + CI
      geom_ribbon(aes(ymin=lwr, ymax=upr, fill="prediction"), data=MyPreds, alpha=0.2) + # include pred. intervals
      scale_fill_manual('Interval', values = c('green', 'blue')) +  #set colours
      labs(title= freq) +
      scale_y_continuous(limits=c(ymin,ymax), breaks= c(seq(ymin, ymax, by=2)), name="Latency (ms)") + #set y axis
      scale_x_continuous(limits=c(0,90), breaks=c(seq(0, 90, by=20)),name="Sound level (dB)") + #set x axis
      # set font size
      theme(axis.title=element_text(size=20, face="bold"),
            axis.text=element_text(size=20,face="bold"),
            legend.text=element_text(size=15, face="bold"),
            legend.title=element_text(size=15),
            legend.position = "right",
            plot.title= element_text(size=20, face="bold"))
  
  
  # dataframe for plotting ID of interest
  if(!is.null(ID)){
    IDdata <- read.csv(paste("./ABR_ID/", dataFolder,"/", ID,"/",freq,".csv", sep=""))
  }  
  # plots ref data points if points = TRUE
  if(points==TRUE){
    my_plot <- my_plot + geom_point(size=3) 
  }
  # plots ID of interest if ID is !NULL
  if(!is.null(ID)){ 
    my_plot <- my_plot +  
      geom_point(data= IDdata,aes(Level.dB., T1.ms.), size=3, colour="red") 
  }

  # output plot
  my_plot
  
  
 
}
