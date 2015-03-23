#
# data <- function(path_to_folder)
# load a data set that could be either the reference data either a new data to analyse, this function load a complete folder
# return a data structure with freq name and data value
# Example: load_data("~/Workspace/R/ABR/ABR_standards/ABR_RZ6_C57 (heating)")

load_data <- function(path_to_folder, recursive=FALSE){
  availf <- c("clicks", "4k","8k", "12k","16k","20k","24k","28k");
  nbrOfF <- length(availf);
  ext <- ".csv";
  mesF = c();
  
  # initiate output
  data <- list( freq_name=vector(), N=vector(), val=list() );
  
  # get all the folder containing all the possible frequencies in the folder
  if( recursive ) {
    animal_folders <- as.vector( dir(path_to_folder) );
  }else{
    animal_folders <- c(".");
  }
  
  # parse each animal folder, find frequency files
  i = 0; last_i = 0;
  for( f in availf ){
    
    Data = vector();
    N = 0;
    for( fold in animal_folders ){
      
      if( (paste0(f,ext)) %in% dir(paste0(path_to_folder,"/",fold)) ){
          #print( paste0("Ready to do ", fold, " and frequence ", f) );
          # there is a file for this frequency
          d = read.csv(paste0(path_to_folder,"/",fold,"/",f,ext), header=TRUE);
          d = cbind(d$Sub..ID, d$Level.dB., d$T1.ms., d$V1.uv., d$T2.ms., d$V2.uv.);
          d = as.data.frame(d);
          colnames(d) <- c("Sub_ID", "Sound_level", "Peak_lat", "Peak_amp", "Trough_lat", "Trough_amp")
          N = N + length(d$Sub_ID);
          Data <- rbind(Data, d);
          
          # check if we already measure this frequency
          if( sum(mesF == f) == 0 ){
            mesF[length(mesF)+1] = f;
          }
          
          if( !recursive ) { i = i + 1; }
      }
     
    }# end for folder
    
    # output
    if( last_i != i){
      data$freq_name=mesF;
      data$N[i] = N;
      data$val[[i]]=Data;
      last_i = i;
    }
    
    if( recursive ){ i = i +1; }
    
  } # end for frequency
  
  
  return(data);
}