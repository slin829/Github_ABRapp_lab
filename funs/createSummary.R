#
# createSummary
# function( refData, newData = "NULL" )
# create a summary based on reference data (refData) and new data (newData). The function will return a strcutur
# that contains the result of the summary (OK, KO). If the result is KO thant additional information about the
# frequencies with problems as a list.
# outputs:
# results global information = [OK, KO, noData] the last one referes to the case when no data is selected\
# freqProb = [] list of frequencies requiring attention of the user
# noData = [] list of frequencies that couldn't be compare since they are absent
# metric = double, global metric illustrating how close newData is to refData
# Example: createSummary( refDataID="ABR_RZ6_C57 (heating)", newDataFolder="abr_data_shelly", newDataID="A1101")
#

createSummary <- function( refDataID, newDataFolder="NULL", newDataID = "NULL" ){
  Dir= getwd();
  source(paste0(Dir,'/funs','/load_data.R'));
  
  
  # Initiate the output
  summary <- list(result="noDataSelected", freqProb=c(), noData=c(), metric=.95); 
  
  #
  # init
  #
  refFolder = "ABR_standards";
  newFolder = "ABR_ID"; # TODO make it parameters
  availf = c("clicks", "4k","8k", "12k","16k","20k","24k","28k");
  
  
  # Load the data set to compare with
  if( is.null(newDataID) || is.null(newDataFolder) ){
    # No data to compare with
    summary <- list(result="noData", freqProb=c(), noData = availf, metric=Inf); 
    return( summary );
  }else{
    newData <- load_data( paste0(getwd(),"/",newFolder,"/",newDataFolder,"/",newDataID) );
  }
  
  # Load the reference data set
  refData <- load_data( paste0(getwd(),"/",refFolder,"/",refDataID) , TRUE);
  
  
  #
  # Compare each frequency of the newData to the reference prediction interval
  #
  for( freq in availf ){
    
    if( sum(refData$freq_name == freq) == 0 || sum(newData$freq_name ==  freq) == 0 ){
      # we can't compare since one of the data set doesn't contain the frequency
      summary$noData[length(summary$noData)+1] = freq;
    }
    
    # compare the amplitude
    
    # compare the frequencies
    
  }
  
  return( summary );
  
}