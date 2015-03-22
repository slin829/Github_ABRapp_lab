#
# createSummary
# function( refData, newData = "NULL" )
# create a summary based on reference data (refData) and new data (newData). The function will return a strcutur
# that contains the result of the summary (OK, KO). If the result is KO thant additional information about the
# frequencies with problems as a list.
# Example: createSummary( refData="ABR_RZ6_C57", newData="")
#

createSummary <- function( refData, newData = "NULL" ){
  Dir= getwd();
  source(paste0(Dir,'/funs','/load_data.R'));
  
  
  # Initiate the output
  summary <- list(result="noData", freqProb=c("cliks","4K","8k"), metric=.95); 
  
  #
  # init
  #
  refFolder = "./ABR_standards/";
  newFolder = "ABR_ID"; # TODO make it parameters
  
  # Load the reference data set
  refData <- load_data( paste0(refFolder,refData) );
  
  # Load the data set to compare with
  if( is.null(newData) ){
    # No data to compare with
    summary <- list(result="noData", freqProb=c(), metric=Inf); 
    return( summary );
  }else{
    newData <- load_data( paste0(newFolder,newData) );
  }
    
  
  for( freq in availf ){
    
    #
    # compare each frequency of the newData to the reference prediction interval
    #
    
  }
  
  return( summary );
  
}