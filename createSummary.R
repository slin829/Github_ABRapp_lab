#
# createSummary
# function( refData, newData = "NULL" )
# create a summary based on reference data (refData) and new data (newData). The function will return a strcutur
# that contains the result of the summary (OK, KO). If the result is KO thant additional information about the
# frequencies with problems as a list.
#

createSummary <- function( refData, newData = "NULL" ){
  # The output
  summary <- list(result="OK", freqProb=c("cliks","4K","8k"), metric=.95); 
  
  #
  # init
  #
  availf = c("clicks", "4k","8k", "12k","16k","20k","24k","28k");
  refFolder = "./ABR_standards/";
  
  for( freq in availf ){
    
    var = dir(paste0(refFolder, system));
    var= as.vector(var);
    # TODO: create a funciton to load data in a standard way.
    
    #
    # Load the reference data set
    #
    
    #check if the frequence exits in the reference folder
    if( !(paste0(freq,".csv")) %in% dir(paste("./ABR_standards/", refData, "/")) ){
      # TODO: add freq in freqProb?
      next
    }
    
    #
    # Load the data set to compare with
    #
    
    #
    # compare each frequency of the newData to the reference prediction interval
    #
    
  }
  
  return( summary );
  
}