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
  
  
  return( summary );
  
}