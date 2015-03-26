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
  summary <- list(result="", freqProb=c(), noData=c(), metric=Inf); 
  
  #
  # init
  #
  refFolder = "ABR_standards";
  newFolder = "ABR_ID"; # TODO make it parameters
  availf = c("clicks", "4k","8k", "12k","16k","20k","24k","28k");
  
  
  # Load the data set to compare with
  if( is.null(newDataID) || is.null(newDataFolder) ){
    # No data to compare with
    summary <- list(result="noDataSelected", freqProb=c(), noData = availf, metric=Inf); 
    return( summary );
  }else{
    newData <- load_data( paste0(getwd(),"/",newFolder,"/",newDataFolder,"/",newDataID) );
  }
  
  #print(newData)
  
  # Load the reference data set
  refData <- load_data( paste0(getwd(),"/",refFolder,"/",refDataID) , TRUE);
  
  #print(refData)
  
  #
  # Compare each frequency of the newData to the reference prediction interval
  #
  for( freq in availf ){
    
    if( sum(refData$freq_name == freq) == 0 || sum(newData$freq_name ==  freq) == 0 ){
      # we can't compare since one of the data set doesn't contain the frequency
      summary$noData[length(summary$noData)+1] = freq;
    
    }else{
      # extract the information to compare
      # amp
      indR = 0; i=1;
      for( f in refData$freq_name ){
        if( f == freq ){
          indR = i;
        }
        i = i + 1;
      }
      refVal = refData$val[[indR]];
      ref_amp_diff <- refVal[,"Peak_amp"]-refVal[,"Trough_amp"];
      refVal <- cbind(refVal, ref_amp_diff)
      refVal <- na.omit(refVal)
      ref_lat <- refVal[,"Peak_lat"];
      ref_sound_level <- refVal$Sound_level;
      
      # freq
      indN = 0; i = 1;
      for( f in newData$freq_name ){
        if( f == freq ){
          indN = i;
        }
        i = i + 1;
      }
      newVal = newData$val[[indN]];
      
      new_amp_diff <- newVal[,"Peak_amp"]-newVal[,"Trough_amp"];
      new_lat <-newVal[,"Peak_lat"];
      
      # Prediction and confidences
      print(paste("freq:", freq, "ind-", indR, ":" ))
#      print(refVal);
#      print("----------------")
#      print(newVal)
#      print("----------------")
     
      
      MyMod <- lm(ref_amp_diff ~ poly(ref_sound_level, 2), data=refVal)
      prediction_amp <-predict(MyMod, interval="prediction", level=0.95)
      
      MyMod <- lm(refVal$Peak_lat~ poly(refVal$Sound_level,2), data= refVal)
      prediction_freq <- predict(MyMod, interval="prediction", level=0.95)

      metric_amp = 0;
      N_outside_amp = 0;
      for( i in 1:length(new_amp_diff) ){
        if( !is.null( newVal[i,"Sound_level"] ) && !is.na( newVal[i,"Sound_level"] ) ){
          
  #        print(paste(i," - ", new_amp_diff[i], newVal[i,"Sound_level"]))
          id = which( refVal$Sound_level %in% newVal[i,"Sound_level"] )
#          print( id )
          
          # if we found a corresponding frequency in the refData
          if( length(id) > 0 ){
    #        print( paste("\t", refVal[id[1],"Sound_level"], " lwr:", prediction_amp[id[1],"lwr"], " upr:", prediction_amp[id[1],"upr"] ) )
            
            # find the closet bound
            comp = prediction_amp[id[1],"upr"];
#            print(paste(A1, "vs", A2))        
            if(  abs(prediction_amp[id[1],"lwr"] - new_amp_diff[i]) < abs(prediction_amp[id[1],"upr"] - new_amp_diff[i]) ){
              comp = prediction_amp[id[1],"lwr"];
            }
            
            # update the metric
            metric_amp = metric_amp + ( (new_amp_diff[i] - comp)^2 )
            if( (prediction_amp[id[1],"lwr"] > new_amp_diff[i])  || (prediction_amp[id[1],"upr"] < new_amp_diff[i]) ){
              N_outside_amp = N_outside_amp + 1;
              print(paste(newVal[i,"Sound_level"], "dB", " +++ ", "amp diff: ", new_amp_diff[i], " --- ", "prediction-lwr: ", prediction_amp[id[1],"lwr"], " - ", (prediction_amp[id[1],"lwr"] < new_amp_diff[i]), "/" , " prediction-upr: ", prediction_amp[id[1],"upr"], " - ", (prediction_amp[id[1],"upr"] > new_amp_diff[i])))
            }
          }
  
        }# if is null
      } #end for i

      print( paste("metric:", metric_amp, "N outside:", N_outside_amp) )

print("-------------------------------------------")

    }# end else
  }
  
  return( summary );
}