#requirements
require(tidyverse) #tidy programming
require(lubridate) #easy working with datetimes
Sys.setenv(TZ="America/New_York")

# wrapper function to unzip a file and place the result in a directory "data"
Unzip_Data = function(zip_file){
  unzip(zipfile = zip_file, overwrite = TRUE, exdir = "./data")
  list.files("./data")%>%
    .[1]%>%
    return()
}

#adds a zero to single digit values for time parsing
Add_Zero = function(string){
  string=string%>%
    as.character()%>%
    as.numeric()
  ifelse(string < 10, paste0("0",string), string)%>%
    return()
}

#split a column with many checked items into a matrix with each item checked
Split_CheckBoxes = function(column, name){
  
  #first, get all the items
  column = as.character(column)
  categories = NULL
  for(line in 1:length(column)){
    items = strsplit(column[line], split=",")%>%unlist()
    categories = c(categories,items)
  }
  categories = unique(categories)
  
  #create an empty dataframe
  toReturn = data.frame(matrix(nrow = length(column), 
                               ncol = length(categories)))
  
  #loop by row of original data, and then by column of categories, 
  #checking if each row has the category
  
  for(line in 1:length(column)){
    for(x in 1:length(categories)){
      toReturn[line,x] = ifelse(grepl(categories[x],column[line]), 1,0)
    }
  }
  
  names(toReturn) = paste0(name,".",gsub(" ", "_", categories))
  
  return(toReturn)
}

# Load and clean a file of call logs from Qualtrics
Load_Data = function(f){
  
  tryCatch({
    #read in the dataset
    #f = list.files("./data")[1]
    dat = read.csv(f,fill=TRUE)
    
    #special case
    if(nrow(dat) < 2){
      return()
    }
    
    #get survey questions
    questions = dat[1,]%>%
      t()%>%
      as.character()
    
    #remove junk rows from qualtrics
    dat = dat[-c(1,2),]
    dat = dat%>%
      filter(Finished == "True" | Finished == "TRUE")
    
    #convert datetimes for call start and end
    call_start = strptime(
      paste0(
        as.character(dat$start_date), " ",
        Add_Zero(dat$start_time_1), ":",
        Add_Zero(dat$start_time_2), " ",
        toupper(as.character(dat$start_time_3))
      ),
      format = "%m-%d-%Y %I:%M %p", 
      tz = "America/New_York")
    
    call_end = strptime(
      paste0(
        as.character(dat$end_date), " ",
        Add_Zero(dat$end_time_1), ":",
        Add_Zero(dat$end_time_2), " ",
        toupper(as.character(dat$end_time_3))
      ), 
      format = "%m-%d-%Y %I:%M %p", 
      tz = "America/New_York")
    
    #record when log was submitted
    submitted = ymd_hms(dat$RecordedDate, tz = "UTC")
    
    #only take columns from relevant call info onwards
    start_column = names(dat)%>%
      grep("primary",.,value=FALSE)%>%
      .[1]
    dat = dat[,start_column:ncol(dat)]
    
    #refactor
    body_ind =  names(dat)%>%
      grep("body",.,value=FALSE)%>%
      .[1]
    dat[,-body_ind] = droplevels.data.frame(dat[,-body_ind])
    dat[,body_ind] = as.character(dat[,body_ind])
    
    feedback_ind = names(dat)%>%
      grep("opinion",.,value=FALSE)%>%
      .[1]
    dat[,feedback_ind] = as.character(dat[,feedback_ind])
    
    checkboxes = names(dat)%>%
      grep("issues|skills|referrals|marketing",.,value = TRUE)
    
    for(i in 1:length(checkboxes)){
      
      if(length(levels(dat[,checkboxes[i]])) < 2){
        dat[,checkboxes[i]] = NULL
        next()
      }
      
      dat = cbind(dat, Split_CheckBoxes(dat[,checkboxes[i]], checkboxes[i]))
      dat[,checkboxes[i]] = NULL
    }
    
    cbind(submitted, call_start, call_end, dat)%>%
      return()
  }, 
  error = function(err){
    print("Load data error! Check that this is the correct file type!")
    return(NULL)
  })
  
}


#get the earliest call start time
getMin = function(df){
  df%>%
    select(call_start)%>%
    slice(which.min(call_start))%>%
    .$call_start%>%
    floor_date("month")%>%
    return()
}

getMax = function(df){
  df%>%
    select(call_start)%>%
    slice(which.max(call_start))%>%
    .$call_start%>%
    ceiling_date("month")%>%
    return()
}

