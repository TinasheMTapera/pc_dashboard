#requirements
require(tidyverse) #tidy programming
require(lubridate) #easy working with datetimes

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
    dat = read.csv(f,fill=TRUE)%>%
      .[-c(1:2),]%>%
      as.tibble()
    
    #special case
    if(nrow(dat) < 2){
      return(NULL)
    }
    
    #remove incomplete rows from qualtrics
    dat = dat%>%
      filter(Finished == "True" | Finished == "TRUE")
    
    #convert datetimes for call start and end
    dat = dat%>%
      mutate(call_start = mdy_hm(paste0(
        as.character(.$start_date), " ",
        Add_Zero(.$start_time_1), ":",
        Add_Zero(.$start_time_2), " ",
        toupper(as.character(.$start_time_3)))),
        call_end = mdy_hm(paste0(
          as.character(.$end_date), " ",
          Add_Zero(.$end_time_1), ":",
          Add_Zero(.$end_time_2), " ",
          toupper(as.character(.$end_time_3)))))%>%
      select(-c(start_date:end_time_3))
    
    #record when log was submitted
    dat = dat%>%
      mutate(Recorded = ymd_hms(.$RecordedDate))%>%
      select(-RecordedDate)
    
    #only take columns from relevant call info onwards
    dat = dat%>%
      select(primary:Recorded)
    
    #refactor
    dat = dat%>%
      mutate(body = as.character(body),
             opinion = as.character(opinion))
    
    # expand grid of checkbox factors
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

