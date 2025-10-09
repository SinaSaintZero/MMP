                  #------------------ MMP method : Test on Urology dataset -----------------------
                      # Author: Dr. Sina NAMAKI ARAGHI
                        # First: Please give us the NORMATIVE PROCESS
                  
                        # Second: Please provide the DOMAIN KNOWLEDGE (set of rules) for the application
                         # Third: please add the event log and indicate which case you
                             #   would like to analyze
                  
                        # fourth: See the diagnosis
                  
                  
                  #-------------------------------------------------------------------------------
                  # libraries
                  library(data.table); library(purrr); library(stringr); library(dplyr); library("R6")
                  library(rlang); 
                  
                           # --------------- Execution of the MMP method ---------------------
                                              #step 1: normative process
                  #normativeProcessString<- toString(process) -> extracted from the Stable Heuristic Miner

                  # For the development of that algorithm you can refer to the publications in :
                  # Stable Heuristic Miner 1 -> DOI : https://doi.org/10.1016/j.iswa.2022.200071
                  # Stable Heuristic Miner 2 -> DOI : https://doi.org/10.1007/s44230-024-00064-4

                  # To just test the method we provided the example below:

                  process <- c("Enter_consultation", "Reception_Waiting_room", "Registration", "Waiting_Room5",
                               "Box_Consultations", "Checkout_Office_UROLOGY", "Exit")
                  
                  
                           # --------------- Execution of the MMP method ---------------------
                                            # STEP 2: The domain knowledge: rules of deviations
                  df_knowledge_MMP <- read.csv(file.choose(), header = TRUE, sep = ";" , stringsAsFactors = FALSE)
                  names(df_knowledge_MMP)<-NULL
                  knowledge_MMP<- as.list(df_knowledge_MMP)
                  
                  listOfRules<- list()
                  for(i in 1:nrow(df_knowledge_MMP)){
                    listOfRules<- append(listOfRules, list(df_knowledge_MMP[i,]))
                  }
                  
                              # --------------- Execution of the MMP method ---------------------
                                    # STEP 3: getting the descriptive process model
                  
                  #----------------- getting the descriptive process model ----------------------
                  # -------------- This procedure will get the process model of an individual case --------
                  eventLog <- read.csv2(file.choose(), header = T, sep = ";" ,  stringsAsFactors = FALSE)
                  df_patients<- data.frame(eventLog [1:4]  ) # identify the columns used for your analyses
                  df_patients
                  
                  
                  #create a data. table from the event log
                  #actvity column should be on 4th column and the ID should be on the first one
                  
                  dt <- data.table(ID=df_patients[,1],Activity=(df_patients[,4]))
                  
                  
                  # the sorting of log.... ID and activities in the proper order
                  #consecutive_id <- dt[,.(first.act=(Activity),second.act=(shift(Activity,type = "lead"))),ID][!is.na(second.act)]
                  shift <- data.table::shift
                  consecutive_id <- dt[,.(first.act=(Activity),second.act=(shift(Activity, type = "lead"))),ID][!is.na(second.act)]
                  consecutive_id
                  #calclate the direct relations
                  
                  n1<- as.integer(readline(prompt = "Enter Patient's ID="))   # getting the case ID
                  
                  
                  Individual_processes_Activity_Direct_Relation <- subset(consecutive_id, consecutive_id$ID == n1)
                  
                  Individual_processes_Activity_Direct_Relation
                  activitiesIndividualProcess<- data.frame(union(Individual_processes_Activity_Direct_Relation$first.act,Individual_processes_Activity_Direct_Relation$second.act))
                  activitiesIndividualProcess
                  
                  
                  # a function to convert the process activities into characters or words if your prefer
                  listOfActivities <- as.character(unlist(activitiesIndividualProcess))
                  listOfActivities
                  
                  stringOf_DM<- listOfActivities ; 
                  stringOf_DM <- paste(stringOf_DM, collapse= ", ") ; 
                  stringOf_DM <- gsub(",","",stringOf_DM)
                  
                  Descriptive_Model<- stringOf_DM
                  
                  
                  # ------------------------------------------------------------------------------------------------
                  # The ProDIST algorithm for measuring the distance
                  # ------------------------------------------------------------------------------------------------
                  ProDIST <- function(x, y, 
                                      split = " ", 
                                      split_x = split, split_y = split, 
                                      case_sensitive = TRUE){
                    #Safety checks
                    if(!is.character(x) || !is.character(y) || 
                       nchar(x) == 0 || nchar(y) == 0)
                      stop("x, y needs to be none empty character strings.")
                    if(length(x) != 1 || length(y) != 1)
                      stop("Currency the function is not vectorized, please provide the strings individually or use lapply.")
                    if(!is.logical(case_sensitive))
                      stop("case_sensitivity needs to be logical")
                    #Extract variable names of our variables
                    # used for the dimension names later on
                    x_name <- deparse(substitute(x))
                    y_name <- deparse(substitute(y))
                    #Expression which when evaluated will name our output
                    dimname_expression <- 
                      parse(text = paste0("dimnames(output) <- list(",make.names(x_name, unique = TRUE)," = x_names,",
                                          make.names(y_name, unique = TRUE)," = y_names)"))
                    #split the strings into words
                    x_names <- str_split(x, split_x, simplify = TRUE)
                    y_names <- str_split(y, split_y, simplify = TRUE)
                    #are we case_sensitive?
                    if(isTRUE(case_sensitive)){
                      x_split <- str_split(tolower(x), split_x, simplify = TRUE)
                      y_split <- str_split(tolower(y), split_y, simplify = TRUE)
                    }else{
                      x_split <- x_names
                      y_split <- y_names
                    }
                    #Create an index in case the two are of different length
                    idx <- seq(1, (n_min <- min((nx <- length(x_split)),
                                                (ny <- length(y_split)))))
                    n_max <- max(nx, ny)
                    #If we have one string that has length 1, the output is simplified
                    if(n_min == 1){ 
                      distances <- seq(1, n_max) - (x_split[idx] == y_split[idx])
                      output <- matrix(distances, nrow = nx)
                      eval(dimname_expression)
                      return(output)
                    }
                    #If not we will have to do a bit of work
                    output <- diag(cumsum(ifelse(x_split[idx] == y_split[idx], 0, 1)))
                    #The loop will fill in the off_diagonal
                    output[2, 1] <- output[1, 2] <- output[1, 1] + 1 
                    if(n_max > 2)
                      for(i in 2:n_min){
                        for(j in 1:(i - 1)){
                          output[i,j] <- output[j,i] <- output[i,i] - output[i - 1, i - 1] + #are the words different?
                            output[i - 1, j] #How many words were different before?
                        }
                      }
                    #comparison if the list is not of the same size
                    if(nx != ny){
                      #Add the remaining words to the side that does not contain this
                      additional_words <- seq(1, n_max - n_min)
                      additional_words <- sapply(additional_words, function(x) x + output[,n_min])
                      #merge the additional words
                      if(nx > ny)
                        output <- rbind(output, t(additional_words))
                      else
                        output <- cbind(output, additional_words)
                    }
                    #set the dimension names, 
                    # I would like the original variable names to be displayed, as such i create an expression and evaluate it
                    eval(dimname_expression)
                    output
                  }
                  
                  
                  #--------------------------the FACT class and its defined methods --------------------------------
                  #An object oriented approach
                  #------------------------------------------------------------------------------------------------
                  
                  Fact <- R6Class (
                    "Fact",
                    private = list(
                      impct_actions = c("Add", "Remove", "Replace")
                    ),
                    public = list(
                      Add = function(ReferenceModel, ReferenceActivity, DeviatingActivity, PAC, After = TRUE){
                        library(stringr)
                        ReferenceModelString <- toString(ReferenceModel)
                        splitString <- strsplit(ReferenceModelString, ",")[[1]]
                        Position<- grep(ReferenceActivity, splitString)
                        
                        
                        for(i in 1:length(ReferenceModel)){
                          if(After==TRUE){
                            GM = append (ReferenceModel, DeviatingActivity, after = Position)
                          }else{
                            GM= prepend(ReferenceModel, DeviatingActivity, before = Position)
                            #PAC <- c(PAC)
                          }
                        }
                        #return(list(GM, PAC))
                        return(GM)
                      },
                      Remove= function(ReferenceModel, RemovingActivity, PAC){
                        library(stringr)
                        ReferenceModelString <- toString(ReferenceModel)
                        splitString <- strsplit(ReferenceModelString, ",")[[1]]
                        Position<- grep(RemovingActivity, splitString)
                        p<- Position
                        GM = ReferenceModel[-p]
                        #PAC <- c(PAC)
                        #return(list(GM, PAC))
                        return(GM)
                      },
                      Replace = function(ReferenceModel, ReferenceActivity, ReplacingActivity, PAC){
                        GM = sub(ReferenceActivity, ReplacingActivity, ReferenceModel)
                        #PAC <- c(PAC)
                        #return(list(GM, PAC))
                        return(GM)
                      }
                    )
                  )         
                  
                  #------the update process function which goes through rules and generate one process by looking at all the domain knowledge provided --------------------------------
                  
                  updateProcess <- function(normativeProcess, activatedRulesCombination){
                    resultingProcess = normativeProcess
                    TransformationObject<-Fact$new()
                    
                    for (rule in activatedRulesCombination){
                      if(is.na(rule)==FALSE){
                        if(rule[3] =="addAfter"){
                          resultingProcess <-  TransformationObject$Add(ReferenceModel = resultingProcess, ReferenceActivity = rule[1], DeviatingActivity = rule[4], PAC = rule[2], After = T)
                          
                        }else if(rule[3] == "addBefore"){
                          resultingProcess <-  TransformationObject$Add(ReferenceModel = resultingProcess, ReferenceActivity = rule[1], DeviatingActivity = rule[4], PAC = rule[2], After = F)
                          
                        }else if(rule[3]== "replace"){
                          resultingProcess <-  TransformationObject$Replace(ReferenceModel = resultingProcess, ReferenceActivity = rule[1], ReplacingActivity = rule[4], PAC = rule[2])
                          
                        }else if(rule[3]=="remove"){
                          resultingProcess <-  TransformationObject$Remove(ReferenceModel = resultingProcess, RemovingActivity = rule[1], PAC = rule[2])
                          
                        }
                      }
                    }
                    return(resultingProcess)
                  }
                  #updateProcess(process, listOfRules)  
                  # creating a string from each rule
                  listOfRules_strings = list()
                  for (rule in listOfRules){
                    listOfRules_strings = append(listOfRules_strings, toString(rule))
                  }         
                  
                  # ----- combinatons of rules
                  lengthListRules <- length(listOfRules_strings)
                  
                  listOfRulesCombinations <- do.call(rbind, lapply(1:lengthListRules, function(x) {
                    mat <- t(combn(listOfRules_strings, x))
                    
                    if (ncol(mat) < lengthListRules)
                      cbind(mat, replicate(lengthListRules - ncol(mat), rep(NA, nrow(mat))))
                    else
                      mat
                  }))
                  
                  listOfRulesCombinations
                  
                  #------now we iterate throgh combination of rules by evoking the update process function --------------------------------
                  updatedProcessList<- list()
                  for(i in 1:nrow(listOfRulesCombinations)){
                    updatedProcessList <- append(updatedProcessList, toString(updateProcess(normativeProcess = process, strsplit(unlist(listOfRulesCombinations[i,]), split = ", ", fixed = TRUE))))
                    
                    #updatedProcessList<- data.table(updatedProcessList)
                    
                  }
                  
                  generatedProcesses <- data.table(updatedProcessList)
                  generatedProcesses
                  
                  
                  # --------- removing commas from each line of the generated process activities --------------------------------------------
                  generatedProcessesWithoutComma<- list()
                  for (i in 1:nrow(generatedProcesses)){
                    generatedProcessesWithoutComma<- append(generatedProcessesWithoutComma,gsub(",", "", updatedProcessList[i]))
                  }
                  generatedProcessesWithoutComma   
                  
                
                  # preparing the list of generated models to apply the distance function ------
                  
                  generatedProcessesStrings<- list()
                  for(i in 1:nrow(generatedProcesses)){
                    generatedProcessesStrings<- as.character(append(generatedProcessesStrings, paste(generatedProcessesWithoutComma[i], collapse = " ")))
                  }
                  generatedProcessesStrings
                  
                  
                  ## ---------------------getting the distances of all the GMs-----------------
                  distanceOfProcesses<- list()
                  for(i in 1:nrow(generatedProcesses)){
                    distanceOfProcesses<- append(distanceOfProcesses,  ProDIST(Descriptive_Model, generatedProcessesStrings[1:n][i])[length(ProDIST(Descriptive_Model, generatedProcessesStrings[1:nrow(generatedProcesses)][i]))])
                    
                  }
                  distanceOfProcesses<- data.table(distanceOfProcesses)
                  distanceOfProcesses<- cbind(distanceOfProcesses, generatedProcessesStrings)
                  
                  
                  distanceOfProcesses <- as.data.frame(distanceOfProcesses)
                  distanceOfProcesses$distanceOfProcesses <- as.numeric(distanceOfProcesses$distanceOfProcesses)
                  distanceOfProcesses
                  distanceOfProcesses$ID <- seq.int(nrow(distanceOfProcesses))
                  
                  #getting the generated model with the minimum value
                  minDistances<- distanceOfProcesses[distanceOfProcesses$distanceOfProcesses== min(as.numeric(distanceOfProcesses$distanceOfProcesses)),]
                  minDistances
                  
                  
                  potentialCauses <- listOfRulesCombinations[minDistances$ID,1:ncol(listOfRulesCombinations)]
                  potentialCauses
                  Descriptive_Model
                  
                  