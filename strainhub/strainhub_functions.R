#'############################
#' @title StrainHub
#' @description StrainHub works to generate a pathogen transmission network graph utilizing genomic data and calculate importance of network based on centrality metrics.
#' @author Adriano Schneider
#' @author John Williams
#' @author Mike Cioce
#' @author Colby Ford
#' 
#' @import treeio
#' @import ggplot2
#' @import adegenet
#' @import ade4
#' @import knitr
#' @import shiny
#' @import readr
#' @import ape
#' @import castor
#' @import visNetwork
#' @import hashmap
#' @import plyr
#' @import network
#' @import igraph
#' @import data.table
#' @import magrittr
#' @export statesConfirmed
#' @export mapStates
#' @export getMetadata
#' @export makeTransNet
#' @export listStates
#' @export getUsableColumns
#'############################
# Load Packages
# library(treeio)
# library(ggplot2)
# library(adegenet)
# library(ade4)
# library(knitr)
# library(dplyr)
# library(shiny)
# library(ape)
# library(castor)
# library(visNetwork)
# library(hashmap)
# library(plyr)
# library(network)
# library(igraph)
# library(data.table)
# library(magrittr)
# library(phangorn)
# library(seqinr)


#'############################
#' @name statesConfirmed
#' @param nextStates
#' @param charLabelList
#' @description Returns a true value if each state read in the next record of the character matrix can be mapped to an integer state that is within the bounds of the character specified.
#'     (i.e. the integer value i is 1<i<length(characterLabel_specified)). Otherwise it throws an error.
#'
#'############################
statesConfirmed <- function(nextStates,charLabelList) {
  
  returnValue <- TRUE
  for (i in 1:length(charLabelList)) {
    nextState <- nextStates[i]
    nextLabel <- charLabelList[[i]]
    isValid <- (nextState <= length(nextLabel))
    if (is.na(isValid)) {
      isValid <- FALSE
    }
    if(!isValid && !is.na(nextState)) {
      
      cat("ERROR: the state mapping is out of bounds for this character")
      stop()
    }
  }
  returnValue
  
}

#'#############################
#' @name mapStates
#' @param nextStates
#' @param symbols
#' @param missing
#' @param gap
#' @description Maps the next record of character state symbols to integer values.
#'     It then returns an integer vector of the mapped states.
#'#############################

mapStates <- function(nextStates, symbols, missing, gap) {
  mappedStates <- c()
  for (i in 1:length(nextStates)) {
    nextState <- nextStates[i]
    stateMap <- match(nextState,symbols) #returns the first index of the symbol vector where the value matches, else returns NA
    if (is.na(stateMap)) {
      isMissing <- (nextState == missing)
      isGap <- (nextState == gap)
      if (!isMissing) {
        cat("\nERROR: symbol is not an element of the $symbol or $missing sets; the character matrix is erroneous")
        stop()
      }
      if(isGap) {
        cat("\nERROR: gap value should not be used in this state matrix. please check your matrix for gap symbols")
        stop()
      }
    }
    mappedStates <- c(mappedStates,stateMap)
  }
  mappedStates
}

#'#############################
#' @name getMetadata Collects the metadata from the nexus file and returns it in a list object.
#' @fileName A character vector containing a string literal that is the directory path to the nexus file to be read in.
#' @return 
#' metadata - a list object with the following components:
#'           
#'           .$charMatrix - a matrix object whose rows are the accession taxa and whose columns are the integer-mapped
#'                             character state values
#'              attributes:
#'                 attr(.,"dimnames") - a list object with 2 character vectors
#'                    [[1]] - character vector of accession taxa
#'                    [[2]] - character vector of character label descriptions
#'
#'           .$characterLabels - a list object of n character vectors, each of which corresponds to a different
#'                                character (e.g. [[1]] == place, [[2]] == country, etc.)
#'              attributes:
#'
#'                attr(.,"numCharacters") - a numeric vector of length 1 that is the number of character vectors (n)
#'
#'           .$charSymbolFormatList - a list object of 4 components that reflects the "FORMAT" line in the CHARACTERS
#'                                      block of the nexus file
#'
#'              .$dataType - a character vector of length 1 whose value is the data type of the character block
#'              .$gap - character vector whose value is the symbol used to represent a gap in the data
#'              .$missing - a character vector whose value is the symbol used to represent missing data
#'              .$symbols - a character vector of lenght m whose values are used to create a mapping table
#'                             (from character symbol to integer)
#'
#'           .$taxaData - a character vector of the accession taxa in the tree and character state matrix
#'              
#'              attributes:
#'                 attr(.,"numTaxa") - a numeric vector whose value is the number of taxa in the character vector
#'              
#'#############################

getMetadata <- function(fileName) {
  
  fileConnection <- file(description = fileName, open = "r") #opens connection to nexus file in "read" mode;
  numCharacters <- 0
  charSymbolFormat <- list()
  charSymbolEnd <- FALSE
  charLabels <- list()
  mappedStates <- c()
  taxaLabelColumn <- c()
  taxaData <- c()
  numTaxa <- c()
  returnList <- list()
  repeat{
    
    nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
    taxaLabelBlockCheck <- grepl("BEGIN TAXA", nextLine)
    charactersBlockCheck <- grepl("BEGIN CHARACTERS",nextLine)
    endMetaData <- grepl("BEGIN TREES",nextLine)
    if (taxaLabelBlockCheck) {
      
      repeat {
        
        nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
        numTaxaCheck <- grepl("DIMENSIONS", nextLine)
        taxaCheck <- grepl("TAXLABELS", nextLine)
        endTaxaBlockCheck <- grepl("END;", nextLine)
        if (numTaxaCheck) {
          nextLine <- strsplit(nextLine, "[^=]+=|;") %>%
            .[[1]] %>%
            .[.!=""] %>%
            as.numeric()
          numTaxa <- nextLine
        }
        if (taxaCheck) {
          
          repeat {
            
            nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
            endTaxa <- grepl(";", nextLine)
            nextLine <- strsplit(nextLine, "\\s|;") %>%
              .[[1]] %>%
              .[.!=""]
            taxaData <- c(taxaData, nextLine)
            if(endTaxa) {
              break
            }
          }
          if(length(taxaData) == numTaxa) {
            
            attr(taxaData,"numTaxa") <- numTaxa   
          } else {
            
            cat("ERROR: the dimensions and the taxa count do not match")
            stop()
          }
          
        }
        if (endTaxaBlockCheck) {
          remove(endTaxaBlockCheck,
                 numTaxaCheck,
                 endTaxa,taxaCheck)
          break
        }
      }
    }
    if (charactersBlockCheck) {
      
      repeat{
        
        nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
        dimensionsCheck <- grepl("DIMENSIONS", nextLine)
        formatCheck <- grepl("FORMAT", nextLine)
        charLabelCheck <- grepl("CHARSTATELABELS", nextLine)
        charMatrixCheck <- grepl("MATRIX", nextLine)
        endCharBlock <- grepl("END;", nextLine)
        if (dimensionsCheck) {
          nextLine <- strsplit(nextLine,"[^=]+=|;") %>%
            .[[1]] %>%
            .[.!=""] %>%
            as.numeric()
          numCharacters <- nextLine
        }
        if (formatCheck) {
          
          formatRegEx <- "\\s|FORMAT|=|\""
          formatString <- strsplit(nextLine,formatRegEx) %>% #the magrittr package is used for shorthand coding where
            .[[1]] %>%                                      # a sequence of functions are acted upon and stored in
            .[.!=""]                                        #the same variable (in this case, formatString)
          for (i in 1:length(formatString)) {
            
            nextToken <- formatString[i]
            dataTypeCheck <- grepl("DATATYPE",nextToken)
            gapCheck <- grepl("GAP",nextToken)
            missingCheck <- grepl("MISSING",nextToken)
            symbolsCheck <- grepl("SYMBOLS",nextToken)
            if (dataTypeCheck) {
              type <- formatString[i+1]
              if (!grepl("STANDARD",type)){
                cat("ERROR: THIS PROGRAM IS SET UP TO READ STANDARD DATATYPES. PLEASE USE A COMPLIANT FILE")
                stop()
              } else {
                charSymbolFormat$dataType <- type
              }
              
            }
            if (gapCheck) {
              charSymbolFormat$gap <- formatString[i+1]
            }
            if (missingCheck) {
              charSymbolFormat$missing <- formatString[i+1]
            }
            if (symbolsCheck) {
              
              repeat {
                
                i <- i+1
                nextToken <- formatString[i]
                confirmSymbol <- grepl("[[:alnum:]]",nextToken)
                if (confirmSymbol){
                  charSymbolFormat$symbols <- c(charSymbolFormat$symbols,nextToken)
                } else {
                  
                  if(nextToken == ";") {
                    charSymbolEnd <- TRUE        
                    break
                  } else {
                    
                    cat("ERROR: ILLEGAL CHARACTER IN SYMBOL DEFINITION: USE ONLY ALPHANUMERIC CHARACTERS AND END WITH ;")
                    stop()
                  }
                  
                }
                
              }
            }
            if (charSymbolEnd) {
              remove(type,
                     symbolsCheck,
                     nextToken,
                     missingCheck,
                     i,
                     gapCheck,
                     formatString,
                     formatRegEx,
                     dimensionsCheck,
                     dataTypeCheck,
                     confirmSymbol,
                     charSymbolEnd)
              break
            }
          }
          
        }
        if (charLabelCheck) {
          
          repeat{
            
            nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
            isLastSet <- grepl(";",nextLine)
            delimRegEx <- "\\s+|(?<![[:alnum:]])/(?![[:alnum:]])|,|;"
            nextLine <- strsplit(nextLine,delimRegEx, perl = TRUE) %>%
              .[[1]] %>%
              .[.!=""]
            index <- as.numeric(nextLine[1])
            charLabels[[index]] <- nextLine[3:length(nextLine)]
            attr(charLabels,"names")[index] <- nextLine[2]
            if (isLastSet) {
              remove(index,isLastSet,delimRegEx)
              break
            }
            
          }
          
        }
        if (charMatrixCheck) {
          
          repeat {
            
            nextLine <- readLines(con = fileConnection, n = 1, encoding = "UTF-8" )
            endMatrix <- grepl(";",nextLine)
            if (!endMatrix) {
              
              nextLine <- strsplit(nextLine,"\\s|;") %>%
                .[[1]] %>%
                .[.!=""]
              if (length(nextLine == 0)) {
                nextTaxon <- nextLine[1]
                nextStates <- nextLine[2] %>%
                  strsplit("") %>%
                  .[[1]] %>%
                  mapStates(charSymbolFormat$symbols,charSymbolFormat$missing,charSymbolFormat$gap)
                taxonConfirmed <- nextTaxon %in% taxaData
                if (taxonConfirmed) {
                  
                  taxaLabelColumn <- c(taxaLabelColumn,nextTaxon)   
                }
                if(statesConfirmed(nextStates,charLabels)) {
                  
                  mappedStates <- c(mappedStates,nextStates)
                }   
              }
              
            } else {
              matrixDimNames <- list(taxaLabelColumn,attr(charLabels,"names"))
              mappedCharMatrix <- matrix(data = mappedStates, ncol = numCharacters, nrow = numTaxa, byrow = TRUE, 
                                         dimnames = matrixDimNames)
              returnList$charMatrix <- mappedCharMatrix
              remove(mappedCharMatrix,taxaLabelColumn,mappedStates,nextStates,nextTaxon,endMatrix)
              break
            }
          }
        }
        if (endCharBlock) {
          remove(taxonConfirmed,
                 formatCheck,
                 charMatrixCheck,
                 charLabelCheck,
                 matrixDimNames,
                 dimensionsCheck,
                 endCharBlock)
          break
        }
      }
    }
    if (endMetaData) {
      remove(taxaLabelBlockCheck,
             charactersBlockCheck,
             endMetaData)
      break
    }
  }
  attr(charLabels,"numCharacters") <- numCharacters
  returnList$characterLabels <- charLabels
  returnList$charSymbolFormat <- charSymbolFormat
  returnList$taxaData <- taxaData
  close(fileConnection)
  returnList
}
#'############################
#' @name listCharacterStates
#' @description Returns data frame of available character states in the input file.
#' @param data Input data (result from getMetadata function)
#'############################

# listCharacterStates <- function(data){
#   chardf <- data.frame(`Index` = 1:length(data$characterLabels),
#                        `Character State` = names(data$characterLabels))
#   return(chardf)
# }

listStates <- function(treedata, metadata = NULL, treeType = "parsimonious"){
  if(treeType == "parsimonious"){
    # dataoriginal <- readr::read_csv(csvFileName, col_names = TRUE) #imports csv metadata file. It has to have header and ID column has to be the first and labeled "Accession" in order for script to work. 
    dataoriginal <- metadata
    
    listofcolumns <- data.frame(`Index` = 1:length(colnames(dataoriginal)),
                                `Column` = colnames(dataoriginal))
    
    # listofcolumns <- as.list(dataoriginal)
    
    # listofcolumns <- data.frame(`Index` = 1:length(names(as.list(dataoriginal))),
    #                             `Column` = names(as.list(dataoriginal)))
  } else if(treeType == "bayesian"){
    ## read annotated tree from Nexus file
    # tree <- read.annotated.nexus(treeFileName) # Don't use
    # tree <- treeio::read.beast(treeFileName)
    tree <- treedata
    
    # ## ladderize the tree
    # ladderized <- ladderize(tree)
    
    ## for each edge (each edge is identified by a terminal node), we have:
    # listofcolumns <- data.frame(`Column` = names(ladderized$annotations[[1]])) %>%
    #   filter(!stringr::str_detect(Column, "_95%_HPD|_median|_range|.prob|.set|.rate"))
    
    listofcolumns <- data.frame(`Column` = names(tree@data)) %>%
      filter(!stringr::str_detect(Column, "_0.95_HPD|_median|_range|.prob|.set|.rate"))
    
    listofcolumns$`Index` <- 1:nrow(listofcolumns)
    listofcolumns <- listofcolumns[c("Index","Column")]
  } else if(treeType == "nj"){
    # dataoriginal <- readr::read_csv(csvFileName, col_names = TRUE) #imports csv metadata file. It has to have header and ID column has to be the first and labeled "Accession" in order for script to work. 
    dataoriginal <- metadata
    
    listofcolumns <- data.frame(`Index` = 1:length(colnames(dataoriginal)),
                                `Column` = colnames(dataoriginal))
  }
  
  
  return(listofcolumns)
}

getUsableColumns <- function(treedata, metadata){
  # nexusTree2 <- read.tree(treeFileName) #imports file in newick format instead of nexus.
  nexusTree2 <- treedata
  
  # dataoriginal <- readr::read_csv(csvFileName, col_names = TRUE) #imports csv metadata file. It has to have header and ID column has to be the first and labeled "Accession" in order for script to work. 
  dataoriginal <- metadata
  
  sortingtable <- as.data.frame(nexusTree2$tip.label) # Takes Tip Label information from Newick tree and transforms into a table, add ID to it and basically reorders the CSV metadata frame to match the Newick file. 
  sortingtable <- tibble::rowid_to_column(sortingtable, "N_ID") ## Was "ID"
  names(sortingtable)[2] <- "Accession"
  sortingdata <- merge(dataoriginal, sortingtable, by = "Accession")
  data <- sortingdata[order(sortingdata$N_ID),] ## Was "ID"
  
  # listofcolumns <- as.list(dataoriginal)
  listofcolumns <- as.list(data)
  columnaccessions <- as.list(data)
  accessioncharacter <- as.character(columnaccessions$Accession) #transforms accession from Factor into character
  # selectedcolumn <- as.numeric(as.factor(listofcolumns[[columnSelection]])) #transforms metadata state column from Factor into numeric
  # names(selectedcolumn) <- accessioncharacter # assign accession ID reference to the variable selected
  
  distinctvalsbycolumn <- data.frame(t(apply(data, 2, function(x) length(unique(x))))) # Get number of unique values by column.
  output <- c(names(distinctvalsbycolumn[which(distinctvalsbycolumn>1)])) #Return vector of usable columns

  return(output)
  
}

#'############################
#' @name makeTransNet
#' @param fileName Path to the nexus file to be read in.
#'    Character string.
#' @param charIndex The character state index of the nexus file from which the network is to be built.
#'    Numeric.
#' @param centralityMetric Number identifying a centrality metric.
#'    - 0 to simply calculate all metrics
#'    - 1 for indegree
#'    - 2 for outdegree
#'    - 3 for betweenness
#'    - 4 for closeness
#'    - 5 for degree
#'    - 6 for Source Hub Ratio
#' @description Creates the network graph object. Use print(graph) to display.
#' @note 
#'  rootedTree structure:
#'    list of 3 components:
#'      $edge - a numeric matrix with 2 columns. It is the table of edges that describes
#'        the phylogenetic tree that was read in by the read.tree() function;
#'      $Nnode - a numeric vector of length one whose value is the number of nodes on the 
#'        inner branches of the tree;
#'      $tip.label - a character vector whose elements are the character string that
#'        identifies a leaf node on the phylogenetic tree;
#' @note 
#'  The asr_max_parsimony() function requires a numeric vector that lists the character states of the leaf nodes in sequence as one of its parameter arguments. The following for loop
#'   walks through the character vector $tip.label in the rootedTree list, starting at the 
#'   index [1], and stores the value of $tip.label[i] in the character vector accession.
#'   This character string is then passed into the find function of the hashmap and its
#'   character state is returned. Thus when the loop is complete, it has populated 
#'   the metadataStates numeric vector with the character states associated with the 
#'   leaf nodes in the order that they appeal in $tip.label;
#'  Get character state for each node that isn't a leaf node (i.e. all the inner nodes)
#'  asr_max_parsimony accepts 3 parameters:
#'     - the list object returned by the read.tree() function
#'     - the character states of the leaf nodes listed in the $tip.label character vector found in the list object
#'     - the number of possible character states of the trait
#' 
#' asr_max_parsimony returns a list object with the following components:
#'
#'   $ancestral_likelihoods - a numeric matrix object with nrows = the number of inner nodes 
#'     in the phylogenetic tree, and ncolumns = to the number of possible character states
#'     of the character trait being studied. The value at $ancestral_likelihoods[n,m] is 
#'     the probability of interior node n being character state m
#'   $success - a logical vector of length one that says whether the process was a success or not
#'############################

makeTransNet <- function(treedata, metadata = NULL, columnSelection, centralityMetric, threshold = 0.9, bootstrapValue = NULL, treeType = "parsimonious", rootSelection = NULL){
  
  if(treeType == "parsimonious"){
    # fileName <- readline(prompt = "Type in the full path to the nexus file you want to read in: ")
    # nexusTree2 <- read.nexus(fileName)
    # nexusData <- getMetadata(fileName)
    
    #charIndex <- readline(prompt ="Type the number equivalent to the character state index of the nexus file you want to build the network from: ")
    # if (charIndex < 1){
    #   cat("ERROR: The character state index must be > 0.")
    # } else{
    #   characterIndex <- as.numeric(charIndex) # Transforms the input from string to numeric so it can be loaded on metadataRef
    # }
    
    # nexusTree2 <- read.tree(treeFileName) #imports file in newick format instead of nexus.
    nexusTree2 <- treedata
    
    # dataoriginal <- readr::read_csv(csvFileName, col_names = TRUE) #imports csv metadata file. It has to have header and ID column has to be the first and labeled "Accession" in order for script to work. 
    dataoriginal <- metadata
    
    sortingtable <- as.data.frame(nexusTree2$tip.label) # Takes Tip Label information from Newick tree and transforms into a table, add ID to it and basically reorders the CSV metadata frame to match the Newick file. 
    sortingtable <- tibble::rowid_to_column(sortingtable, "N_ID") ## Was "ID"
    names(sortingtable)[2] <- "Accession"
    sortingdata <- merge(dataoriginal, sortingtable, by = "Accession")
    data <- sortingdata[order(sortingdata$N_ID),] ## Was "ID"
    
    # listofcolumns <- as.list(dataoriginal)
    listofcolumns <- as.list(data)
    columnaccessions <- as.list(data)
    accessioncharacter <- as.character(columnaccessions$Accession) #transforms accession from Factor into character
    selectedcolumn <- as.numeric(as.factor(listofcolumns[[columnSelection]])) #transforms metadata state column from Factor into numeric
    names(selectedcolumn) <- accessioncharacter #assign accession ID reference to the variable selected
    
    characterlabels1 <- unique(listofcolumns[[columnSelection]]) #extract unique labels from selected column
    characterlabels <- sort(as.character(characterlabels1)) #sort and create list of characters from previous vector - has to sort to match the order from the $country as when it becomes numeric is transformed to numbers in alphabetical order.
    
    
    rootedTree <- nexusTree2
    
    # metadataRef <- nexusData$charMatrix[,characterIndex] # CharacterIndex change the number of the character state index of the nexus file you want to use
    # ref2 <- attr(metadataRef, "names")
    
    # Builds a hashmap using the leaf node strings as keys and the character states as values
    # H <- hashmap(ref2, metadataRef)
    H <- hashmap::hashmap(accessioncharacter, selectedcolumn)
    
    # numCharStates <- length(nexusData$characterLabels[[characterIndex]]) # Change to the number above
    # ancestralStates = asr_max_parsimony(rootedTree,
    #                                     metadataRef,
    #                                     numCharStates)
    
    numCharStates <- length(characterlabels) ##### change to the number above
    
    ancestralStates = asr_max_parsimony(rootedTree, selectedcolumn, numCharStates)
    
    # Deletes all keys and values from the hashmap
    H$clear()
    
    # Rebuilds hashmap using sequential numbers 1 through the number of leaf nodes as the key/index 
    #and using the integer values found in metadataStates as values. It essentially builds a hashmap 
    #of the leaf nodes of the tree: their index and their value.
    for(i in 1:length(selectedcolumn)) {
      H$insert(i, selectedcolumn[i])
    }
    
    # Loop through the inner nodes of the phylogenetic tree and assign the most likely character state
    # to that tree node;
    numLeaves <- length(selectedcolumn)
    numInnerNodes <- rootedTree$Nnode
    totalTreeNodes <- numLeaves + numInnerNodes
    innerNodeIndices <- (numLeaves+1):totalTreeNodes
    numCharacterStates <- length(ancestralStates$ancestral_likelihoods[1,])
    counter <- c() #initializes counter vector
    
    for (i in innerNodeIndices) # 474:945  # 473 leaf nodes + 472 inner nodes = 945 total;
    {                                                                         
      counter <- ancestralStates$ancestral_likelihoods[i - numLeaves,] #numeric vector of character state 
      # probabilities for inner node of index i
      H$insert(i,
               match(max(counter),
                     counter)) #enters a new key-value pair 
      #(inner node i -> most likely character state)
    }
    
    #after the previous for loop executes, we now have an ASR of the phylogenetic tree given in the beginning.
    sourceList <- c()
    targetList <- c()
    
    #walk through each edge in the phylogenetic tree. if there's a state change between the two nodes, 
    #add the character states to their repspective vector 
    #(diedge tail == sourceList, diedge head == targetList)
    
    for(row in 1:nrow(rootedTree$edge)) 
    {
      nextEdge <- rootedTree$edge[row,]
      edgeStates <- c(H$find(nextEdge[1]),
                      H$find(nextEdge[2]))
      
      if (edgeStates[1] != edgeStates[2]) 
      {
        sourceList <- c(sourceList,
                        edgeStates[1])
        targetList <- c(targetList,
                        edgeStates[2])
      }
    }
    
    # This creates a table (in the form of a data frame) of the state changes that occur 
    #in the phylogenetic tree;
    dat <- data.frame(from = sourceList,
                      to = targetList)
    #counts the frequency of a specific state change occurring
    edges <- plyr::count(dat)
    names(edges)[names(edges) == "freq"] <- "value"
    
    # Extract the selected metadata state label from the nexusData
    # metastates <- nexusData$characterLabels[[characterIndex]]
    metastates <- characterlabels
    
    nodes <- data.frame(id = 1:length(metastates),
                        label = metastates) #, fixed = list(x = T, y = T))
    
    igraph.Object <- graph.data.frame(edges,
                                      directed = T,
                                      vertices = nodes)
  } else if(treeType == "bayesian"){
    ## read annotated tree from Nexus file
    # tree <- treeio::read.beast(treeFileName)
    tree <- treedata
    
    state <- tree@data[[columnSelection]]
    stateprob <- tree@data[[paste0(columnSelection,".prob")]]
    index <- as.numeric(tree@data$node)
    
    ## Tree edges - relationship between nodes
    tree.edges = tree@phylo$edge
    
    ## Rename edges with state (https://csgillespie.github.io/efficientR/dplyr.html / https://r4ds.had.co.nz/ )
    
    tree.edges2 = as_tibble(tree.edges)
    state_tib = tibble(
      State = state
    ) %>% 
      mutate(ind = index)
    
    stateprob_tib = tibble(
      Stateprob = stateprob
    ) %>% 
      mutate(ind = index)
    
    #Edge_tib = left_join(tree.edges2, state_tib, by = c("V1" = "ind")) %>% 
    #  left_join(state_tib, by = c("V2" = "ind"), suffix = c("_org", "_dst")) %>% 
    #  select(State_org, State_dst)
    
    ## Join node probabilities and names of nodes on dataframe containing edges of the tree
    
    Edge_tib = left_join(tree.edges2, state_tib, by = c("V1" = "ind")) %>% 
      left_join(state_tib, by = c("V2" = "ind"), suffix = c("_org", "_dst")) %>% 
      left_join(stateprob_tib, by =c("V1" = "ind")) %>%
      left_join(stateprob_tib, by =c("V2" = "ind"), suffix = c("_org", "_dst")) %>%
      select(Stateprob_org, State_org, State_dst, Stateprob_dst)
    
    ## Filter out pairs of changes between same states and also with probabilities below a certain threshold. For Shiny have to make it user input.
    
    Edge_notdup_threshold = Edge_tib %>% 
      filter(State_org != State_dst) %>%
      filter(Stateprob_org >= threshold) %>% #Probability of node of origin being on that state
      filter(Stateprob_dst >= threshold) #Probability of node of destiny being on that state
    
    Edge_filtered = Edge_notdup_threshold %>%
      select(State_org,State_dst)
    
    ## Creates structure for network
    
    ## This creates a table (in the form of a data frame) of the state changes that occur in the phylogenetic tree;
    
    dat <- data.frame(from = Edge_filtered$State_org,
                      to = Edge_filtered$State_dst)
    
    ## counts the frequency of a specific state change occurring
    
    characterlabels <- as.character(unique(state)) ## extract unique labels from state column
    
    edges <- plyr::count(dat)
    names(edges)[names(edges) == "freq"] <- "value"
    # metastates <- characterlabels
    metastates <- unique(combine(as.character(edges$to),as.character(edges$from)))
    
    nodes <- data.frame(id = 1:length(metastates), label = metastates) ##, fixed = list(x = T, y = T))
    
    nodes = as_tibble(nodes)
    edges = as_tibble(edges)
    
    
    edges = left_join(edges, nodes, by = c("from" = "label")) %>% 
      left_join(nodes, by = c("to" = "label"), suffix = c("_org", "_dst")) %>% 
      select(id_org, id_dst, value)
    names(edges) <- c("from", "to", "value")
    
    igraph.Object <- graph.data.frame(edges,
                                      directed = T,
                                      vertices = nodes)
  } else if(treeType == "nj"){

    # nexusTree2 <- make_nj_tree(dna = treedata, accession = rootSelection) # Don't Use
    nexusTree2 <- NJ_build_collapse(dna = treedata, accession = rootSelection, bootstrapValue = bootstrapValue)
    # nexusTree2 <- treedata
    
    # dataoriginal <- readr::read_csv(csvFileName, col_names = TRUE) #imports csv metadata file. It has to have header and ID column has to be the first and labeled "Accession" in order for script to work. 
    dataoriginal <- metadata
    
    sortingtable <- as.data.frame(nexusTree2$tip.label) # Takes Tip Label information from Newick tree and transforms into a table, add ID to it and basically reorders the CSV metadata frame to match the Newick file. 
    sortingtable <- tibble::rowid_to_column(sortingtable, "N_ID") ## Was "ID"
    names(sortingtable)[2] <- "Accession"
    sortingdata <- merge(dataoriginal, sortingtable, by = "Accession")
    data <- sortingdata[order(sortingdata$N_ID),] ## Was "ID"
    
    # listofcolumns <- as.list(dataoriginal)
    listofcolumns <- as.list(data)
    columnaccessions <- as.list(data)
    accessioncharacter <- as.character(columnaccessions$Accession) #transforms accession from Factor into character
    selectedcolumn <- as.numeric(as.factor(listofcolumns[[columnSelection]])) #transforms metadata state column from Factor into numeric
    names(selectedcolumn) <- accessioncharacter #assign accession ID reference to the variable selected
    
    characterlabels1 <- unique(listofcolumns[[columnSelection]]) #extract unique labels from selected column
    characterlabels <- sort(as.character(characterlabels1)) #sort and create list of characters from previous vector - has to sort to match the order from the $country as when it becomes numeric is transformed to numbers in alphabetical order.
    
    
    rootedTree <- nexusTree2
    
    # metadataRef <- nexusData$charMatrix[,characterIndex] # CharacterIndex change the number of the character state index of the nexus file you want to use
    # ref2 <- attr(metadataRef, "names")
    
    # Builds a hashmap using the leaf node strings as keys and the character states as values
    # H <- hashmap(ref2, metadataRef)
    H <- hashmap::hashmap(accessioncharacter, selectedcolumn)
    
    # numCharStates <- length(nexusData$characterLabels[[characterIndex]]) # Change to the number above
    # ancestralStates = asr_max_parsimony(rootedTree,
    #                                     metadataRef,
    #                                     numCharStates)
    
    numCharStates <- length(characterlabels) ##### change to the number above
    
    ancestralStates = asr_max_parsimony(rootedTree, selectedcolumn, numCharStates)
    
    # Deletes all keys and values from the hashmap
    H$clear()
    
    # Rebuilds hashmap using sequential numbers 1 through the number of leaf nodes as the key/index 
    #and using the integer values found in metadataStates as values. It essentially builds a hashmap 
    #of the leaf nodes of the tree: their index and their value.
    for(i in 1:length(selectedcolumn)) {
      H$insert(i, selectedcolumn[i])
    }
    
    # Loop through the inner nodes of the phylogenetic tree and assign the most likely character state
    # to that tree node;
    numLeaves <- length(selectedcolumn)
    numInnerNodes <- rootedTree$Nnode
    totalTreeNodes <- numLeaves + numInnerNodes
    innerNodeIndices <- (numLeaves+1):totalTreeNodes
    numCharacterStates <- length(ancestralStates$ancestral_likelihoods[1,])
    counter <- c() #initializes counter vector
    
    for (i in innerNodeIndices) # 474:945  # 473 leaf nodes + 472 inner nodes = 945 total;
    {                                                                         
      counter <- ancestralStates$ancestral_likelihoods[i - numLeaves,] #numeric vector of character state 
      # probabilities for inner node of index i
      H$insert(i,
               match(max(counter),
                     counter)) #enters a new key-value pair 
      #(inner node i -> most likely character state)
    }
    
    #after the previous for loop executes, we now have an ASR of the phylogenetic tree given in the beginning.
    sourceList <- c()
    targetList <- c()
    
    #walk through each edge in the phylogenetic tree. if there's a state change between the two nodes, 
    #add the character states to their repspective vector 
    #(diedge tail == sourceList, diedge head == targetList)
    
    for(row in 1:nrow(rootedTree$edge)) 
    {
      nextEdge <- rootedTree$edge[row,]
      edgeStates <- c(H$find(nextEdge[1]),
                      H$find(nextEdge[2]))
      
      if (edgeStates[1] != edgeStates[2]) 
      {
        sourceList <- c(sourceList,
                        edgeStates[1])
        targetList <- c(targetList,
                        edgeStates[2])
      }
    }
    
    # This creates a table (in the form of a data frame) of the state changes that occur 
    #in the phylogenetic tree;
    dat <- data.frame(from = sourceList,
                      to = targetList)
    #counts the frequency of a specific state change occurring
    edges <- plyr::count(dat)
    names(edges)[names(edges) == "freq"] <- "value"
    
    # Extract the selected metadata state label from the nexusData
    # metastates <- nexusData$characterLabels[[characterIndex]]
    metastates <- characterlabels
    
    nodes <- data.frame(id = 1:length(metastates),
                        label = metastates) #, fixed = list(x = T, y = T))
    
    igraph.Object <- graph.data.frame(edges,
                                      directed = T,
                                      vertices = nodes)
  }
  
  
  
  #ui <- readline(prompt = "Select a centrality metric. Enter 0 to simply calculate all metrics, 1 for indegree, 2 for outdegree, 3 betweenness, 4 closeness, 5 for degree or 6 for Source Hub Ratio: ")
  ui <- as.character(centralityMetric)
  
  ## Create Matrix of all Metrics
  # Calculates all the metrics and export on a text file delimited by comma.
  indegree <- centr_degree(igraph.Object,
                           mode = c("in")) #Calculates indegree = Destiny of shifts of metadata state for all nodes
  outdegree <- centr_degree(igraph.Object,
                            mode = c("out")) #Calculates the Outdegree = Source of shifts of metadata state for all nodes
  all.degree <- centr_degree(igraph.Object,
                             mode = c("all")) #Calculates the Degree = Hub, in and out of shifts of metadata state
  between.centrality <- betweenness(igraph.Object) #Calculates Betweenness Centrality
  closeness.centrality <- closeness(igraph.Object,
                                    mode = c("all")) #Calculates Closeness Centrality
  sourcehubratio <- outdegree$res/all.degree$res # This is the basic "Source Hub Ratio", still have to work on the normalizing formula
  
  # Create empty matrix and populate with the metrics
  outputFileMatrix <- matrix(ncol = 0,
                             nrow = length(metastates)) %>%
    cbind(metastates,
          all.degree$res,
          indegree$res,
          outdegree$res,
          between.centrality,
          closeness.centrality,
          sourcehubratio)
  
  colnames(outputFileMatrix,
           do.NULL = FALSE)
  colnames(outputFileMatrix) <- c("Metastates",
                                  "Degree Centrality",
                                  "Indegree Centrality",
                                  "Outdegree Centrality",
                                  "Betweenness Centrality",
                                  "Closeness Centrality",
                                  "Source Hub Ratio")
  
  metrics <- as.data.frame(outputFileMatrix)
  
  write.table(metrics,
              append = FALSE,
              file = "StrainHub_metrics.csv",
              sep = ",",
              fileEncoding = "UTF-8",
              col.names = TRUE,
              row.names = FALSE,
              quote = FALSE)
  
  if (ui > 0 & ui <= 6){
    if (ui == "1"){
      ## indegree
      indegree <- centr_degree(igraph.Object,
                               mode = c("in"))
      nodes <- data.frame(nodes,
                          value = indegree$res,
                          group = indegree$res)
      graph <- visNetwork(nodes = nodes,
                          edges = edges,
                          main = list(text = "Indegree Centrality",
                                      style = "font-family:Lato, Helvetica Neue, Arial, sans-serif;font-weight:bold;font-size:20px;text-align:center;")) %>%
        visPhysics(solver = "repulsion")%>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(selectedBy = "value",
                   highlightNearest = TRUE, 
                   nodesIdSelection = TRUE) %>%
        visEdges(arrows = list(to = list(enabled = T,
                                         scaleFactor = 0.75)))
    } else if (ui == "2"){
      ## outdegree
      outdegree <- centr_degree(igraph.Object,
                                mode = c("out"))
      nodes <- data.frame(nodes,
                          value = outdegree$res,
                          group = outdegree$res)
      graph <- visNetwork(nodes = nodes,
                          edges = edges,
                          main = list(text = "Outdegree Centrality",
                                      style = "font-family:Lato, Helvetica Neue, Arial, sans-serif;font-weight:bold;font-size:20px;text-align:center;")) %>%
        visPhysics(solver = "repulsion")%>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(selectedBy = "value",
                   highlightNearest = TRUE, 
                   nodesIdSelection = TRUE) %>%
        visEdges(arrows = list(to = list(enabled = T,
                                         scaleFactor = 0.75)))
      
    } else if (ui == "3"){
      ## betweenness centrality
      between.centrality <- betweenness(igraph.Object)
      nodes <- nodes <- data.frame(nodes,
                                   value = between.centrality,
                                   group = between.centrality)
      graph <- visNetwork(nodes = nodes,
                          edges = edges,
                          main = list(text = "Betweenness Centrality",
                                      style = "font-family:Lato, Helvetica Neue, Arial, sans-serif;font-weight:bold;font-size:20px;text-align:center;")) %>%
        visPhysics(solver = "repulsion")%>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(selectedBy = "value",
                   highlightNearest = TRUE, 
                   nodesIdSelection = TRUE) %>%
        visEdges(arrows = list(to = list(enabled = T,
                                         scaleFactor = 0.75)))
      
    } else if(ui == "4"){
      ## closeness centrality
      closeness.centrality <- closeness(igraph.Object,
                                        mode = c("all"))
      nodes <- data.frame(nodes,
                          value = closeness.centrality,
                          group = closeness.centrality)
      graph <- visNetwork(nodes = nodes,
                          edges = edges,
                          main = list(text = "Closeness Centrality",
                                      style = "font-family:Lato, Helvetica Neue, Arial, sans-serif;font-weight:bold;font-size:20px;text-align:center;")) %>%
        visPhysics(solver = "repulsion")%>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(selectedBy = "value",
                   highlightNearest = TRUE, 
                   nodesIdSelection = TRUE)%>%
        visEdges(arrows = list(to = list(enabled = T,
                                         scaleFactor = 0.75)))
      
    } else if (ui == "5"){
      ## all indegree/outdegree = degree centrality
      all.degree <- centr_degree(igraph.Object,
                                 mode = c("all"))
      nodes <- data.frame(nodes,
                          value = all.degree$res,
                          group = all.degree$res)
      graph <- visNetwork(nodes = nodes,
                          edges = edges,
                          main = list(text = "Degree Centrality",
                                      style = "font-family:Lato, Helvetica Neue, Arial, sans-serif;font-weight:bold;font-size:20px;text-align:center;")) %>%
        visPhysics(solver = "repulsion")%>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(selectedBy = "value",
                   highlightNearest = TRUE, 
                   nodesIdSelection = TRUE) %>%
        visEdges(arrows = list(to = list(enabled = T,
                                         scaleFactor = 0.75)))
      
    } else if (ui == "6"){
      ## Source Hub Ratio
      outdegree <- centr_degree(igraph.Object,
                                mode = c("out")) #Calculates the Outdegree = Source of shifts of metadata state for all nodes
      all.degree <- centr_degree(igraph.Object,
                                 mode = c("all")) #Calculates the Degree = Hub, in and out of shifts of metadata state
      sourcehubratio <- outdegree$res/all.degree$res # This is the basic "Source Hub Ratio", still have to work on the normalizing formula
      nodes <- data.frame(nodes,
                          value = sourcehubratio,
                          group = sourcehubratio)
      graph <- visNetwork(nodes = nodes,
                          edges = edges,
                          main = list(text = "Source Hub Ratio: Sink ~0 / Hub = .5 / Source = ~1",
                                      style = "font-family:Lato, Helvetica Neue, Arial, sans-serif;font-weight:bold;font-size:20px;text-align:center;")) %>%
        visPhysics(solver = "repulsion")%>%
        visInteraction(navigationButtons = TRUE) %>%
        visOptions(selectedBy = "value",
                   highlightNearest = TRUE, 
                   nodesIdSelection = TRUE) %>%
        visEdges(arrows = list(to = list(enabled = T,
                                         scaleFactor = 0.75)))
    }
    
  } else {
    cat("ERROR: Please enter an integer between 1 and 6 to select a centrality metric.")
  }
  
  return(graph)
  ## For export functionality, use:   (Bad quality)
  # return(graph %>% visExport(type = "png", background = "#00FFFFFF", style = 'class = "btn-outline-primary"'))
}

# make_nj_tree <- function(dna, accession){
#   #values <- read.dna(filePath, format="fasta")
# 
#   values_phyDat <- phyDat(dna, type="DNA", levels = NULL)
#   mt <- modelTest(values_phyDat)
#   reducedmt <- mt[c(1,5),c(1,3)] #extracts rows 1 and 5 from modeltest, columns 1 and 3
#   maxmt <- reducedmt[which.max(reducedmt$logLik),]
#   dna_dist <- dist.ml(values_phyDat, model=maxmt$Model)
#   values_NJ <- bionj(dna_dist)
#   plot(values_NJ, main="Neighbor Joining", cex=.6)
#   names(values_phyDat) #ID names for outgroup user selection down here:
#   tre2 <- root(values_NJ, outgroup = accession, resolve.root = TRUE)
#   tre2 <- ladderize(tre2)
#   plot(tre2, main="Neighbor Joining ladderized and rooted", cex=.6)
#   nexusTree2 <- tre2
# 
#   return(nexusTree2)
# }

# Neighbor Joining Tree Builder # 
## Function which creates distance matrix from alignment, builds NJ tree, performs bootstrap analysis, collapses weakly supported nodes - Requires ape, phangorn, and seqinr libraries ##
NJ_build_collapse <- function(dna, accession, bootstrapValue) {
  # dna <- read.dna(dna, format="fasta")
  ## Create data frame in phangorn format
  aln_phyDat <- phyDat(dna, type="DNA", levels = NULL)
  
  isnotwindows <- Sys.info()['sysname'] != "Windows" ## detect OS for use of multicores in model testing
  
  mt <- modelTest(aln_phyDat, multicore = isnotwindows, mc.cores = parallel::detectCores()-1)
  reducedmt <- mt[c(1,5),c(1,3)] #extracts rows 1 and 5 from modeltest, columns 1 and 3
  maxmt <- reducedmt[which.max(reducedmt$logLik),]
  dna_dist <- dist.ml(aln_phyDat, model=maxmt$Model)
  # dna_dist <- dist.dna(dna, model = maxmt$Model)
  
  #Building NJ tree from distance matrix
  aln_NJ <- bionj(dna_dist)
  
  # myBoots <- boot.phylo(aln_NJ, dna, function(e)
  #   root(nj(dist.dna(e, model=maxmt$Model)),accession)) #This roots on first sequence in alignment, can substitute with accession number
  # myBoots
  
  myBoots <- ape::boot.phylo(aln_NJ, dna, function(e) # Run bootstrap
    # root(nj(dist.dna(e, model=maxmt$Model)), accession))
    ape::dist.dna(e, model = maxmt$Model) %>%
      ape::nj() %>%
      ape::root.phylo(phy = ., outgroup = accession),
    B = 10
  )
  #myBoots

  ## Collapse branches of poorly supported nodes into multifurcations with bootstrap values less than X%
  temp <- aln_NJ # Dont want to change chikv_NJ file itself, assign to new variable for safekeeping
  N <- length(aln_NJ$tip.label) # Get total number of taxa from tree
  toCollapse <- match(which(myBoots<bootstrapValue)+N, temp$edge[,2]) # Match bootstrap value at node to 'destination' edge in second column, returns node number with bs <x%, to be collapsed
  temp$edge.length [toCollapse] <- 0 # Assigns 0 to edge lengths of nodes with bs <x%, collapses
  ## di2multi collapse or resolve multichotomies in phylogenetic trees
  collapsedTree <- di2multi(temp, tol=.00001) # For branch to be considered separate, must be at least this length

  collapsedTree <- ladderize(collapsedTree)
  finaltree <- ape::root.phylo(collapsedTree, outgroup = accession, resolve.root = TRUE)
  # finaltree <- ape::root(collapsedTree, outgroup = accession, resolve.root = TRUE)
  # rootedTree <<- ladderize(finaltree)
  rootedTree <- ladderize(finaltree)
  return(rootedTree)
  # return(aln_NJ)
}

# Tree and metadata parser #
parse_metaandtree <- function(treePath, metadata){
  # rootedTree <<- read.tree(treePath) #imports file in newick format instead of nexus.
  rootedTree <- treePath
  
  #\
  #  rootedTree structure:
  #    list of 3 components:
  #      $edge - a numeric matrix with 2 columns. It is the table of edges that describes
  #        the phylogenetic tree that was read in by the read.tree() function;
  #      $Nnode - a numeric vector of length one whose value is the number of nodes on the 
  #        inner branches of the tree;
  #      $tip.label - a character vector whose elements are the character string that
  #        identifies a leaf node on the phylogenetic tree;
  #/
  # dataoriginal = read.csv(metadataPath, header = TRUE) # Imports csv metadata file. It has to have header and ID column has to be the first and labeled "accession" in order for script to work.
  dataoriginal <- metadata
  
  sortingtable <- as.data.frame(rootedTree$tip.label) # Takes Tip Label information from Newick tree and transforms into a table, add ID to it and basically reorders the CSV metadata frame to match the Newick file. 
  sortingtable <- tibble::rowid_to_column(sortingtable, "ID")
  names(sortingtable)[2] <- "Accession"
  sortingdata <- merge(dataoriginal, sortingtable, by = "Accession")
  data <- sortingdata[order(sortingdata$ID),]
  listofcolumns <- as.list(data)
  accessioncharacter <- as.character(listofcolumns$Accession) # Transforms accession from Factor into character
  country <- as.numeric(listofcolumns$Country) # Transforms metadata state country from Factor into numeric
  names(country) <- accessioncharacter # Assign accession ID reference to the variable country
  characterlabels1 <- unique(listofcolumns$Country) #extract unique labels from Country column
  characterlabels <- sort(as.character(characterlabels1)) #sort and create list of characters from previous vector - has to sort to match the order from the $country as when it becomes numeric is transformed to numbers in alphabetical order.

  parsedInfo <- list(accessioncharacter = accessioncharacter,
                     country = country,
                     characterlabels = characterlabels)
  return(parsedInfo)
  
  }

parsimony_ancestral_reconstruction <- function(accessioncharacter, country, characterlabels, rootedTree) {
  
  #builds a hashmap using the leaf node strings as keys and the character states as values
  H <- hashmap::hashmap(accessioncharacter, country)
  
  # The asr_max_parsimony() function requires a numeric vector that lists the character states
  #   of the leaf nodes in sequence as one of its parameter arguments. The following for loop
  #   walks through the character vector $tip.label in the rootedTree list, starting at the 
  #   index [1], and stores the value of $tip.label[i] in the character vector accession.
  #   This character string is then passed into the find function of the hashmap and its
  #   character state is returned. Thus when the loop is complete, it has populated 
  #   the metadataStates numeric vector with the character states associated with the 
  #   leaf nodes in the order that they appeal in $tip.label;
  # Get character state for each node that isn't a leaf node (i.e. all the inner nodes)
  #asr_max_parsimony accepts 3 parameters:
  # - the list object returned by the read.tree() function
  # - the character states of the leaf nodes listed in the $tip.label character vector found in
  #     the list object
  # - the number of possible character states of the trait
  # 
  # it returns a list object with the following components:
  #
  #   $ancestral_likelihoods - a numeric matrix object with nrows = the number of inner nodes 
  #     in the phylogenetic tree, and ncolumns = to the number of possible character states
  #     of the character trait being studied. The value at $ancestral_likelihoods[n,m] is 
  #     the probability of interior node n being character state m
  #   $success - a logical vector of length one that says whether the process was a success
  #               or not
  #/
  numCharStates <- length(characterlabels) ##### change to the number above
  ancestralStates = asr_max_parsimony(rootedTree, country, numCharStates)
  
  # Deletes all keys and values from the hashmap
  H$clear()
  
  # Rebuilds hashmap using sequential numbers 1 through the number of leaf nodes as the key/index 
  #and using the integer values found in metadataStates as values. It essentially builds a hashmap 
  #of the leaf nodes of the tree: their index and their value.
  for(i in 1:length(country)) {
    H$insert(i, country[i])
  }
  
  # Loop through the inner nodes of the phylogenetic tree and assign the most likely character state
  # to that tree node;
  numLeaves <- length(country)
  numInnerNodes <- rootedTree$Nnode
  totalTreeNodes <- numLeaves + numInnerNodes
  innerNodeIndices <- (numLeaves+1):totalTreeNodes
  numCharacterStates <- length(ancestralStates$ancestral_likelihoods[1,])
  counter <- c() #initializes counter vector
  for (i in innerNodeIndices) # 474:945  # 473 leaf nodes + 472 inner nodes = 945 total;
  {                                                                         
    counter <- ancestralStates$ancestral_likelihoods[i - numLeaves,] #numeric vector of character state 
    # probabilities for inner node of index i
    H$insert(i, match(max(counter), counter)) #enters a new key-value pair 
    #(inner node i -> most likely character state)
  }
  
  #after the previous for loop executes, we now have an ASR of the phylogenetic tree given in the beginning.
  sourceList <- c()
  targetList <- c()
  
  #walk through each edge in the phylogenetic tree. if there's a state change between the two nodes, 
  #add the character states to their respective vector 
  #(diedge tail == sourceList, diedge head == targetList)
  rootedTree <- rootedTree
  
  for(row in 1:nrow(rootedTree$edge)) 
  {
    nextEdge <- rootedTree$edge[row,]
    edgeStates <- c(H$find(nextEdge[1]), H$find(nextEdge[2]))
    if (edgeStates[1] != edgeStates[2]) 
    {
      sourceList <- c(sourceList, edgeStates[1])
      targetList <- c(targetList, edgeStates[2])
    }
  }
  
  # This creates a table (in the form of a data frame) of the state changes that occur 
  #in the phylogenetic tree;
  dat <- data.frame(from = sourceList, to = targetList)
  #counts the frequency of a specific state change occurring
  #edges_file <<- plyr::count(dat)
  edges_file <- plyr::count(dat)
  names(edges_file)[names(edges_file) == "freq"] <- "value"
  
  # Extract the selected metadata state label from the data
  #metastates <<- characterlabels
  metastates <<- characterlabels
  
  # Create table for map from edge list 
  
  dat2 = as_tibble(dat) #transforms transition state data to tibble
  metastates2 = tibble::enframe(metastates) #transforms metastates in a tibble 
  
  Edge_tib = left_join(dat2, metastates2, by = c("from" = "name")) %>% 
    left_join(metastates2, by = c("to" = "name"), suffix = c("_org", "_dst")) %>% 
    select(value_org, value_dst)
  
  colnames(Edge_tib) <-c("State_org","State_dst")
  
  #Edge_list <<- Edge_tib # Edge list
  Edge_list <- Edge_tib # Edge list
  return(Edge_list)
}

## Plot transmission network in map using Leaflet - requires leaflet and geosphere libraries ##
make_nj_map <- function(geodata, transmissionpath, linecolor = "red", circlecolor = "grey"){
  org_dst <- transmissionpath # transmissionpath = probability user filtered table for beast output, org dst table for parsimony ancestry reconstruction 
  # latlong <- read.csv(filePath) #User have to input csv table with "Location","Latitude","Longitude" headers.
  latlong <- geodata
  lat_long = as_tibble(latlong)
  map_coord = left_join(org_dst, lat_long, by = c("State_org" = "Location")) %>% #Join lat_long and org_dst tables.
    left_join(lat_long, by = c("State_dst" = "Location"), suffix = c("_org", "_dst"))
  mydf <- data.frame(InitialLoc = map_coord$State_org, #Rename and reorganize table to be input ready 
                     InitialLat = map_coord$Latitude_org, 
                     InitialLong = map_coord$Longitude_org,
                     NewLat = map_coord$Latitude_dst,
                     NewLong = map_coord$Longitude_dst,
                     EndLoc = map_coord$State_dst
  )
  p1 <- as.matrix(mydf[,c(3,2)]) # it's important to list lng before lat here
  p2 <- as.matrix(mydf[,c(5,4)]) # and here
  map = gcIntermediate(p1, p2,  # This enforces the pairs of Origin and Destination for the polylines (otherwise it will enforce all locations to be connected)
                       breakAtDateLine = TRUE,
                       n=100,
                       addStartEnd=TRUE,
                       sp=TRUE) %>% 
    leaflet() %>% 
    addTiles() %>%   # addProviderTiles(providers$CartoDB.Positron) %>% #Alternative to regular tiles
    addCircleMarkers(lng = mydf$InitialLong, lat = mydf$InitialLat, popup= mydf$InitialLoc, color = circlecolor)%>% #Origin is circle marker, transparency is related to number of transmissions to/from place. 
    #   addMarkers(lng = mydf$NewLong, lat = mydf$NewLat, popup= mydf$EndLoc, color = circlecolor)%>% #Events of transmission to place are pinpointed, transparency is related to number of transmissions to/from place.
    addPolylines(color = linecolor) # The darker the line the more traffic there is between the nodes.
  
  return(map)
}


make_map_OLD <- function(treedata, metadata){
  # nexusTree2 <- read.tree(treeFileName) #imports file in newick format instead of nexus.
  nexusTree2 <- treedata
  
  # dataoriginal <- read.csv(csvFileName, header = TRUE) #imports csv metadata file. It has to have header and ID column has to be the first and labeled "Acession" in order for script to work. 
  dataoriginal <- metadata
  
  sortingtable <- as.data.frame(nexusTree2$tip.label) # Takes Tip Label information from Newick tree and transforms into a table, add ID to it and basically reorders the CSV metadata frame to match the Newick file. 
  sortingtable <- tibble::rowid_to_column(sortingtable, "ID")
  names(sortingtable)[2] <- "Accession"
  sortingdata <- merge(dataoriginal, sortingtable, by = "Accession")
  data <- sortingdata[order(sortingdata$ID),]
  
  listofcolumns <- as.list(data)
  accessioncharacter <- as.character(listofcolumns$Accession) #transforms acession from Factor into character
  country <- as.numeric(listofcolumns$Country) #transforms metadata state country from Factor into numeric
  names(country) <- accessioncharacter #assign acession ID reference to the variable country
  
  characterlabels1 <- unique(listofcolumns$Country) #extract unique labels from Country column
  characterlabels <- sort(as.character(characterlabels1)) #sort and create list of characters from previous vector - has to sort to match the order from the $country as when it becomes numeric is transformed to numbers in alphabetical order.
  
  ####################################### END OF NEW BLOCK ###########################################
  
  
  #charIndex <- readline(prompt ="Type the number equivalent to the character state index of the nexus file you want to build the network from: ")
  #characterIndex <- as.numeric(charIndex) #Transforms the input from string to numeric so it can be loaded on metadataRef
  rootedTree <- nexusTree2
  
  #\
  #  rootedTree structure:
  #    list of 3 components:
  #      $edge - a numeric matrix with 2 columns. It is the table of edges that describes
  #        the phylogenetic tree that was read in by the read.tree() function;
  #      $Nnode - a numeric vector of length one whose value is the number of nodes on the 
  #        inner branches of the tree;
  #      $tip.label - a character vector whose elements are the character string that
  #        identifies a leaf node on the phylogenetic tree;
  #/
  
  
  #metadataRef <- nexusData$charMatrix[,characterIndex] #CharacterIndex change the number of the character state index of the nexus file you want to use
  #ref2 <- attr(metadataRef,"names")
  
  #builds a hashmap using the leaf node strings as keys and the character states as values
  #H <- hashmap(ref2, metadataRef)
  H <- hashmap::hashmap(accessioncharacter, country)
  
  # The asr_max_parsimony() function requires a numeric vector that lists the character states
  #   of the leaf nodes in sequence as one of its parameter arguments. The following for loop
  #   walks through the character vector $tip.label in the rootedTree list, starting at the 
  #   index [1], and stores the value of $tip.label[i] in the character vector accession.
  #   This character string is then passed into the find function of the hashmap and its
  #   character state is returned. Thus when the loop is complete, it has populated 
  #   the metadataStates numeric vector with the character states associated with the 
  #   leaf nodes in the order that they appeal in $tip.label;
  # Get character state for each node that isn't a leaf node (i.e. all the inner nodes)
  #asr_max_parsimony accepts 3 parameters:
  # - the list object returned by the read.tree() function
  # - the character states of the leaf nodes listed in the $tip.label character vector found in
  #     the list object
  # - the number of possible character states of the trait
  # 
  # it returns a list object with the following components:
  #
  #   $ancestral_likelihoods - a numeric matrix object with nrows = the number of inner nodes 
  #     in the phylogenetic tree, and ncolumns = to the number of possible character states
  #     of the character trait being studied. The value at $ancestral_likelihoods[n,m] is 
  #     the probability of interior node n being character state m
  #   $success - a logical vector of length one that says whether the process was a success
  #               or not
  #/
  #numCharStates <- length(nexusData$characterLabels[[characterIndex]]) ##### change to the number above
  numCharStates <- length(characterlabels) ##### change to the number above
  ancestralStates = asr_max_parsimony(rootedTree, country, numCharStates)
  
  # Deletes all keys and values from the hashmap
  H$clear()
  
  # Rebuilds hashmap using sequential numbers 1 through the number of leaf nodes as the key/index 
  #and using the integer values found in metadataStates as values. It essentially builds a hashmap 
  #of the leaf nodes of the tree: their index and their value.
  for(i in 1:length(country)) {
    H$insert(i, country[i])
  }
  
  # Loop through the inner nodes of the phylogenetic tree and assign the most likely character state
  # to that tree node;
  numLeaves <- length(country)
  numInnerNodes <- rootedTree$Nnode
  totalTreeNodes <- numLeaves + numInnerNodes
  innerNodeIndices <- (numLeaves+1):totalTreeNodes
  numCharacterStates <- length(ancestralStates$ancestral_likelihoods[1,])
  counter <- c() #initializes counter vector
  for (i in innerNodeIndices) # 474:945  # 473 leaf nodes + 472 inner nodes = 945 total;
  {                                                                         
    counter <- ancestralStates$ancestral_likelihoods[i - numLeaves,] #numeric vector of character state 
    # probabilities for inner node of index i
    H$insert(i, match(max(counter), counter)) #enters a new key-value pair 
    #(inner node i -> most likely character state)
  }
  
  #after the previous for loop executes, we now have an ASR of the phylogenetic tree given in the beginning.
  sourceList <- c()
  targetList <- c()
  
  #walk through each edge in the phylogenetic tree. if there's a state change between the two nodes, 
  #add the character states to their respective vector 
  #(diedge tail == sourceList, diedge head == targetList)
  
  for(row in 1:nrow(rootedTree$edge)) 
  {
    nextEdge <- rootedTree$edge[row,]
    edgeStates <- c(H$find(nextEdge[1]), H$find(nextEdge[2]))
    if (edgeStates[1] != edgeStates[2]) 
    {
      sourceList <- c(sourceList, edgeStates[1])
      targetList <- c(targetList, edgeStates[2])
    }
  }
  
  # This creates a table (in the form of a data frame) of the state changes that occur 
  #in the phylogenetic tree;
  dat <- data.frame(from = sourceList, to = targetList)
  #counts the frequency of a specific state change occurring
  edges <- plyr::count(dat)
  names(edges)[names(edges) == "freq"] <- "value"
  
  # Extract the selected metadata state label from the data
  metastates <- characterlabels
  
  ##### NEW CODE BELOW TO CREATE TABLE FOR MAP FROM PARSIMONY DATA #####
  
  dat2 = as_tibble(dat) #transforms transition state data to tibble
  metastates2 = tibble::enframe(metastates) #transforms metastates in a tibble 
  
  Edge_tib = left_join(dat2, metastates2, by = c("from" = "name")) %>% 
    left_join(metastates2, by = c("to" = "name"), suffix = c("_org", "_dst")) %>% 
    select(value_org, value_dst)
  
  colnames(Edge_tib) <-c("State_org","State_dst")
  
  Edge_filtered = Edge_tib
  
  library(leaflet)
  library(geosphere)
  
  org_dst <- Edge_filtered # Edge_filtered = probability user filtered table 
  
  latlong <- read.csv(csvFileName) #User have to input csv table with "Location","Latitude","Longitude" headers.
  
  lat_long = as_tibble(latlong)
  
  map_coord = left_join(org_dst, lat_long, by = c("State_org" = "Location")) %>% #Join lat_long and org_dst tables.
    left_join(lat_long, by = c("State_dst" = "Location"), suffix = c("_org", "_dst"))
  
  mydf2 <- data.frame(InitialLoc = map_coord$State_org, #Rename and reorganize table to be input ready 
                      InitialLat = map_coord$Latitude_org, 
                      InitialLong = map_coord$Longitude_org,
                      NewLat = map_coord$Latitude_dst,
                      NewLong = map_coord$Longitude_dst,
                      EndLoc = map_coord$State_dst
  )
  
  p1 <- as.matrix(mydf2[,c(3,2)]) # it's important to list lng before lat here
  p2 <- as.matrix(mydf2[,c(5,4)]) # and here
  
  mapplot <- gcIntermediate(p1, p2,  #This enforces the pairs of Origin and Destination for the polylines (otherwise it will enforce all locations to be connected)
                            breakAtDateLine = TRUE,
                            n=100, 
                            addStartEnd=TRUE,
                            sp=TRUE) %>% 
    leaflet() %>% 
    addTiles() %>% 
    addCircleMarkers(lng = mydf2$InitialLong, lat = mydf2$InitialLat, popup= mydf2$InitialLoc)%>% #Origin is circle marker, transparency is related to number of transmissions to/from place. 
    addMarkers(lng = mydf2$NewLong, lat = mydf2$NewLat, popup= mydf2$EndLoc)%>% #Events of transmission to place are pinpointed, transparency is related to number of transmissions to/from place.
    addPolylines() #The darker the line the more traffic there is between the nodes.
  
  return(mapplot)
  
}

make_globe <- function(graph, geodata, columnSelection){
  # locations <- graph$x$nodes %>% inner_join(geodata, by = c("label" = colnames(geodata)[1]))
  locations <- graph$x$nodes %>% inner_join(geodata, by = c("label" = columnSelection))
  
  graph_df <- graph$x$edges %>%
    select(-value) %>% 
    dplyr::inner_join(locations, by = c("from" = "id")) %>% 
    dplyr::inner_join(locations, by = c("to" = "id"), suffix = c(".from", ".to")) %>% 
    mutate(path = paste0(label.from,"->",label.to),
           stroke = as.numeric(value.from)/10,
           color = randomcoloR::distinctColorPalette(k = nrow(graph$x$edges)))
           #color = RColorBrewer::brewer.pal(length(graph$x$edges), "Set1"))
  
  globe <- create_globe() %>% 
    globe_pov(graph_df$Latitude.from[1],graph_df$Longitude.from[1]) %>% 
    arcs_data(graph_df) %>% 
    arcs_start_lat("Latitude.from") %>% 
    arcs_start_lon("Longitude.from") %>% 
    arcs_end_lat("Latitude.to") %>% 
    arcs_end_lon("Longitude.to") %>%
    arcs_color("color") %>% 
    arcs_label("path") %>%
    #arcs_stroke("stroke") %>% 
    #arcs_on_hover(func = "function(data) {var globe = get_globe(data.path);}") %>% 
    #arcs_on_click(func = "function(data) {var globe = get_globe(data.path);}") %>% 
    labels_data(geodata) %>% 
    labels_lat("Latitude") %>% 
    labels_lon("Longitude") %>% 
    labels_text(columnSelection) %>% 
    labels_include_dot(include = TRUE) %>% 
    labels_dot_radius(radius = 0.3) %>% 
    #scale_labels_size() %>% 
    #scale_labels_radius() %>% 
    globe_background("#fff") %>% 
    show_atmosphere(TRUE) %>%
    show_graticules(TRUE) %>% 
    globe_img_url(url = image_url("blue-marble"))
  
  return(globe)
}

make_map <- function(graph, geodata, columnSelection, basemapLayer = "Imagery", hideArrowHead = FALSE, arrowFilled = TRUE, showLabels = FALSE, labelColor = "#000000", showPoints = FALSE, pointColor = "#000000", pointOpacity = 0.5){
  # locations <- graph$x$nodes %>% inner_join(geodata, by = c("label" = colnames(geodata)[1]))
  locations <- graph$x$nodes %>% inner_join(geodata, by = c("label" = columnSelection))
  
  graph_df <- graph$x$edges %>%
    select(-value) %>% 
    dplyr::inner_join(locations, by = c("from" = "id")) %>% 
    dplyr::inner_join(locations, by = c("to" = "id"), suffix = c(".from", ".to")) %>% 
    mutate(path = paste0(label.from,"->",label.to),
           #stroke = as.numeric(value.from)/10,
           stroke = as.numeric(value.from)/5,
           color = randomcoloR::distinctColorPalette(k = nrow(graph$x$edges)))
  
  ## Setup for Swoopy.js
  # esriPlugin <- htmlDependency("leaflet.esri", "1.0.3",
  #                              src = c(href = "https://cdn.jsdelivr.net/leaflet.esri/1.0.3/"),
  #                              script = "esri-leaflet.js"
  # )
  
  esriPlugin <- htmlDependency("leaflet.esri", "1.0.3",
                               src = normalizePath("www"),
                               script = "esri-leaflet.js"
  )
  
  # swoopyPlugin <- htmlDependency("leaflet-swoopy", "3.4.1", 
  #                                src = c(href = "https://unpkg.com/leaflet-swoopy@3.4.1/build/"),
  #                                script = "Leaflet.SwoopyArrow.js"
  # )
  
  # swoopyPlugin <- htmlDependency("leaflet-swoopy", "3.4.1", 
  #                                src = c(href = "https://unpkg.com/leaflet-swoopy@3.4.1/build/"),
  #                                script = "Leaflet.SwoopyArrow.min.js"
  # )
  
  swoopyPlugin <- htmlDependency("leaflet-swoopy", "3.4.1", 
                                 src = normalizePath("www"),
                                 script = "Leaflet.SwoopyArrow.min.js"
  )
  
  registerleafletPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  header <- "function(el, x) {"
  ## https://esri.github.io/esri-leaflet/api-reference/layers/basemap-layer.html
  basemap <- paste0("L.esri.basemapLayer('",basemapLayer,"').addTo(this);")
  swoopys <- ""
  for (i in 1:nrow(graph_df)){
    fromLat <- graph_df$Latitude.from[i]
    fromLong <- graph_df$Longitude.from[i]
    toLat <- graph_df$Latitude.to[i]
    toLong <- graph_df$Longitude.to[i]
    color <- graph_df$color[i]
    arrow <- if(arrowFilled){"true"}else{"false"}
    head <- if(hideArrowHead){"true"}else{"false"}
    swoopyIter <- paste0("L.swoopyArrow([",fromLat,",",fromLong,"], [",toLat,",",toLong,"], {color: '",color,"', factor: 0.7, weight: 2, hideArrowHead: ",head,", arrowFilled: ",arrow,"}).addTo(this);")
    #print(swoopyIter)
    swoopys <- paste0(swoopys, swoopyIter)
  }
  
  fromLocs <- graph_df %>% select(label.from, Latitude.from, Longitude.from)
  colnames(fromLocs) <- c("location", "latitude", "longitude")
  toLocs <- graph_df %>% select(label.to, Latitude.to, Longitude.to)
  colnames(toLocs) <- c("location", "latitude", "longitude")
  allLocs <- fromLocs %>% rbind(toLocs) %>% unique()
  
  labels <- ""
  if (showLabels){
    
    for (i in 1:nrow(allLocs)){
      fromLat <- allLocs$latitude[i]
      fromLong <- allLocs$longitude[i]
      toLat <- allLocs$latitude[i]
      toLong <- allLocs$longitude[i]
      loc <- allLocs$location[i]
      labelIter <- paste0("L.swoopyArrow([",fromLat,",",fromLong,"], [",toLat,",",toLong,"], {label: '",loc,"', labelColor: '",labelColor,"', labelFontSize: 14, iconAnchor: [-5, -5], iconSize: [20, 16], factor: 0.7, weight: 0}).addTo(this);")
      #print(labelIter)
      labels <- paste0(labels, labelIter)
    }
  }
  
  points <- ""
  
  if (showPoints){
    for (i in 1:nrow(allLocs)){
      fromLat <- allLocs$latitude[i]
      fromLong <- allLocs$longitude[i]
      toLat <- allLocs$latitude[i]
      toLong <- allLocs$longitude[i]
      loc <- allLocs$location[i]
      pointsIter <- paste0("L.circleMarker([",fromLat,",",fromLong,"], {color: '",pointColor,"', fillColor: '",pointColor,"', fillOpacity: ",pointOpacity,", stroke: 0, }).addTo(this);")
      points <- paste0(points, pointsIter)
    }
  }
  
  
  footer <- "}"
  
  renderText <- paste0(header, basemap, points, swoopys, labels, footer)
  #renderText <- paste0(header, swoopys, labels, footer)
  
  leafletmap <- leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addScaleBar(position = "bottomleft") %>% 
    setView(mean(graph_df$Longitude.from), mean(graph_df$Latitude.from), zoom = 5) %>%
    registerleafletPlugin(esriPlugin) %>%
    registerleafletPlugin(swoopyPlugin) %>% 
    onRender(renderText)
  
  return(leafletmap)
}
