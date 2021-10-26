#' DCEmgmt function
#'
#' Prepare the results of a DCE to be analysed through choice models.
#' @param data Data.frame with the survey data in wide data format. See 'data(survey)'
#' @param droprow Rows to be deleted. Specify a single row (droprow = x) or more than one (dropdown = x:y).
#' @param keepvar Select the case-specific variables to be kept. For example: keepvar = c("x", "y").
#' @param create.id Create a variable called "id" with a unique identificator per respondent. create.id = TRUE
#' @param blocks Specifies the variables designating each choice set block. Each choice set variable must be named
#' as a letter (for instance Q) followed by a number (starting by 1). Thus, the first block of choice sets can be
#' named Q1, Q2, Q3, Q4 whereas the second can be called F5, F6, F7, F8. In this case, block = c("Q", "F"). If there
#' is only one block, block = c("Q").
#' @param sets Specify the number of choice sets per block. If there is only one block, specify the total number
#' of choice sets. For instance, sets = 8.
#' @param alts Specify the number of alternatives per choice set. For example, alts = 3.
#' @param options Vector indicating the string used to designate the choice. For example, option = c("Option A", "Option B", "Option C"). See examples.
#' @param design Data frame with the design matrix in dummy or effects coding. PONER EJEMPLO

#' @examples
#' data(survey)
#' data(design)
#'
#' data.c<-DCEmgmt(data = data,
#'         droprow = 1:5,
#'         keepvar = c("age", "height"),
#'         create.id = TRUE,
#'         blocks = c("B1_", "B2_"),
#'         sets = 8,
#'         alts = 2,
#'         options = c("Prefiero el servicio A", "Prefiero el servicio B"),
#'         design = design)
#'
#' #The data frame data.c contains the coded DCE data

#' @export
DCEmgmt <- function(data, droprow, keepvar, create.id, blocks, sets, alts, options, design){
  data.n <- data
  #only an integer
  if (missing(droprow)){
    data.n <- data.n
  } else {
    data.n <- data.n[-droprow,]

  }

  #create user id
  if (create.id == TRUE){
    data.n$id <- seq.int(nrow(data.n))
    data.n <- data.n
  }
  if (create.id == FALSE){
    data.n <- data.n
  }
  data.c <- data.frame(id=c(), resp=c(), cs=c())


  for (b in 1:length(blocks)){
    setst <- sets*b
    start <- (b-1)*sets+1
    for (i in seq(from = start, to = setst)) {
      merged <- cbind(data.n$id,data.n[paste0(blocks[b], i)], i)
      names(merged) <- c("id", "resp", "cs")
      data.c <- rbind(data.c, merged)
    }
    data.c <- data.c
  }

  #keep variables
  if (missing(keepvar)){

  } else {
   for (x in 1:length(keepvar)) {
    data.c <- merge(data.c, cbind(id = data.n$id, data.n[paste0(keepvar[x])]), by = "id")
   }}
  remove(data.n)
  data.c <- data.c

  #alternatives and sorting
  data.c1 <- data.frame()
  for (i in 1:alts){
    data.c1 <- rbind(data.c1,data.c)
  }
  #data.c1 <<- data.c
  data.c <- data.c1[with(data.c1, order(id, cs)),]
  data.c <- data.c
  remove(data.c1)

  #cs groups
  data.c$gid <- rep(1:(nrow(data.c)/alts), each=alts)
  data.c <- data.c

  #omit nulls
  data.c <- stats::na.omit(data.c)
  data.c <- data.c

  #set alts
  data.c$alts <- rep(1:alts, nrow(data.c)/alts)
  data.c <- data.c

  #options treatment
  data.c$choice <- 0
  if (alts == 2){
    data.c$choice <- ifelse((data.c$resp==options[1] & data.c$alts==1)|(data.c$resp==options[2] & data.c$alts==2) , 1,0)
  }
  if (alts == 3){
    data.c$choice <- ifelse((data.c$resp==options[1] & data.c$alts==1)|(data.c$resp==options[2] & data.c$alts==2)|(data.c$resp==options[3] & data.c$alts==3), 1,0)
  }
  if (alts == 4){
    data.c$choice <- ifelse((data.c$resp==options[1] & data.c$alts==1)|(data.c$resp==options[2] & data.c$alts==2)|(data.c$resp==options[3] & data.c$alts==3)|(data.c$resp==options[4] & data.c$alts==4), 1,0)
  }
  if (alts == 5){
    data.c$choice <- ifelse((data.c$resp==options[1] & data.c$alts==1)|(data.c$resp==options[2] & data.c$alts==2)|(data.c$resp==options[3] & data.c$alts==3)|(data.c$resp==options[4] & data.c$alts==4)|(data.c$resp==options[5] & data.c$alts==5), 1,0)
  }
  if (missing(design)){
    data.c <<- data.c
  } else {
    data.c <- merge(data.c,design, by=c("cs","alts"))
  }
  data.c <- data.c[with(data.c, order(id, cs)),]
  return(data.c)
}

