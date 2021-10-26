#' DCEestm function
#'
#' Code DCE survey raw data so that it can be analysed through discrete choice models.
#' @param data.c Data coded with the function DCEmgmt
#' @param model Select the model to be estimated: conditional logit ("clogit"), random parameters logit model ("mixlogit"), both models ("all")
#' @param params Vector with the model parameters. For example: params <- c("pers2", "pers3", "pers4")
#' @param rand Vector with the model random parameters. For example: params <- c("pers2", "pers3")
#' @examples
#'
#'#First, the DCEmgmt function needs to be used to code the data
#'
#'
#'#Once we have the "data.c" file, we can analyse the results.
#'
#' data(data.c)
#'
#' \dontrun{
#' DCEestm(data.c = data, model = "clogit",
#' params = c("pers2", "pers3", "pers4", "form2","rut2", "prec2", "prec3", "prec4"),
#' rand = c("pers2", "pers3", "pers4", "form2", "rut2", "prec2", "prec3", "prec4"))
#' }
#'
#' @export
#'

require(survival)
require(mlogit)
require(stats)

DCEestm <- function(data.c, model, params, rand) {

  if (missing(data.c)){
    warning("Error: data is missing")
  }
  if (missing(model)){
    warning("Error: specify a model. Options: conditional logit model -> 'clogit', random parameters logit model -> 'mixlogit' ")
  } else {
    if (model == "clogit" | model == "all"){
      if (missing(params)){
        warning(command=paste0("Error: you need to specify the regressors of the model. For example ", shQuote("params <- c(\"pers2\", \"pers3\", \"pers4\")", type = "sh")))
      } else {
        params <- append(params, "strata(gid)")
        titles <- paste(params, collapse ="+")
        resultsCLM <- survival::clogit(stats::as.formula(paste("choice~",titles)),data=data.c)
        resultsCLM <<- resultsCLM
        warning("Conditional logit results: \n \n")
        print(summary(resultsCLM))
      }
    }
    if (model == "mixlogit" | model == "all") {
        if(missing(rand)){
          warning("Error: you need to specify the random variables. For example ", shQuote("rand <- c(\"form2\", \"rut2\")", type = "sh"))
        } else {
          data.c.mlogit <- mlogit::mlogit.data(data.c, shape = "long", alt.var = "alts", chid.var = "gid", id.var = "id")
          x <- length(rand)
          y <- 0
          rand.c <- c()
          while(y<x){
            rand.c <- cbind(rand, "n")
            y <- y+1
          }
          rand.e <- rand.c[,2]
          names(rand.e) <- rand
          resultsXLM <- mlogit::mlogit(stats::as.formula(paste("choice~",paste(params, collapse = "+"), " | 0")), data=data.c.mlogit, rpar=rand.e, R=100, halton=NA, panel=TRUE)
          resultsXLM <<- resultsXLM
          warning("Random parameters logit results: \n \n")
          print(summary(resultsXLM))
    }}
    if (model == "all") {
      if (missing(rand)){
        warning("Error: you need to specify the random variables for the random parameters logit model. For example ", shQuote("rand <- c(\"form2\", \"rut2\")", type = "sh"))
      } else{

        warning("Conditional logit results: \n \n")
        print(summary(resultsCLM))
        warning("------------------------------------------------------------------ \n")
        warning("Random parameters logit results: \n \n")
        print(summary(resultsXLM))
        warning("------------------------------------------------------------------ \n")
      }
    }
  }
}
