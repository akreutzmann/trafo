check_woparam <- function(trafo, custom_trafo) {
  
  if (!(trafo %in% c("log", "neglog", "reciprocal", "glog"))) {
    stop(paste0(trafo, " is not a supported transformation. 
                Please provide valid variable name for trafo."))
  } 
  if (!is.null(custom_trafo)) {
    
    if (!inherits(custom_trafo, "list")) {
      stop("An additional transformation needs to be added in argument 
           custom_trafo as a list with the transformation function.")
    }
    
    N_custom <- length(custom_trafo)
    for (i in 1:N_custom) {
      if (!inherits(custom_trafo[[i]], "function")) {
        stop("The element of the list needs to be a function. This Function 
             for custom transformations needs to have exactly one 
             argument y and only one return yt.")
      }
      else if (inherits(custom_trafo[[i]], "function") 
               && !all(names(formals(custom_trafo[[i]])) == c("y"))) {
        stop("Function for custom transformation needs to have exactly one 
             argument y and only one return yt")
      }
    }
}
  
  
    }