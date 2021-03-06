
# This creates a generic validate function, so that the developers of a hydrologic
# model just have to add the method.



RHydro = function(object, newval, model,  ...) {
  dots = list(...)
  if (length(dots) > 1 & !missing(newval)) stop("you cannot provide arguments both through newval and ellipsis argument (...)")  
  if (missing(newval)) newval = dots
#  if (!missing(model) && !missing(Parameters)) {
#    opar = Parameters
#    Parameters = list()
#    Parameters[[model]] = opar 
#  } 
  if ("Parameters" %in% names(newval)) {
    Parameters = newval$Parameters
    newval = newval[-which(names(newval) == "Parameters")]
    if (!missing(model)) {
      Parameters = list(Parameters)
      names(Parameters) = model
    }
    if (is.list(Parameters)) {
      for (ip in 1:length(Parameters)) {
        lpar = Parameters[[ip]]
        if (!"model" %in% names(lpar)) lpar$model = names(Parameters)[ip]
        pnames = slotNames("HMPar")
        names(lpar) = match.arg(names(lpar), pnames, several.ok = TRUE)
        Parameters[[ip]] = do.call("HMPar", lpar)
      }
    } else stop(paste("Expecting Parameters as list, got", class(Parameters)))
  } else Parameters = list()
  if ("Dots" %in% names(newval)) {
    Dots = newval$Dots
    newval = newval[-which(names(newval) =="Dots")]
  } else Dots = list()
  if ("control" %in% names(newval)) {
    control = newval$control
    newval = newval[-which(names(newval) == "control")]
  } else control = list()
  if ("performance" %in% names(newval)) {
    performance = newval$performance
    if (!is.list(performance)) {
      performance = list(performance)
      names(performance) = model
    }
    newval = newval[-which(names(newval) == "performance")]
  } else performance = list()
  if ("Obs" %in% names(newval) || "Pred" %in% names(newval)) {
    if ("Obs" %in% names(newval)) Obs = do.call("HMData", newval$Obs) else Obs = NULL
    if ("Pred" %in% names(newval)) {
      for (ip in 1:length(newval$Pred)) {
        lpred = do.call("HMData", newval$Pred[[ip]])
        if (ip == 1) Pred = list(p1 = lpred) else Pred = c(Pred, lpred)
      }
      names(Pred) = names(newval$Pred)
    } else Pred = list()
  } else {    # The elements of Obs are given separately, no Pred
    Obs = do.call("HMData", newval)
    Pred = list()
  }
  
  if (missing(object) || is.null(object)) {
    object = HM(Obs = Obs, Pred = Pred, Parameters = Parameters, Dots = Dots, control = control)
  } else {
    if (length(Parameters) > 0) object@Parameters = updateParameters(object@Parameters, Parameters)
    if (!is.null(Obs)) object@Obs = updateHMData(HMobs(object), Obs)
    if (length(Pred) > 0) object@Pred = updateHMData(HMpred(object), Pred)
    if (length(control) > 0) object@control = modifyList(object@control, control)   
    if (length(performance) > 0) object@performance = modifyList(object@control, performance)   
    if (length(Dots) > 0) object@Dots = modifyList(object@Dots, Dots)   
  }
  if (length(Parameters) > 0) {
    for (ip in 1:length(Parameters)) {
      mod = names(Parameters)[ip]
      valfun = paste0("validate.", mod)
      stopifnot(do.call(valfun, list(object)))
    }
  }
  object  
}


