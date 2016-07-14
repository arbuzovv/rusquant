"importDefaults" <-
function(calling.fun=NULL) {
  sc <- sys.call(-1)
  if(is.null(calling.fun)) calling.fun <- as.character(sc[[1]])
  if(is.function(calling.fun)) calling.fun <- deparse(substitute(calling.fun))
  if(is.null(sc)) 
    stop("importDefaults is only valid inside a function call") 
  if(as.character(sc[[1]]) != calling.fun) return()
  #calling.fun <- as.character(match.call(call=as.call(sys.call(-1)))[1])
  all.defaults <- getDefaults(calling.fun)
  if(is.null(all.defaults)) return()
  envir <- as.environment(-1)
  #passed.args <- names(sapply(match.call(call=as.call(sys.call(-1)))[-1],deparse))
  passed.args <- names(as.list(match.call(
                       definition=eval(parse(text=calling.fun)),
                       call=as.call(sys.call(-1)))))[-1]
  formal.args <- names(formals(as.character(sys.call(-1))))
  default.args <- names(which(sapply(all.defaults,function(x) !is.null(x))==TRUE))
  for(arg in formal.args) {
    if(!arg %in% passed.args) {
      if(arg %in% default.args) {
        if(typeof(all.defaults[arg][[1]])=='list') {
          assign(arg, as.vector(all.defaults[arg][[1]]),envir=envir)
        } 
        else if(typeof(all.defaults[arg][[1]]) %in% c('symbol','language')) {
          assign(arg, all.defaults[arg][[1]],envir=envir)
        }
        else if(typeof(all.defaults[arg][[1]])=="character") {
           if(length(all.defaults[arg][[1]])==1) {
             assign(arg, eval(parse(text=all.defaults[arg][[1]])),envir=envir)
           } else {
             assign(arg, as.character(parse(text=all.defaults[arg][[1]])),envir=envir)
           }
        }
        else {
          assign(arg, as.vector(unlist(all.defaults[arg][[1]])),envir=envir)
        }
      }
    }
  }
}
`.importDefaults` <- importDefaults
`useDefaults` <-
function(name)
{
  if(is.function(name))
    name <- deparse(substitute(name))
  if(!is.function(eval(parse(text=name))))
    stop("argument 'name' must be a function")
  if(!identical(grep('importDefaults',deparse(body(name))),integer(0))) invisible(return())
  cc <- paste('if(exists(".importDefaults")) .importDefaults(calling.fun="',name,'")',sep='')
  if(deparse(body(name))[1] == "{") {
    new.body <- as.call(parse(text=c(deparse(body(name))[1],
                                   cc,
                                   deparse(body(name))[-1])))[[1]]
  } else {
    new.body <- as.call(parse(text=c("{",cc,deparse(body(name))[1],"}")))[[1]]
  }
  f <- as.function(c(formals(name),new.body))
  attributes(f) <- attributes(eval(parse(text=name)))
  # if in Global, just change Global
  if(!exists(name,globalenv(),inherits=FALSE)) {
  # influenced by mtrace code in package:debug
    # big trick: set environment of new to original
    environment(f) <- environment(eval(parse(text=name)))
    locked <- bindingIsLocked(name,as.environment(find(name)))
    # if binding is Locked unlock, relock...
    if(locked)
      unlockBinding(name,as.environment(find(name)))
    assign(name,f,as.environment(find(name)))
    if(locked) {
      ow <- options("warn")
      on.exit(options(ow))
      options(warn=-1)
      lockBinding(name,as.environment(find(name)))
    }
  } else {
    # Global - just reassign
    environment(f) <- globalenv()
    assign(name,f,globalenv())
  }
  invisible(f)
}
`usingDefaults`<-
function(name)
{
  if(length(name)==1) {
    if(is.function(name)) 
      name <- deparse(substitute(name))
    try(
      if(is.function(eval(parse(text=name)))) {
        return(!identical(grep('importDefaults',deparse(body(name))[2:3]),integer(0)))
      },silent=TRUE)
      return(NA)
  } else {
    sapply(name,usingDefaults)
  }
}
`useDefaults.original` <-
function(name)
{
  #check for .Defaults <- TRUE to NOT REPEAT
  env <- as.environment(-1)
  if(is.function(name))
    name <- deparse(substitute(name))

  simple.name <- name

  if(!is.function(eval(parse(text=name))))
    stop("argument must be a function")

  if(identical(grep("(\\.usingDefaults) | (importDefaults)",deparse(body(get(name,
     env=globalenv())))), integer(0))) { 
    # only process if not been processes before
    pkg <- find(name)[1]
    if(pkg != ".GlobalEnv") {
      pkg <- strsplit(pkg,":")[[1]][2]
      ns <- try(loadNamespace(pkg),silent=TRUE)
      if(isNamespace(ns)) {
        name <- paste(pkg,"::",name,sep='')
      }
    } else {
      # if defined in global environment, or not a namespace
      # create new function name .name.CALL in GlobalEnv
      name <- paste(".",name,".CALL",sep="")
      # assign to .GlobalEnv for internal call.
      assign(name,get(simple.name,env=globalenv()),env=globalenv())
    }
    new.body <- as.call(parse(text=
                  c("{",
                      ".usingDefaults <- TRUE",
                      "mc <- match.call()[-1]",
    "all.defaults <- getDefaults(as.character(match.call()[1]))",
    "passed.args <- names(as.list(mc))",
    "formal.args <- names(formals(as.character(match.call()[1])))",
    "default.args <- names(all.defaults)",
    "final.args <- list()",
    "for (arg in formal.args) {",
         "if (!arg %in% passed.args) {",
             "if (arg %in% default.args) {",
                 "if (typeof(all.defaults[arg][[1]]) == 'list') {",
                   "final.args[[arg]] <- as.vector(all.defaults[arg][[1]])",
                 "}",
                 "else if (typeof(all.defaults[arg][[1]]) %in% c('symbol', ",
                   "'language')) {",
                   "final.args[[arg]] <- all.defaults[arg][[1]]",
                 "}",
                 "else if (typeof(all.defaults[arg][[1]]) == 'character') {",
                   "final.args[[arg]] <- (all.defaults[arg][[1]])",
                   "if(length(final.args[[arg]])>1)",
                     "final.args[[arg]] <- as.character(parse(text=final.args[[arg]]))",
                 "}",
                 "else {",
                   "final.args[[arg]] <- as.vector(unlist(all.defaults[arg][[1]]))",
                 "}",
             "}",
         "}",
         "else {",
             "if (typeof(as.list(mc)[arg][[1]]) == 'list') {",
                 "final.args[[arg]] <- as.vector(as.list(mc)[arg][[1]])",
             "}",
             "else if (typeof(as.list(mc)[arg][[1]]) %in% c('symbol', ",
                 "'language')) {",
                 "final.args[[arg]] <- as.list(mc)[arg][[1]]",
             "}",
             "else if (typeof(as.list(mc)[arg][[1]]) == 'character') {",
                 "final.args[[arg]] <- deparse(as.list(mc)[arg][[1]])",
             "}",
             "else {",
                 "final.args[[arg]] <- as.vector(unlist(as.list(mc)[arg][[1]]))",
             "}",
         "}",
    "}",
      "if(length(names(final.args))==0) {",
        "final.args <- ''",
      "} else {",
        "final.args <- paste(names(final.args),'=',",
                            "as.list(final.args),sep='',collapse=',')",
      "}",
      paste("eval(parse(text=paste('",name,
            "(',final.args,')',sep='')),env=sys.parent())",
            sep=''
           ),
    "}")
    ))[[1]] 
    assign(simple.name,as.function(c(formals(simple.name),new.body)),env)
  }
}
`unDefaults` <-
function(name)
{
  if(is.function(name)) name <- deparse(substitute(name))
  # if Defaults was hard-coded - return from unDefaults
  if(identical(grep('\\.importDefaults',deparse(body(name))),integer(0))) {
    invisible(return())
  }
    #stop(paste(dQuote("useDefaults"),"not set for",substitute(name)))
  old.body <- as.call(parse(text=
                      deparse(body(name))[-grep('importDefaults',
                                                deparse(body(name)))]))[[1]]
  f <- as.function(c(formals(name),old.body))
  attributes(f) <- attributes(eval(parse(text=name)))
  if(!exists(name,globalenv(),inherits=FALSE)) {
    environment(f) <- environment(eval(parse(text=name)))
    locked <- bindingIsLocked(name,as.environment(find(name)))
    if(locked)
      unlockBinding(name,as.environment(find(name)))
    assign(name,f,as.environment(find(name)))
    if(locked) {
      ow <- options("warn")
      on.exit(options(ow))
      options(warn=-1)
      lockBinding(name,as.environment(find(name)))
    }
  } else {
    # Global - just reassign
    environment(f) <- globalenv()
    assign(name,f,globalenv())
  }
  invisible(f)
}

"unDefaults.original" <-
function(name)
{
  if(!identical(grep('\\.usingDefaults',deparse(body(name))),
               integer(0))) {
    env <- as.environment(-1)
    if(is.function(name)) name <- deparse(substitute(name))
    if(exists(paste('.',name,'.CALL',sep=''),env,inherits=FALSE)) {
      assign(name,get(paste('.',name,'.CALL',sep=''),env),env)
      remove(list=paste('.',name,'.CALL',sep=''),envir=env)
    } else {
      remove(list=name,envir=env)
    }
  } else { 
    stop(paste(dQuote("useDefaults"),"not set for",substitute(name)))
  }
}

`setDefaults` <-
function (.name, ...) 
{
    if (is.function(.name)) 
        .name <- deparse(substitute(.name))
    if(!is.function(eval(parse(text=.name))))
      stop("argument '.name' must be a function")
    useDefaults(.name)
    default.name <- paste(.name, "Default", sep = ".")
    old.defaults <- getDefaults(.name)
    new.defaults <- list(...)
    avail.defaults <- formals(.name)
    matched.defaults <- list()
    for(arg in names(new.defaults)) {
      if(!is.na(pmatch(arg,names(avail.defaults)))) {
        # if partial match is made:
        arg.name <- match.arg(arg,names(avail.defaults))
        mc <- match.call()[[arg]]
        if(typeof(mc)=='language') mc <- eval(mc)
        if(is.character(mc))
            new.defaults[[arg]] <-  paste("'", mc, "'", sep = "")
        if(is.name(mc))
            new.defaults[[arg]] <- as.character(mc)
        matched.defaults[[arg.name]] <- new.defaults[[arg]]       
        if(is.null(new.defaults[[arg]])) old.defaults[[arg.name]]<-NULL
      } else {
        warning(paste(
                sQuote(arg),"was not set, possibly not a formal arg for",
                sQuote(.name)))
      }
    }
    # merge original and new, then take first value only
    all.and.matched <- c(matched.defaults,old.defaults)
    all.and.matched <- all.and.matched[unique(names(all.and.matched))]
    if (length(all.and.matched) == 0) {
      if(!is.null(getDefaults(.name)))  unsetDefaults(.name, confirm = FALSE)
    }
    else {
        env <- as.environment(-1)
        eval(parse(text = paste("options(", default.name, "=list(", 
            paste(paste(names(all.and.matched), "=", lapply(all.and.matched, 
                function(x) {
                  if (is.character(x)) {
                    deparse(x)
                  }
                  else {
                    x
                  }
                })), collapse = ","), "))", sep = "")), envir = env)
    }
}


`unsetDefaults` <-
function(name,confirm=TRUE) {
  importDefaults(calling.fun='unsetDefaults')
  if(is.function(name)) name <- deparse(substitute(name))
  if(is.null(getDefaults(name))) 
    invisible(return())
    #stop(paste("no Defaults set for",sQuote(name)))
  remove.yes <- TRUE
  if(confirm) {
    CONFIRMATION <- readline(prompt=
            paste("Are you sure you want to remove",
                  sQuote(name),"defaults? (N): "))
    if(toupper(substr(CONFIRMATION,1,1))!="Y") {
      remove.yes <- FALSE
      cat(paste(sQuote(name),"Defaults NOT removed\n"))
    } else {
      if(confirm)
        cat(paste(sQuote(name),"Defaults removed!\n"))
    }
  }
  if(remove.yes) {
    default.name <- paste(name,"Default",sep=".")
    env <- as.environment(-1)
    unDefaults(name)
    eval(parse(text=paste('options(',default.name,'=NULL)',sep='')),envir=env)
  }
}
"getDefaults" <-
function(name=NULL,arg=NULL) {
  if(is.function(name)) name <- deparse(substitute(name))
  if(!is.null(name)) {
    if(length(name) > 1) {
      if(!is.character(name))
        stop(paste(sQuote('name'),"must be a character vector",
                   "or visible function")) 
      all.names=list()
    }
    for(each.name in name) {
      default.name <- paste(each.name,"Default",sep=".")
      if(is.null(arg)) {
        if(exists('all.names',inherit=FALSE)) {
          all.names[[each.name]] <- options(default.name)[[1]]
        } else {
          return(options(default.name)[[1]])
        }
      } else {
        default.list <- list()
        for(each.arg in arg) {
          default.list[[each.arg]] <- options(default.name)[[1]][[each.arg]]
        }
        if(exists('all.names',inherit=FALSE)) {
          all.names[[each.name]] <- default.list
        } else {
          return(default.list)
        }
      }
    }
    return(all.names)
  } else {
    all.options <- names(options())
    all.Defaults <-as.character(
                     sapply(all.options[grep('.Default$',all.options)],
                       FUN=function(x) {
                         gsub('.Default$','',x)
                       })
                   )
    if(identical(all.Defaults,character(0))) return(NULL)
    return(all.Defaults)
  }
}
