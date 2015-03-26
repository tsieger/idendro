### Internal functions.

.ls.objects <-
function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head =     TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session)
  # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
  # a data frame of the objects and their associated storage needs.
  napply <- function(names, fn) sapply(names, function(x)
          fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size) / 10^6 # megabytes
  obj.dim <- t(napply(names, function(x)
            as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

.dbg.internal<-0


mySeq <- function
### Sequence generation resembling matlab ':' operator.
### The difference from R `seq' is the behaviour in case when
### from=2, to=1 and b=1 - in that case R seq raises an error, while
### matlab returns an empty sequence.
### This function returns an empty sequence as well in this case.
##keyword<<internal
(from,##<< starting value
to,##<< ending value
by=1##<< step (defaults to 1)
){
    if ((from<to) == (by>0)) return(seq(from,to,by))
    else if (from==to) return(from)
    else return(vector(class(from),0))
}

printWithName<-function
### Print the name and value of a variable.
##keyword<<internal
(x##<< variable to print
) {
    cat(paste(deparse(substitute(x)),'\n',sep=''))
    print(x)
}

printVar<-function
### Print the name, type and value of a variable.
##keyword<<internal
(x##<< variable to print
) {
    #cat(paste(deparse(substitute(x)),' [',class(x),', ',length(x),']: ',paste(x,collapse=' '),'\n',sep=''))
    cat(paste(deparse(substitute(x)),
        ' [',paste(class(x),collapse=','),', ',
        ifelse(!is.null(dim(x)),paste(dim(x),collapse=','),length(x)),']: ',sep=''))
    if (is.environment(x)) {
        cat(paste(paste(ls(x),collapse=','),'\n',sep=''))
    } else if (length(x)>0) {
        for (i in 1:length(x)) {
            if (is.environment(x[i])) {
                tmp<-as.data.frame(x[i])
            } else {
                tmp<-x[i]
            }
            if (!is.null(names(x[i]))) {
                cat(paste(names(x[i]),'=',tmp,ifelse(i<length(x),', ','\n'),sep=''))
            } else {
                cat(paste(tmp,ifelse(i<length(x),', ','\n'),sep=''))
            }
        }
    } else {
        cat('\n')
    }
    #print(x)
}

.findVar<-function(idfName) {
    found<-FALSE
    value<-NA
    #print(sys.nframe())
    for (i in sys.nframe():1) {
        #print(i)
        if (exists(idfName,envir=sys.frame(i),inherits=FALSE)) {
            value<-get(idfName,envir=sys.frame(i),inherits=FALSE)
            found<-TRUE
            break
        }
    }
    return(list(value=value,found=found))
}

.gfc<-function
### GFC: "get from caller" function makes given variable that appears
### in the environment of some caller of the function calling the
### `.gfc' function usable directly by the caller of the `.gfc'
### function.
###
### Example:
###  f1() { a<-1; f2()}
###  f2() { a<-.gfc(a); <<now a copy of `a' appears in f2>> }
(nm,##<< identifier (not a character name) of a variable to get
required=TRUE##<< Require this variable and err if not found? If FALSE,
## NULL return value indicates either the variable exists and has the
## NULL value assigned, or the variable has not been found.
) {
    if (.dbg.internal) cat(paste('gcf: looking for \'',deparse(substitute(nm)),'\'\n',sep=''))
    rv<-.findVar(deparse(substitute(nm)))
    if (rv$found) {
        return(rv$value)
    } else {
        if (required) {
            print(traceback())
            stop(paste('\'',deparse(substitute(nm)),'\' not found in caller stack',sep=''))
        }
        # obsoleted fallback:
        #return(attr(rv$value,'.sharedEnv')[deparse(substitute(nm))])
        return(NULL)
    }
}

last<-function
###Extract the first element of the argument.
##keyword<<internal
(x) tail(x,1)

first<-function
###Extract the last element of the argument.
##keyword<<internal
(x) head(x,1)

.sharedVarNames<-function() {
    list(
    'dbg',
    'dbg.args',
    'dbg.mouse',
    'dbg.gui',
    'dbg.tx',
    'dbg.dendro',
    'dbg.dendro.zoom',
    'dbg.dendro.axis',
    'dbg.dendro.limits',
    'dbg.dendro.select',
    'dbg.dendro.cut',
    'dbg.dendro.info',
    'dbg.axis',
    'dbg.heatmap',
    'dbg.heatmap.text',
    'dbg.heatmap.limits',
    'dbg.brushedmap',
    'dendroZoom',
    'dendroZoomMouseSelection',
    'params',
    'df')
}
