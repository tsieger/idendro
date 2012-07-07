.ls.objects <-
function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head =     TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-    manage-the-available-memory-in-an-r-session) 
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

dbg.internal<-0

printWithName<-function (x) {
    cat(paste(deparse(substitute(x)),'\n',sep=''))
    print(x)
}

printVar<-function (x) {
    cat(paste(deparse(substitute(x)),' [',class(x),', ',length(x),']: ',paste(x,collapse=' '),'\n',sep=''))
    #print(x)
}

findVar<-function(idfName) {
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

gfc<-function(nm) {
    if (dbg.internal) cat(paste('gcf: looking for \'',deparse(substitute(nm)),'\'\n',sep=''))
    rv<-findVar(deparse(substitute(nm)))
    if (rv$found) {
        return(rv$value)
    } else {
#        rv<-findVar('scene')
        if (!rv$found) {
            print(tb())
            #stop(paste('\'scene\' not found in caller stack'))
            stop(paste('\'',deparse(substitute(nm)),'\' not found in caller stack'))
        }
        return(attr(rv$value,'.sharedEnv')[deparse(substitute(nm))])
    }
}

last<-function(x) tail(x,1)
first<-function(x) head(x,1)


sharedVarNames<-function() {
    list(
    'dbg',
    'dbg.tx',
    'dbg.dendro',
    'dbg.heatmap',
    'dbg.clusterSelector',
    'charmW',
    'strangeW',
    'dendroG',
    'dendroZoom',
    'dendroZoomMouseSelection',
    'selectorG',
    'heatmapG',
    'params',
    'df')
}
