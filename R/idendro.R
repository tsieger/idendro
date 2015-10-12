idendro<-structure(function# Interactive Dendrogram
###
### 'idendro' is a plot enabling users to visualize a dendrogram and
### inspect it interactively: to select and color clusters anywhere in
### the dendrogram, to zoom and pan the dendrogram, and to visualize
### the clustered data not only in a built-in heat map, but also in any
### interactive plot implemented in the 'cranvas' package.
### The integration with 'cranvas' (but also with the user's code) is
### made possible by communicating over mutable data frames
### (mutaframes) from the 'plumbr' package.
### 'idendro' can be used to inspect quite large dendrograms (tens
### of thousands of observations, at least).
###
##details<<
## 'idendro' displays an interactive dendrogram enriched, optionally,
## with a heat map and/or a brushed map.
##
## The dendrogram represents the result of a hierarchical cluster
## analysis performed on a set of observations (see e.g. 'hclust').
## There is an axis drawn below the dendrogram displaying the "height"
## of the clusters in the dendrogram.
##
## The heat map visualizes the observations living in k-dimensional
## feature space by mapping their features onto a color scale and
## displaying them as rows of 'k' colored rectangles. By default,
## normalization (scaling) of individual features to a common visual
## scale is enabled. Scaling of observations is also supported (see the
## 'doScaleHeatmapByRows' argument).
## Individual features of individual observations can be studied by
## flying over the heatmap having the left mouse button pressed.
##
## The brushed map indicates which observations are currently
## selected by some external plot/tool 'idendro' is integrated
## with (e.g. a cranvas plot). Technically speaking, the current
## selection is determined by the value of the '.brushed' metadata
## column in the 'qx' mutable data frame. 'idendro' listens to changes
## made to the '.brushed' column by the external plot/tool.
##
## The dendrogram can be zoomed and panned. To zoom in a
## specific region, right click and drag in the dendrogram.
## Mouse wheel can also be used to zoom in and out (the amount of zoom
## can be controlled by the 'zoomFactor' argument). To pan a zoomed
## dendrogram, middle click and drag the mouse. Zooming and panning
## history is available (see 'GUI').
##
## User can select clusters manually one by one (by clicking
## at individual clusters in the dendrogram), or automatically by
## "cutting" the dendrogram at a specified height. To cut the
## dendrogram, navigate the mouse close to the dendrogram axis
## (a dashed line will appear across the dendrogram at a specified
## height), and left click. Clusters just beneath the cutting
## height will get selected, replacing the clusters currently
## selected. Selection history is available (see 'GUI').
##
##   \emph{Graphic User interface (GUI):}
##
## In the left part of the dendrogram window, there is a simple GUI.
## In the top part of the GUI come cluster-specific controls and info
## panels arranged in rows. (The number of rows is determined by the
## 'maxClusterCount' argument.)
## In each row, there is the current cluster selector (a radio button
## decorated with a cluster ID and a color code (determined by the
## 'clusterColors' argument)), and cluster-specific statistics: the
## total number (and the ratio) of the observations in that specific
## cluster out of the total number of observations, and the number
## (and the ratio) of the observations in that cluster out of the
## observations brushed.
## The current cluster determines which color and ID will be
## associated with a cluster selected in the dendrogram,
## At any time, exactly one cluster is selected as the current
## cluster. 
## The observations forming the current cluster are indicated by
## the '.inCurrentCluster' column in the 'qx' mutaframe, which can be
## used by external applications to display the current
## cluster-specific information (see 'idendroDemoWithUserCallback').
##
## At the bottom of the GUI window, there are buttons controling
## zooming, cluster selection, and heat map smoothing:
##
## "Undo zoom" - retrieves the previous zoom region from history
##
## "Full view" - zooms the dendrogram out maximally
##
## "Undo selection" - retrieves the previous cluster selection from
##     history
##
## "Unselect" - unselects the current cluster in the dendrogram
##
## "Unselect all" - unselects all clusters
##
## The "heat map smoothing" mode can be set to one of:
##
##   "none" - the heat map gets never smoothed, it displays the
##      features of all the individual observations
##
##   "cluster" - the heat map displays the average features for the 
##       currently selected clusters
##
##   "zoom" - the heat map displays the average feature for each
##      elementary (i.e. the finest) cluster seen in the dendrogram
##      currently. When the dendrogram is zoomed out maximally,
##      the features of all the elementary clusters (i.e. the
##      individual observations) are displayed. When the user zooms in
##      the dendrogram, such that some clusters get hidden, the
##      features of the observations forming the hidden clusters get
##      averaged.
##
##  "Quit"
##
##
(
    h, ##<< object of class 'stats::hclust' (or other class
    ## convertible to class 'hclust' by the 'as.hclust' function)
    ## describing a hierarchical clustering.
    ## If _inversions_ in heights (see 'hclust') is detected,
    ## the heights get fixed in a simple naive way by preserving
    ## non-negative relative differences in the heights, but changing
    ## negative differences to zero. Using clustering with monotone
    ## distance measure should be considered in that case.

    qx=NULL,##<< mutaframe holding observations that were clustered
    ## giving rise to 'h', with metadata (special columns) for
    ## interaction, as created by 'cranvas::qdata'.
    ## If 'qx' is enriched with 'idendro'-specific
    ## metadata columns (i.e. '.cluster', '.inCurrentCluster'), the
    ## initial cluster selection is based on them; otherwise there are
    ## no clusters selected initially.
    ## A regular data frame can be passed instead of a mutaframe, in
    ## which case it will get converted into a mutaframe
    ## automatically.
    ## This parameter is optional.

    x=qx, ##<< data frame holding observations tha were clustered
    ## giving rise to 'h'.
    ## The heat map will depict this data. (The heat map can be scaled
    ## - see the 'doScaleHeatmap' and 'doScaleHeatmapByRows' arguments.)
    ## Non-numeric types will get converted to numeric using 'as.numeric'.
    ## This parameter is optional. If missing, it will be guessed
    ## from 'qx' by omitting any columns starting in '.'.

    zoomFactor=1/240, ##<< amount of zoom in/out as controlled by the
    ## mouse wheel

    observationAnnotationEnabled=TRUE, ##<< shall the names of individual
    ## observations (rownames of 'x') be shown next to the
    ## dendrogram/heat map?

    clusterColors=c('red','green','blue','yellow','magenta','cyan','darkred','darkgreen','purple','darkcyan'),##<< colors
    ## of individual clusters

    unselectedClusterColor='black',##<< the color of unselected dendrogram
    ## branches

    maxClusterCount=length(clusterColors), ##<< the maximum number of
    ## clusters user can select. If greater than the number of
    ## 'clusterColors', cluster colors will get recycled.
    ## This parameter affects the size of the GUI and the number of
    ## clusters that can be selected automatically by "cutting" the
    ## dendrogram.

    heatmapEnabled=TRUE, ##<< shall the heat map be drawn?

    doSmoothHeatmap=NULL,##<< (deprecated, use `heatmapSmoothing'
    ## instead)

    heatmapSmoothing=c('none','cluster','zoom'),##<< heat map smoothing mode,
    ## one of
    ## 'none' - the heat map gets never smoothed, it displays the
    ##      features of all the individual observations
    ## 'cluster' - the heat map depicts the average features
    ##      for the currently selected clusters,
    ## 'zoom' - the heat map displays the average feature for each
    ##      elementary (i.e. the finest) cluster seen in the
    ##      dendrogram currently.

    heatmapColors=colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))(10), ##<< heat map
    ## color palette represented by a list of colors, e.g.
    ## a sequential palette generated by `brewer.pal', or
    ## `colorRampPalette(.)(.)', `gray.colors(.)', or `hsv(.)'.
    ## [DEPRECATED:] Alternatively, a function that takes a single
    ## numeric argument, the number of heat map colors (see
    ## the `heatmapColorCount' argument) and returns colors to be
    ## used in heat map can be supplied.
    ## WARNING: the number of colors used by heat map can influence
    ## the time spent drawing the heat map significantly (for large
    ## data sets).

    heatmapColorCount=10, ##<< [DEPRECATED] the number of colors used
    ## in the heat map. This argument gets used only if the
    ## `heatmapColors' argument refers to a function that takes a
    ## single numeric argument, `n', and generates a list of `n'
    ## colors. If `heatmapColors' refers to a function,
    ## `heatmapColorCount' is passed as the argument to that function.
    ## WARNING: the number of colors used by heat map can influence
    ## the time spent drawing the heat map significantly (for large
    ## data sets).

    doScaleHeatmap=TRUE, ##<< scale each heat map column to the <0,1> range?
    ## (The default is TRUE.)

    doScaleHeatmapByRows=FALSE, ##<< scale heat map rows, not columns
    ## (The default is FALSE.)

    heatmapRelSize=.2, ##<< relative size of the heat map - the ratio
    ## of the heat map width to the width of both the dendrogram and
    ## the heat map. The default is 20%.

    heatmapInspectFormatFun = format, ##<< function used to format
    ## features of observations when flying over the heatmap. By
    ## default, the 'format' function gets used.

    brushedmapEnabled=!is.null(qx), ##<< shall brushed map be drawn?

    separateGui=FALSE, ##<< shall GUI be integrated into the dendrogram
    ## window, or shall it be separated in a standalone window?
    ## (The default is integrated GUI.)

    graphicalClusterInfos=TRUE, ##<< depict cluster-specific statistics
    ## graphically? (The default is TRUE.)

    textualClusterInfos=TRUE, ##<< depict cluster-specific statistics
    ## textually? (The default is TRUE.)

    geometry=c(0,0,600,400), ##<< window geometry (The default
    ## is 600x400.)

    clipDendro=TRUE, ##<< clip dendrogram to the dendrogram layer?
    ## The default is TRUE, meaning the  dendrogram does not interfere
    ## with other layers. On some systems, however, clipping might
    ## not work properly (border branches might not get rendered),
    ## so disabling dendrogram clipping might be desired.

    opengl=NULL ##<< use OpenGL for qtpaint scene rendering?
    ## If non-NULL, the value supplied is passed as the `opengl'
    ## argument to qtpaint::qplotView. Useful when OpenGL does
    ## not work properly on your system. Note, however, that
    ## for large data sets it slows drawing down considerably.

) {
# TODO: debugs

##seealso<<hclust, plclust, identify.hclust, rect.hclust,
## cutree, dendrogram, cranvas::qdata

    #### debugs
    ####
    # general debug
    dbg<-0
    # debugs controlling individual features,
    # the higher the value, the finer the debugs
    dbg.qupdate<-0*dbg
    dbg.args<-01*dbg
    dbg.mouse<-0*dbg
    dbg.gui<-0*dbg
    dbg.tx<-0*dbg
    dbg.dendro<-1*dbg
    dbg.dendro.zoom<-0*dbg
    dbg.dendro.axis<-0*dbg
    dbg.dendro.limits<-0*dbg
    dbg.dendro.select<-0*dbg
    dbg.dendro.cut<-0*dbg
    dbg.dendro.info<-0*dbg
    dbg.axis<-0*dbg
    dbg.heatmap<-1*dbg
    dbg.heatmap.text<-0*dbg
    dbg.heatmap.limits<-0*dbg
    dbg.heatmap.smooth<-0*dbg
    dbg.brushedmap<-0*dbg
    dbg.brushedmap.limits<-0*dbg

    #### arguments handling
    ####
    if (dbg.args) cat('--- user supplied arguments: ---\n')
    if (dbg.args) printVar(!is.null(qx))
    if (dbg.args) printVar(!is.null(x))
    if (dbg.args) printVar(heatmapEnabled)
    if (dbg.args) printVar(doScaleHeatmap)
    if (dbg.args) printVar(brushedmapEnabled)
    if (dbg.args) printVar(observationAnnotationEnabled)

    if (!inherits(h,'hclust')) {
        h<-as.hclust(h)
    }

    if (inherits(qx,'dist')) {
        stop('\'qx\' argument of invalid type of class \'dist\'')
    }
    if (inherits(x,'dist')) {
        stop('\'x\' argument of invalid type of class \'dist\'')
    }

    if (!is.null(doSmoothHeatmap)) {
        warning('argument `doSmoothHeatmap\' is deprecated (and ignored)')
    }

    if (heatmapEnabled && is.null(x) && is.null(qx)) {
        # can't draw heat map if we have no data
        heatmapEnabled<-FALSE
    }

    # enable passing qdata in the `x' argument
    if (!is.mutaframe(qx)) {
        if (is.null(x)) {
            x<-qx
        }
        qx<-NULL
    }

    # create qdata from data, if possible
    if (is.null(qx) && !is.null(x)) qx<-qdata(x)
    # create data from qdata, if possible
    if (is.null(x) && !is.null(qx)) x<-qx
    if (is.mutaframe(x)) {
        if (!is.null(colnames(x))) {
            # heuristics: try to get rid of columns added to the original data
            x<-as.data.frame(x[,-grep('^\\.',colnames(x))])
        }
    }
    if (!is.null(x) && !is.data.frame(x) && !is.matrix(x)) {
        x<-as.data.frame(x)
    }

    if (is.unsorted(h$height)) {
        message('Note: non-monotone distance detected, applying a simple workaround. Consider using clustering with monotone distance.')
        # 1  4  2  7  6  5  8  9  # h$height
        #    3 -2  5 -1 -1  3  1  # tmp<-diff(h$height),  min(tmp[tmp>0]) = 1
        #    2 -3  4 -2 -2  2  0  # tmp2<-tmp-min(tmp[tmp>0]
        #    0 -3  0 -2 -2  0  0  # tmp2*I(tmp<0)
        #    0 -3 -3 -5 -7 -7 -7  # cumsum(tmp2*I(tmp<0))
        #    0  3  3  5  7  7  7  # -cumsum(tmp2*I(tmp<0))
        # 1  4  2  7  6  5  8  9  # h$height
        # 1  4  5 10 11 12 15 16  # h$height + c(0,-cumsum(tmp*I(tmp<0)))
        tmp<-diff(h$height)
        tmp2<-tmp-min(tmp[tmp>0])
        h$height<-h$height+c(0,-cumsum(tmp2*I(tmp<0)))
    }

    n<-length(h$height)+1
    if (!is.null(x) && nrow(x)!=n) {
        stop(paste('Clustering (of ',n,' objects) does not fit data (',nrow(x),' rows).',sep=''))
    }

    if (is.function(heatmapColors)) {
        warning('[DEPRECATED:]\'heatmapColors\' argument of type \'function\', supply a color list instead')
        heatmapColors<-heatmapColors(heatmapColorCount)
    }
    heatmapColorCount<-length(heatmapColors)

    # convert non-numeric data to numeric, if necessary
    if (heatmapEnabled) {
        xOrig<-x
        nonNumericColumnFound<-FALSE
        for (i in 1:ncol(x)) {
            if (!is.numeric(x[,i])) {
                x[,i]<-as.numeric(x[,i])
                nonNumericColumnFound<-TRUE
            }
        }
        if (nonNumericColumnFound) {
            message('Note: non-numeric data found, converting to numeric (in order to enable heatmap drawing).')
        }
    } else {
        xOrig<-NULL
    }

    # scale heat map
    if (heatmapEnabled && doScaleHeatmap) {
        scaleVector<-function(x) {
            mn<-min(x,na.rm=TRUE)
            mx<-max(x,na.rm=TRUE)
            if (mn!=mx) {
                x<-(x-mn)/(mx-mn)
            } else {
                x<-x-mn+.5
            }
            x
        }
        if (doScaleHeatmapByRows) {
            for (i in 1:nrow(x)) {
                x[i,]<-scaleVector(x[i,])
            }
        } else {
            for (i in 1:ncol(x)) {
                x[,i]<-scaleVector(x[,i])
            }
        }
    }

    if (brushedmapEnabled && is.null(qx)) {
        qx<-qdata(1:(length(h$height)+1))
    }

    dendroZoom<-dendroZoomMin<-list(g=last(h$height)*c(-.05,1),w=.5+n*c(0,1))
    dendroZoomMouseSelection<-list(g=c(NA,NA),w=c(NA,NA))
    mouseLeftButtonPressed<-FALSE
    mouseRightButtonPressed<-FALSE
    mouseMiddleButtonPressed<-FALSE
    mouseMiddleButtonPressPos<-NULL
    axisCut<-NA
    heatmapTipTextSet<-FALSE
    heatmapTipText<-NA
    heatmapTipPos<-c(NA,NA)

    df<-prepareDendro(h,x,xOrig,doFlipG=TRUE,dbg.dendro)
    # initialize clusters from leaf colors, if supplied
    if (!is.null(qx) && !is.null(colnames(qx)) && '.cluster'%in%colnames(qx)) {
        if (dbg.dendro) cat('.cluster found in qx\n')
        df<-createClustersFromLeafColors(df,qx$.cluster,maxClusterCount,dbg.dendro)
        # set the current cluster, if it is one of the clusters visible in the data
        currentClusterIndices<-qx$.cluster[qx$.inCurrentCluster]
        if (length(currentClusterIndices)>0 && length(unique(currentClusterIndices))==1) {
            df$currentCluster<-currentClusterIndices[1]
        }
    }
    if (dbg.dendro>1) printVar(df)

    # initialize heat map cutting points, so they stay constant
    # regardless of smoothing/zoom
    if (heatmapEnabled) {
        # border points: [X0,X1), [X1,X2), ... [Xn-1, Xn]
        tmp<-as.matrix(df$xOrdered)
        df$heatmapBorderPoints<-seq(min(tmp,na.rm=TRUE),max(tmp,na.rm=TRUE),len=heatmapColorCount+1)
    }

    # observation annotations
    if (!is.null(x) && !is.null(rownames(x))) {
        df$observationLabelsOrdered<-rownames(x)
    } else {
        df$observationLabelsOrdered<-h$labels
    }
    if (!is.null(df$observationLabelsOrdered)) {
        df$observationLabelsOrdered<-df$observationLabelsOrdered[df$leafOrder]
    }
    if (observationAnnotationEnabled && is.null(df$observationLabelsOrdered)) {
        # if annotation not available, nothing to draw
        observationAnnotationEnabled<-FALSE
    }

    # dim annotations
    if (!is.null(x) && !is.null(colnames(x))) {
        df$dimLabels<-colnames(x)
    } else {
        df$dimLabels<-NULL
    }

    df$dendroZoomHistory<-list()
    lastDendroZoomHistorySaver<-'none'

    df$selectionHistory<-list()

    if (!is.null(geometry)) {
        if (is.numeric(geometry)) {
            if (length(geometry)!=4) {
                stop('Invalid geometry')
            }
            geometry<-Qt$QRect(geometry[1],geometry[2],geometry[3],geometry[4])
        } else {
            # unify instances of qrect/QRect/... into Qt$QRect
            geometry<-Qt$QRect(geometry$topLeft()$x(),geometry$topLeft()$y(),geometry$width(),geometry$height())
        }
    }

    if (dbg.args) cat('--- consolidated arguments: ---\n')
    if (dbg.args) printVar(!is.null(qx))
    if (dbg.args) printVar(!is.null(x))
    if (dbg.args) printVar(heatmapEnabled)
    if (dbg.args) printVar(brushedmapEnabled)
    if (dbg.args) printVar(observationAnnotationEnabled)

    #### wrap arguments in a shared params structure
    ####
    params<-NULL
    params$zoomFactor<-zoomFactor
    params$observationAnnotationEnabled<-observationAnnotationEnabled
    params$clusterColors<-clusterColors
    params$unselectedClusterColor<-unselectedClusterColor
    params$allColors<-c(unselectedClusterColor,clusterColors)
    params$maxClusterCount<-maxClusterCount
    params$heatmapEnabled<-heatmapEnabled
    params$heatmapSmoothing<-match.arg(heatmapSmoothing)
    params$heatmapColors<-heatmapColors
    params$heatmapColorCount<-heatmapColorCount
    params$brushedmapEnabled<-brushedmapEnabled
    params$geometry<-geometry

    #### internal functions
    ####

    qupdate<-function(x) {
        if (dbg.qupdate) cat(paste('qupdate(',deparse(substitute(x)),') called\n'))
        qtpaint::qupdate(x)
    }

    # Color observations according to clusters they belong to.
    colorizeLeafs<-function(qx,df,params) {
        if (!is.null(qx)) {
            qx$.border<-qx$.color<-params$allColors[df$leafColorIdxs+1]
            qx$.cluster<-df$leafColorIdxs
        }
        qx
    }
    qx<-colorizeLeafs(qx,df,params)

    # Mark observations belonging to the current cluster in the `qx'
    # mutable data frame and set `qx' into `df'.
    setCurrentClusterInQx<-function(qx,df) {
        if (!is.null(qx)) {
            qx$.inCurrentCluster<-df$leafColorIdxs==df$currentCluster
            df$qx<-qx
        }
    }
    setCurrentClusterInQx(qx,df)

    # Determine color for cluster of given ID (starting at 1).
    clusterColor<-function(id) {
        params$clusterColors[((id-1)%%length(params$clusterColors))+1]
    }

    # Callback invoked when clusters change.
    updateClustersOnChange<-function(.sharedEnv,qx,guiWindow) {
        df<-.sharedEnv$df
        qupdate(.sharedEnv$dendroLayer)
        qx<-colorizeLeafs(qx,df,params)
        setCurrentClusterInQx(qx,df)
        guiWindow$updateClusterInfos()
        guiWindow$update()
        if (.sharedEnv$params$heatmapSmoothing=='cluster') {
            df<-smoothHeatmapAccordingToClusters(df,dbg.heatmap.smooth)
            qupdate(.sharedEnv$heatmapLayer)
        }
        df
    }

    # Callback invoked when heat map smoothing mode gets changed (via
    # GUI buttons).
    heatmapSmoothingChanged<-function(.sharedEnv) {
        df<-.sharedEnv$df
        switch(.sharedEnv$params$heatmapSmoothing,
            'none'={
                # restore the original heat map
                df$xOrderedSmoothed<-df$xOrdered
                df$elemClusterCount<-df$n
                },
            'cluster'={
                df<-smoothHeatmapAccordingToClusters(df,dbg.heatmap.smooth)
                df$elemClusterCount<-df$n
                },
            'zoom'={
                # restore the original heat map
                df$xOrderedSmoothed<-df$xOrdered
                # and let heatmapPainter do the rest of work
                }
        )
        .sharedEnv$df<-df
        qupdate(.sharedEnv$heatmapLayer)
    }

    # Painter clearing the whole layer with white color (considered
    # being backfround).
    # TODO: background color specification
    clearLayerBackground<-function(layer,painter,borderSaving=c(0,0,0,0)) {
        qdrawRect(painter,
            layer$limits()$left()+layer$limits()$width()*borderSaving[1],
            layer$limits()$top()+layer$limits()$height()*borderSaving[2],
            layer$limits()$right()-layer$limits()$width()*borderSaving[3],
            layer$limits()$bottom()-layer$limits()$height()*borderSaving[4],
            stroke=rgb(0,0,0,0),fill=qcolor('white'))
    }


    ##################################################################
    ## scene#FOLD01
    ##################################################################
    scene<-qscene()
    attr(scene,'.sharedEnv')<-environment() # the current environment
    ## this environment will be shared by this function as well as
    ## functions called from within qtpaint/qtbase

    ##################################################################
    ## painters#FOLD02
    ##################################################################

    # Debug function drawing a filled rectangle depicting the extent of
    # a layer.
    drawLayerLimits<-function(painter,layer,layerName,fillColor,hilightSomePos=FALSE) {
        if (hilightSomePos) {
            qdrawText(painter,'0,0',0,0,color='black')
            qdrawText(painter,'0,1',0,1,color='black')
            qdrawText(painter,'.5,1',.5,1,color='black')
            qdrawText(painter,'0,2',0,2,color='black')
        }

        eps<-.1
        qdrawRect(painter,
            layer$limits()$left()+eps,
            layer$limits()$bottom()+eps,
            layer$limits()$right()-eps,
            layer$limits()$top()-eps,stroke=fillColor,fill=scales::alpha(fillColor,.3))

        cat(paste(layerName,':\n',sep=''))
        #printVar(layer$limits())
        printVar(layer$limits()$left())
        printVar(layer$limits()$bottom())
        printVar(layer$limits()$right())
        printVar(layer$limits()$top())
    }

    dendroPainter<-function(layer,painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro) cat('dendroPainter called\n')

        dendroPainterImpl<-function(layer,painter) {
            if (dbg.dendro) cat('dendroPainterImpl called\n')
            if (dbg.dendro>1) with(df$unselectedBranches$branches,printVar(x1s))
            if (dbg.dendro>1) with(df$unselectedBranches$branches,printVar(y1s))
            if (dbg.dendro>1) with(df$unselectedBranches$branches,printVar(x2s))
            if (dbg.dendro>1) with(df$unselectedBranches$branches,printVar(y2s))
            with(df$unselectedBranches$branches,qdrawSegment(painter,x1s,y1s,x2s,y2s,stroke=qcolor('black')))
            for (i in seq(along=df$clusters)) {
                if (!is.null(df$clusters[[i]]) && length(df$clusters[[i]]$branches)>0) {
                    if (dbg.dendro) cat(sprintf('cluster %i: color %s\n',i,clusterColor(i)))
                    with(df$clusters[[i]]$branches,qdrawSegment(painter,x1s,y1s,x2s,y2s,stroke=qcolor(clusterColor(i))))
                }
            }

            # zooming region being defined by mouse
            xy<-gw2xy(dendroZoomMouseSelection)
            qdrawRect(painter, xy$x[1], xy$y[1], xy$x[2], xy$y[2], stroke='yellow', fill=scales::alpha('black',.3))

            # dendrogram cutting limit defined by axis position
            tmp<-dendroZoom
            tmp$w<-seq(dendroZoom$w[1],dendroZoom$w[2],len=50)
            tmp$g<-rep(.sharedEnv$axisCut,length(tmp$w))
            xy<-gw2xy(tmp)
            clrs<-rep(c(scales::alpha('gray20',.7),scales::alpha('yellow',.7)),len=length(xy$x)-1)
            qdrawRect(painter, xy$x[-length(xy$x)], xy$y[-length(xy$y)], xy$x[-1], xy$y[-1], stroke=clrs)

            if (dbg.dendro.limits) {
                drawLayerLimits(painter,layer,'dendro','blue')
            }
        }
        dendroPainterImpl(layer,painter)
    }

    ##
    ## brushed map
    ##
    brushedmapPainter<-function(layer,painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.brushedmap) cat('brushedmapPainter called\n')

        if (dbg.brushedmap) printVar(brushedmapEnabled)
        if (!brushedmapEnabled) return()

        brushedmapPainterImpl<-function(layer,painter) {
            if (dbg.brushedmap) cat('brushedmapPainterImpl called\n')

            borderSize<-0
            g1<-rep(borderSize,each=df$n)
            g2<-g1+1-2*borderSize
            w1<-1:df$n-.5+borderSize
            w2<-w1+1-2*borderSize
            if (dbg.brushedmap>1) printVar(g1)
            if (dbg.brushedmap>1) printVar(g2)
            if (dbg.brushedmap>1) printVar(w1)
            if (dbg.brushedmap>1) printVar(w2)
            coords1<-gw2xy(heatmap2fig(list(g1,w1))) # TODO: heatmap2fig change or remove
            coords2<-gw2xy(heatmap2fig(list(g2,w2))) # TODO: heatmap2fig change or remove

            # draw brushed leafs
            i<-qx$.brushed[df$leafOrder]
            qdrawRect(painter,
                coords1[[1]][i],coords1[[2]][i],coords2[[1]][i],coords2[[2]][i],
                stroke=rgb(0,0,0,0),fill=qcolor('black'))

            # draw leafs not brushed (it is needed since zoomed
            # dendrogram can collide with span brushed map, so we need
            # to restore the background color by overdrawing
            # the dendrogram)
            i<-!qx$.brushed[df$leafOrder]
            qdrawRect(painter,
                coords1[[1]][i],coords1[[2]][i],coords2[[1]][i],coords2[[2]][i],
                stroke=rgb(0,0,0,0),fill=qcolor('white'))

            if (dbg.brushedmap.limits) {
                drawLayerLimits(painter,layer,'brushedmap','yellow')
            }
        }
        brushedmapPainterImpl(layer,painter)
    }

    ##
    ## heat map
    ##
    heatmapPainter<-function(layer,painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.heatmap) cat('heatmapPainter called\n')

        if (dbg.heatmap) printVar(heatmapEnabled)
        if (!heatmapEnabled) return()

        # clear the background to cover the branches of a zoomed
        # dendrogram
        clearLayerBackground(layer,painter)

        heatmapPainterImpl<-function(layer,painter) {
            if (dbg.heatmap) cat('heatmapPainterImpl called\n')

            g1<-rep(seq(0,df$k-1),each=df$n)
            g2<-g1+1
            w1<-rep(1:df$n,df$k)-.5
            w2<-w1+1
            if (dbg.heatmap>1) printVar(g1)
            if (dbg.heatmap>1) printVar(g2)
            if (dbg.heatmap>1) printVar(w1)
            if (dbg.heatmap>1) printVar(w2)
            coords1<-gw2xy(heatmap2fig(list(g1,w1)))
            coords2<-gw2xy(heatmap2fig(list(g2,w2)))
            if (dbg.heatmap.smooth) cat(paste('heatmapSmoothing mode:',.sharedEnv$params$heatmapSmoothing,'\n',sep=''))
            if (.sharedEnv$params$heatmapSmoothing=='zoom') {
                # not all observations visible in the dendrogram,
                # smooth heat map to carry info about the currently
                # elementary clusters in the zoomed dendro
                hght<-dendroZoom$g[2]
                if (df$doFlipG) hght<-df$h$height[df$clusterCount]-hght
                ch<-cutree(df$h,h=hght)
                if (max(ch)!=df$elemClusterCount) {
                    if (dbg.heatmap) cat('smoothing heat map\n')
                    df$xOrderedSmoothed<-smoothHeatmap(df$xOrdered,ch[df$leafOrder],dbg.heatmap.smooth)
                    df$elemClusterCount<-max(ch)
                }
            }
            # We need to ensure that cutting the current
            # (perhaps smoothed) heat map results in the same intervals
            # as if the cut operated over the original heat map.
            # We use a hack here: we add some data from the whole range
            # of the original heat map ("border points") to the data
            # being cut and remove them after cutting.
            if (dbg.heatmap.smooth>1) printWithName(df$heatmapBorderPoints)
            if (dbg.heatmap.smooth>1) printWithName(as.matrix(df$xOrderedSmoothed))
            if (dbg.heatmap.smooth>1) printWithName(c(as.matrix(df$xOrderedSmoothed),df$heatmapBorderPoints))
            colIdx<-cut(c(as.matrix(df$xOrderedSmoothed),df$heatmapBorderPoints),breaks=df$heatmapBorderPoints,include.lowest=TRUE,right=TRUE,labels=FALSE)
            # get rid of the artificially added data
            colIdx<-colIdx[1:(length(colIdx)-length(df$heatmapBorderPoints))]
            if (dbg.heatmap.smooth>1) printWithName(colIdx)
            if (dbg.heatmap.smooth>1) printWithName(df$xOrderedSmoothed)
            if (dbg.heatmap>1) printVar(colIdx)
            colPalette<-params$heatmapColors
            if (dbg.heatmap>1) printVar(colPalette)
            clusterColors<-colPalette[colIdx]
            if (dbg.heatmap>1) printVar(clusterColors)
            # draw heat map by colors, it is much faster compared to drawing in all colors in one single call
            #qdrawRect(painter,coords1[[1]],coords1[[2]],coords2[[1]],coords2[[2]],stroke=rgb(0,0,0,0),fill=clusterColors)
            for (c in colPalette) {
                i<-clusterColors==c
                qdrawRect(painter,coords1[[1]][i],coords1[[2]][i],coords2[[1]][i],coords2[[2]][i],stroke=rgb(0,0,0,0),fill=c)
            }
            # TODO: add NA colors here

            if (dbg.heatmap.limits) {
                drawLayerLimits(painter,layer,'heatmap','green')
            }

            if (heatmapTipTextSet) {
                # txtXXX - text to display
                # txtToMeasureXXX - text used to determine how much space
                #   is needed to display txtXXX
                if (.sharedEnv$heatmapTipHalign>0 && .sharedEnv$heatmapTipValign<0) {
                    # text to the lower right of the mouse pointer
                    # prepend some space such that the mouse pointer does not obscure the text
                    txtPrefix<-'   '
                    txtPostfix<-' '
                    txtToMeasurePrefix<-'((('
                    txtToMeasurePostfix<-')'
                } else if (.sharedEnv$heatmapTipHalign<0 && .sharedEnv$heatmapTipValign<0) {
                    # text to the lower left of the mouse pointer
                    # append some space such that the mouse pointer does not obscure the text
                    txtPrefix<-' '
                    txtPostfix<-' '
                    txtToMeasurePrefix<-'('
                    txtToMeasurePostfix<-')'
                } else {
                    # text to the upper left/right of the mouse pointer
                    # just reserve some space for the text
                    txtPrefix<-' '
                    txtPostfix<-' '
                    txtToMeasurePrefix<-'('
                    txtToMeasurePostfix<-')'
                }

                # split multi-line text
                ## TODO: does this work on Windows? Or shall we use \r\f instead?
                txtSplit<-strsplit(heatmapTipText,'\n|\r')[[1]]
                # prepend/append space
                txt<-paste(sapply(txtSplit,function(x)
                    paste(txtPrefix,x,txtPostfix,sep='')),collapse='\n')
                # determine text width
                txtWidth<-max(sapply(txtSplit,function(x)
                    qstrWidth(painter,paste(txtToMeasurePrefix,x,txtToMeasurePostfix,sep=''))))
                # determine text height
                #   qstrHeight does not seem to work well, e.g. with 'versicolor',
                #   so take the maximum height over all reasonable characters
                elementaryHeight<-qstrHeight(painter,'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890,!@#$%&(){}[]|/?')
                # reserve some more vertical space
                elementaryHeight<-1.1*elementaryHeight
                txtHeight<-length(txtSplit)*elementaryHeight
                qdrawRect(painter,
                    heatmapTipPos$x,
                    heatmapTipPos$y,
                    heatmapTipPos$x+.sharedEnv$heatmapTipHalign*txtWidth,
                    heatmapTipPos$y+.sharedEnv$heatmapTipValign*txtHeight,
                    stroke=rgb(0,0,0,0),fill=qcolor('white',alpha=255*17/20))
                qdrawText(painter,txt,heatmapTipPos$x,heatmapTipPos$y,
                    halign=ifelse(.sharedEnv$heatmapTipHalign>0,'left','right'),
                    valign=ifelse(.sharedEnv$heatmapTipValign>0,'bottom','top'),
                    color='black')
            }
            df
        }
        df<-heatmapPainterImpl(layer,painter)
        .sharedEnv$df<-df
    }

    heatmapLegendPainter<-function(layer,painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.heatmap) cat('heatmapLegendPainter called\n')

        # clear the background to cover the branches of a zoomed
        # dendrogram
        clearLayerBackground(layer,painter)

        if (dbg.heatmap) printVar(heatmapEnabled)
        if (!heatmapEnabled) return()

        heatmapLegendPainterImpl<-function(layer,painter) {
            if (dbg.heatmap) cat('heatmapLegendPainterImpl called\n')

            g1<-seq(df$k*.25,df$k*.75,length=heatmapColorCount+1)
            g2<-g1[-1]
            g1<-g1[-length(g1)]
            w1<-.25
            w2<-.75
            if (dbg.heatmap>1) printVar(g1)
            if (dbg.heatmap>1) printVar(g2)
            if (dbg.heatmap>1) printVar(w1)
            if (dbg.heatmap>1) printVar(w2)
            coords1<-gw2xy(heatmap2fig(list(g1,w1)))
            coords2<-gw2xy(heatmap2fig(list(g2,w2)))
            colPalette<-params$heatmapColors
            if (dbg.heatmap>1) printVar(colPalette)
            qdrawRect(painter,coords1[[1]],coords1[[2]],coords2[[1]],coords2[[2]],stroke='black',fill=colPalette)
            df
        }
        df<-heatmapLegendPainterImpl(layer,painter)
        .sharedEnv$df<-df
    }

    heatmapDimAnnotationPainter<-function(layer,painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.heatmap) cat('heatmapDimAnnotationPainter called\n')

        # clear the background to cover the branches of a zoomed
        # dendrogram
        clearLayerBackground(layer,painter)

        heatmapDimAnnotationPainterImpl<-function(layer,painter) {
            if (!is.null(df$dimLabels)) {
                gLabelDim<-seq(0,df$k-1)+.5
                wLabelDim<-rep(0,df$k)
                coordsLabelDim<-gw2xy(heatmap2fig(list(gLabelDim,wLabelDim)))

                if (dbg.heatmap.text) {
                    printVar(df$dimLabels)
                    printVar(coordsLabelDim[[1]])
                    printVar(coordsLabelDim[[2]])
                }
                qdrawText(painter,df$dimLabels,coordsLabelDim[[1]],coordsLabelDim[[2]],color='black',halign='left',rot=90)

                if (!.sharedEnv$heatmapDimAnnotationLayerSized) {
                    # resize heatmapDimAnnotationLayer such that dim annotations fit in nicely
                    x0<-layer$mapToScene(0,0)$x()
                    labels<-df$dimLabels
                    if (brushedmapEnabled) labels<-c(labels,'(brushed)')
                    xs<-apply(as.array(labels),1,function(x) {
                         # reserve more space for the labels
                        x<-paste(x,')',sep='')
                        layer$mapToScene(qstrWidth(painter,x),qstrHeight(painter,x))$x()
                    })
                    layout$setRowMinimumHeight(0,max(xs)-x0)
                    .sharedEnv$heatmapDimAnnotationLayerSized<-TRUE
                    if (dbg.heatmap) cat('heatmapDimAnnotationLayer sized\n')
                }
            }
        }
        if (heatmapEnabled) {
            # annotate data dimensions
            heatmapDimAnnotationPainterImpl(layer,painter)
        }
    }

    brushedmapAnnotationPainter<-function(layer,painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.brushedmap) cat('brushedmapAnnotationPainter called\n')

        # clear the background to cover the branches of a zoomed
        # dendrogram
        clearLayerBackground(layer,painter)

        brushedmapAnnotationPainterImpl<-function(layer,painter) {
            gLabelDim<-.5
            wLabelDim<-0
            coordsLabelDim<-gw2xy(heatmap2fig(list(gLabelDim,wLabelDim)))
            qdrawText(painter,'(brushed)',coordsLabelDim[[1]],coordsLabelDim[[2]],color='black',halign='left',rot=90)
        }
        if (brushedmapEnabled) {
            # annotate the brushed map
            brushedmapAnnotationPainterImpl(layer,painter)
        }
    }

    observationAnnotationPainter<-function(layer,painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.heatmap) cat('observationAnnotationPainter called\n')

        # clear the background to cover the branches of a zoomed
        # dendrogram
        clearLayerBackground(layer,painter)

        observationAnnotationPainterImpl<-function(layer,painter) {
            if (dbg.heatmap>1) printVar(df$observationLabelsOrdered)
            gLabelObs<-rep(0,df$n)
            wLabelObs<-seq(1,df$n)
            coordsLabelObs<-gw2xy(heatmap2fig(list(gLabelObs,wLabelObs)))
            # annotate observations
            qdrawText(painter,df$observationLabelsOrdered,coordsLabelObs[[1]],coordsLabelObs[[2]],color='black',halign='left')

            if (!.sharedEnv$observationAnnotationLayerSized) {
                # resize observationAnnotationLayer such that dim annotations fit in nicely
                x0<-layer$mapToScene(0,0)$x()
                # determine width of observatrion annotations; the
                # problem is that the width depends on the font used
                # and cen be determined by calling qstrWidth() only;
                # however, this call is quite slow, so we will not call
                # qstrWidth() for all annotations if the number of
                # annotations is high, but only for few of them -
                # those potentially long - i.e. for long character
                # strings
                if (n>200) {
                    # select a few long observation labels
                    lens<-sapply(df$observationLabelsOrdered,nchar)
                    if (sum(lens==max(lens))>10) {
                        len.threshold<-max(lens)
                    } else {
                        len.threshold<-floor(quantile(lens,.9))
                    }
                    tmp<-df$observationLabelsOrdered[sample(which(lens>=len.threshold),100)]
                } else {
                    # use all observation labels
                    tmp<-df$observationLabelsOrdered
                }
                xs<-sapply(tmp,function(x)layer$mapToScene(qstrWidth(painter,x),NA)$x())
                layout$setColumnMinimumWidth(3,max(xs)-x0+2)
                .sharedEnv$observationAnnotationLayerSized<-TRUE
                if (dbg.heatmap) cat('observationAnnotationLayer sized\n')
            }
            if (dbg.heatmap.limits) {
                drawLayerLimits(painter,layer,'observations','red')
            }
        }
        if (observationAnnotationEnabled) {
            # annotate the observations
            observationAnnotationPainterImpl(layer,painter)
        }
    }

    axisPainter<-function(layer,painter) {
        if (dbg.dendro.axis) cat('axisPainter called\n')

        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        df<-.sharedEnv$df

        clearLayerBackground(layer,painter)

        # axis
        xy<-gw2xy(list(g=c(0,last(df$h$height)),w=c(.5,.5)))
        qdrawSegment(painter,xy$x[1],xy$y[1],xy$x[2],xy$y[2],stroke='black')

        # ticks
        ticks<-pretty(dendroZoomMin$g[2]-dendroZoom$g)
        if (dbg.axis) printVar(ticks)
        ticksG<-rep(ticks,each=2)
        ticksW<-rep(c(.4,.6),length=length(ticksG))
        xy<-gw2xy(list(g=dendroZoomMin$g[2]-ticksG,w=ticksW))
        idxOdd<-seq(1,length(xy$x),by=2)
        idxEven<-seq(2,length(xy$x),by=2)
        #qdrawSegment(painter,xy$x[idxOdd],xy$y[idxOdd],xy$x[idxEven],xy$y[idxEven],stroke='black')
        qdrawSegment(painter,xy$x[idxOdd[-length(idxOdd)]],xy$y[idxOdd[-length(idxOdd)]],xy$x[idxEven[-length(idxEven)]],xy$y[idxEven[-length(idxEven)]],stroke='black')
        #tmp<-xy$x[idxOdd[2]]
        # TODO: raw segment for '0'
        qdrawSegment(painter,xy$x[idxOdd[length(idxOdd)]],xy$y[idxOdd[length(idxOdd)]],xy$x[idxEven[length(idxEven)]],xy$y[idxEven[length(idxEven)]],stroke='black')
        # tick labels
        #qdrawText(painter,ticks,xy$x[idxOdd],xy$y[idxOdd],color='black',valign='top')
        qdrawText(painter,(ticks)[-1],xy$x[idxOdd[-1]],xy$y[idxOdd[-1]],color='black',valign='top')
        qdrawText(painter,(ticks)[1],xy$x[idxOdd[1]],xy$y[idxOdd[1]],color='black',halign='right',valign='top')
    }

    ##################################################################
    ## interactions#FOLD02
    ##################################################################
    mousePressFun<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        if (dbg.mouse) cat('mousePressFun called\n')
        if (event$button()==1) {
            # left mouse button
            .sharedEnv$mouseLeftButtonPressed<-TRUE
            clusterSelector(layer, event)
        } else if (event$button()==2) {
            # right mouse button
            .sharedEnv$mouseRightButtonPressed<-TRUE
            dendroZoomSelectionStarter(layer, event)
        } else if (event$button()==4) {
            # middle mouse button
            .sharedEnv$mouseMiddleButtonPressed<-TRUE
            .sharedEnv$mouseMiddleButtonPressPos<-event$pos()
        } else {
            # ignored
        }
    }

    mouseMoveFun<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.mouse) cat('mouseMoveFun called\n')
        if (dbg.mouse>1) print(event$pos())

        if (.sharedEnv$mouseRightButtonPressed) {
            # update zooming region
            dendroZoomSelectionUpdater(layer, event)
        }

        if (.sharedEnv$mouseMiddleButtonPressed) {
            if (lastDendroZoomHistorySaver!='mouseMoveFun') {
                .sharedEnv$df<-pushDendroZoomHistory(.sharedEnv$df,.sharedEnv$dendroZoom,dbg.dendro.zoom)
                lastDendroZoomHistorySaver<<-'mouseMoveFun'
            }

            ## panning dendro

            # maximal move allowed (such that no empty space
            # appears at any dendro border)
            gwMaxDiff<-.sharedEnv$dendroZoomMin
            gwMaxDiff$g<-.sharedEnv$dendroZoomMin$g-.sharedEnv$dendroZoom$g
            gwMaxDiff$w<-.sharedEnv$dendroZoomMin$w-.sharedEnv$dendroZoom$w
            xyMaxDiff<-gw2xy(gwMaxDiff)
            xy<-gw2xy(.sharedEnv$dendroZoom)
            # real xy diff requested
            xyDiff<-list(
                x=.sharedEnv$mouseMiddleButtonPressPos$x()-event$pos()$x(),
                y=.sharedEnv$mouseMiddleButtonPressPos$y()-event$pos()$y())
            # restrict the real move by the maximal allowed move
            xyDiff$x<-min(max(xyMaxDiff$x),max(min(xyMaxDiff$x),xyDiff$x))
            xyDiff$y<-min(max(xyMaxDiff$y),max(min(xyMaxDiff$y),xyDiff$y))
            xy$x<-xy$x+xyDiff$x
            xy$y<-xy$y+xyDiff$y

            gw<-xy2gw(xy)
            if (dbg.dendro) printVar(gw)
            .sharedEnv$dendroZoom<-gw
            if (dbg.dendro) printVar(.sharedEnv$dendroZoom)

            # do the pan
            zoomDendroBrushedmapAndHeatmap(layer$scene())
        }
    }

    mouseReleaseFun<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        if (dbg.mouse) cat('mouseReleaseFun called\n')
        if (event$button()==1) {
            # left mouse button
            .sharedEnv$mouseLeftButtonPressed<-FALSE
        } else if (event$button()==2) {
            # right mouse button
            .sharedEnv$mouseRightButtonPressed<-FALSE
            dendroZoomSelectionFinisher(layer, event)
        } else if (event$button()==4) {
            # middle mouse button
            .sharedEnv$mouseMiddleButtonPressed<-FALSE
        } else {
            # ignored
        }
    }

    # select current cluster
    clusterSelector<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro.select) cat('clusterSelector called\n')

        clusterSelectorImpl<-function(layer,event,df) {
            if (dbg.dendro.select) cat('clusterSelectorImpl called\n')
            if (dbg.dendro.select) print(as.numeric(event$pos()))

            if (dbg.dendro.select) printVar(df$currentCluster)

            df<-selectCluster(event$pos(),df,dendroZoom,dbg.dendro.select)
        }
        .sharedEnv$df<-clusterSelectorImpl(layer,event,.sharedEnv$df)
        .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
    }

    # Make GW to lie within given GW rectangle.
    # Used to make sure that the rectangle we're zooming to does not
    # lie out of the dendro.
    #
    restrictGw<-function(gw,limitGw) {
        gw$g<-sort(gw$g,decreasing=F)
        gw$w<-sort(gw$w,decreasing=F)

        gw$g[1]<-max(limitGw$g[1],min(limitGw$g[2],gw$g[1]))
        gw$g[2]<-min(limitGw$g[2],max(limitGw$g[1],gw$g[2]))
        gw$w[1]<-max(limitGw$w[1],min(limitGw$w[2],gw$w[1]))
        gw$w[2]<-min(limitGw$w[2],max(limitGw$w[1],gw$w[2]))
        gw
    }

    dendroZoomSelectionStarter<-function(layer, event) {
        if (dbg.dendro.select) cat('dendroZoomSelectionStarter called\n')
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        .sharedEnv$dendroZoomMouseSelection<-xy2gw(list(x=rep(event$pos()$x(),2),y=rep(event$pos()$y(),2)))
        qupdate(.sharedEnv$dendroLayer)
    }

    dendroZoomSelectionUpdater<-function(layer, event) {
        if (dbg.dendro.select) cat('dendroZoomSelectionUpdater called\n')
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        gw<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))
        .sharedEnv$dendroZoomMouseSelection$g[2]<-gw$g
        .sharedEnv$dendroZoomMouseSelection$w[2]<-gw$w
        qupdate(.sharedEnv$dendroLayer)
    }

    dendroZoomSelectionFinisher<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro.select) cat('dendroZoomSelectionFinisher called\n')
        if (event$button()==2) {
            # right mouse button

            .sharedEnv$df<-pushDendroZoomHistory(.sharedEnv$df,.sharedEnv$dendroZoom,dbg.dendro.zoom)
            lastDendroZoomHistorySaver<<-'dendroZoomSelectionFinisher'

            # update the selection region
            dendroZoomSelectionUpdater(layer, event)

            .sharedEnv<-attr(layer$scene(),'.sharedEnv')
            # set the current zoom to the selection region
            .sharedEnv$dendroZoom<-restrictGw(.sharedEnv$dendroZoomMouseSelection,.sharedEnv$dendroZoomMin)
            if (dbg.dendro.zoom>1) printVar(.sharedEnv$dendroZoom)
            # reset the selection region
            .sharedEnv$dendroZoomMouseSelection<-list(g=c(NA,NA),w=c(NA,NA))
            # zoom to the current zoom
            zoomDendroBrushedmapAndHeatmap(layer$scene())

            #qupdate(scene)
        } else {
            # ignored
        }
    }

    # Dendrogram-zooming mouse wheel button handler.
    dendroZoomer<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro.zoom) cat('dendroZoomer called\n')

        .sharedEnv$df<-pushDendroZoomHistory(.sharedEnv$df,.sharedEnv$dendroZoom,dbg.dendro.zoom)
        lastDendroZoomHistorySaver<<-'dendroZoomer'

        dendroZoomerImpl<-function(layer, event) {
            delta<-event$delta()
            if (dbg.dendro.zoom>1) printVar(delta)
            pos<-as.numeric(event$pos())
            if (dbg.dendro.zoom>1) printVar(pos)
            gw<-fig2dendro(xy2gw(list(pos[1],pos[2])))
            if (dbg.dendro.zoom>1) printVar(gw)
            if (dbg.dendro.zoom>1) printVar(dendroZoom)

            # Construct the (absolute) zoomFactor based on the
            # user-supplied relative zoomFactor and the zooming amount
            # requested by the mouse wheel.
            # The zoomFactor must be less than 1 in order to prevent
            # range reversal.
            zoomFactor<-min(.5,params$zoomFactor*abs(delta))

            if (delta>0) {
                # zoom-in
                o<-newDendroZoom<-dendroZoom
                newDendroZoom$g<-dendroZoom$g-(dendroZoom$g-gw$g)*zoomFactor
                newDendroZoom$w<-dendroZoom$w-(dendroZoom$w-gw$w)*zoomFactor
                dendroZoom<-newDendroZoom
            } else {
                # zoom-out
                newDendroZoom<-dendroZoom
                newDendroZoom$g<-dendroZoom$g+(dendroZoom$g-gw$g)*(zoomFactor/(1-zoomFactor))
                newDendroZoom$w<-dendroZoom$w+(dendroZoom$w-gw$w)*(zoomFactor/(1-zoomFactor))
                # do not zoom out beyond the original zoom (do not shrink the dendrogram too much)
                newDendroZoom$g[1]<-max(dendroZoomMin$g[1],newDendroZoom$g[1])
                newDendroZoom$g[2]<-min(dendroZoomMin$g[2],newDendroZoom$g[2])
                newDendroZoom$w[1]<-max(dendroZoomMin$w[1],newDendroZoom$w[1])
                newDendroZoom$w[2]<-min(dendroZoomMin$w[2],newDendroZoom$w[2])
                dendroZoom<-newDendroZoom
            }
            dendroZoom
        }
        dendroZoom<-dendroZoomerImpl(layer, event)
        .sharedEnv$dendroZoom<-dendroZoom
        if (dbg.dendro.zoom>1) printVar(dendroZoom)

        zoomDendroBrushedmapAndHeatmap(layer$scene())
    }

    zoomDendroBrushedmapAndHeatmap<-function(scene) {
        .sharedEnv<-attr(scene,'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro.zoom) cat('zoomDendroBrushedmapAndHeatmap called\n')

        gw<-dendro2fig(dendroZoom)
        xy<-gw2xy(gw)
        if (dbg.dendro.zoom) printVar(gw)
        if (dbg.dendro.zoom) printVar(xy)

        # zoom dendrogram
        dendroLayer$setLimits(qrect(xy[[1]][1],xy[[2]][1],xy[[1]][2],xy[[2]][2]))
        # zoom axis
        axisLayerLimits<-axisLayer$limits()
        axisLayerLimits$setLeft(xy[[1]][1])
        axisLayerLimits$setRight(xy[[1]][2])
        if (dbg.dendro.zoom) printVar(axisLayerLimits$left())
        if (dbg.dendro.zoom) printVar(axisLayerLimits$top())
        if (dbg.dendro.zoom) printVar(axisLayerLimits$right())
        if (dbg.dendro.zoom) printVar(axisLayerLimits$bottom())
        axisLayer$setLimits(axisLayerLimits)

        # zoom brushed map
        tmp<-brushedmapLayer$limits()
        tmp$setTop(xy[[2]][1])
        tmp$setBottom(xy[[2]][2])
        brushedmapLayer$setLimits(tmp)

        # zoom heat map
        if (!is.null(heatmapLayer)) {
            # zoom heat map
            if (dbg.heatmap) cat('setting heatmap limits\n')
            tmp<-heatmapLayer$limits()
            tmp$setTop(xy[[2]][1])
            tmp$setBottom(xy[[2]][2])
            heatmapLayer$setLimits(tmp)

            # zoom observations
            observationAnnotationLayer<-.sharedEnv$observationAnnotationLayer
            tmp<-observationAnnotationLayer$limits()
            tmp$setTop(xy[[2]][1])
            tmp$setBottom(xy[[2]][2])
            observationAnnotationLayer$setLimits(tmp)
        }

        qupdate(scene)
    }

    axisCutterUpdate<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutterUpdate called\n')
        .sharedEnv$axisCut<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))$g

        qupdate(.sharedEnv$dendroLayer)
    }

    axisCutterLeave<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutterLeave called\n')
        .sharedEnv$axisCut<-NA

        qupdate(.sharedEnv$dendroLayer)
    }

    axisCutter<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutter called\n')
        cutG<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))$g

        impl<-function(df) {
            #printVar(df)
            cutDendro(df,cutG,.sharedEnv$dendroZoom,dbg.dendro.cut)
        }
        rv<-impl(.sharedEnv$df)
        if (dbg.dendro.cut) printVar(length(rv$clusters))
        if (dbg.dendro.cut>1) printVar(rv)
        if (rv$selectedClusterCount>params$maxClusterCount) {
            Qt$QMessageBox$information(NULL,'idendro',
                paste('You have selected ',rv$selectedClusterCount,
                    ' clusters, but the maximal configured number of clusters (maxClusterCount) is ',
                    params$maxClusterCount,'.',sep=''))
        } else {
            .sharedEnv$df<-rv$df
            .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
        }
    }

    heatmapMousePressFun<-function(layer, event) {
        heatmapInspector(layer,event)
    }

    heatmapMouseMoveFun<-function(layer, event) {
        heatmapInspector(layer,event)
    }

    heatmapMouseReleaseFun<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        .sharedEnv$heatmapTipTextSet<-FALSE
        qupdate(.sharedEnv$heatmapLayer)
    }

    heatmapInspector<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

        if (dbg.mouse) cat('heatmapAnnotator called\n')
        if (dbg.mouse>1) print(event$pos())

        xy<-list(x=event$pos()$x(),y=event$pos()$y())
        gw<-fig2heatmap(xy2gw(xy))
        if (dbg.mouse) printVar(gw)
        # g: 0 .. df$k -> col: 1 .. df$k
        col<-round(gw$g+.5)
        # w: .5 .. (df$n+.5) -> row: 1 .. df$n
        row<-round(gw$w)
        if (dbg.mouse>1) printVar(col)
        if (dbg.mouse>1) printVar(row)

        annotationChanged<-FALSE
        if (col>0 && col<=df$k && row>0 && row<=df$n) {
            .sharedEnv$heatmapTipText<-heatmapInspectFormatFun(df$xOrigOrdered[row,col])
            .sharedEnv$heatmapTipTextSet<-TRUE
            annotationChanged<-TRUE
            .sharedEnv$heatmapTipPos<-xy
            .sharedEnv$heatmapTipHalign<-ifelse(col>df$k/2,-1,1)
            .sharedEnv$heatmapTipValign<-ifelse(row>df$n/2,-1,1)
        } else {
            if (.sharedEnv$heatmapTipTextSet) {
                annotationChanged<-TRUE
                .sharedEnv$heatmapTipTextSet<-FALSE
            }
        }
        if (annotationChanged) qupdate(.sharedEnv$heatmapLayer)
    }

    backgroundClearingPainter<-function(layer,painter) {
        # clear the background
        clearLayerBackground(layer,painter)
    }

    backgroundClearingPainterRigthToAxis<-function(layer,painter) {
        # clear the background
        clearLayerBackground(layer,painter, c(0.05,0,0,0))
    }

    ## hack: to disable warnings in 'qlayer' like:
    ## In qlayer(scene, paintFun = brushedmapPainter, clip = FALSE, cache = TRUE,  :
    ##  Enabling caching implicitly enables clipping
    ## we use 'suppressWarnings()'

    ## dendrogram#FOLD01
    ################
    dendroLimits<-gw2xy(dendro2fig(dendroZoomMin))
    #dendroLimits$x<-extendRange(dendroLimits$x,.05)
    dendroLimits<-unlist(dendroLimits)
    if (dbg.dendro) printVar(dendroLimits)
    dendroLayer<-suppressWarnings(qlayer(scene,paintFun=dendroPainter,
            mousePressFun=mousePressFun,
            mouseMoveFun=mouseMoveFun,
            mouseReleaseFun=mouseReleaseFun,
            wheelFun=dendroZoomer,clip=clipDendro,cache=FALSE,
            limits=qrect(dendroLimits[1],dendroLimits[3],dendroLimits[2],dendroLimits[4])))

    axisLimits<-dendroZoomMin
    axisLimits$w<-c(0,1)
    axisLimits<-gw2xy(dendro2fig(axisLimits))
    #axisLimits$x<-extendRange(axisLimits$x,.05)
    axisLimits<-unlist(axisLimits)
    if (dbg.dendro.axis) printVar(axisLimits)

    axisLayer<-qlayer(scene,paintFun=axisPainter,
            hoverMoveFun=axisCutterUpdate,
            hoverEnterFun=axisCutterUpdate,
            hoverLeaveFun=axisCutterLeave,
            mousePressFun=axisCutter,
            limits=qrect(axisLimits[1],axisLimits[3],axisLimits[2],axisLimits[4]),
            clip=FALSE)

    ## brushed map#FOLD01
    ################
    #TODO: scale
    brushedmapLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=.5+df$n*c(0,1)))))
    if (dbg.brushedmap) printVar(brushedmapLimits)
    brushedmapLayer<-suppressWarnings(qlayer(scene,paintFun=brushedmapPainter,clip=FALSE,cache=TRUE,
        limits=qrect(brushedmapLimits[1],brushedmapLimits[3],brushedmapLimits[2],brushedmapLimits[4])))

    ## brushed map annotation
    brushedmapAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=c(0,1)))))
    brushedmapAnnotationLayer<-suppressWarnings(qlayer(scene,
        paintFun=brushedmapAnnotationPainter,
        limits=qrect(brushedmapAnnotationLimits[1],brushedmapAnnotationLimits[3],
                brushedmapAnnotationLimits[2],brushedmapAnnotationLimits[4]),
        clip=FALSE,cache=TRUE))

    ## heat map#FOLD01
    ################
    #TODO: scale
    heatmapLimits<-unlist(gw2xy(heatmap2fig(list(
        g=c(0,max(1,df$k)), # the positive difference in 'g' makes
        # layers size manageable easily
        w=.5+df$n*c(0,1)))))
    if (dbg.heatmap) printVar(heatmapLimits)
    heatmapLayer<-suppressWarnings(qlayer(scene,paintFun=heatmapPainter,clip=FALSE,cache=TRUE,
        mousePressFun=heatmapMousePressFun,
        mouseReleaseFun=heatmapMouseReleaseFun,
        mouseMoveFun=heatmapMouseMoveFun,
        limits=qrect(heatmapLimits[1],heatmapLimits[3],heatmapLimits[2],heatmapLimits[4])))

    heatmapLegendLimits<-unlist(gw2xy(heatmap2fig(list(
        g=c(0,max(1,df$k)), # the positive difference in 'g' makes
        # layers size manageable easily
        w=c(0,1)))))
    if (dbg.heatmap) printVar(heatmapLegendLimits)
    heatmapLegendLayer<-suppressWarnings(qlayer(scene,paintFun=heatmapLegendPainter,clip=FALSE,cache=TRUE,
            limits=qrect(heatmapLegendLimits[1],heatmapLegendLimits[3],heatmapLegendLimits[2],heatmapLegendLimits[4])))

    ## heat map dim annotations
    heatmapDimAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(
        g=c(0,max(1,df$k)), # the positive difference in 'g' makes
        # layers size manageable easily
        w=c(0,1)))))
    heatmapDimAnnotationLayer<-suppressWarnings(qlayer(scene,
        paintFun=heatmapDimAnnotationPainter,
        limits=qrect(heatmapDimAnnotationLimits[1],heatmapDimAnnotationLimits[3],
            heatmapDimAnnotationLimits[2],heatmapDimAnnotationLimits[4]),
        clip=FALSE,cache=TRUE))
    heatmapDimAnnotationLayerSized<-FALSE

    ## observations annotations
    observationAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=c(0,df$n)+.5))))
    observationAnnotationLayer<-suppressWarnings(qlayer(scene,
        paintFun=observationAnnotationPainter,
        limits=qrect(observationAnnotationLimits[1],observationAnnotationLimits[3],
            observationAnnotationLimits[2],observationAnnotationLimits[4]),
            clip=FALSE,cache=TRUE))
    observationAnnotationLayerSized<-FALSE

    backgroundClearingLayer1<-qlayer(scene,paintFun=backgroundClearingPainter,limits=qrect(0,0,1,1))
    backgroundClearingLayer2<-qlayer(scene,paintFun=backgroundClearingPainter,limits=qrect(0,0,1,1))
    backgroundClearingLayer3<-qlayer(scene,paintFun=backgroundClearingPainter,limits=qrect(0,0,1,1))
    backgroundClearingLayer4<-qlayer(scene,paintFun=backgroundClearingPainter,limits=qrect(0,0,1,1))

    #############
    ## layout#FOLD01
    #############
    figLayer<-qlayer(scene)
    figLayer[1,0]<-dendroLayer
    figLayer[1,1]<-heatmapLayer
    figLayer[1,2]<-brushedmapLayer
    figLayer[1,3]<-observationAnnotationLayer
    figLayer[2,0]<-axisLayer
    figLayer[2,1]<-heatmapLegendLayer
    figLayer[2,2]<-backgroundClearingLayer1
    figLayer[2,3]<-backgroundClearingLayer2
    figLayer[0,0]<-backgroundClearingLayer3
    if (!is.null(heatmapDimAnnotationLayer)) figLayer[0,1]<-heatmapDimAnnotationLayer
    figLayer[0,2]<-brushedmapAnnotationLayer
    figLayer[0,3]<-backgroundClearingLayer4

    # let's make dendroWidth+heatmapWidth =~ 600
    # dendroWidth =~ 600-heatmapWidth = 600*(1-heatmapRelSize)
    dendroPreferredWidth<-600*(1-heatmapRelSize)
    dendroPreferredHeight<-300

    layout<-figLayer$gridLayout()
    # heat map dimensions annotation
    if (heatmapEnabled) {
        layout$setRowPreferredHeight(0,75)
    } else {
        layout$setRowMaximumHeight(0,0)
    }
    # dendro + heat map + brushed map + observations annotation
    layout$setRowPreferredHeight(1,dendroPreferredHeight)
    # axis
    layout$setRowPreferredHeight(2,50)
    # heat map dimensions annotation: fixed
    layout$setRowStretchFactor(0,0)
    # dendro + heat map + brushed map + observations annotation: stretchable
    layout$setRowStretchFactor(1,1)
    # axis: fixed
    layout$setRowStretchFactor(2,0)

    # dendro
    if (heatmapEnabled) {
        layout$setColumnPreferredWidth(0,dendroPreferredWidth)
    } else {
        layout$setColumnPreferredWidth(0,dendroPreferredWidth)
    }
    # heat map
    if (heatmapEnabled) {
        layout$setColumnPreferredWidth(1,dendroPreferredWidth/(1-heatmapRelSize)*heatmapRelSize)
    } else {
        layout$setColumnMaximumWidth(1,0)
    }
    # brushed map
    if (brushedmapEnabled) {
        layout$setColumnPreferredWidth(2,25)
    } else {
        layout$setColumnMaximumWidth(2,0)
    }
    # observations annotation
    if (observationAnnotationEnabled) {
        layout$setColumnPreferredWidth(3,50)
    } else {
        layout$setColumnMaximumWidth(3,0)
    }

    # dendro stretchable
    if (heatmapEnabled) {
        layout$setColumnStretchFactor(0,10*(1-heatmapRelSize))
    } else {
        layout$setColumnStretchFactor(0,1)
    }
    if (heatmapEnabled) {
        # heat map stretchable
        layout$setColumnStretchFactor(1,10*heatmapRelSize)
    } else {
        layout$setColumnStretchFactor(1,0)
    }
    # brushed map: fixed size
    layout$setColumnStretchFactor(2,0)
    # observations annotations: fixed size
    layout$setColumnStretchFactor(3,0)

    ## listeners on the data (which column updates which layer(s))
    if (!is.null(qx)) {
        qx.listener<-add_listener(qx,function(i,j) {
            idx<-which(j==c('.brushed'))
            if (length(idx)>0) {
                if (dbg.brushedmap) cat('qx listener: brushed status changed\n')
                qupdate(brushedmapLayer)
                guiWindow$updateClusterInfos()
            }
        })
        qconnect(dendroLayer,'destroyed',function(x) {
            remove_listener(qx, qx.listener)
        })
    }

    if (!is.null(opengl)) {
        view<-qplotView(scene=scene,opengl=opengl)
    } else {
        view<-qplotView(scene=scene)
    }
    if (separateGui) print(view)

    #######################################################
    #######################################################
    ## GUI#FOLD01
    if (dbg.gui) cat('initializing GUI\n')
    suppressWarnings(qsetClass("Window",Qt$QWidget,function(parent=NULL) {
        super(parent)

        # +--------------------- windowLayout ----------------------+
        # | +------------- guiLayout ------------+ +---- view ----+ |
        # | | +-------- clustersLayout --------+ | |              | |
        # | | | cluster  123 total  42 brushed | | |              | |
        # | | |   1        6 (5%)     0 (0%)   | | |              | |
        # | | |   2        0 (0%)     0 (0%)   | | |  dendrogram  | |
        # | | |   3        0 (0%)     0 (0%)   | | |              | |
        # | | +--------------------------------+ | |              | |
        # | |              GUI                   | |              | |
        # | |            buttons                 | |              | |
        # | |                                    | |              | |
        # | +------------------------------------+ +--------------+ |
        # +---------------------------------------------------------+
        guiLayout<-Qt$QVBoxLayout()
        guiLayout$setAlignment(Qt$Qt$AlignVCenter)
        ## NOTE: the layout does not take ownership of the widgets, so we
        ## need to assign the layout to our widget up-front. Our widget then
        ## takes ownership of the widgets in the layout.

        clustersLayout<-Qt$QGridLayout()
        # 'cluster' label
        clusterLabel<-Qt$QLabel('cluster')
        clustersLayout$addWidget(clusterLabel,1,1)#,Qt$Qt$AlignJustify) # left
        # 'N total' label
        label<-Qt$QLabel(paste(df$n,'total'))
        clustersLayout$addWidget(label,1,2,Qt$Qt$AlignJustify)
        if (brushedmapEnabled) {
            # 'M brushed' label
            this$brushedDesc<-Qt$QLabel('0 brushed')
            this$brushedDesc$setMinimumWidth(computeMaxLabelWidth('',' brushed'))
            clustersLayout$addWidget(this$brushedDesc,1,3,Qt$Qt$AlignJustify)
        }
        # populate clustersLayout with ...
        createButtons(clusterLabel$sizeHint$width(),where=topenv(parent.frame(3)))
        for (i in 1:params$maxClusterCount) {
            # ... currentCluster selectors,
            clustersLayout$addWidget(this$clusterSelectorButtons[[i]],i+1,1)
            # ... clusterInfos (out of total number of observations)
            clustersLayout$addWidget(this$clusterInfosTotal[[i]],i+1,2)
            if (brushedmapEnabled) {
                # ... clusterInfos (out of brushed observations)
                clustersLayout$addWidget(this$clusterInfosBrushed[[i]],i+1,3)
            }
        }
        # set the current cluster
        this$clusterSelectorButtons[[df$currentCluster]]$setChecked(TRUE)

        guiLayout$addLayout(clustersLayout,0)

        # GUI buttons
        zoomLayout<-Qt$QHBoxLayout()
        zoomLayout$addStretch(1)
        zoomLayout$addWidget(this$zoomBackButton)
        zoomLayout$addWidget(this$fullViewButton)
        zoomLayout$addStretch(1)
        guiLayout$addLayout(zoomLayout)

        selectBackLayout<-Qt$QHBoxLayout()
        selectBackLayout$addStretch(1)
        selectBackLayout$addWidget(this$selectBackButton)
        selectBackLayout$addStretch(1)
        guiLayout$addLayout(selectBackLayout,0)
        selectionLayout<-Qt$QHBoxLayout()
        selectionLayout$addStretch(1)
        selectionLayout$addWidget(this$unselectButton)
        selectionLayout$addWidget(this$unselectAllButton)
        selectionLayout$addStretch(1)
        guiLayout$addLayout(selectionLayout,0)

        heatmapSmoothingLabelLayout<-Qt$QHBoxLayout()
        heatmapSmoothingLabelLayout$addWidget(this$heatmapSmoothingLabel)
        heatmapSmoothingLayout<-Qt$QHBoxLayout()
        heatmapSmoothingLayout$addWidget(this$heatmapSmoothingRadioButton_none)
        heatmapSmoothingLayout$addWidget(this$heatmapSmoothingRadioButton_cluster)
        heatmapSmoothingLayout$addWidget(this$heatmapSmoothingRadioButton_zoom)
        guiLayout$addLayout(heatmapSmoothingLabelLayout,0)
        guiLayout$addLayout(heatmapSmoothingLayout,0)

        quitLayout<-Qt$QHBoxLayout()
        quitLayout$addStretch(1)
        quitLayout$addWidget(this$quitButton)
        quitLayout$addStretch(1)
        guiLayout$addLayout(quitLayout,0)

        windowLayout<-Qt$QHBoxLayout()
        windowLayout$addLayout(guiLayout,0)
        if (!separateGui) windowLayout$addWidget(view,1)
        setLayout(windowLayout)

        setWindowTitle("idendro")
        if (!is.null(params$geometry)) setGeometry(params$geometry)
        if (dbg.gui) cat('Window created\n')

    },where=environment()))

    getClusterInfoTotal<-function(idx) {
        info<-'0 (0%)'
        ratio<-0
        if (dbg.dendro.select>1) printVar(df$clusters)
        if (!is.null(df$clusters)) {
            if (idx<=length(df$clusters) && length(df$clusters[[idx]]$indices)>0) {
                clusterSize<-length(df$clusters[[idx]]$indices)+1
                ratio<-clusterSize/df$n
                info<-paste(
                    clusterSize,' (',round(100*ratio),'%)',
                    sep='')
            }
        }
        return(list(info=info,ratio=ratio))
    }
    getClusterInfoBrushed<-function(idx) {
        info<-'--'
        ratio<-0
        if (brushedmapEnabled) {
            if (dbg.dendro.select>1) printVar(df$clusters)
            if (!is.null(df$clusters)) {
                if (idx<=length(df$clusters) && length(df$clusters[[idx]]$indices)>0) {
                    clusterBrushedSize<-sum(df$leafColorIdxs==idx & qx$.brushed)
                    brushedSize<-sum(qx$.brushed)
                    if (brushedSize>0) {
                        ratio<-clusterBrushedSize/brushedSize
                        info<-paste(
                            clusterBrushedSize,' (',round(100*ratio),'%)',
                            sep='')
                    }
                }
            }
        }
        return(list(info=info,ratio=ratio))
    }

    qsetMethod("updateClusterInfos", Window, function() {
        if (dbg.dendro.info) cat('updateClusterInfos called\n')
        .sharedEnv<-attr(scene,'.sharedEnv')
        df<-.sharedEnv$df
        if (brushedmapEnabled) {
            this$brushedDesc$setText(paste(sum(qx$.brushed),'brushed'))
        }
        for (i in 1:params$maxClusterCount) {
            ci<-getClusterInfoTotal(i)
            this$clusterInfosTotal[[i]]$setText(ci$info)
            this$clusterInfosTotal[[i]]$setRelWidth(ci$ratio)
            if (brushedmapEnabled) {
                ci<-getClusterInfoBrushed(i)
                this$clusterInfosBrushed[[i]]$setText(ci$info)
                this$clusterInfosBrushed[[i]]$setRelWidth(ci$ratio)
            }
        }
        update()
    })

    qsetMethod("createButtons", Window, function(clusterLabelWidth,...) {
        if (dbg.gui) cat('createButtons called\n')

        ## Radio button with a colored background.
        # Used to identify the current cluster.
        suppressWarnings(qsetClass("ColoredRadioButton",Qt$QRadioButton,function(parent=NULL,color=color,...) {
            super(parent,...)
            this$color<-color
        },...))
        qsetMethod("paintEvent",ColoredRadioButton,function(event) {
            painter<-Qt$QPainter(this)
            # draw background
            rect<-event$rect()
            painter$fillRect(rect,qbrush(this$color))
            # after calling 'super('paintEvent',event)' the painter becomes
            # unusable (!?!), so let's end it here and create a new one later
            painter$end() 
            # draw the radio button
            super('paintEvent',event)
            # draw border
            painter<-Qt$QPainter(this)
            painter$setPen(Qt$QColor(200,200,200))
            painter$drawLine(rect$left(),rect$bottom(),rect$left(),rect$top())
            painter$drawLine(rect$left(),rect$top(),rect$right(),rect$top())
            painter$setPen(Qt$QColor(148,148,148))
            painter$drawLine(rect$left(),rect$bottom(),rect$right(),rect$bottom())
            painter$drawLine(rect$right(),rect$bottom(),rect$right(),rect$top())
            # must finish the painter
            painter$end()
        })

        # Label with a partially filled background.
        # Used to represent the ratio of members in a cluster.
        suppressWarnings(qsetClass("ColoredRectWithLabel",Qt$QLabel,function(parent=NULL,color='black',relWidth=0,...) {
            super(parent,...)
            setAlignment(0x84) # Justifies the text in the available space.
            this$color<-color
            this$relWidth<-relWidth
        },...))
        qsetMethod("setRelWidth",ColoredRectWithLabel,function(relWidth) {
            this$relWidth<-relWidth
        })
        qsetMethod("paintEvent",ColoredRectWithLabel,function(event) {
            painter<-Qt$QPainter(this)
            rect<-Qt$QRect(event$rect())
            if (graphicalClusterInfos) {
                # fill part of the rectangle, excluding border
                rect$setWidth(rect$width()*this$relWidth)
                painter$fillRect(rect,qbrush(this$color))
            }
            # draw border
            rect<-Qt$QRect(event$rect())
            painter$setPen(Qt$QColor(200,200,200))
            painter$drawLine(rect$left(),rect$bottom(),rect$left(),rect$top())
            painter$drawLine(rect$left(),rect$top(),rect$right(),rect$top())
            painter$setPen(Qt$QColor(148,148,148))
            painter$drawLine(rect$left(),rect$bottom(),rect$right(),rect$bottom())
            painter$drawLine(rect$right(),rect$bottom(),rect$right(),rect$top())
            # must finish the painter
            painter$end()
            if (textualClusterInfos) {
                # draw the label
                super('paintEvent',event)
            }
        })

        this$clusterSelectorButtons<-vector('list',params$maxClusterCount)
        this$clusterInfosTotal<-vector('list',params$maxClusterCount)
        this$clusterInfosBrushed<-vector('list',params$maxClusterCount)

        # width of the "cluster" label, used to make cluster selectors wide at least of this size
        maxClusterSelectorWidth<-clusterLabelWidth
        # a wide label used to measure the maximal width of a cluster info label ("XXX (100%)")
        wideLabel<-ColoredRectWithLabel(paste(df$n,'(100%)'))
        # minimal width of a cluster info label
        maxClusterInfoWidth<-wideLabel$sizeHint$width()+2*3 # add some space at the borders to make it visually more appealing
        for (i in 1:params$maxClusterCount) {
            this$clusterSelectorButtons[[i]]<-ColoredRadioButton(sprintf('%2d',i),params$clusterColors[i])
            qconnect(this$clusterSelectorButtons[[i]],"pressed",function(i) {
                df<-attr(scene,'.sharedEnv')$df
                if (df$currentCluster!=i) {
                    df$currentCluster<-i
                    setCurrentClusterInQx(qx,df)
                    attr(scene,'.sharedEnv')$df<-df
                }
            },i)
            this$clusterInfosTotal[[i]]<-ColoredRectWithLabel('0 (0%)',params$clusterColors[i])
            # the width of cluster info label is known - set it!
            this$clusterInfosTotal[[i]]$setMinimumWidth(maxClusterInfoWidth)
            if (brushedmapEnabled) {
                this$clusterInfosBrushed[[i]]<-ColoredRectWithLabel('--',params$clusterColors[i])
                # the width of cluster info label is known - set it!
                this$clusterInfosBrushed[[i]]$setMinimumWidth(maxClusterInfoWidth)
            }
            # the width of current cluster selector is not know yet - keep accumulating the maximal value encountered so far
            maxClusterSelectorWidth<-max(maxClusterSelectorWidth,this$clusterSelectorButtons[[i]]$sizeHint$width())
        }
        for (i in 1:params$maxClusterCount) {
            # finally, set the minimal width to the maximum of widths of current cluster selectors
            this$clusterSelectorButtons[[i]]$setMaximumWidth(maxClusterSelectorWidth)
        }

        this$fullViewButton<-Qt$QPushButton('&Full view')
        qconnect(this$fullViewButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            # do not attempt to change zoom if zoomed fully already
            if (!(dendroZoom$g==dendroZoomMin$g && dendroZoom$w==dendroZoomMin$w)) {
                .sharedEnv$df<-pushDendroZoomHistory(.sharedEnv$df,.sharedEnv$dendroZoom,dbg.dendro.zoom)
                lastDendroZoomHistorySaver<<-'fullView'
                .sharedEnv$dendroZoom<-dendroZoomMin
                zoomDendroBrushedmapAndHeatmap(scene)
            }
        })

        this$zoomBackButton<-Qt$QPushButton('Undo &zoom')
        qconnect(this$zoomBackButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.zoom) cat('zoomBack button pressed\n')

            rv<-popDendroZoomHistory(.sharedEnv$df,dbg.dendro.zoom)
            if (!is.null(rv$dendroZoom)) {
                .sharedEnv$dendroZoom<-rv$dendroZoom
                .sharedEnv$df<-rv$df
                if (dbg.dendro.zoom) printVar(.sharedEnv$dendroZoom)
                zoomDendroBrushedmapAndHeatmap(scene)
            }
        })

        this$unselectButton<-Qt$QPushButton('&Unselect')# current cluster')
        qconnect(this$unselectButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.select) cat('unselect button pressed\n')

            rv<-unselectCurrentCluster(.sharedEnv$df,dbg.dendro.select)
            .sharedEnv$df<-rv$df
            if (rv$selectionChanged) {
                .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
            }
        })

        this$unselectAllButton<-Qt$QPushButton('Unselect &all')# clusters')
        qconnect(this$unselectAllButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.select) cat('unselectAll button pressed\n')

            rv<-unselectAllClusters(.sharedEnv$df,dbg.dendro.select)
            .sharedEnv$df<-rv$df
            if (rv$selectionChanged) {
                .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
            }
        })

        this$selectBackButton<-Qt$QPushButton('Undo &selection')
        qconnect(this$selectBackButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.select) cat('selectBack button pressed\n')

            rv<-popSelectionHistory(.sharedEnv$df,dbg.dendro.select)
            if (!is.null(rv)) {
                .sharedEnv$df<-rv
                .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
            } else {
                # no history to recall
            }
        })

        this$heatmapSmoothingLabel<-Qt$QLabel('heat map smoothing:')
        this$heatmapSmoothingRadioButton_none<-Qt$QRadioButton('none')
        qconnect(this$heatmapSmoothingRadioButton_none,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            if (.sharedEnv$params$heatmapSmoothing!='none') {
                .sharedEnv$params$heatmapSmoothing<-'none'
                heatmapSmoothingChanged(.sharedEnv)
            }
        })
        this$heatmapSmoothingRadioButton_cluster<-Qt$QRadioButton('cluster')
        qconnect(this$heatmapSmoothingRadioButton_cluster,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            if (.sharedEnv$params$heatmapSmoothing!='cluster') {
                .sharedEnv$params$heatmapSmoothing<-'cluster'
                heatmapSmoothingChanged(.sharedEnv)
            }
        })
        this$heatmapSmoothingRadioButton_zoom<-Qt$QRadioButton('zoom')
        qconnect(this$heatmapSmoothingRadioButton_zoom,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            if (.sharedEnv$params$heatmapSmoothing!='zoom') {
                .sharedEnv$params$heatmapSmoothing<-'zoom'
                heatmapSmoothingChanged(.sharedEnv)
            }
        })
        heatmapSmoothingRadioButtonGroup<-Qt$QButtonGroup()
        heatmapSmoothingRadioButtonGroup$addButton(this$heatmapSmoothingRadioButton_none)
        heatmapSmoothingRadioButtonGroup$addButton(this$heatmapSmoothingRadioButton_cluster)
        heatmapSmoothingRadioButtonGroup$addButton(this$heatmapSmoothingRadioButton_zoom)
        switch(attr(scene,'.sharedEnv')$params$heatmapSmoothing,
            'none'=this$heatmapSmoothingRadioButton_none$setChecked(TRUE),
            'cluster'=this$heatmapSmoothingRadioButton_cluster$setChecked(TRUE),
            'zoom'=this$heatmapSmoothingRadioButton_zoom$setChecked(TRUE)
        )

        this$quitButton<-Qt$QPushButton('&Quit')
        qconnect(this$quitButton,"pressed",function() {
            # close dendrogram
            view$close()
            # close GUI
            close()
        })
    })

    # Compute the width of a label constructed as "paste(txt1,<several digits>,txt2).
    # As we do not know which digit is the widest one, we try all of them and take the maximum width.
    computeMaxLabelWidth<-function(txt1,txt2) {
        maxWidth<-0
        for (i in 0:9) {
            maxWidthLabel<-Qt$QLabel(paste(txt1,paste(rep(i,ceiling(log10(df$n+1))),collapse=''),txt2,sep=''))
            maxWidth<-max(maxWidth,maxWidthLabel$sizeHint$width())
        }
        maxWidth
    }

    guiWindow<-Window()
    guiWindow$show()

    return(invisible(qx))
    ### a mutaframe 'qx' (or a mutaframe created from regular data
    ### frame 'x') enriched with dynamic metadata describing the
    ### current cluster selection in terms of the '.cluster' and
    ### '.inCurrentCluster' columns.
    ### For each observation, the '.cluster' holds the ID of the cluster
    ### that the observation is a member of (or 0, if the observation
    ### does not belong to any cluster). Similarly, the
    ### '.inCurrentCluster' metadata determines whether the given
    ### observation is a member of the current cluster.
    ### The returned mutaframe can be used for bidirectional
    ### live interaction with 'idendro'.
    ## 'idendro' alters the '.color' and '.border' metadata to color
    ### observations according to the color of the clusters they appear
    ### in, and listens to changes being made to the '.brushed'
    ### metadata to learn what observations are currently being brushed.
    ###
    ### Note that if the value returned is passed to a subsequent call
    ### to 'idendro', the cluster selection saved in the mutaframe will
    ### get restored. 
},ex=function() {
    data(iris, envir = environment())
    hx <- hclust(dist(iris[, 1:4]))
    idendro(hx, iris)

    # see demos for more examples
})

