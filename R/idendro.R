idendro<-structure(function# Interactive Dendrogram
###
### 'idendro' is a plot enabling visualization and interactive
### inspection of a dendrogram, with a heatmap attached to it,
### optionally.
### Clusters anywhere in the dendrogram hierarchy can be selected.
### The dendrogram can be zoomed and panned.
### 'idendro' can be integrated with other plots and tools
### by communicating over a shared mutable data frames (mutaframes).
### 'idendro' can be used to display quite large dendrograms (tens
### of thousands of observations, at least).
###
##details<<
## 'idendro' displays an interactive dendrogram enriched, optionally,
## with a heatmap and/or a brushedmap.
##
## The dendrogram depicts the result of a hierarchical cluster
## analysis on a set of observations. There is an axis drawn by the
## side of the dendrogram displaying the "height" of clusters. 
##
## The heatmap visualizes the observations living in k-dimensional
## space by mapping their values onto a color scale and displaying
## them as a row of 'k' colored rectangles.
##
## The brushedmap indicates which observations are currently
## being selected by some external plot/tool 'idendro' is integrated
## with. Technically speaking, the current selection is determined by
## a hidden column in the 'qx' mutable data frame being changed by
## the external plot/tool. 'idendro' listens to changes to this column.
##
## The dendrogram can be zoomed and panned. To zoom in to a
## specific region, right click and drag in the dendrogram.
## Mouse wheel can also be used to zoom in and out (the amount of zoom
## can be controlled by 'zoomFactor'). To pan a zoomed dendrogram,
## middle click and drag the mouse. Zooming and panning history is
## available (see 'GUI').
##
## User can select clusters manually one by one (by clicking
## at individual clusters in the dendrogram), or automatically by
## "cutting" the dendrogram at a specified height. To cut the
## dendrogram, navigate the mouse close to the dendrogram axis
## (a dashed line will appear across the dendrogram at a specified
## height), and left click. Clusters just beneath the cutting
## height will get selected, replacing the clusters being currently
## selected. Selection history is available (see 'GUI').
##
##   \emph{Graphical User interface (GUI):}
##
## Besides the dendrogram window, there is a window offering a few
## GUI controls. In the top part of the GUI window come
## cluster-specific controls and info panels arranged in rows.
## (The number of rows is determined by 'maxClusterCount')
## In each row, from left to right,
## there is the current cluster indicator, the cluster number and
## color code (determined by 'clusterColors'), and
## cluster-specific statistics: the total number (and ratio) of
## observations in the specific cluster out of the total number
## of observations in the data set, and the number (and ratio) of
## observations in the cluster out of the observations brushed
## externally. At any time, exactly one cluster is being the current
## cluster. Manual cluster selection (re)defines which cluster (as
## appearing in the dendrogram) is pointed to by the current cluster.
## The observations forming the current cluster are indicated by
## the '.inCurrentCluster' column in the 'qx' mutaframe, which can be
## used by external applications to display the current
## cluster-specific information (see 'idendroDemoWithUserCallback').
## At the bottom of the GUI window, there are buttons controling
## zooming and cluster selection:
##
## "Undo zoom" - retrieves the previous zoom region from history
##
## "Full view" - zooms dendrogram out maximally
##
## "Undo selection" - retrieves the previous cluster selection from
##     history
##
## "Unselect" - unselects the current cluster (makes the current
##     cluster to point to no cluster in the dendrogram, so it
##     decolorizes dendrogram branches associated with
##     the current cluster)
##
## "Unselect all" - unselects all clusters (decolorizes all the
##     clusters in the dendrogram)
##
##  "Quit"
##
##
(
    h, ##<< an object of class 'stats::hclust' describing a clustering.
    ## If _inversions_ in heights (see 'hclust') is detected,
    ## the heights get fixed in a simple naive way by preserving
    ## non-negative relative differences in the heights, but changing
    ## negative differences to zero. Using clustering with monotone
    ## distance measure should be considered.

    qx=NULL,##<< a mutaframe holding observations tha were clustered
    ## giving rise to 'h', with hidden columns for interaction, as created
    ## by 'cranvas::qdata'. If 'qx' is enriched with 'idendro'-specific
    ## hidden columns (i.e. '.cluster', '.inCurrentCluster'), the
    ## initial cluster selection is based on them; otherwise there are
    ## no clusters selected initially.
    ## A regular data frame can be passed instead of a mutaframe, in
    ## which case it will get converted into a mutaframe
    ## automatically.
    ## This parameter is optional.

    x=qx, ##<< a data frame holding observations tha were clustered
    ## giving rise to 'h'.
    ## Heatmap will depict this data.
    ## Non-numeric types will get converted to numeric using 'as.numeric'.
    ## This parameter is optional. If missing, it will be guessed
    ## from 'qx' by omitting any columns starting in '.'.

    zoomFactor=1/240, ##<<the amount of zoom in/out as controlled by the
    ## mouse wheel

    observationAnnotationEnabled=TRUE, ##<< shall the names of individual
    ## observations (rownames of 'x') be shown next to the
    ## dendrogram/heatmap?

    clusterColors=c('red','green','blue','yellow','magenta','cyan','darkred','darkgreen','purple','darkcyan'),##<< colors
    ## of individual clusters selected in the dendrogram

    unselectedClusterColor='black',##<< the color of unselected dendrogram
    ## branches

    maxClusterCount=length(clusterColors), ##<< maximum number of
    ## clusters user can select. If greater than the number of
    ## 'clusterColors', cluster colors will get recycled.
    ## This parameter affects the size of GUI and the number of
    ## clusters which can be selected automatically by "cutting" the
    ## dendrogram.

    heatmapEnabled=TRUE, ##<< shall the heatmap be drawn?

    doSmoothHeatmap=NULL,##<< (deprecated, use `heatmapSmoothing'
    ## instead)

    heatmapSmoothing=c('none','cluster','zoom'),##<< heatmap smoothing mode,
    ## one of
    ## 'none' - i.e. heatmap gets never smoothed,
    ## 'cluster' - heatmap depicts the mean data values
    ## for the currently selected clusters,
    ## 'zoom' - heatmap depicts the mean data values
    ## for the clusters currently shown in the dendrogram,
    ## disregarding the current cluster selection.

    heatmapColors=colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000")), ##<< heatmap
    ## color map scheme. It is represented by a function taking a
    ## single argument, the number of colors in heatmap (see the
    ## `heatmapColorCount' argument), and returning colors to be
    ## used in heatmap.  For example, to enable gray heatmap color
    ## scheme, use `heatmapColors = gray.colors'.

    heatmapColorCount=10, ##<< the number of colors used by the heatmap,
    ## picked (interpolated) from 'heatmapColors'.
    ## WARNING: the number of colors used by heatmap can influence
    ## the time spent drawing the heatmap significantly (for large
    ## data sets).

    doScaleHeatmap=TRUE, ##<< scale each heatmap column to the <0,1> range?
    ## (The default is TRUE.)

    heatmapRelSize=.2, ##<< relative size of the heatmap - the ratio
    ## of the heatmap width to the width of both the dendrogram and
    ## heatmap. The default is 20%.

    brushedmapEnabled=!is.null(qx), ##<< shall brushed map be drawn?

    separateGui=FALSE, ##<< shall GUI be integrated into the dendrogram
    ## window, or shall it be separated in a standalone window?
    ## (The default is integrated GUI.)

    graphicalClusterInfos=TRUE, ##<< depict cluster-specific statistics
    ## graphically? (The default is TRUE.)

    textualClusterInfos=TRUE, ##<< depict cluster-specific statistics
    ## textually? (The default is TRUE.)

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
# TODO:

##seealso<<hclust, plclust, identify.hclust, rect.hclust,
## cutree, dendrogram, cranvas::qdata

    #### required libraries
    ####
    require(scales) # alpha
    require(qtpaint)
    require(qtbase)
    require(cranvas) # qdata
    require(plumbr) # add_listener
    require(grDevices) # needed to generate jet palette by `colorRampPalette'

    #### debugs
    ####
    # general debug
    dbg<-0
    # debugs controlling individual features,
    # the hugher the value, the finer the debugs
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

    if (!is.null(doSmoothHeatmap)) {
        warning('argument `doSmoothHeatmap\' is deprecated (and ignored)')
    }

    if (heatmapEnabled && is.null(x) && is.null(qx)) {
        # can't draw heatmap if we have no data
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

    if (is.unsorted(h$height)) {
        warning('Non-monotone distance detected, applying a simple workaround. Consider using clustering with monotone distance.')
        tmp<-diff(h$height)
        h$height<-h$height+c(0,-cumsum(tmp*I(tmp<0)))
    }

    n<-length(h$height)+1
    if (!is.null(x) && nrow(x)!=n) {
        stop(paste('Clustering (of ',n,' objects) does not fit data (',nrow(x),' rows).',sep=''))
    }

    # convert non-numeric data to numeric, if necessary
    if (heatmapEnabled) {
        nonNumericColumnFound<-FALSE
        for (i in 1:ncol(x)) {
            if (!is.numeric(x[,i])) {
                x[,i]<-as.numeric(x[,i])
                nonNumericColumnFound<-TRUE
            }
        }
        if (nonNumericColumnFound) {
            warning('Non-numeric data found, converting to numeric (in order to enable heatmap drawing).')
        }
    }

    # scale heatmap
    if (heatmapEnabled && doScaleHeatmap) {
        for (i in 1:ncol(x)) {
            tmp<-x[,i]
            tmp.mn<-min(tmp,na.rm=TRUE)
            tmp.mx<-max(tmp,na.rm=TRUE)
            if (tmp.mn!=tmp.mx) {
                tmp<-(tmp-tmp.mn)/(max(tmp,na.rm=TRUE)-tmp.mn)
            } else {
                tmp<-tmp-tmp.mn+.5
            }
            x[,i]<-tmp
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


    df<-prepareDendro(h,x,dbg.dendro)
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

    # initialize heatmap cutting points, so they stay constant
    # regardless of smoothing/zoom
    if (heatmapEnabled) {
        # border points: [X0,X1), [X1,X2), ... [Xn-1, Xn]
        tmp<-as.matrix(df$xOrdered)
        df$heatmapBorderPoints<-seq(min(tmp,na.rm=TRUE),max(tmp,na.rm=TRUE),len=heatmapColorCount+1)
    }

    # observation annotations
    if (!is.null(x) && !is.null(rownames(x))) {
        df$observationLabels<-rownames(x)
    } else {
        df$observationLabels<-h$labels
    }
    if (observationAnnotationEnabled && is.null(df$observationLabels)) {
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
    df$lastSelectionSaver<-'none'

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

    # Callback invoked when heatmap smoothing mode gets changed (via
    # GUI buttons).
    heatmapSmoothingChanged<-function(.sharedEnv) {
        df<-.sharedEnv$df
        switch(.sharedEnv$params$heatmapSmoothing,
            'none'={
                # restore the original heatmap
                df$xOrderedSmoothed<-df$xOrdered
                df$elemClusterCount<-df$n
                },
            'cluster'={
                df<-smoothHeatmapAccordingToClusters(df,dbg.heatmap.smooth)
                df$elemClusterCount<-df$n
                },
            'zoom'={
                # restore the original heatmap
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
    ## brushedmap
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
            # dendrogram can collide with span brushedmap, so we need
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
    ## heatmap
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
                # smooth heatmap to carry info about the currently
                # elementary clusters in the zoomed dendro
                ch<-cutree(df$h,h=df$h$height[df$clusterCount]-dendroZoom$g[2])
                if (max(ch)!=df$elemClusterCount) {
                    if (dbg.heatmap) cat('smoothing heatmap\n')
                    df$xOrderedSmoothed<-smoothHeatmap(df$xOrdered,ch[df$leafOrder],dbg.heatmap.smooth)
                    df$elemClusterCount<-max(ch)
                }
            }
            # We need to ensure that cutting the current
            # (perhaps smoothed) heatmap results in the same intervals
            # as if the cut operated over the original heatmap.
            # We use a hack here: we add some data from the whole range
            # of the original heatmap ("border points") to the data
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
            colPalette<-params$heatmapColors(params$heatmapColorCount)
            if (dbg.heatmap>1) printVar(colPalette)
            clusterColors<-colPalette[colIdx]
            if (dbg.heatmap>1) printVar(clusterColors)
            # draw heatmap by colors, it is much faster compared to drawing in all colors in one single call
            #qdrawRect(painter,coords1[[1]],coords1[[2]],coords2[[1]],coords2[[2]],stroke=rgb(0,0,0,0),fill=clusterColors)
            for (c in colPalette) {
                i<-clusterColors==c
                qdrawRect(painter,coords1[[1]][i],coords1[[2]][i],coords2[[1]][i],coords2[[2]][i],stroke=rgb(0,0,0,0),fill=c)
            }
            # TODO: add NA colors here

            if (dbg.heatmap.limits) {
                drawLayerLimits(painter,layer,'heatmap','green')
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
            colPalette<-params$heatmapColors(params$heatmapColorCount)
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
                    xs<-apply(as.array(labels),1,function(x)layer$mapToScene(qstrWidth(painter,x),qstrHeight(painter,x))$x())
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
            # annotate the brushedmap
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
            if (dbg.heatmap>1) printVar(df$observationLabels)
            gLabelObs<-rep(0,df$n)
            wLabelObs<-seq(1,df$n)
            coordsLabelObs<-gw2xy(heatmap2fig(list(gLabelObs,wLabelObs)))
            # annotate observations
            qdrawText(painter,df$observationLabels[df$leafOrder],coordsLabelObs[[1]],coordsLabelObs[[2]],color='black',halign='left')

            if (!.sharedEnv$observationAnnotationLayerSized) {
                # resize observationAnnotationLayer such that dim annotations fit in nicely
                x0<-layer$mapToScene(0,0)$x()
                xs<-apply(as.array(df$observationLabels),1,function(x)layer$mapToScene(qstrWidth(painter,x),qstrHeight(painter,x))$x())
                layout$setColumnMinimumWidth(3,max(xs)-x0)
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
                pushDendroZoomHistory(.sharedEnv)
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

        clusterSelectorImpl<-function(layer, event) {
            if (dbg.dendro.select) cat('clusterSelectorImpl called\n')
            if (dbg.dendro.select) print(as.numeric(event$pos()))

            if (dbg.dendro.select) printVar(df$currentCluster)

            df<-selectCluster(event$pos())
        }
        .sharedEnv$df<-clusterSelectorImpl(layer, event)
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

            pushDendroZoomHistory(.sharedEnv)
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

        pushDendroZoomHistory(.sharedEnv)
        lastDendroZoomHistorySaver<<-'dendroZoomer'

        dendroZoomerImpl<-function(layer, event) {
            delta<-event$delta()
            if (dbg.dendro.zoom>1) printVar(delta)
            pos<-as.numeric(event$pos())
            if (dbg.dendro.zoom>1) printVar(pos)
            gw<-fig2dendro(xy2gw(list(pos[1],pos[2])))
            if (dbg.dendro.zoom>1) printVar(gw)
            if (dbg.dendro.zoom>1) printVar(dendroZoom)

            # construct the (absolute) zoomFactor based on the
            # user-supplied relative zoomFactor and the zooming amount
            # requested by the mouse wheel
            zoomFactor<-params$zoomFactor*abs(delta)
            # the zoomFactor must be less than 1 in order to prevent
            # range reversal
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

        # zoom brushedmap
        tmp<-brushedmapLayer$limits()
        tmp$setTop(xy[[2]][1])
        tmp$setBottom(xy[[2]][2])
        brushedmapLayer$setLimits(tmp)

        # zoom heatmap
        if (!is.null(heatmapLayer)) {
            # zoom heatmap
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

        impl<-function() {
            df<-.gfc(df)
            #printVar(df)
            cutDendro(df,cutG,.sharedEnv$dendroZoom)
        }
        rv<-impl()
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

    ## brushedmap#FOLD01
    ################
    #TODO: scale
    brushedmapLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=.5+df$n*c(0,1)))))
    if (dbg.brushedmap) printVar(brushedmapLimits)
    brushedmapLayer<-suppressWarnings(qlayer(scene,paintFun=brushedmapPainter,clip=FALSE,cache=TRUE,
        limits=qrect(brushedmapLimits[1],brushedmapLimits[3],brushedmapLimits[2],brushedmapLimits[4])))

    ## brushedmap annotation
    brushedmapAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=c(0,1)))))
    brushedmapAnnotationLayer<-suppressWarnings(qlayer(scene,
        paintFun=brushedmapAnnotationPainter,
        limits=qrect(brushedmapAnnotationLimits[1],brushedmapAnnotationLimits[3],
                brushedmapAnnotationLimits[2],brushedmapAnnotationLimits[4]),
        clip=FALSE,cache=TRUE))

    ## heatmap#FOLD01
    ################
    #TODO: scale
    heatmapLimits<-unlist(gw2xy(heatmap2fig(list(
        g=c(0,max(1,df$k)), # the positive difference in 'g' makes
        # layers size manageable easily
        w=.5+df$n*c(0,1)))))
    if (dbg.heatmap) printVar(heatmapLimits)
    heatmapLayer<-suppressWarnings(qlayer(scene,paintFun=heatmapPainter,clip=FALSE,cache=TRUE,
            limits=qrect(heatmapLimits[1],heatmapLimits[3],heatmapLimits[2],heatmapLimits[4])))

    heatmapLegendLimits<-unlist(gw2xy(heatmap2fig(list(
        g=c(0,max(1,df$k)), # the positive difference in 'g' makes
        # layers size manageable easily
        w=c(0,1)))))
    if (dbg.heatmap) printVar(heatmapLegendLimits)
    heatmapLegendLayer<-suppressWarnings(qlayer(scene,paintFun=heatmapLegendPainter,clip=FALSE,cache=TRUE,
            limits=qrect(heatmapLegendLimits[1],heatmapLegendLimits[3],heatmapLegendLimits[2],heatmapLegendLimits[4])))

    ## heatmap dim annotations
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
    # heatmap dimensions annotation
    if (heatmapEnabled) {
        layout$setRowPreferredHeight(0,75)
    } else {
        layout$setRowMaximumHeight(0,0)
    }
    # dendro+heatmap+brushedmap+observations annotation
    layout$setRowPreferredHeight(1,dendroPreferredHeight)
    # axis
    layout$setRowPreferredHeight(2,50)
    # heatmap dimensions annotation: fixed
    layout$setRowStretchFactor(0,0)
    # dendro+heatmap+brushedmap+observations annotation: stretchable
    layout$setRowStretchFactor(1,1)
    # axis: fixed
    layout$setRowStretchFactor(2,0)

    # dendro
    if (heatmapEnabled) {
        layout$setColumnPreferredWidth(0,dendroPreferredWidth)
    } else {
        layout$setColumnPreferredWidth(0,dendroPreferredWidth)
    }
    # heatmap
    if (heatmapEnabled) {
        layout$setColumnPreferredWidth(1,dendroPreferredWidth/(1-heatmapRelSize)*heatmapRelSize)
    } else {
        layout$setColumnMaximumWidth(1,0)
    }
    # brushedmap
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
        # heatmap stretchable
        layout$setColumnStretchFactor(1,10*heatmapRelSize)
    } else {
        layout$setColumnStretchFactor(1,0)
    }
    # brushedmap: fixed size
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
            clustersLayout$addWidget(brushedDesc,1,3,Qt$Qt$AlignJustify)
        }
        # populate clustersLayout with ...
        createButtons(clusterLabel$sizeHint$width())
        for (i in 1:params$maxClusterCount) {
            # ... currentCluster selectors,
            clustersLayout$addWidget(clusterSelectorButtons[[i]],i+1,1)
            # ... clusterInfos (out of total number of observations)
            clustersLayout$addWidget(clusterInfosTotal[[i]],i+1,2)
            if (brushedmapEnabled) {
                # ... clusterInfos (out of brushed observations)
                clustersLayout$addWidget(clusterInfosBrushed[[i]],i+1,3)
            }
        }
        # set the current cluster
        this$clusterSelectorButtons[[df$currentCluster]]$setChecked(TRUE)

        guiLayout$addLayout(clustersLayout,0)

        # GUI buttons
        zoomLayout<-Qt$QHBoxLayout()
        zoomLayout$addStretch(1)
        zoomLayout$addWidget(zoomBackButton)
        zoomLayout$addWidget(fullViewButton)
        zoomLayout$addStretch(1)
        guiLayout$addLayout(zoomLayout)

        selectBackLayout<-Qt$QHBoxLayout()
        selectBackLayout$addStretch(1)
        selectBackLayout$addWidget(selectBackButton)
        selectBackLayout$addStretch(1)
        guiLayout$addLayout(selectBackLayout,0)
        selectionLayout<-Qt$QHBoxLayout()
        selectionLayout$addStretch(1)
        selectionLayout$addWidget(unselectButton)
        selectionLayout$addWidget(unselectAllButton)
        selectionLayout$addStretch(1)
        guiLayout$addLayout(selectionLayout,0)

        heatmapSmoothingLabelLayout<-Qt$QHBoxLayout()
        heatmapSmoothingLabelLayout$addWidget(heatmapSmoothingLabel)
        heatmapSmoothingLayout<-Qt$QHBoxLayout()
        heatmapSmoothingLayout$addWidget(heatmapSmoothingRadioButton_none)
        heatmapSmoothingLayout$addWidget(heatmapSmoothingRadioButton_cluster)
        heatmapSmoothingLayout$addWidget(heatmapSmoothingRadioButton_zoom)
        guiLayout$addLayout(heatmapSmoothingLabelLayout,0)
        guiLayout$addLayout(heatmapSmoothingLayout,0)

        quitLayout<-Qt$QHBoxLayout()
        quitLayout$addStretch(1)
        quitLayout$addWidget(quitButton)
        quitLayout$addStretch(1)
        guiLayout$addLayout(quitLayout,0)

        windowLayout<-Qt$QHBoxLayout()
        windowLayout$addLayout(guiLayout,0)
        if (!separateGui) windowLayout$addWidget(view,1)
        setLayout(windowLayout)

        setWindowTitle("idendro")
        #resize(480, 320)
        if (dbg.gui) cat('Window created\n')

    },where=environment()))

    getClusterInfoTotal<-function(idx) {
        info<-'0 (0%)'
        ratio<-0
        if (dbg.dendro.select>1) printVar(df$clusters)
        if (!is.null(df$clusters)) {
            if (idx<=length(df$clusters) && !is.null(df$clusters[idx]) && length(df$clusters[[idx]]$indices)>0) {
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
                if (idx<=length(df$clusters) && !is.null(df$clusters[idx]) && length(df$clusters[[idx]]$indices)>0) {
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

    qsetMethod("createButtons", Window, function(clusterLabelWidth) {
        if (dbg.gui) cat('createButtons called\n')

        ## Radio button with a colored background.
        # Used to identify the current cluster.
        suppressWarnings(qsetClass("ColoredRadioButton",Qt$QRadioButton,function(parent=NULL,color=color,...) {
            super(parent,...)
            this$color<-color
        }))
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
        }))
        qsetMethod("setRelWidth",ColoredRectWithLabel,function(relWidth) {
            this$relWidth<-relWidth
        })
        qsetMethod("paintEvent",ColoredRectWithLabel,function(event) {
            painter<-Qt$QPainter(this)
            rect<-Qt$QRect(event$rect())
            if (graphicalClusterInfos) {
                # fill part of the rectangle, excluding border
                rect$setWidth(rect$width()*relWidth)
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
            qconnect(clusterSelectorButtons[[i]],"pressed",function(i) {
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
        qconnect(fullViewButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            # do not attempt to change zoom if zoomed fully already
            if (!(dendroZoom$g==dendroZoomMin$g && dendroZoom$w==dendroZoomMin$w)) {
                pushDendroZoomHistory(.sharedEnv)
                lastDendroZoomHistorySaver<<-'fullView'
                .sharedEnv$dendroZoom<-dendroZoomMin
                zoomDendroBrushedmapAndHeatmap(scene)
            }
        })

        this$zoomBackButton<-Qt$QPushButton('Undo &zoom')
        qconnect(zoomBackButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.zoom) cat('zoomBack button pressed\n')

            rv<-popDendroZoomHistory(.sharedEnv)
            if (!is.null(rv)) {
                .sharedEnv$dendroZoom<-rv
                if (dbg.dendro.zoom) printVar(.sharedEnv$dendroZoom)
                zoomDendroBrushedmapAndHeatmap(scene)
            }
        })

        this$unselectButton<-Qt$QPushButton('&Unselect')# current cluster')
        qconnect(unselectButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.select) cat('unselect button pressed\n')

            rv<-unselectCurrentCluster(.sharedEnv$df)
            .sharedEnv$df<-rv$df
            if (rv$selectionChanged) {
                .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
            }
        })

        this$unselectAllButton<-Qt$QPushButton('Unselect &all')# clusters')
        qconnect(unselectAllButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.select) cat('unselectAll button pressed\n')

            rv<-unselectAllClusters(.sharedEnv$df)
            .sharedEnv$df<-rv$df
            if (rv$selectionChanged) {
                .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
            }
        })

        this$selectBackButton<-Qt$QPushButton('Undo &selection')
        qconnect(selectBackButton,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in .sharedVarNames()) assign(vn,eval(parse(text=vn),envir=.sharedEnv))

            if (dbg.dendro.select) cat('selectBack button pressed\n')

            rv<-popSelectionHistory(.sharedEnv$df)
            if (!is.null(rv)) {
                .sharedEnv$df<-rv
                .sharedEnv$df<-updateClustersOnChange(.sharedEnv,qx,guiWindow)
            } else {
                # no history to recall
            }
        })

        this$heatmapSmoothingLabel<-Qt$QLabel('heatmap smoothing:')
        this$heatmapSmoothingRadioButton_none<-Qt$QRadioButton('none')
        qconnect(heatmapSmoothingRadioButton_none,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            if (.sharedEnv$params$heatmapSmoothing!='none') {
                .sharedEnv$params$heatmapSmoothing<-'none'
                heatmapSmoothingChanged(.sharedEnv)
            }
        })
        this$heatmapSmoothingRadioButton_cluster<-Qt$QRadioButton('cluster')
        qconnect(heatmapSmoothingRadioButton_cluster,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            if (.sharedEnv$params$heatmapSmoothing!='cluster') {
                .sharedEnv$params$heatmapSmoothing<-'cluster'
                heatmapSmoothingChanged(.sharedEnv)
            }
        })
        this$heatmapSmoothingRadioButton_zoom<-Qt$QRadioButton('zoom')
        qconnect(heatmapSmoothingRadioButton_zoom,"pressed",function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            if (.sharedEnv$params$heatmapSmoothing!='zoom') {
                .sharedEnv$params$heatmapSmoothing<-'zoom'
                heatmapSmoothingChanged(.sharedEnv)
            }
        })
        heatmapSmoothingRadioButtonGroup<-Qt$QButtonGroup()
        heatmapSmoothingRadioButtonGroup$addButton(heatmapSmoothingRadioButton_none)
        heatmapSmoothingRadioButtonGroup$addButton(heatmapSmoothingRadioButton_cluster)
        heatmapSmoothingRadioButtonGroup$addButton(heatmapSmoothingRadioButton_zoom)
        switch(attr(scene,'.sharedEnv')$params$heatmapSmoothing,
            'none'=heatmapSmoothingRadioButton_none$setChecked(TRUE),
            'cluster'=heatmapSmoothingRadioButton_cluster$setChecked(TRUE),
            'zoom'=heatmapSmoothingRadioButton_zoom$setChecked(TRUE)
        )

        this$quitButton<-Qt$QPushButton('&Quit')
        qconnect(quitButton,"pressed",function() {
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
    ### frame 'x') enriched with dynamic (interactive) clusters
    ### selection ('.cluster' column), and a flag determining which
    ### observations form the current cluster ('.inCurrentCluster'
    ###column). Observations not appearing in any cluster have 0 in the
    ### '.cluster' column, while observations forming the 'i'-th
    ### cluster have the value 'i' there.
    ### The '.inCurrentCluster' column holds logical flags determining
    ### whether given observation is a member of the current cluster.
    ### The returned mutaframe can be used for bidirectional
    ### interaction with 'idendro': external plot(s)/tool(s) can brush
    ### some observations ('idendro' will notice it and update the
    ### brushedmap), and also respond to any changes made to cluster
    ### assignment and/or the current cluster selection.
    ###
    ### Note that if the value returned is passed to a subsequent call
    ### to 'idendro', the cluster selection saved in the mutaframe will
    ### be restored. This feature can be regarded as a simple means of
    ### cluster selection persistency.
},ex=function() {
    # data to be clustered
    x<-data.frame(x1=1:10,x2=seq(10,1,-1))
    # clustered data (dendrogram)
    h<-hclust(dist(x))
    idendro(h,x)

    # for more examples please see demos
})

