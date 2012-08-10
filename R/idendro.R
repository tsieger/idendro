#
# return: qx
#
idendro<-function(h,
    qx=NULL,
    x=NULL,
    zoomFactor=1/240,
    observationAnnotationEnabled=NULL,
    clusterColors=c('red','green','blue','yellow','magenta','cyan','darkred','darkgreen','purple','darkcyan'),
    unselectedClusterColor='black',
    maxClusterCount=length(clusterColors),
    heatmapEnabled=NULL,
    doSmoothHeatmap=TRUE,
    heatmapColors=colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")),#c("red", "blue"),space = "Lab"),
    heatmapColorCount=10,
    brushedmapEnabled=!is.null(qx)
    ) {

    #### required libraries
    ####
    require(qtpaint)
    require(qtbase)
    require(cranvas) # qdata
    require(plumbr) # add_listener
    require(grDevices) # needed to generate jet palette by `colorRampPalette'
    require(Hmisc) # cut2

    #### debugs
    ####
    # general debug
    dbg<-0
    dbg.args<-01*dbg
    dbg.mouse<-01*dbg
    dbg.gui<-01*dbg
    dbg.tx<-0*dbg
    dbg.dendro<-1*dbg
    dbg.dendro.zoom<-1*dbg
    dbg.dendro.axis<-1*dbg
    dbg.dendro.limits<-0*dbg
    dbg.dendro.select<-1*dbg
    dbg.dendro.cut<-1*dbg
    dbg.dendro.info<-1*dbg
    dbg.axis<-1*dbg
    dbg.heatmap<-1*dbg
    dbg.heatmap.text<-0*dbg
    dbg.heatmap.limits<-0*dbg
    dbg.brushedmap<-1*dbg

    #### arguments handling
    ####
    if (dbg.args) cat('--- user supplied arguments: ---\n')
    if (dbg.args) printVar(!is.null(qx))
    if (dbg.args) printVar(!is.null(x))
    if (dbg.args) printVar(heatmapEnabled)
    if (dbg.args) printVar(brushedmapEnabled)
    if (dbg.args) printVar(observationAnnotationEnabled)

    # do draw heatmap?
    if (is.null(heatmapEnabled)) {
        # if not determined by user, draw heatmap if possible
        heatmapEnabled<-!is.null(x) || !is.null(qx)
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
    if (is.null(x) && !is.null(qx)) {
        # heuristics: try to get rid of columns added to the original data
        x<-as.data.frame(qx[,-grep('^\\.',colnames(qx))])
    }
    if (brushedmapEnabled && is.null(qx)) {
        qx<-qdata(1:(length(hx$height)+1))
    }
    n<-length(h$height)+1

    dendroZoom<-dendroZoomMin<-list(g=last(h$height)*c(-.05,1),w=.5+n*c(0,1))
    dendroZoomMouseSelection<-list(g=c(NA,NA),w=c(NA,NA))
    mouseLeftButtonPressed<-FALSE
    mouseRightButtonPressed<-FALSE
    mouseMiddleButtonPressed<-FALSE
    mouseMiddleButtonPressPos<-NULL
    axisCut<-NA

    df<-prepareDendro(h,x,dbg.dendro)
    if (!is.null(x) && !is.null(rownames(x))) {
        df$observationLabels<-rownames(x)
    } else {
        df$observationLabels<-hx$labels
    }
    if (is.null(observationAnnotationEnabled)) {
        # if user did not specify whether to draw observation annotations,
        # draw them if available
        observationAnnotationEnabled<-!is.null(df$observationLabels)
    } else if (observationAnnotationEnabled && is.null(df$observationLabels)) {
        # if annotation not available, nothing to draw
        observationAnnotationEnabled<-FALSE
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
    params$doSmoothHeatmap<-doSmoothHeatmap
    params$heatmapColors<-heatmapColors
    params$heatmapColorCount<-heatmapColorCount
    params$brushedmapEnabled<-brushedmapEnabled

    #### create environment to be shared by functions called from this
    # function as well as functions called from within qtpaint/qtbase
    .sharedEnv<-new.env()
    for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn)),env=.sharedEnv)

    #### internal functions
    ####

    # Color observations according to clusters they belong to.
    colorizeLeafs <- function(qx,df,params) {
        if (!is.null(qx)) {
            qx$.border<<-qx$.color<<-params$allColors[df$leafColorIdxs+1]
            qx$.cluster<<-df$leafColorIdxs
        }
    }
    colorizeLeafs(qx,df,params)

    # Mark observations belonging to the current cluster in the `qx' mutable data frame.
    setCurrentClusterInQx <- function(qx,df) {
        if (!is.null(qx)) {
            qx$.currentCluster<<-df$leafColorIdxs==df$currentCluster
        }
    }
    setCurrentClusterInQx(qx,df)

    # Determine color for cluster of given ID (starting at 1).
    clusterColor <- function(id) {
        params$clusterColors[((id-1)%%length(params$clusterColors))+1]
    }

    #extendRange <- function(x,amount) {
    #    tmp<-diff(x)*amount/2
    #    x<-x+tmp*c(-1,1)
    #    x
    #}

    # Painter clearing the whole layer with white color (considered
    # being backfround).
    # TODO: background color specification
    clearLayerBackground <- function(layer,painter,borderSaving=c(0,0,0,0)) {
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
    scene <- qscene()
    attr(scene,'.sharedEnv')<-environment() # the current environment
    attr(scene,'.df')<-df
    .sharedEnv$scene<-scene

    ##################################################################
    ## painters#FOLD02
    ##################################################################
    dendroPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro) cat('dendroPainter called\n')

        dendroPainterImpl <- function(layer, painter) {
            if (dbg.dendro) cat('dendroPainterImpl called\n')
            with(df$unselectedBranches$branches,qdrawSegment(painter,x1s,y1s,x2s,y2s,stroke=qcolor('black')))
            for (i in seq(along=df$clusters)) {
                if (!is.null(df$clusters[[i]]) && length(df$clusters[[i]]$branches)>0) {
                    if (dbg.dendro) cat(sprintf('cluster %i: color %s\n',i,clusterColor(i)))
                    with(df$clusters[[i]]$branches,qdrawSegment(painter,x1s,y1s,x2s,y2s,stroke=qcolor(clusterColor(i))))
                }
            }

            # zooming region being defined by mouse
            xy<-gw2xy(dendroZoomMouseSelection)
            qdrawRect(painter, xy$x[1], xy$y[1], xy$x[2], xy$y[2], stroke='yellow', fill=alpha('black',.3))

            # dendrogram cutting limit defined by axis position
            tmp<-dendroZoom
            tmp$w<-seq(dendroZoom$w[1],dendroZoom$w[2],len=50)
            tmp$g<-rep(.sharedEnv$axisCut,length(tmp$w))
            xy<-gw2xy(tmp)
            clrs<-rep(c(alpha('gray20',.7),alpha('yellow',.7)),len=length(xy$x)-1)
            qdrawRect(painter, xy$x[-length(xy$x)], xy$y[-length(xy$y)], xy$x[-1], xy$y[-1], stroke=clrs)

            if (dbg.dendro.limits) {
                qdrawText(painter,'0,0',0,0,col='blue')
                qdrawText(painter,'0,3',0,3,col='blue')
                qdrawText(painter,'.5,1',.5,1,col='blue')
                qdrawText(painter,'0,4',0,4,col='blue')

                eps<-.1
                qdrawRect(
                    painter,layer$limits()$left()-eps,
                    layer$limits()$bottom()+eps,
                    layer$limits()$right()+eps,
                    layer$limits()$top()-eps,stroke='blue')

                print('dendro:')
                #printVar(layer$limits())
                printVar(layer$limits()$left())
                printVar(layer$limits()$bottom())
                printVar(layer$limits()$right())
                printVar(layer$limits()$top())
            }
        }
        dendroPainterImpl(layer,painter)
    }

    ##
    ## brushedmap
    ##
    brushedmapPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.brushedmap) cat('brushedmapPainter called\n')

        if (dbg.brushedmap) printVar(brushedmapEnabled)
        if (!brushedmapEnabled) return()

        brushedmapPainterImpl <- function(layer, painter) {
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
            # to restore the background color by overdrawinf the
            # dendrogram)
            i<-!qx$.brushed[df$leafOrder]
            qdrawRect(painter,
                coords1[[1]][i],coords1[[2]][i],coords2[[1]][i],coords2[[2]][i],
                stroke=rgb(0,0,0,0),fill=qcolor('white'))
        }
        brushedmapPainterImpl(layer,painter)
    }

    ##
    ## heatmap
    ##
    heatmapPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.heatmap) cat('heatmapPainter called\n')

        if (dbg.heatmap) printVar(heatmapEnabled)
        if (!heatmapEnabled) return()

        heatmapPainterImpl <- function(layer, painter) {
            if (dbg.heatmap) cat('heatmapPainterImpl called\n')

            g1<-rep(seq(0,ncol(df$x)-1),each=nrow(df$x))
            g2<-g1+1
            w1<-rep(1:nrow(df$x),ncol(df$x))-.5
            w2<-w1+1
            if (dbg.heatmap>1) printVar(g1)
            if (dbg.heatmap>1) printVar(g2)
            if (dbg.heatmap>1) printVar(w1)
            if (dbg.heatmap>1) printVar(w2)
            coords1<-gw2xy(heatmap2fig(list(g1,w1)))
            coords2<-gw2xy(heatmap2fig(list(g2,w2)))
            if (params$doSmoothHeatmap) {
                # not all observations visible in the dendrogram,
                # smooth heatmap to carry info about the currently
                # elementary clusters in the zoomed dendro
                ch<-cutree(df$h,h=df$h$height[df$clusterCount]-dendroZoom$g[2])
                if (max(ch)!=df$elemClusterCount) {
                    if (dbg.heatmap) cat('smoothing heatmap\n')
                    df$xOrderedSmoothed<-smoothHeatmap(df$xOrdered,ch[df$leafOrder],dbg.heatmap)
                    df$elemClusterCount<-max(ch)
                }
            }
            colIdx<-as.numeric(cut2(as.matrix(df$xOrderedSmoothed),g=params$heatmapColorCount))
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

            if (dbg.heatmap.limits) {
                qdrawText(painter,'0,0',0,0,col='black')
                qdrawText(painter,'0,1',0,1,col='black')
                qdrawText(painter,'.5,1',.5,1,col='black')
                qdrawText(painter,'0,2',0,2,col='black')

                eps<-.1
                qdrawRect(painter,
                    layer$limits()$left()+eps,
                    layer$limits()$bottom()+eps,
                    layer$limits()$right()-eps,
                    layer$limits()$top()-eps,stroke='blue',fill=NA)

                cat('heatmap:\n')
                #printVar(layer$limits())
                printVar(layer$limits()$left())
                printVar(layer$limits()$bottom())
                printVar(layer$limits()$right())
                printVar(layer$limits()$top())
            }
            df
        }
        df<-heatmapPainterImpl(layer,painter)
        .sharedEnv$df<-df
    }

    heatmapDimAnnotationPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.heatmap) cat('heatmapDimAnnotationPainter called\n')

        if (!heatmapEnabled) return()

        heatmapDimAnnotationPainterImpl <- function(layer, painter) {
            # clear background
            clearLayerBackground(layer, painter)

            gLabelDim<-seq(0,df$k-1)+.5
            wLabelDim<-rep(0,df$k)
            coordsLabelDim<-gw2xy(heatmap2fig(list(gLabelDim,wLabelDim)))

            if (dbg.heatmap.text) {
                printVar(colnames(x))
                printVar(coordsLabelDim[[1]])
                printVar(coordsLabelDim[[2]])
            }
            qdrawText(painter,colnames(df$x),coordsLabelDim[[1]],coordsLabelDim[[2]],color='black',halign='left',rot=90)
        }
        heatmapDimAnnotationPainterImpl(layer,painter)
    }

    brushedmapAnnotationPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.brushedmap) cat('brushedmapAnnotationPainter called\n')

        if (!brushedmapEnabled) return()

        brushedmapAnnotationPainterImpl <- function(layer, painter) {
            # clear background
            clearLayerBackground(layer, painter)

            gLabelDim<-.5
            wLabelDim<-0
            coordsLabelDim<-gw2xy(heatmap2fig(list(gLabelDim,wLabelDim)))
            qdrawText(painter,'brushed',coordsLabelDim[[1]],coordsLabelDim[[2]],color='black',halign='left',rot=90)
        }
        brushedmapAnnotationPainterImpl(layer,painter)
    }

    observationAnnotationPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.heatmap) cat('observationAnnotationPainter called\n')

        if (!observationAnnotationEnabled) return()

        observationAnnotationPainterImpl <- function(layer, painter) {
            # clear background
            clearLayerBackground(layer, painter)

            gLabelObs<-rep(0,df$n)
            wLabelObs<-seq(1,df$n)
            coordsLabelObs<-gw2xy(heatmap2fig(list(gLabelObs,wLabelObs)))
            # annotate observations
            qdrawText(painter,df$observationLabels[df$leafOrder],coordsLabelObs[[1]],coordsLabelObs[[2]],color='black',halign='left')
        }
        observationAnnotationPainterImpl(layer,painter)
    }

    axisPainter <- function(layer, painter) {
        if (dbg.dendro.axis) cat('axisPainter called\n')

        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        df<-.sharedEnv$df

        clearLayerBackground(layer, painter)

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
    mousePressFun <- function(layer, event) {
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

    mouseMoveFun <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

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

    mouseReleaseFun <- function(layer, event) {
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
    clusterSelector <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.select) cat('clusterSelector called\n')

        clusterSelectorImpl <- function(layer, event) {
            if (dbg.dendro.select) cat('clusterSelectorImpl called\n')
            if (dbg.dendro.select) print(as.numeric(event$pos()))

            if (dbg.dendro.select) printVar(df$currentCluster)

            df<-selectCluster(event$pos())
        }
        df<-clusterSelectorImpl(layer, event)
        .sharedEnv$df<-df
        qupdate(.sharedEnv$dendroLayer)
        guiWindow$updateClusterInfos()
        guiWindow$update()
        colorizeLeafs(qx,df,params)
        setCurrentClusterInQx(qx,df)
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

    dendroZoomSelectionStarter <- function(layer, event) {
        if (dbg.dendro.select) cat('dendroZoomSelectionStarter called\n')
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        .sharedEnv$dendroZoomMouseSelection<-xy2gw(list(x=rep(event$pos()$x(),2),y=rep(event$pos()$y(),2)))
        qupdate(.sharedEnv$dendroLayer)
    }

    dendroZoomSelectionUpdater <- function(layer, event) {
        if (dbg.dendro.select) cat('dendroZoomSelectionUpdater called\n')
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        gw<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))
        .sharedEnv$dendroZoomMouseSelection$g[2]<-gw$g
        .sharedEnv$dendroZoomMouseSelection$w[2]<-gw$w
        qupdate(.sharedEnv$dendroLayer)
    }

    dendroZoomSelectionFinisher <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

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
    dendroZoomer <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.zoom) cat('dendroZoomer called\n')

        pushDendroZoomHistory(.sharedEnv)
        lastDendroZoomHistorySaver<<-'dendroZoomer'

        dendroZoomerImpl <- function(layer, event) {
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
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

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
        redrawRequested<-FALSE
        heatmapLayer<-.sharedEnv$heatmapLayer
        if (!is.null(heatmapLayer)) {
            # zoom heatmap
            if (params$doSmoothHeatmap) {
                # not all observations visible in the dendrogram,
                # smooth heatmap to carry info about the currently
                # elementary clusters in the zoomed dendro
                ch<-cutree(df$h,h=dendroZoom$g[2])
                if (max(ch)!=df$elemClusterCount) {
                    # the number of elementary clusters changed
                    if (dbg.heatmap) cat('requesting heatmap redraw, smoothing needed\n')
                    redrawRequested<-TRUE
                }
            }
            tmp<-heatmapLayer$limits()
            tmp$setTop(xy[[2]][1])
            tmp$setBottom(xy[[2]][2])
            heatmapLayer$setLimits(tmp)

            observationAnnotationLayer<-.sharedEnv$observationAnnotationLayer
            if (!is.null(observationAnnotationLayer)) {
                # zoom observations
                tmp<-observationAnnotationLayer$limits()
                tmp$setTop(xy[[2]][1])
                tmp$setBottom(xy[[2]][2])
                observationAnnotationLayer$setLimits(tmp)
            }
        }

        if (redrawRequested) {
            qupdate(scene)
        }
    }

    axisCutterUpdate<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutterUpdate called\n')
        .sharedEnv$axisCut<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))$g

        qupdate(.sharedEnv$dendroLayer)
    }

    axisCutterLeave<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutterLeave called\n')
        .sharedEnv$axisCut<-NA

        qupdate(.sharedEnv$dendroLayer)
    }

    axisCutter<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutter called\n')
        cutG<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))$g

        impl<-function() {
            df<-gfc(df)
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
            .sharedEnv$df<-df<-rv$df
             qupdate(.sharedEnv$dendroLayer)
            #if (rv$clustersChanged) {
            # colorize leafs
            colorizeLeafs(qx,df,params)
            setCurrentClusterInQx(qx,df)
            #}
            guiWindow$updateClusterInfos()
            guiWindow$update()
        }
    }

    #keyPressFun<-function(layer, event) {
    #    if (dbg) print(event)
    #    print(event$key())
    #    print(event$text())
    #}

    #titlePainter <- function(layer, painter) {
    #    # clear background
    #    clearLayerBackground(layer, painter)
    #
    #    qdrawText(painter, 'title',0.5,0.5,color=qcolor('black'))
    #}

    backgroundClearingPainter <- function(layer, painter) {
        # clear background
        clearLayerBackground(layer, painter)
        #qdrawText(painter, 'empty',0,0,color=qcolor('red'))
    }

    backgroundClearingPainterRigthToAxis <- function(layer, painter) {
        # clear background
        clearLayerBackground(layer, painter, c(0.05,0,0,0))
        #qdrawText(painter, 'empty',0,0,color=qcolor('red'))
    }

    ## dendrogram#FOLD01
    ################
    dendroLimits<-gw2xy(dendro2fig(dendroZoomMin))
    #dendroLimits$x<-extendRange(dendroLimits$x,.05)
    dendroLimits<-unlist(dendroLimits)
    if (dbg.dendro) printVar(dendroLimits)
    dendroLayer <- qlayer(scene, paintFun = dendroPainter,
            mousePressFun = mousePressFun,
            mouseMoveFun = mouseMoveFun,
            mouseReleaseFun = mouseReleaseFun,
            wheelFun = dendroZoomer, clip = F, cache = F,#keyPressFun=keyPressFun,
            limits = qrect(dendroLimits[1],dendroLimits[3],dendroLimits[2],dendroLimits[4]))
            #qrect(0,0,dendroG,1),#qrect(dendroLimits[[1]][1],dendroLimits[[2]][1],dendroLimits[[1]][2],dendroLimits[[2]][2]),#,
    .sharedEnv$dendroLayer<-dendroLayer

    axisLimits<-dendroZoomMin
    axisLimits$w<-c(0,1)
    axisLimits<-gw2xy(dendro2fig(axisLimits))
    #axisLimits$x<-extendRange(axisLimits$x,.05)
    axisLimits<-unlist(axisLimits)
    if (dbg.dendro.axis) printVar(axisLimits)

    axisLayer <- qlayer(scene, paintFun = axisPainter,
            hoverMoveFun = axisCutterUpdate,
            hoverEnterFun = axisCutterUpdate,
            hoverLeaveFun = axisCutterLeave,
            mousePressFun = axisCutter,
            limits = qrect(axisLimits[1],axisLimits[3],axisLimits[2],axisLimits[4]),
            clip = F)

    ## brushedmap#FOLD01
    ################
    #TODO: scale
    brushedmapLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=.5+df$n*c(0,1)))))
    if (dbg.brushedmap) printVar(brushedmapLimits)
    brushedmapLayer <- qlayer(scene, paintFun = brushedmapPainter,clip = F,
        limits = qrect(brushedmapLimits[1],brushedmapLimits[3],brushedmapLimits[2],brushedmapLimits[4]))
    .sharedEnv$brushedmapLayer<-brushedmapLayer

    ## brushedmap annotation
    brushedmapAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=c(0,1)))))
    brushedmapAnnotationLayer <- qlayer(scene, paintFun = brushedmapAnnotationPainter,
        limits=qrect(brushedmapAnnotationLimits[1],brushedmapAnnotationLimits[3],
                brushedmapAnnotationLimits[2],brushedmapAnnotationLimits[4]))

    ## heatmap#FOLD01
    ################
    #TODO: scale
    heatmapLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,df$k),w=.5+df$n*c(0,1)))))
    #heatmapLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,df$k),w=.5+df$n*c(-.025,1.025)))))
    if (dbg.heatmap) printVar(heatmapLimits)
    heatmapLayer <- qlayer(scene,paintFun=heatmapPainter,clip=F,cache=TRUE,
            limits = qrect(heatmapLimits[1],heatmapLimits[3],heatmapLimits[2],heatmapLimits[4]))
            #qrect(dendroG,0,1,1)))#qrect(heatmapLimits[[1]][1],heatmapLimits[[2]][1],heatmapLimits[[1]][2],heatmapLimits[[2]][2])))#
    .sharedEnv$heatmapLayer<-heatmapLayer

    ## heatmap dim annotations
    if (!is.null(x) && !is.null(colnames(x))) {
        heatmapDimAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,df$k),w=c(0,1)))))
        heatmapDimAnnotationLayer <- qlayer(scene, paintFun = heatmapDimAnnotationPainter,
            limits=qrect(heatmapDimAnnotationLimits[1],heatmapDimAnnotationLimits[3],
                heatmapDimAnnotationLimits[2],heatmapDimAnnotationLimits[4]))
    } else {
        heatmapDimAnnotationLayer <- NULL
    }

    ## observations annotations
    if (observationAnnotationEnabled) {
        observationAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=c(0,df$n)+.5))))
        observationAnnotationLayer <- qlayer(scene, paintFun = observationAnnotationPainter,
            limits=qrect(observationAnnotationLimits[1],observationAnnotationLimits[3],
                observationAnnotationLimits[2],observationAnnotationLimits[4]))
    } else {
        observationAnnotationLayer <- NULL
    }
    .sharedEnv$observationAnnotationLayer<-observationAnnotationLayer

    #titleLayer <- qlayer(scene, paintFun = titlePainter, limits=qrect(0,0,1,1))

    backgroundClearingLayer1 <- qlayer(scene, paintFun = backgroundClearingPainter, limits=qrect(0,0,1,1))
    backgroundClearingLayer2 <- qlayer(scene, paintFun = backgroundClearingPainter, limits=qrect(0,0,1,1))
    backgroundClearingLayer3 <- qlayer(scene, paintFun = backgroundClearingPainter, limits=qrect(0,0,1,1))
    backgroundClearingLayer4 <- qlayer(scene, paintFun = backgroundClearingPainter, limits=qrect(0,0,1,1))
    backgroundClearingLayer5 <- qlayer(scene, paintFun = backgroundClearingPainter, limits=qrect(0,0,1,1))

    #############
    ## layout#FOLD01
    #############
    figLayer <- qlayer(scene)
    figLayer[1, 0] <- dendroLayer
    figLayer[1, 1] <- heatmapLayer
    figLayer[1, 2] <- brushedmapLayer
    figLayer[1, 3] <- observationAnnotationLayer
    figLayer[2, 0] <- axisLayer
    figLayer[2, 1] <- backgroundClearingLayer1
    figLayer[2, 2] <- backgroundClearingLayer2
    figLayer[2, 3] <- backgroundClearingLayer3
    figLayer[0, 0] <- backgroundClearingLayer4
    figLayer[0, 1] <- heatmapDimAnnotationLayer
    figLayer[0, 2] <- brushedmapAnnotationLayer
    figLayer[0, 3] <- backgroundClearingLayer5
    
    layout <- figLayer$gridLayout()
    # heatmap dimensions annotation
    if (heatmapEnabled) {
        layout$setRowPreferredHeight(0, 75)
    } else {
        layout$setRowMaximumHeight(0, 0)
    }
    # dendro+heatmap+brushedmap+observations annotation
    layout$setRowPreferredHeight(1, 200)
    # axis
    layout$setRowPreferredHeight(2, 50)
    # heatmap dimensions annotation: fixed
    layout$setRowStretchFactor(0, 0)
    # dendro+heatmap+brushedmap+observations annotation: stretchable
    layout$setRowStretchFactor(1, 1)
    # axis: fixed
    layout$setRowStretchFactor(2, 0)

    # dendro
    layout$setColumnPreferredWidth(0, 400)
    # heatmap
    if (heatmapEnabled) {
        layout$setColumnPreferredWidth(1, 100)
    } else {
        layout$setColumnMaximumWidth(1, 0)
    }
    # brushedmap
    if (brushedmapEnabled) {
        layout$setColumnPreferredWidth(2, 25)
    } else {
        layout$setColumnMaximumWidth(2, 0)
    }
    # observations annotation
    layout$setColumnPreferredWidth(3, 50)

    # dendro stretchable
    layout$setColumnStretchFactor(0, 4)
    # heatmap stretchable
    layout$setColumnStretchFactor(1, 1)
    # brushedmap: fixed size
    layout$setColumnStretchFactor(2, 0)#1/df$k)
    # observations annotations: fixed size
    layout$setColumnStretchFactor(3, 0)

    #print(layout$addItem)
    #layout$addWidget(createFirstExclusiveGroup(), 0, 0)
    #layout$addWidget(Qt$QRadioButton("&Radio button 1"),0,0)
    #layout$addItem(Qt$QLabel("&Radio button 1"),0,0)

    ## listeners on the data (which column updates which layer(s))
    if (!is.null(qx)) {
        qx.listener <- add_listener(qx, function(i, j) {
            idx = which(j == c('.brushed'))
            if (length(idx) > 0) {
                if (dbg.brushedmap) cat('qx listener: brushed status changed\n')
                qupdate(.sharedEnv$brushedmapLayer)
                guiWindow$updateClusterInfos()
            }
        })
        qconnect(.sharedEnv$dendroLayer, 'destroyed', function(x) {
            remove_listener(qx, qx.listener)
        })
    }

    view <- qplotView(scene = scene)
    print(view)

    #######################################################
    #######################################################
    ## GUI#FOLD01
    if (dbg.gui) cat('initializing GUI\n')
    qsetClass("Window", Qt$QWidget, function(parent = NULL) {
        super(parent)
  
        createButtons()
        this$mainLayout<-Qt$QVBoxLayout()
        ## NOTE: the layout does not take ownership of the widgets, so we
        ## need to assign the layout to our widget up-front. Our widget then
        ## takes ownership of the widgets in the layout.
        setLayout(mainLayout)

        descLayout<-Qt$QHBoxLayout()
        descLayout$addWidget(Qt$QLabel('cluster'),0)
        descLayout$addWidget(Qt$QLabel(''),1)
        descLayout$addStretch(1)
        descLayout$addWidget(Qt$QLabel(paste('of',df$n,'total')),3)
        this$brushedDesc<-Qt$QLabel('of 0 brushed')
        descLayout$addWidget(brushedDesc,3)
        mainLayout$addLayout(descLayout)

        for (i in 1:params$maxClusterCount) {
            layout<-Qt$QHBoxLayout()
            layout$addWidget(clusterSelectorButtons[[i]],1)#,Qt$Qt$AlignLeft)
            layout$addWidget(clusterColoredRect[[i]],1)
            layout$addStretch(1)
            layout$addWidget(clusterInfosTotal[[i]],3)
            layout$addWidget(clusterInfosBrushed[[i]],3)
            #layout$addWidget(Qt$QPicture())
            mainLayout$addLayout(layout)
        }
        # initially, the first cluster is the current one
        this$clusterSelectorButtons[[1]]$setChecked(TRUE)

        zoomLayout<-Qt$QHBoxLayout()
        zoomLayout$addWidget(fullViewButton)
        zoomLayout$addWidget(zoomBackButton)
        mainLayout$addLayout(zoomLayout)

        selectionLayout<-Qt$QHBoxLayout()
        selectionLayout$addWidget(unselectButton)
        selectionLayout$addWidget(unselectAllButton)
        selectionLayout$addWidget(selectBackButton)
        mainLayout$addLayout(selectionLayout)

        mainLayout$addStretch(1)
        quitLayout<-Qt$QHBoxLayout()
        quitLayout$addStretch(1)
        quitLayout$addWidget(quitButton,0)
        quitLayout$addStretch(1)
        mainLayout$addLayout(quitLayout)

        setWindowTitle("idendro")
        resize(480, 320)
        if (dbg.gui) cat('Window created\n')

    },where=environment())

    getClusterInfoTotal <- function(idx) {
        info<-'0 (0%)'
        if (dbg.dendro.select>1) printVar(df$clusters)
        if (!is.null(df$clusters)) {
            if (idx<=length(df$clusters) && !is.null(df$clusters[idx]) && length(df$clusters[[idx]]$indices)>0) {
                clusterSize<-length(df$clusters[[idx]]$indices)+1
                info<-paste(
                    clusterSize,' (',round(100*clusterSize/df$n),'%)',
                    sep='')
            }
        }
        return(info)
    }
    getClusterInfoBrushed <- function(idx) {
        info<-'0 (0%)'
        if (brushedmapEnabled) {
            if (dbg.dendro.select>1) printVar(df$clusters)
            if (!is.null(df$clusters)) {
                if (idx<=length(df$clusters) && !is.null(df$clusters[idx]) && length(df$clusters[[idx]]$indices)>0) {
                    clusterBrushedSize<-sum(df$leafColorIdxs==idx & qx$.brushed)
                    brushedSize<-sum(qx$.brushed)
                    info<-paste(
                        clusterBrushedSize,' (',round(100*clusterBrushedSize/brushedSize),'%)',
                        sep='')
                }
            }
        }
        return(info)
    }

    qsetMethod("updateClusterInfos", Window, function() {
        if (dbg.dendro.info) cat('updateClusterInfos called\n')
        .sharedEnv<-attr(scene,'.sharedEnv')
        df<-.sharedEnv$df
        if (brushedmapEnabled) {
            this$brushedDesc$setText(paste('out of',sum(qx$.brushed),'brushed'))
        }
        for (i in 1:params$maxClusterCount) {
                this$clusterInfosTotal[[i]]$setText(getClusterInfoTotal(i))
                this$clusterInfosBrushed[[i]]$setText(getClusterInfoBrushed(i))
        }
        update()
    })

    qsetMethod("createButtons", Window, function() {
        if (dbg.gui) cat('createButtons called\n')

        ## colored rectangle identifying clusters
        ##
        qsetClass("ColoredRect", Qt$QWidget, function(parent = NULL) {
            super(parent)
        })
        qsetMethod("setColor", ColoredRect, function(color) {
            this$color<-color
        })
        qsetMethod("paintEvent", ColoredRect, function(event) {
            painter <- Qt$QPainter(this)
            redrawRect <- event$rect()  
            painter$fillRect(redrawRect, qbrush(this$color)) # clear area
            #painter$drawText(10,10,'asd')
            painter$end()
        })

        this$clusterSelectorButtons<-vector('list',params$maxClusterCount)
        this$clusterInfosTotal<-vector('list',params$maxClusterCount)
        this$clusterInfosBrushed<-vector('list',params$maxClusterCount)
        for (i in 1:params$maxClusterCount) {
            this$clusterSelectorButtons[[i]]<-Qt$QRadioButton(sprintf('%2d',i))
            qconnect(clusterSelectorButtons[[i]], "pressed", function(i) {
                if (attr(scene,'.sharedEnv')$df$currentCluster!=i) {
                    attr(scene,'.sharedEnv')$df$currentCluster<-i
                    setCurrentClusterInQx(qx,attr(scene,'.sharedEnv')$df)
                }
            },i)
            # TODO: create label with given background color instead of widget with explicit paintEvent handler
            this$clusterColoredRect[[i]]<-ColoredRect()
            this$clusterColoredRect[[i]]$setColor(params$clusterColors[i])
            this$clusterInfosTotal[[i]]<-Qt$QLabel('0 (0%)')
            this$clusterInfosBrushed[[i]]<-Qt$QLabel('0 (0%)')
        }

        this$fullViewButton<-Qt$QPushButton('Full view')
        qconnect(fullViewButton, "pressed", function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

            # do not attempt to change zoom if zoomed fully already
            if (!(dendroZoom$g==dendroZoomMin$g && dendroZoom$w==dendroZoomMin$w)) {
                pushDendroZoomHistory(.sharedEnv)
                lastDendroZoomHistorySaver<<-'fullView'
                .sharedEnv$dendroZoom<-dendroZoomMin
                zoomDendroBrushedmapAndHeatmap(scene)
            }
        })

        this$zoomBackButton<-Qt$QPushButton('Zoom &back')
        qconnect(zoomBackButton, "pressed", function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

            if (dbg.dendro.zoom) cat('zoomBack button pressed\n')

            rv<-popDendroZoomHistory(.sharedEnv)
            if (!is.null(rv)) {
                .sharedEnv$dendroZoom<-rv
                if (dbg.dendro.zoom) printVar(.sharedEnv$dendroZoom)
                zoomDendroBrushedmapAndHeatmap(scene)
            }
        })

        this$unselectButton<-Qt$QPushButton('&Unselect')# current cluster')
        qconnect(unselectButton, "pressed", function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

            if (dbg.dendro.select) cat('unselect button pressed\n')

            rv<-unselectCurrentCluster(.sharedEnv$df)
            .sharedEnv$df<-rv$df
            if (rv$selectionChanged) {
                qupdate(.sharedEnv$dendroLayer)
                guiWindow$updateClusterInfos()
                guiWindow$update()
            }
        })

        this$unselectAllButton<-Qt$QPushButton('Unselect all')# clusters')
        qconnect(unselectAllButton, "pressed", function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

            if (dbg.dendro.select) cat('unselectAll button pressed\n')

            rv<-unselectAllClusters(.sharedEnv$df)
            .sharedEnv$df<-rv$df
            if (rv$selectionChanged) {
                qupdate(.sharedEnv$dendroLayer)
                guiWindow$updateClusterInfos()
                guiWindow$update()
            }
        })

        this$selectBackButton<-Qt$QPushButton('Select &back')
        qconnect(selectBackButton, "pressed", function() {
            .sharedEnv<-attr(scene,'.sharedEnv')
            for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

            if (dbg.dendro.select) cat('selectBack button pressed\n')

            rv<-popSelectionHistory(.sharedEnv$df)
            if (!is.null(rv)) {
                .sharedEnv$df<-rv
                qupdate(.sharedEnv$dendroLayer)
                guiWindow$updateClusterInfos()
                guiWindow$update()
            } else {
                # no history to recall
            }
        })

        this$quitButton<-Qt$QPushButton('&Quit')
        qconnect(quitButton, "pressed", function() {
            # close dendrogram
            view$close()
            # close GUI
            close()
        })
    })

    guiWindow<-Window()
    guiWindow$show()

    qx
}
