
idendro<-function(h,
    x=NULL,
    cols=c('red','green','blue','yellow','magenta','cyan'),
    maxClusterCount=length(cols),
    doSmoothHeatmap=TRUE,
    zoomFactor=1/240
    ) {

    require(qtpaint)
    require(qtbase)
    require(cranvas)
    require(grDevices) # needed to generate jet palette by `colorRampPalette'
    require(Hmisc) # cut2

    dbg<-01
    dbg.tx<-dbg*0
    dbg.dendro<-1
    dbg.dendro.zoom<-2
    dbg.dendro.axis<-1
    dbg.dendro.limits<-0
    debug.dendro.cut<-1
    dbg.heatmap<-1
    dbg.heatmap.text<-0
    dbg.heatmap.limits<-0
    dbg.clusterSelector<-1

    charmW<-.1
    strangeW<-.1

    dendroG<-.5
    selectorG<-0#.05#min(c(.05,heatmapG/ncol(x)))
    heatmapLabelG<-.1
    heatmapG<-1-dendroG-selectorG-heatmapLabelG

    n<-length(h$height)+1

    dendroZoom<-dendroZoomMin<-list(g=c(last(h$height)*1.05,0),w=c(n+.5,1-.5))
    #dendroZoom<-c(0,0,dendroG,1)
    dendroZoomMouseSelection<-list(g=c(NA,NA),w=c(NA,NA))
    mouseLeftButtonPressed<-FALSE
    mouseRightButtonPressed<-FALSE
    mouseMiddleButtonPressed<-FALSE
    mouseMiddleButtonPressPos<-NULL
    axisCut<-NA

    df<-prepareDendro(h,x,dbg.dendro)

    params<-NULL
    params$cols<-cols
    params$doSmoothHeatmap<-doSmoothHeatmap
    params$zoomFactor<-zoomFactor
    params$maxClusterCount<-maxClusterCount

    .sharedEnv<-new.env()
    for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn)),env=.sharedEnv)

    # determine color for cluster of given ID (starting at 1)
    clusterColor <- function(id) {
        df$cols[((id-1)%%length(df$cols))+1]
    }

    ##################################################################
    ## scene
    ##################################################################
    scene <- qscene()
    attr(scene,'.sharedEnv')<-environment() # the current environment
    attr(scene,'.df')<-df

    ##################################################################
    ## painters
    ##################################################################
    dendroPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        #if (dbg) print(dbg.dendro,ls(env=.sharedEnv))
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro) cat('dendroPainter called\n')

        dendroPainterImpl <- function(layer, painter) {
            if (dbg.dendro) cat('dendroPainterImpl called\n')
            with(df$unselectedBranches$branches,qdrawSegment(painter,x1s,y1s,x2s,y2s,stroke=qcolor('black')))
            for (i in seq(along=df$clusters)) {
                if (!is.null(df$clusters[[i]]) && length(df$clusters[[i]]$branches)>0) {
                    if (dbg) cat(sprintf('cluster %i: color %s\n',i,clusterColor(i)))
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
            qdrawRect(painter, xy$x[-length(xy$x)], xy$y[-length(xy$y)], xy$x[-1], xy$y[-1], stroke=c(alpha('gray20',.7),alpha('yellow',.7)))

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
                printVar(layer$limits())
                printVar(layer$limits()$left())
                printVar(layer$limits()$bottom())
                printVar(layer$limits()$right())
                printVar(layer$limits()$top())
            }
        }
        dendroPainterImpl(layer,painter)
    }

    ##
    ## heatmap
    ##
    heatmapPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.heatmap) cat('heatmapPainter called\n')

        heatmapPainterImpl <- function(layer, painter) {
            if (dbg.heatmap) cat('heatmapPainterImpl called\n')

            jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
            jet.colors <-colorRampPalette(c("blue", "red"))
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
            colorNum<-20
            if (params$doSmoothHeatmap) {
                # not all observations visible in the dendrogram,
                # smooth heatmap to carry info about the currently
                # elementary clusters in the zoomed dendro
                ch<-cutree(df$h,h=dendroZoom$g[2])
                if (max(ch)!=df$elemClusterCount) {
                    if (dbg.heatmap) cat('smoothing heatmap\n')
                    df$xOrderedSmoothed<-smoothHeatmap(df$xOrdered,ch[df$leafOrder],dbg.heatmap)
                    df$elemClusterCount<-max(ch)
                }
            }
            colIdx<-as.numeric(cut2(df$xOrderedSmoothed,g=colorNum))
            if (dbg.heatmap>1) printVar(colIdx)
            #cols<-jet.colors(colorNum)
            colPalette<-heat.colors(colorNum)
            if (dbg.heatmap>1) printVar(colPalette)
            #cols<-sapply(cols,function(x)qcolor(x))
            cols<-colPalette[colIdx]
            if (dbg.heatmap>1) printVar(cols)
            # draw heatmap by colors, it is much faster compared to drawing in all colors in one single call
            #qdrawRect(painter,coords1[[1]],coords1[[2]],coords2[[1]],coords2[[2]],stroke=rgb(0,0,0,0),fill=cols)
            for (c in colPalette) {
                i<-cols==c
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

                print('heatmap:')
                printVar(layer$limits())
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
        if (dbg.heatmap) cat('heatmapDimAnnotationPainter called\n')

        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        heatmapDimAnnotationPainterImpl <- function(layer, painter) {
            gLabelDim<-seq(0,ncol(df$x)-1)+.5
            wLabelDim<-rep(0,ncol(df$x))
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

    heatmapObsAnnotationPainter <- function(layer, painter) {
        if (dbg.heatmap) cat('heatmapObsAnnotationPainter called\n')

        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        heatmapObsAnnotationPainterImpl <- function(layer, painter) {
            gLabelObs<-rep(0,nrow(df$x))
            wLabelObs<-seq(1,nrow(df$x))
            coordsLabelObs<-gw2xy(heatmap2fig(list(gLabelObs,wLabelObs)))
            # annotate observations
            qdrawText(painter,rownames(df$x)[df$leafOrder],coordsLabelObs[[1]],coordsLabelObs[[2]],color='black',halign='left')
        }
        heatmapObsAnnotationPainterImpl(layer,painter)
    }

    axisPainter <- function(layer, painter) {
        if (dbg.dendro.axis) cat('axisPainter called\n')

        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        df<-.sharedEnv$df

        # axis
        xy<-gw2xy(list(g=c(last(df$h$height),0),w=c(.5,.5)))
        qdrawSegment(painter,xy$x[1],xy$y[1],xy$x[2],xy$y[2],stroke='black')

        # ticks
        ticks<-pretty(dendroZoomMin$g)
        ticksG<-rep(ticks,each=2)
        ticksW<-rep(c(.4,.6),length=length(ticksG))
        xy<-gw2xy(list(g=ticksG,w=ticksW))
        idxOdd<-seq(1,length(xy$x),by=2)
        idxEven<-seq(2,length(xy$x),by=2)
        qdrawSegment(painter,xy$x[idxOdd],xy$y[idxOdd],xy$x[idxEven],xy$y[idxEven],stroke='black')

        # tick labels
        qdrawText(painter,ticks,xy$x[idxOdd],xy$y[idxOdd],color='black',valign='top')
    }

    ##################################################################
    ## interactions
    ##################################################################
    mousePressFun <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        if (dbg) cat('mousePressFun called\n')
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
        if (dbg) cat('mouseMoveFun called\n')
        #if (dbg) print(event$pos())

        if (.sharedEnv$mouseRightButtonPressed) {
            # update zooming region
            dendroZoomSelectionUpdater(layer, event)
        }

        if (.sharedEnv$mouseMiddleButtonPressed) {
            gwMaxDiff<-.sharedEnv$dendroZoomMin
            gwMaxDiff$g<-.sharedEnv$dendroZoomMin$g-.sharedEnv$dendroZoom$g
            gwMaxDiff$w<-.sharedEnv$dendroZoomMin$w-.sharedEnv$dendroZoom$w
            xyMaxDiff<-gw2xy(gwMaxDiff)

            xy<-gw2xy(.sharedEnv$dendroZoom)
            xyDiff<-list(
                x=.sharedEnv$mouseMiddleButtonPressPos$x()-event$pos()$x(),
                y=.sharedEnv$mouseMiddleButtonPressPos$y()-event$pos()$y())
            xyDiff$x<-min(max(xyMaxDiff$x),max(min(xyMaxDiff$x),xyDiff$x))
            xyDiff$y<-min(max(xyMaxDiff$y),max(min(xyMaxDiff$y),xyDiff$y))
            xy$x<-xy$x+xyDiff$x
            xy$y<-xy$y+xyDiff$y

            gw<-xy2gw(xy)
            if (dbg.dendro) printVar(gw)
            .sharedEnv$dendroZoom<-gw
            if (dbg.dendro) printVar(.sharedEnv$dendroZoom)

            zoomDendroAndHeatmap(layer$scene())
        }
    }

    mouseReleaseFun <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        if (dbg) cat('mouseReleaseFun called\n')
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

    clusterSelector <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.clusterSelector) cat('clusterSelector called\n')

        clusterSelectorImpl <- function(layer, event) {
            if (dbg.clusterSelector) cat('clusterSelectorImpl called\n')
            if (dbg.clusterSelector) print(as.numeric(event$pos()))

            if (radio1$isChecked()) df$currentCluster<<-1
            else if (radio2$isChecked()) df$currentCluster<<-2
            else if (radio3$isChecked()) df$currentCluster<<-3

            if (dbg.clusterSelector) printVar(df$currentCluster)

            df<-selectCluster(event$pos())
        }
        df<-clusterSelectorImpl(layer, event)
        .sharedEnv$df<-df
        qupdate(scene)
    }

    restrictGw<-function(gw,limitGw) {
        gw$g<-sort(gw$g,decreasing=T)
        gw$w<-sort(gw$w,decreasing=T)

        gw$g[1]<-min(limitGw$g[1],max(limitGw$g[2],gw$g[1]))
        gw$g[2]<-max(limitGw$g[2],min(limitGw$g[1],gw$g[2]))
        gw$w[1]<-min(limitGw$w[1],max(limitGw$w[2],gw$w[1]))
        gw$w[2]<-max(limitGw$w[2],min(limitGw$w[1],gw$w[2]))
        gw
    }

    dendroZoomSelectionStarter <- function(layer, event) {
        if (dbg.clusterSelector) cat('dendroZoomSelectionStarter called\n')
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        .sharedEnv$dendroZoomMouseSelection<-xy2gw(list(x=rep(event$pos()$x(),2),y=rep(event$pos()$y(),2)))
        qupdate(scene)
    }

    dendroZoomSelectionUpdater <- function(layer, event) {
        if (dbg.clusterSelector) cat('dendroZoomSelectionUpdater called\n')
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        gw<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))
        .sharedEnv$dendroZoomMouseSelection$g[2]<-gw$g
        .sharedEnv$dendroZoomMouseSelection$w[2]<-gw$w
        qupdate(scene)
    }

    dendroZoomSelectionFinisher <- function(layer, event) {
        if (dbg.clusterSelector) cat('dendroZoomSelectionFinisher called\n')
        if (event$button()==2) {
            # right mouse button

            # update the selection region
            dendroZoomSelectionUpdater(layer, event)

            .sharedEnv<-attr(layer$scene(),'.sharedEnv')
            # set the current zoom to the selection region
            .sharedEnv$dendroZoom<-restrictGw(.sharedEnv$dendroZoomMouseSelection,.sharedEnv$dendroZoomMin)
            if (dbg.dendro.zoom>1) printVar(dendroZoom)
            # reset the selection region
            .sharedEnv$dendroZoomMouseSelection<-list(g=c(NA,NA),w=c(NA,NA))
            # zoom to the current zoom
            zoomDendroAndHeatmap(layer$scene())

            #qupdate(scene)
        } else {
            # ignored
        }
    }

    dendroZoomer <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.zoom) cat('dendroZoomer called\n')

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
                newDendroZoom$g[1]<-min(dendroZoomMin$g[1],newDendroZoom$g[1])
                newDendroZoom$g[2]<-max(dendroZoomMin$g[2],newDendroZoom$g[2])
                newDendroZoom$w[1]<-min(dendroZoomMin$w[1],newDendroZoom$w[1])
                newDendroZoom$w[2]<-max(dendroZoomMin$w[2],newDendroZoom$w[2])
                dendroZoom<-newDendroZoom
            }
            dendroZoom
        }
        dendroZoom<-dendroZoomerImpl(layer, event)
        .sharedEnv$dendroZoom<-dendroZoom
        if (dbg.dendro.zoom>1) printVar(dendroZoom)

        zoomDendroAndHeatmap(layer$scene())
    }

    zoomDendroAndHeatmap<-function(scene) {
        .sharedEnv<-attr(scene,'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        gw<-dendro2fig(dendroZoom)

        xy<-gw2xy(gw)
        dendroLayer$setLimits(qrect(xy[[1]][1],xy[[2]][2],xy[[1]][2],xy[[2]][1]))

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
            tmp$setTop(xy[[2]][2])
            tmp$setBottom(xy[[2]][1])
            heatmapLayer$setLimits(tmp)

            heatmapObsAnnotationLayer<-.sharedEnv$heatmapObsAnnotationLayer
            if (!is.null(heatmapObsAnnotationLayer)) {
                # zoom heatmap observations
                tmp<-heatmapObsAnnotationLayer$limits()
                tmp$setTop(xy[[2]][2])
                tmp$setBottom(xy[[2]][1])
                heatmapObsAnnotationLayer$setLimits(tmp)
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

        qupdate(scene)
    }

    axisCutterLeave<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutterLeave called\n')
        .sharedEnv$axisCut<-NA

        qupdate(scene)
    }

    axisCutter<-function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro.axis) cat('axisCutter called\n')
        cutG<-xy2gw(list(x=event$pos()$x(),y=event$pos()$y()))$g
#TODO: 

        #impl<-function() {
        #    df<-gfc(df)
        #    #printVar(df)
        #    cutDendro(df,cutG,params$maxClusterCount,.sharedEnv$dendroZoom,2)
        #}
        #df<-impl()
        #printVar(df)
        #if (!is.null(df)) .sharedEnv$df<-df

        qupdate(scene)
    }

    keyPressFun<-function(layer, event) {
        if (dbg) print(event)
        print(event$key())
        print(event$text())
        #Qt$QApplication$exit(0)
        #Qt$QApplication$sendEvent(At$QMainWindow(),Qt$CloseEvent())
        #qupdate(scene)
    }

    controlPainter <- function(layer, painter) {
        #checkBox1 <- Qt$QCheckBox("&Checkbox 1")
        #qdrawText(painter, 'title',100,0)
    }
    titlePainter <- function(layer, painter) qdrawText(painter, 'title',100,0)


    ##################################################################
    ## scene
    ##################################################################

    ## dendrogram
    ################
    dendroLimits<-unlist(gw2xy(dendro2fig(dendroZoomMin)))
    if (dbg.dendro) printVar(dendroLimits)
    dendroLayer <- with(df$allBranches$branches,
        qlayer(scene, paintFun = dendroPainter,
            mousePressFun = mousePressFun,
            mouseMoveFun = mouseMoveFun,
            mouseReleaseFun = mouseReleaseFun,
            wheelFun = dendroZoomer, clip = F, cache = F,#keyPressFun=keyPressFun,
            limits = qrect(dendroLimits[1],dendroLimits[4],dendroLimits[2],dendroLimits[3])))#qrect(0,0,dendroG,1),#qrect(dendroLimits[[1]][1],dendroLimits[[2]][1],dendroLimits[[1]][2],dendroLimits[[2]][2]),#,

    axisLimits<-dendroZoomMin
    axisLimits$w<-c(1,0)
    axisLimits<-unlist(gw2xy(dendro2fig(axisLimits)))
    if (dbg.dendro.axis) printVar(axisLimits)
    axisLayer <- qlayer(scene, paintFun = axisPainter,
            hoverMoveFun = axisCutterUpdate,
            hoverEnterFun = axisCutterUpdate,
            hoverLeaveFun = axisCutterLeave,
            mousePressFun = axisCutter,
            limits = qrect(axisLimits[1],axisLimits[4],axisLimits[2],axisLimits[3]))

    ## heatmap
    ################
    heatmapLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,df$k),w=c(df$n,0)+.5))))
    heatmapLayer <- with(df$allBranches$branches,
        qlayer(scene, paintFun = heatmapPainter,clip=F,
            limits = qrect(heatmapLimits[1],heatmapLimits[4],heatmapLimits[2],heatmapLimits[3])))#qrect(dendroG,0,1,1)))#qrect(heatmapLimits[[1]][1],heatmapLimits[[2]][1],heatmapLimits[[1]][2],heatmapLimits[[2]][2])))#
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

    ## heatmap observations annotations
    if (!is.null(x) && !is.null(rownames(x))) {
        heatmapObsAnnotationLimits<-unlist(gw2xy(heatmap2fig(list(g=c(0,1),w=c(0,df$n)+.5))))
        heatmapObsAnnotationLayer <- qlayer(scene, paintFun = heatmapObsAnnotationPainter,
            limits=qrect(heatmapObsAnnotationLimits[1],heatmapObsAnnotationLimits[3],
                heatmapObsAnnotationLimits[2],heatmapObsAnnotationLimits[4]))
    } else {
        heatmapObsAnnotationLayer <- NULL
    }
    .sharedEnv$heatmapObsAnnotationLayer<-heatmapObsAnnotationLayer

    #controlLayer <- qlayer(scene, paintFun = controlPainter)

    titleLayer <- qlayer(scene, paintFun = titlePainter)

    #############
    ## layout
    #############
    figLayer <- qlayer(scene)
    figLayer[0, 0] <- titleLayer
    figLayer[0, 1] <- heatmapDimAnnotationLayer
    #figLayer[1, 0] <- controlLayer
    figLayer[1, 0] <- dendroLayer
    figLayer[1, 1] <- heatmapLayer
    figLayer[1, 2] <- heatmapObsAnnotationLayer
    figLayer[2, 0] <- axisLayer
    
    layout <- figLayer$gridLayout()
    layout$setRowPreferredHeight(0, 50)
    layout$setRowPreferredHeight(1, 200)
    layout$setRowPreferredHeight(2, 50)
    layout$setRowStretchFactor(0, 0)
    layout$setRowStretchFactor(1, 1)
    layout$setRowStretchFactor(2, 0)
    layout$setColumnPreferredWidth(0, 200)
    layout$setColumnPreferredWidth(1, 200)
    layout$setColumnPreferredWidth(2, 50)
    layout$setColumnStretchFactor(0, 1)
    layout$setColumnStretchFactor(1, 1)
    layout$setColumnStretchFactor(2, 0)

    #print(layout$addItem)
    #layout$addWidget(createFirstExclusiveGroup(), 0, 0)
    #layout$addWidget(Qt$QRadioButton("&Radio button 1"),0,0)
    #layout$addItem(Qt$QLabel("&Radio button 1"),0,0)
    
    view <- qplotView(scene = scene)
    print(view)

    #######################################################
    #######################################################
    ## GUI
    qsetClass("Window", Qt$QWidget, function(parent = NULL) {
        super(parent)
  
        grid <- Qt$QGridLayout()
          ## NOTE: the layout does not take ownership of the widgets, so we
          ## need to assign the layout to our widget up-front. Our widget then
          ## takes ownership of the widgets in the layout.
          setLayout(grid) 
          grid$addWidget(createFirstExclusiveGroup(), 0, 0)
  
          setWindowTitle("idendro")
          resize(480, 320)
    },where=environment())

    #qsetMethod("currentClusterChanged", Window, function(index) {
    #    cat('currentClusterChanged called\n')
    #})


    radio1 <- Qt$QRadioButton("1")
    radio2 <- Qt$QRadioButton("2")
    radio3 <- Qt$QRadioButton("3")

    qsetMethod("createFirstExclusiveGroup", Window, function() {
        groupBox <- Qt$QGroupBox("Clusters")

        radio1$setChecked(TRUE)
        # TODO: connect boxes to "state change" listener

        vbox <- Qt$QVBoxLayout()
        vbox$addWidget(radio1)
        vbox$addWidget(radio2)
        vbox$addWidget(radio3)
        vbox$addStretch(1)
        groupBox$setLayout(vbox)

        groupBox
    }, "private")


    Window()$show()
#    print(radio1$isChecked())
#    print(radio2$isChecked())
}
