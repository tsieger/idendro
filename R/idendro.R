
idendro<-function(hx,x=NULL,cols=c('red','green','blue','yellow','magenta','cyan')) {

    require(qtpaint)
    require(qtbase)
    require(cranvas)
    require(grDevices) # needed to generate jet palette by `colorRampPalette'
    require(Hmisc) # cut2

    dbg<-0
    dbg.tx<-dbg*01
    dbg.dendro<-dbg*01
    dbg.heatmap<-dbg*01
    dbg.clusterSelector<-dbg*01
    charmW<-.1
    strangeW<-.1
    dendroG<-.5
    heatmapG<-.3

    df<-prepareDendro(hx,x,dbg.dendro)
    df$cols<-cols

    .sharedEnv<-new.env()
    for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn)),env=.sharedEnv)

    # determine color for cluster of given ID (starting at 1)
    clusterColor <- function(id) {
        df$cols[((id-1)%%length(df$cols))+1]
    }

    ##################
    ## scene
    scene <- qscene()
    attr(scene,'.sharedEnv')<-environment() # the current environment
    attr(scene,'.df')<-df

    dendroPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        #if (dbg) print(dbg.dendro,ls(env=.sharedEnv))
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.dendro) print('dendroPainter called')

        dendroPainterImpl <- function(layer, painter) {
            if (dbg.dendro) print('dendroPainterImpl called')
            with(df$unselectedBranches$branches,qdrawSegment(painter,x1s,y1s,x2s,y2s,stroke=qcolor('black')))
            for (i in seq(along=df$clusters)) {
                if (!is.null(df$clusters[[i]]) && length(df$clusters[[i]]$branches)>0) {
                    if (dbg) cat(sprintf('cluster %i: color %s\n',i,clusterColor(i)))
                    with(df$clusters[[i]]$branches,qdrawSegment(painter,x1s,y1s,x2s,y2s,stroke=qcolor(clusterColor(i))))
                }
            }
        }
        dendroPainterImpl(layer,painter)
    }

    heatmapPainter <- function(layer, painter) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.heatmap) print('heatmapPainter called')

        heatmapPainterImpl <- function(layer, painter) {
            if (dbg.heatmap) print('heatmapPainterImpl called')

            jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
            jet.colors <-colorRampPalette(c("blue", "red"))
            g1<-rep(seq(0,ncol(df$x)-1),each=nrow(df$x))
            g2<-g1+1
            w1<-rep(1:nrow(df$x),ncol(df$x))
            w2<-w1+1
            if (dbg.heatmap) printVar(g1)
            if (dbg.heatmap) printVar(g2)
            if (dbg.heatmap) printVar(w1)
            if (dbg.heatmap) printVar(w2)
            coords1<-gw2xy(heatmap2fig(list(g1,w1)))
            coords2<-gw2xy(heatmap2fig(list(g2,w2)))
            colorNum<-20
            colIdx<-as.numeric(cut2(x,g=colorNum))
            if (dbg.heatmap) printVar(colIdx)
            #cols<-jet.colors(colorNum)
            colPalette<-heat.colors(colorNum)
            if (dbg.heatmap) printVar(colPalette)
            #cols<-sapply(cols,function(x)qcolor(x))
            cols<-colPalette[colIdx]
            if (dbg.heatmap) printVar(cols)
            # draw heatmap by colors, it is much faster compared to drawing in all colors in one single call
            #qdrawRect(painter,coords1[[1]],coords1[[2]],coords2[[1]],coords2[[2]],stroke=rgb(0,0,0,0),fill=cols)
            for (c in colPalette) {
                i<-cols==c
                qdrawRect(painter,coords1[[1]][i],coords1[[2]][i],coords2[[1]][i],coords2[[2]][i],stroke=rgb(0,0,0,0),fill=c)
            }
        }
        heatmapPainterImpl(layer,painter)
    }

    clusterSelector <- function(layer, event) {
        .sharedEnv<-attr(layer$scene(),'.sharedEnv')
        for (vn in sharedVarNames()) assign(vn,eval(parse(text=vn),env=.sharedEnv))

        if (dbg.clusterSelector) print('clusterSelector called')

        clusterSelectorImpl <- function(layer, event) {
            if (dbg.clusterSelector) print('clusterSelectorImpl called')
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

    mover <- function(layer, event) {
        if (dbg) print('mover called')
        if (dbg) print(event$delta())
        df$y<<-df$y+event$delta()/60
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

    ##################
    ## scene
    dendroLayer <- with(df$allBranches$branches,
        qlayer(scene, paintFun = dendroPainter,limits = qrect(0,0,1,1),
        mousePressFun=clusterSelector,wheelFun=mover))#,keyPressFun=keyPressFun)
    heatmapLayer <- with(df$allBranches$branches,
        qlayer(scene, paintFun = heatmapPainter,limits = qrect(0,0,1,1)))
    #controlLayer <- qlayer(scene, paintFun = controlPainter)
    titleLayer <- qlayer(scene, paintFun = titlePainter)

    figLayer <- qlayer(scene)
    
    figLayer[0, 0] <- titleLayer
    #figLayer[1, 0] <- controlLayer
    figLayer[1, 0] <- dendroLayer
    figLayer[1, 0] <- heatmapLayer
    
    layout <- figLayer$gridLayout()
    layout$setRowPreferredHeight(0, 50)
    layout$setRowPreferredHeight(1, 200)
    layout$setRowStretchFactor(0, 0)
    layout$setRowStretchFactor(1, 1)

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
    #    print('currentClusterChanged called')
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
