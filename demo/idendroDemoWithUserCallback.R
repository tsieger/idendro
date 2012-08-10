## Demo showing how user-supplied callback function can respond
## interactively to changes made to `idendro' dendrogram and GUI.
##

# generate data in feature space
n<-10
x<-data.frame(x1=c(rnorm(n,-1),rnorm(n,1)),x2=c(rnorm(n,-1),rnorm(n,1)))
rownames(x)<-1:(2*n)

# compute pairwise distances
dx<-dist(x)

# perform hierarchical clustering
hx<-hclust(dx)

# prepare mutable data frame enabling to communicate between
# idendro and user callback
qx<-qdata(x)

# visualize clusters, qx mutaframe enriched with `.cluster'
# and `.currentCluster' returned
qx<-idendro(hx,qx)

# bind a listener to changes to cluster assignment and current cluster setting
qx.listener <- add_listener(qx, function(i, j) {
    idx = which(j == c('.cluster','.currentCluster'))
    if (length(idx) > 0) {
        cat('qx listener: current cluster changed\n')
        print(qx)
        userCallbackWindow$updateClusterInfo(qx)
    }
})

# a simple GUI
qsetClass("UserCallbackWindow", Qt$QWidget, function(parent = NULL) {
    super(parent)

    this$currentClusterSize<-0
    this$currentClusterMeanX1<-NA
    this$currentClusterMeanX2<-NA
  
    this$label<-Qt$QLabel('There is no current cluster.')
    this$quitButton<-Qt$QPushButton("&Quit")
    mainLayout <- Qt$QGridLayout()
    mainLayout$addWidget(label)
    mainLayout$addWidget(quitButton)
    setLayout(mainLayout)
    setWindowTitle("User Callback")

    qconnect(quitButton, "pressed", function(data) {
        remove_listener(data[[1]], data[[2]])
        close()
    },list(qx,qx.listener))

    qsetMethod("updateClusterInfo", UserCallbackWindow, function(qx) {
        this$label$setText(sprintf(
            'The current cluster consists of %d observation(s).\nThe mean x1 is %.3f, the mean x2 is %.3f.',
            sum(qx$.currentCluster),
            mean(qx$x1[qx$.currentCluster]),
            mean(qx$x2[qx$.currentCluster])))
    })
})

userCallbackWindow<-UserCallbackWindow()
userCallbackWindow$updateClusterInfo(qx)
userCallbackWindow$show()

