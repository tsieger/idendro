## Demo showing how user-supplied callback function can respond
## interactively to changes made to `idendro' dendrogram and GUI.
##

data(iris)

# compute pairwise distances
dx <- dist(iris[, 1:4])

# perform hierarchical clustering
hx <- hclust(dx)

# prepare mutable data frame enabling to communicate between
# idendro and user callback
qx <- qdata(iris)

# visualize clusters, qx mutaframe enriched with `.cluster'
# and `.inCurrentCluster' returned
qx <- idendro(hx,qx)

# bind a listener to changes to cluster assignment and current cluster setting
qx.listener <- add_listener(qx, function(i, j) {
    if ('.inCurrentCluster' %in% j) {
        cat('qx listener: current cluster changed\n')
        print(qx)
        userCallbackWindow$updateClusterInfo(qx)
    }
})

# a simple GUI
qsetClass("UserCallbackWindow", Qt$QWidget, function(parent = NULL) {
    super(parent)

    this$currentClusterSize <- 0
    this$currentClusterMeanX1 <- NA
    this$currentClusterMeanX2 <- NA
  
    this$label <- Qt$QLabel('There is no current cluster.')
    this$quitButton <- Qt$QPushButton("&Quit")
    mainLayout <- Qt$QGridLayout()
    mainLayout$addWidget(label)
    mainLayout$addWidget(quitButton)
    setLayout(mainLayout)
    setWindowTitle("User Callback")

    qconnect(quitButton, "pressed", function(data) {
        remove_listener(data[[1]], data[[2]])
        close()
    },list(qx, qx.listener))

    qsetMethod("updateClusterInfo", UserCallbackWindow, function(qx) {
        this$label$setText(sprintf(
            'The current cluster consists of %d observation(s).\nThe mean sepal length is %.3f, the mean sepal width is %.3f.',
            sum(qx$.inCurrentCluster),
            mean(qx$Sepal.Length[qx$.inCurrentCluster]),
            mean(qx$Sepal.Width[qx$.inCurrentCluster])))
    })
})

userCallbackWindow <- UserCallbackWindow()
userCallbackWindow$updateClusterInfo(qx)
userCallbackWindow$show()

