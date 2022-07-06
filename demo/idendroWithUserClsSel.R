## Demo showing how users can interactively control cluster selection.
##

library(idendro) # idendro
library(cranvas) # qdata
library(qtbase) # Qt, qconnect, qsetMethod

data(iris)

# compute pairwise distances
x<-iris[,1:4]

# perform hierarchical clustering
hx<-hclust(dist(x))

# colors of observations
clr<-iris[,5]

# Get members of clusters in HCA.
hcaMembers<-function(hc) {
  n<-length(hc$height) # cluster count
  members<-vector('list',n)
  for (i in 1:n) {
    if (hc$merge[i,1]<0) {
      m1<--hc$merge[i,1]
    } else {
      m1<-members[[hc$merge[i,1]]]
    }
    if (hc$merge[i,2]<0) {
      m2<--hc$merge[i,2]
    } else {
      m2<-members[[hc$merge[i,2]]]
    }
    members[[i]]<-c(m1,m2)
  }
  return(members)
}

# number of obsevations
n<-nrow(x)

# members of individual clusters
members<-hcaMembers(hx)

# prepare mutable data frame enabling to communicate between
# idendro and user callback
qx<-qdata(iris)

# feature space projection of data
print(qscatter(Sepal.Length,Sepal.Width,qx))

# a simple GUI
qsetClass("UserCallbackWindow", Qt$QWidget, function(parent = NULL) {
    super(parent)

    this$virginicaButton <- Qt$QPushButton("Select clusters of Virginica flowers") # 141,135,134
    this$versicolorButton <- Qt$QPushButton("Select clusters of Versicolor flowers") # 140,144
    this$setosaButton <- Qt$QPushButton("Select clusters of Setosa flowers") # 146
    this$quitButton <- Qt$QPushButton("&Quit")
    mainLayout <- Qt$QGridLayout()
    mainLayout$addWidget(virginicaButton)
    mainLayout$addWidget(versicolorButton)
    mainLayout$addWidget(setosaButton)
    mainLayout$addWidget(quitButton)
    setLayout(mainLayout)
    setWindowTitle("User Callback")

    qconnect(virginicaButton, "pressed", function() {
      cls<-rep(0,n)
      cls[members[[141]]]<-1
      cls[members[[135]]]<-2
      cls[members[[134]]]<-3
      qx$.cluster<-cls
    })
    qconnect(versicolorButton, "pressed", function() {
      cls<-rep(0,n)
      cls[members[[140]]]<-1
      cls[members[[144]]]<-2
      qx$.cluster<-cls
    })
    qconnect(setosaButton, "pressed", function() {
      cls<-rep(0,n)
      cls[members[[146]]]<-1
      qx$.cluster<-cls
    })
    qconnect(quitButton, "pressed", function() {
        close()
    })
})

userCallbackWindow <- UserCallbackWindow()
userCallbackWindow$show()

idendro(hx,qx)
