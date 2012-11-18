\name{idendro}
\alias{idendro}
\title{Interactive Dendrogram}
\description{
'idendro' is a plot enabling visualization and interactive
inspection of a dendrogram, with a heatmap attached to it,
optionally.
Clusters anywhere in the dendrogram hierarchy can be selected.
The dendrogram can be zoomed and panned.
'idendro' can be integrated with other plots and tools
by communicating over a shared mutable data frames (mutaframes).
'idendro' can be used to display quite large dendrograms (tens
of thousands of observations, at least).
}
\usage{idendro(h, qx = NULL, x = qx, zoomFactor = 1/240, observationAnnotationEnabled = TRUE, 
    clusterColors = c("red", "green", "blue", "yellow", "magenta", 
        "cyan", "darkred", "darkgreen", "purple", "darkcyan"), 
    unselectedClusterColor = "black", maxClusterCount = length(clusterColors), 
    heatmapEnabled = TRUE, doSmoothHeatmap = TRUE, heatmapColors = colorRampPalette(c("#00007F", 
        "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", 
        "red", "#7F0000")), heatmapColorCount = 10, heatmapRelSize = 0.2, 
    brushedmapEnabled = !is.null(qx), separateGui = FALSE, graphicalClusterInfos = TRUE, 
    textualClusterInfos = TRUE)}
\arguments{
  \item{h}{an object of class 'stats::hclust' describing a clustering}
  \item{qx}{a mutaframe holding observations tha were clustered
giving rise to 'h', with hidden columns for interaction, as created
by 'cranvas::qdata'. If 'qx' is enriched with 'idendro'-specific
hidden columns (i.e. '.cluster', '.inCurrentCluster'), the
initial cluster selection is based on them; otherwise there are
no clusters selected initially.
A regular data frame can be passed instead of a mutaframe, in
which case it will get converted into a mutaframe
automatically.
This parameter is optional.}
  \item{x}{a data frame holding observations tha were clustered
giving rise to 'h'.
Heatmap will depict this data.
This parameter is optional. If missing, it will be guessed
from 'qx' by omitting any columns starting in '.'.}
  \item{zoomFactor}{the amount of zoom in/out as controlled by the
mouse wheel}
  \item{observationAnnotationEnabled}{shall the names of individual
observations (rownames of 'x') be shown next to the
dendrogram/heatmap?}
  \item{clusterColors}{colors
of individual clusters selected in the dendrogram}
  \item{unselectedClusterColor}{the color of unselected dendrogram
branches}
  \item{maxClusterCount}{maximum number of
clusters user can select. If greater than the number of
'clusterColors', cluster colors will get recycled.
This parameter affects the size of GUI and the number of
clusters which can be selected automatically by "cutting" the
dendrogram.}
  \item{heatmapEnabled}{shall the heatmap be drawn?}
  \item{doSmoothHeatmap}{shall the heatmap depict the mean data
values associated with the clusters currently shown in the
dendrogram (TRUE, the defult), or shall it depict all the
individual observations forming the clusters, even if the
individual observations are not currently visible in the
dendrogram (FALSE)?}
  \item{heatmapColors}{heatmap
color map scheme}
  \item{heatmapColorCount}{the number of colors used by the heatmap,
picked (interpolated) from 'heatmapColors'.
WARNING: the number of colors used by heatmap can influence
the time spent drawing the heatmap significantly (for large
data sets).}
  \item{heatmapRelSize}{relative size of the heatmap - the ratio
of the heatmap width to the width of both the dendrogram and
heatmap. The default is 20\%.}
  \item{brushedmapEnabled}{shall brushed map be drawn?}
  \item{separateGui}{shall GUI be integrated into the dendrogram
window, or shall it be separated in a standalone window?
(The default is integrated GUI.)}
  \item{graphicalClusterInfos}{depict cluster-specific statistics
graphically? (The default is TRUE.)}
  \item{textualClusterInfos}{depict cluster-specific statistics
textually? (The default is TRUE.)}
}
\details{'idendro' displays an interactive dendrogram enriched, optionally,
with a heatmap and/or a brushedmap.

The dendrogram depicts the result of a hierarchical cluster
analysis on a set of observations. There is an axis drawn by the
side of the dendrogram displaying the "height" of clusters. 

The heatmap visualizes the observations living in k-dimensional
space by mapping their values onto a color scale and displaying
as a row of 'k' colored rectangles.

The brushedmap indicates which observations are currently
being selected by some external plot/tool 'idendro' is integrated
with. Technically speaking, the current selection is determined by
a hidden column in the 'qx' mutable data frame being changed by
the external plot/tool. 'idendro' listens to changes to this column.

The dendrogram can be zoomed and panned. To zoom in to a
specific region, right click and drag in the dendrogram.
Mouse wheel can also be used to zoom in and out (the amount of zoom
can be controlled by 'zoomFactor'). To pan a zoomed dendrogram,
middle click and drag the mouse. Zooming and panning history is
available (see 'GUI').

User can select clusters manually one by one (by clicking
at individual clusters in the dendrogram), or automatically by
"cutting" the dendrogram at a specified height. To cut the
dendrogram, navigate the mouse close to the dendrogram axis
(a dashed line will appear across the dendrogram at a specified
height), and left click. Clusters just beneath the cutting
height will get selected, replacing the clusters being currently
selected. Selection history is available (see 'GUI').

\emph{Graphical User interface (GUI):}

Besides the dendrogram window, there is a window offering a few
GUI controls. In the top part of the GUI window come
cluster-specific controls and info panels arranged in rows.
(The number of rows is determined by 'maxClusterCount')
In each row, from left to right,
there is the current cluster indicator, the cluster number and
color code (determined by 'clusterColors'), and
cluster-specific statistics: the total number (and ratio) of
observations in the specific cluster out of the total number
of observations in the data set, and the number (and ratio) of
observations in the cluster out of the observations brushed
externally. At any time, exactly one cluster is being the current
cluster. Manual cluster selection (re)defines which cluster (as
appearing in the dendrogram) is pointed to by the current cluster.
The observations forming the current cluster are indicated by
the '.inCurrentCluster' column in the 'qx' mutaframe, which can be
used by external applications to display the current
cluster-specific information (see 'idendroDemoWithUserCallback').
At the bottom of the GUI window, there are buttons controling
zooming and cluster selection:

"Full view" - zooms dendrogram out maximally

"Zoom back" - retrieves the previous zoom region from history

"Unselect" - unselects the current cluster (makes the current
cluster to point to no cluster in the dendrogram, so it
decolorizes dendrogram branches associated with
the current cluster)

"unselect All" - unselects all clusters (decolorizes all the
clusters in the dendrogram)

"select Back" - retrieves the previous cluster selection from
history

"Quit"

}
\value{a mutaframe 'qx' (or a mutaframe created from regular data
frame 'x') enriched with dynamic (interactive) clusters
selection ('.cluster' column), and a flag determining which
observations form the current cluster ('.inCurrentCluster'
column). Observations not appearing in any cluster have 0 in the
'.cluster' column, while observations forming the 'i'-th
cluster have the value 'i' there.
The '.inCurrentCluster' column holds logical flags determining
whether given observation is a member of the current cluster.
The returned mutaframe can be used for bidirectional
interaction with 'idendro': external plot(s)/tool(s) can brush
some observations ('idendro' will notice it and update the
brushedmap), and also respond to any changes made to cluster
assignment and/or the current cluster selection.

Note that if the value returned is passed to a subsequent call
to 'idendro', the cluster selection saved in the mutaframe will
be restored. This feature can be regarded as a simple means of
cluster selection persistency.}

\author{Tomas Sieger}



\seealso{hclust, plclust, identify.hclust, rect.hclust,
cutree, dendrogram, cranvas::qdata}
\examples{
# data to be clustered
x<-data.frame(x1=1:10,x2=seq(10,1,-1))
# clustered data (dendrogram)
h<-hclust(dist(x))
idendro(h,x)

# for more examples please see demos
}
