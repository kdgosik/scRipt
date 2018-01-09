# scRipt
 single cell R interactive playground tool is meant to have an interactive user interface for single cell analysis.  It has functions from cellranger and Seurat currently with more to come.  There will be options to create static html reports to be saved.  These can be loaded into your browser for exploration anytime or you can share with collaborators for an initial look at your data.  These are meant for exploration and a quick look at data.  It is not meant to be a comprehensive analysis.
 
 There is also an interactive part where you can upload previously analyzed data and explore in more detail.  

## Installation

```{r}
install.packages("devtools")
devtools:install_github("kdgosik/scRipt")
```


## Useage

```{r}
library(scRipt)

## run the scRiptUI shiny app
scRiptUI()
```
