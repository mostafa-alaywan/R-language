
library(ade4)
library(factoextra)
library(psych)        # for Kaiser-Meyer-Olkin Test (KMO)


# Define and Explore the Data ---------------------------------------------

data <- mtcars

str(data)

summary(data)


# Determinant of the correlation matrix ----------------------------------

corr_matrix <- cor(data)

det(corr_matrix)


# Kaiser-Meyer-Olkin Bartlett's  Tests  ----------------------------------

KMO(data)

bartlett.test(data)


# Runing the PCA on matcars dataset   ------------------------------------

acp <- dudi.pca(data,center=T,scale=T,scannf = F)

summary(acp)

attributes(acp)


# Extract the results for individuals  --------------------

ind <- get_pca_ind(acp)

print(ind)

head(ind$coord)

head(ind$contrib)

head(ind$cos2)


# Extract the results for variables ---------------------------------------

var <- get_pca_var(acp) 

print(var)

head(var$cor)

head(var$cos2)


# Graphical Representation of the individuals in the first plane  ---------

fviz_pca_ind(acp,col.ind = "cos2",gradien.cols=c("#00AFBB", "#E7B800", "#FC4E07"),repel = T)


# Graphical Representation of the variables in the first plane ------------

fviz_pca_var(acp,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
