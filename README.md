# mesh_process
## An R tool for batch-processing of meshes and landmark files for shape analyses

#### Here we provide several functions that batch import, process (by 
reducing the number of faces, closing holes, removing isolated pieces, etc. as per 
the R package Rvcg), and save multiple meshes in different formats (e.g. PLY, STL, 
VTK). It also allows batch-importing and saving landmark files in differnt formats 
(.aim, .tag, .mps, etc.).

`library(devtools)`

`devtools::install_github("marta-vidalgarcia/mesh_process")`

`library(mesh_process)`


***

If you have any questions you can contact Marta Vidal-García at **marta.vidalga@gmail.com**

***

Citation:

Daniel Adler, Duncan Murdoch and others (2020). rgl: 3D Visualization Using OpenGL. 
R package version 0.100.54.https://CRAN.R-project.org/package=rgl

Tim Schäfer (2020). freesurferformats: Read and Write 'FreeSurfer' Neuroimaging File Formats. 
R package version 0.1.10. https://CRAN.R-project.org/package=freesurferformats

Schlager S (2017). “Morpho and Rvcg - Shape Analysis in R.” In Zheng G, Li S, 
Szekely G (eds.), _Statistical Shape and Deformation Analysis_, 217-256. Academic 
Press. ISBN 9780128104934.
