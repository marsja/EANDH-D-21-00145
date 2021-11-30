fit50 <- readRDS("./Fig_table_Data/finalModel50_sep_revision.rds")


# New names for the latent and indicator variables
mfs <- c("Hagerman \n 4-talker fast", "Hagerman \n 4-talker np",
        "Hagerman \n 4-talker nr", "Hagerman \n SSN fast",
        "Hagerman \n SSN np", "Hagerman \n SSN nr",
        "Reading \n Span", "Semantic \n Word-Pairs", "Visuospatial \n WM", "Raven",
        "PTA4", "Age", "SPiN", "Cognitive \n Functioning")

# Here we are generating the .tiff submitted to EANDH:
tiff('./Figures/Figure-2-rev2.tiff', units="in", width=17.4, height=7.2, res=1200, compression = 'lzw')

# We use layout so we can visualize both models next to each other:
layout(t(1:2))
semPlot::semPaths(fit50, include = 1, what='path', ask=F, layout='tree3', 
                  style="lisrel", sizeMan=14, sizeMan2 = 5, sizeLat = 16, sizeLat2 = 8,
                  whatLabels='std', edge.color='black',title.color = 'black',
                  edge.label.cex=.7, label.cex = 1, residuals= T, optimizeLatRes = T,
                  rotation = 1, curvePivot = T, intAtSide = F, 
                  sizeInt = 1.5, nCharNodes = 0, intercepts = F,
                  title = F, nodeLabels = mfs , mar=c(10, 3, 8, 6))

title("Normal Hearing", line = 3, cex.main = 1.3)
semPlot::semPaths(fit50, include = 2, what='path', ask=F, layout='tree3', 
                  style="lisrel", sizeMan=14, sizeMan2 = 5, sizeLat = 16, sizeLat2 = 8,
                  whatLabels='std', edge.color='black',title.color = 'black',
                  edge.label.cex=.7, label.cex = 1, residuals= T, optimizeLatRes = T,
                  rotation = 1, curvePivot = T, intAtSide = F, 
                  sizeInt = 1.5, nCharNodes = 0, intercepts = F,
                  title = F, nodeLabels = mfs , mar=c(10, 3, 8, 6))
title("Hearing Aid Users", line = 3, cex.main = 1.3)
par(mfrow=c(1,1))
dev.off()


# This is for the theoretical model (Figure 1)
# Note, however, that this generated figure was also edited in an image editor software (i.e., Affinity Designer)
freepar <- readRDS("./Figures/for_vis_sep_revision.rds")

edLbls <- seq(1, 25)
tiff('./Manuscript_Files/Figures/Figure-1-rev2.tiff', units="in", width=8, height=4, res=1200, compression = 'lzw')
test <- semPlot::semPaths(freepar, include=2, what='path', edgeLabels=edLbls, ask=F, layout='tree3', 
                  style="lisrel", sizeMan=14, sizeMan2=6, sizeLat = 16, sizeLat2 = 8,
                  whatLabels='name', edge.color='black',title.color = 'black',
                  edge.label.cex=.7, label.cex = .9, residuals= T,
                  rotation = 1, curvePivot=T, intAtSide = F, 
                  sizeInt = 1.5, nCharNodes = 0, intercepts = F,
                  title = F, nodeLabels = mfs , mar=c(8 ,3, 8, 3), fixedStyle = 1)

dev.off()





