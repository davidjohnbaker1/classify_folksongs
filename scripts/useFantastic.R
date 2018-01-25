#======================================================================================================
# Use Fantastic
#======================================================================================================
setwd("projects/classify_folksongs/data/symbolicData/densmore_bin/csv/")
list.files()

densmoreFeatures <- compute.features(melody.filenames = list.files(pattern=".csv"), 
                                     dir = ".",
                                     use.segmentation = FALSE, 
                                     write.out = TRUE)
library(data.table)
fwrite(densmoreFeatures,"CompleteDensmore.csv")
