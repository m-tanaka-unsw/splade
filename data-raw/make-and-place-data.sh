## Run this shell script to copy generated data files into the
## inst/extdata/ directory of the package. 
##
##  sh copy-into-inst-extdat.sh 
## 


# run the R code to generate data:
R CMD BATCH data-functions.R


# After generating data files, put some into external data section of pkg

# NB "usethis" in the R script already installs some of the data as .rda
# objects within data/ 
#
# The following lines put the other text files in the inst/extdata/ directory

DEST='../inst/extdata/' 


rsync -ai sequences-initial.txt $DEST
rsync -ai sequences-additional.txt $DEST
rsync -ai tree-inferred-initial.newick  $DEST

rsync -ai initial.foladdress  $DEST
rsync -ai initial.folweight  $DEST
rsync -ai updated.foladdress  $DEST
rsync -ai updated.folweight  $DEST
