##
##  R code to generate all the data for the package
##
##  Call this from the shell script 
##


## Call devtools and load a version of splade package
library("devtools")

## Need to first setwd and remember orig place
here <- getwd()
setwd("../../splade")  # assuming this is where the dev version of package lives

load_all()
##  Now go back to original place 
setwd(here)



##--------------------------------------------------------------------
## Set parameters for setting up sequences and other things
##--------------------------------------------------------------------
setp <- function(num.seq=24,  # total sequences for simulation 
                 num.sub=18,   # number of seqs in subsample 
                 seq.len=5000, 
                 seqfile="dat-seq-sim.txt",      # orig sim seqs
                 seqsubfile="sequences-initial.txt",  # subsample of seqs
                 seqcompfile="sequences-additional.txt", # removed (complement) seqs
                 treefile="tree-original.newick",    # true tree as newick
                 njtreefile="tree-inferred-initial.newick", # nj tree from sample
                 initfolioxfiles="initial",   # foliox name base - initial
                 updatedfolioxfiles="updated", # foliox name base - updated
                 ranseed=1,   #3  4  6
                 seqgen.model="-mHKY",   # model in seq-gen
                 seqgen.scale="-s0.1",   # scale parameter in seq-gen
                 model="F84"  # for tree inference 
                 ) {
   list(num.seq=num.seq, num.sub=num.sub,seq.len=seq.len,
        seqfile=seqfile, seqsubfile=seqsubfile, seqcompfile=seqcompfile,
        treefile=treefile, njtreefile=njtreefile,
        initfolioxfiles=initfolioxfiles, updatedfolioxfiles=updatedfolioxfiles,
        ranseed=ranseed,
        seqgen.model=seqgen.model, seqgen.scale=seqgen.scale, model=model)
}

##--------------------------------------------------------------------
## Simulate tree and sequences and store subsets in external file
##--------------------------------------------------------------------
make_sequences <- function(p) {
   with(p,{
      set.seed(ranseed)           # random seed for simulated trees, sequences
      tree <- ape::rtree(num.seq)    # get random tree in phylo format
      ntree <- ape::write.tree(tree) # convert to newick 
      write(ntree,treefile)          # then record newick in external file
      ## Note that spacing is important in the option string: 
      opt.string <- paste(seqgen.model," -l", seq.len, " ",seqgen.scale ,sep="")
      catn("string:",opt.string)
      seqs <- phyclust::seqgen(opts = opt.string, newick.tree = ntree)
      write(seqs, seqfile)      # write sequences into a file
      sraw <- read.table(seqfile, header=TRUE)  # read seqs back in as text 
      smat <- as.matrix(sraw)   # turn seqs into matrix
      ## Now sample sequences randomly to form initial and additional sequences
      i.subset <- sample(c(1:num.seq), size=num.sub) # generate random indices
      i.comp <- setdiff(c(1:num.seq), i.subset) # non-selected indices [complement]
      sel.seq <- rbind(c(num.sub,seq.len), smat[i.subset,] ) # generate data subset
      write.table(sel.seq, seqsubfile, 
                  col.names=FALSE, row.names=FALSE, quote=FALSE
                  ) 
      comp.seq <- rbind(c(num.seq-num.sub,seq.len), smat[i.comp,]) # take complement
      write.table(comp.seq, seqcompfile, 
                  col.names=FALSE, row.names=FALSE, quote=FALSE)
   })
}

##--------------------------------------------------------------------
## Read in sequences: subsample and the new sequences to be added
##   add sequences one at a time; make a new foliox each time 
##--------------------------------------------------------------------
demo_add_sequence <- function(p) { #, plot.result=TRUE) {
   seq.init <- ape::read.dna(p$seqsubfile, format="sequential") # read initial sequences
   new.seq <- ape::read.dna(p$seqcompfile, format="sequential") # read seqs to be added
   
   dist <- ape::dist.dna(seq.init, model=p$model) # make dist matrix from init sequences
   tree.nj  <- ape::nj(dist)              # make nj tree#

   ## To root the trees, choose arbitrary taxon as outgroup: 
   og <- c("t1")
   tree.nj <- ape::root(tree.nj, outgroup=og) # root the tree 

   ape::write.tree(tree.nj, p$njtreefile) # write INFERRED tree to file

   foliox_init <- phylo2foliox(tree.nj)       # also make foliox from init inferred tree
   write_foliox(foliox_init, p$initfolioxfiles) # and write foliox to files
   
   ## Now add sequences to form new foliox [can take some time depending on parameters]
   foliox_updated <- add_seqs_to_tree(p$seqsubfile,
                                    p$njtreefile,
                                    p$seqcompfile)
   write_foliox(foliox_updated, p$updatedfolioxfiles)   # write updated foliox to files
   foliox_updated # return the updated foliox 
}




##====================================================================
##
##                       GLOBAL
## 
##====================================================================


## -------------------------------------
## Simulated sequences
##  - make tree, make foliox, update foliox by adding more sequences
## -------------------------------------

p <- setp()

make_sequences(p) # simulate sequences, write to files
## and keep in memory 
egfoliox_updated <- demo_add_sequence(p) # add new sequences; write results to files 
egfoliox_initial <- read_foliox(p$initfolioxfiles) # read in file made in demo call above

## Install the foliox data in the package 
usethis::use_data(egfoliox_initial, overwrite = TRUE)
usethis::use_data(egfoliox_updated, overwrite = TRUE)


## -------------------------------------
## Now make the other example folio objects
## -------------------------------------

## Here is a foliox with polytomies 
polytomy_newick <- "( 1, (2, 8, 9, 10, (((3, 4, 7), 5), 6) ) );"
egfoliox_polytomy <- newick2foliox(text=polytomy_newick)


## Here is a foliox with many subdivided edges 
egfoliox_subdivs <- list(name=letters[1:11], address=c(
         "1", "12-2",  "12-3", "4", 
         "12-3.1-5", 
         "12-3.01-6", 
         "12-3.01-6.1-7", 
         "12-3.01-6.11-8", 
         "12-3.01-6.111-9", 
         "12-3.1-5.1-10", 
         "12-3.1-5.01-11"),
         headv = c("1", "2", "3", "3.1", "3.01", "4", "5", "5.1", "5.01",
                   "6","6.1","6.11","6.111", "7", "8","9","10","11","12"),
         weight=c(0.1, 0.2, 0.3, 0.3, 0.3, 0.4, 0.5, 0.5, 0.5,
                  0.6, 0.6, 0.6, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)
         )
class(egfoliox_subdivs) <- "foliox"


## Install the foliox data as R data files in splade/data/ 
usethis::use_data(egfoliox_polytomy, overwrite = TRUE)
usethis::use_data(egfoliox_subdivs, overwrite = TRUE)

