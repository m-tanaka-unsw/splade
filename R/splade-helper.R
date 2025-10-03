

## Helper functions for splade package
## For functions that are not exported 

##--------------------------------------------------------------------
## cat with a line ending
##--------------------------------------------------------------------
catn <- function(...){
   cat(...,"\n")
}
##--------------------------------------------------------------------



##--------------------------------------------------------------------
##  Overall structure of conversion from foliox:
##
##   foliox2raw_edges: foliox --> raw edge list
##   resolve_rawedges: raw edges --> edges with resolved subdivided edges
##   edge2apex:  edge list --> ape tree + other bits
## 
##--------------------------------------------------------------------
##  Make RAW (first-pass) edge list from ALL leaf addresses with our
##  foliox labelling system. Allows for subdivided edges.
## 
##  Output: matrix with columns:
##    1:tail  2:head  3:stem  4:terminal(1/0)
##    5:corresponding index of name in foliox [needed later by another fn]
##    6:weight if it is supplied
##--------------------------------------------------------------------
foliox2raw_edges <- function(foliox) {
   edge <- c()       # matrix of edges as (tail,head) nodes 
   numtips <- length(foliox$name)   # number of taxa
   for (i in 1:numtips) { # for each leaf
      segment <- unlist(stringr::str_split(foliox$address[i], "-")) 
      L <- length(segment)   # how many edge segments? 
      for (j in 1:L) {       # for each component (edge address)
         if (segment[j] %in% edge[,2]) { # not found yet 
            ## do nothing
         } else {
            if (j==1) {
               tail <- "0"  # reference vertex
            } else {
               tail <- segment[j-1]
            }
            if (j==L)  # record if terminal vertex (leaf/tip)
               terminal <- 1
            else
               terminal <- 0
            head <- segment[j]
            stem <- floor(as.numeric(head))
            k <- which(foliox$headv==head)
            w <- foliox$weight[k]
            ## col 6 stores w, weight of corr edge
            ## if weights don't exist w is added as NULL [ie not added]
            edge <- rbind(edge, c(tail,head,stem,terminal,i,w)) 
         }
      }
   }
   edge
}


##--------------------------------------------------------------------
## Divide edges grouped with the same stem 
##  Input: matrix with same stem [column 3 value]
## Output: matrix with new edges based on divisions
##--------------------------------------------------------------------
edge_divide <- function(m){
   newm <- c()
   ## order by second column (head), decreasing order 
   n <- m[order(m[,2],decreasing=T),,drop=FALSE]
   for (i in 1:length(n[,1])){
      if (i==1){
         newm <- rbind(newm, n[1,])
      } else { 
         newm <- rbind(newm, c(n[i-1,2], n[i,2:ncol(n)]) )
      }
   }
   newm
}

##--------------------------------------------------------------------
## Resolve edges with subdivisions
##   Input: raw edges from foliox2raw_edges
##   Output:
##   mat = edge matrix with columns:
##            1    2    3     4       5    6
##           tail head stem terminal ind weights
##       ind: index of foliox names/addresses
##  tail and head labels retain foliox format 
##  We let another function take foliox-related steps
##--------------------------------------------------------------------
resolve_rawedges <- function(redge){
   subdive <- c()  # matrix of subdivided edges
   divstem  <- c() # stems of subdivided edges
   k <- c()        # indices to remove later 
   for (i in 1:length(redge[,3])) {
      if (grepl("[.]", redge[i,2])) { # if dot in the head label
         if (redge[i,3] %in% divstem){  # stem found in list
            ## do nothing;  stem is already there
         } else { # has not been dealt with yet; do it now
            divstem <- c(divstem, redge[i,3])     # collect new stem
            w <- which(redge[,3] %in% redge[i,3]) # find matching stems
            m <- redge[w,,drop=FALSE]     # form a matrix with the common stems
            k <- c(k, w)       # collect indices to remove later 
            subdive <- rbind(subdive, edge_divide(m)) # collect new subdiv edges
         }
      }
   }
   ## new matrix, atomised edges
   if (is.null(k)) {
      mat <- redge  # no subdivided edges; -k doesn't make sense here
   } else { 
      mat <- rbind(redge[-k,], subdive)
   }
   mat
}

##--------------------------------------------------------------------
## Convert edges from foliox to tree in ape's phylo class 
## 
## Input: m = edges fully resolved with respect to subdivisions
##            [ie, output of resolve_rawedges]
##     foliox = foliox that corresponds to m 
##     address.for.tips = whether to put addresses into ape labels
## 
## Output: ape tree (phylo class) and
##         head vertex labels
##         interior node labels
##         tip numbers
## NB:
##   ape object is of class "phylo" and includes
##       edge = matrix of edges
##       tip.label = names of taxa
##       Nnode = number of interior nodes
##   The ape tree has its own internal numbering system:
##   if T is the number of taxa (leaves/tips), 1:T for the taxa, and
##   the root is T+1; the interior nodes get subsequent integers. 
##
##--------------------------------------------------------------------
edge2apex <- function(m, foliox, address.for.tips){
   ## Compute number of internal vertices:
   num.int <- length(foliox$headv) + 1 -length(foliox$name) 

   ape.edge <- NULL       # initialise edge matrix 
   tips <- m[m[,4]==1,,drop=FALSE]   # separate terminal edges 
   intr <- m[m[,4]==0,,drop=FALSE]   # separate interior edges
   n <- rbind(tips,intr)  # reconstitute the matrix
   tip.labels <- tips[,2]
   int.labels <- intr[,2]
   ape.nodes <- c(tip.labels, "0", int.labels) # index is ape number
   for (i in 1:length(n[,1])) {
      tail <- match(n[i,1], ape.nodes) # find tail in list of nodes
      head <- match(n[i,2], ape.nodes) # find head in list of nodes
      ape.edge <- rbind(ape.edge, c(tail,head))
   }
   ## recover the original corresponding names or address
   if (address.for.tips)
      tlabel <- foliox$address[as.numeric(tips[,5])]
   else 
      tlabel <- foliox$name[as.numeric(tips[,5])]
   if (ncol(n)==6)  # if edge weight exists in column 6 
      ape.tree <- list(edge=ape.edge,
                       tip.label=tlabel,
                       Nnode=num.int,
                       edge.length=as.numeric(n[,6]) # add edge.length to tree object
                       )
   else # if no weight column, don't include it: 
      ape.tree <- list(edge=ape.edge,
                       tip.label=tlabel,
                       Nnode=num.int)
   class(ape.tree) <- "phylo"
   list(ape=ape.tree                # tree in ape format 
      , head.label=n[,2]            # label of head vertex
      , intr.label=c("0",intr[,2])  # interior node labels 
      , tip.nums=tip.labels)        # tip numbering from OUR matrix 

}


##--------------------------------------------------------------------
## Internal Helper: Generate Edge Labels from APE Tree for jplace Mapping
## Input:
##  - ape.tree: An ape::phylo object
## Output:
##  - A list mapping child node numbers to their corresponding edge information
##--------------------------------------------------------------------
#' Internal Helper: Generate Edge Labels from APE Tree for jplace Mapping
#'
#' This internal function constructs a mapping of edge labels from an `ape::phylo`
#' tree that mirrors the default labeling scheme used by `phylo2foliox`.
#' This is crucial for correctly interpreting `jplace` `edge_num` values as
#' `headv` (edge labels) when adding new leaves to a `foliox` object.
#'
#' @param ape.tree An object of class `phylo` (from the `ape` package) representing
#'   the reference phylogenetic tree.
#' @return A list mapping child node numbers to their corresponding edge information,
#'   including the `headv` label (as a character string of the 1-based edge index
#'   in the `ape.tree$edge` matrix), the parent node number, and the original
#'   edge index.
#' @keywords internal foliox jplace
generate_edge_labels_from_ape_for_jplace <- function(ape.tree) {
  edges <- ape.tree$edge
  Nedges <- nrow(edges)
  
  # Labels edges sequentially based on their order in ape.tree$edge
  edge.labels <- as.character(seq_len(Nedges))
  
  # Create a map from the child node to its edge information
  child_node_to_edge_info_map <- vector("list", max(edges))
  for (i in seq_len(Nedges)) {
    child_node <- edges[i, 2]
    parent_node <- edges[i, 1]
    child_node_to_edge_info_map[[child_node]] <- list(
      label = edge.labels[i], # This is the 'headv' label for this edge
      parent = parent_node,
      edge_index = i # The 1-based index in ape.tree$edge, matching jplace_edge_num
    )
  }
  return(child_node_to_edge_info_map)
}


##====================================================================
## 
##  INFERRING WHERE TO ADD A LEAF USING THE FITCH-MARGOLIASH CRITERION 
## 
##  -- an implementation of method very similar to the APPLES method but
##     using foliox construction
## 
##====================================================================

##--------------------------------------------------------------------
##   ingroup vs outgroup 
##--------------------------------------------------------------------
## Establish the ingroup and the outgroup AND find the tail of the query edge
## Input: foliox and edge, where edge is labelled by the head vertex.
## Approach for in/out group:
##  -find all edges with the same stem
##  -get all addresses containing any headv smaller than or equal to query headv
## NB like other functions, this doesn't check if edge is legitimate
##  btw 0 gives the opposite result to what's expected. but we should never put 0 in
##--------------------------------------------------------------------
get_in_out <- function(edge,foliox){
   in.headv <- NULL
   out.headv <- NULL
   in.name <- NULL; out.name <- NULL
   stem.family <- NULL
   candidate.tail <- NULL
   L <- length(foliox$address)
   for (i in 1:L) {
      a <- stringr::str_split(foliox$address[i], pattern="-")[[1]] # split by -
      if (edge==a[1])        # if query edge is first segment  
         candidate.tail <- 0 # then tail must be the reference
      else if (edge %in% a)  # o/w if query edge found in address vector
         candidate.tail <- a[match(edge,a)-1] # grab previous entry
      stems <- floor(as.numeric(a))  # Now get list of stems
      if (floor(as.numeric(edge)) %in% stems) {  # if same stem found
         m <- match(floor(as.numeric(edge)),stems)  # get its index
         if (edge >= a[m]) { # if query edge is bigger than vertex with same stem
            in.headv <- c(in.headv, a[length(a)]) # collect last segment
            in.name <- c(in.name, foliox$name[i])
         } else { # there's a headv larger than edge but with same stem
            stem.family <- c(stem.family, a[m])
         }
      } else {
         out.headv <- c(out.headv, a[length(a)])
         out.name  <- c(out.name, foliox$name[i])
      }
   }
   if (is.null(stem.family)) { # if no common stems found with larger valued vertex
      tail <- candidate.tail   
   } else  { # o/w look for the right vertex among the stem family
      tail <- min(stem.family) # choose smallest of these
   }
   list(in.name=in.name, out.name=out.name,
        in.headv=in.headv, out.headv=out.headv,
        tail=tail)
}

##--------------------------------------------------------------------
## Extract the distances from a selected edge to leaves in the
##  ingroup and outgroup
##--------------------------------------------------------------------
get_fi_fj <- function(edge,foliox) {
   part <- get_in_out(edge,foliox)  # partition tree: ingroup, outgroup
   ing <- part$in.headv
   outg <- part$out.headv
   tail <- part$tail
   fi <- c()
   fj <- c()
   for (i in 1:length(ing)) {
      fi <- c(fi, distance_between(edge,ing[i],foliox))
   }
   for (j in 1:length(outg)) {
      fj <- c(fj, distance_between(tail,outg[j],foliox))
   }
   list(fi=fi, fj=fj,
        in.name=part$in.name,
        out.name=part$out.name)                                        
}

##--------------------------------------------------------------------
## Get distance between input seq and all others
## Output: distances and sequence labels
##--------------------------------------------------------------------
dist_seq_next <- function(seq, next.seq, distmodel){
   ## Use the sequences and tree to compute FM 
   numseqs <- dim(seq)[1]  # get num sequences
   ## Now get distances from next.seq to the existing sequences
   dist <- c() 
   for (i in 1:numseqs) {
      m11 <- rbind(seq[i,], next.seq)
      dist <- c(dist, ape::dist.dna(m11, model=distmodel))
   }
   list(dist=dist, lab=labels(seq))
}

##--------------------------------------------------------------------
## Compute optimised Fitch-Margoliash criterion value for a given edge
## Inputs:
##   - foliox (so it doesn't need to be recomputed many times)
##   - selected edge
##   - distance: new sequence to all other sequences
##   - lab: labels of existing sequences
## Output:
##   - the Fitch-Margoliash value
##   - estimates of b, c, e-c
##
## With constraint to keep estimate on edge e 
##  That is, we impose condition 0 <= c <= e
##--------------------------------------------------------------------
edge_fitch_margoliash <- function(foliox, selected.edge, dist, lab) {
   ## Use the sequences and tree to compute FM 
   e <- foliox$weight[foliox$headv==selected.edge]  # weight on selected edge 
   in.out <- get_fi_fj(selected.edge,foliox)   # get fi, fj for in,out-groups
   fi <- in.out$fi
   fj <- in.out$fj
   ## Now get the distances for in,out-groups
   ##   lab is a vector of sequence names
   di <- rep(NA, length(fi))
   dj <- rep(NA, length(fj))
   for (i in 1:length(fi)) { # for each of the fi
      w <- match(in.out$in.name[i], lab) #where's matching entry among dists?
      di[i] <- dist[w]
   }
   for (j in 1:length(fj)) { # for each of the fj
      w <- match(in.out$out.name[j], lab) #where's matching entry among dists?
      dj[j] <- dist[w]
   }
   ## Estimate b-c and b+c
   b.minus.c <- sum((di-fi-e)/di^2)/sum(1/di^2)
   b.plus.c <- sum((dj-fj)/dj^2)/sum(1/dj^2)
   ## and then \hat{b} and \hat{c}
   bhat <- max( (b.minus.c + b.plus.c)/2, 0) # prevent negative value
   chat <- (b.plus.c - b.minus.c)/2
   ## Now constrain the values 
   if (chat<0)
      chat <- 0
   else if (chat>e)
      chat  <- e
   ## else { } 
   est <- c(bhat, chat, e-chat)  #  cannot be negative 
   fm <- sum((di-fi-e-(bhat-chat))^2/di^2) + sum((dj-fj-(bhat+chat))^2/dj^2)
   list(fm=fm, est=est)
}


##--------------------------------------------------------------------
## Go through each edge and check edge FM value 
## Input:
##   - seq in ape's DNAbin format
##   - a single seq to add 
##   - foliox 
## Output:
##  optimal edge and other optimised stuff: 
##   - proposed edge to attach next leaf to
##   - value of FM at that edge - minimum among edge FMs
##--------------------------------------------------------------------
optimal_fm_exhaustive <- function(seq, next.seq, foliox, distmodel="F84"){
   pos <- NULL  # to store only positive estimates
   d  <-  dist_seq_next(seq, next.seq, distmodel)
   d0 <- match(0, d$dist) 
   if (length(which(d$dist==0))>0)  { # if there's a zero among d$dist values
      m <- match(0, d$dist)  # where is the zero distance? 
      catn("Zero distance found; added seq matches",d$lab[m]) # corr name
      ax <- foliox2apex(foliox) # make apex 
      mhv <- match(d$lab[m], ax$ape$tip.label) # find name in apex list of tips
      tw <- foliox$weight[foliox$headv==ax$tip.nums[mhv]]  # weight on terminal edge
      opt.edge <- ax$tip.nums[mhv]
      opt.est <- c(0, tw,  0)
      minQ <- 0  # don't bother computing real criterion value 
   } else { # look for placement using FM criterion
      for (i in 1:length(foliox$headv)) {
         efm <- edge_fitch_margoliash(foliox, selected.edge=foliox$headv[i]
                                    , d$dist, d$lab)
         ## put all results in matrix
         pos <- rbind(pos, c(as.numeric(foliox$headv[i]), efm$fm,  efm$est))
      }
      w <- which (min(pos[,2])==pos[,2] )  # take minimal Qfm estimate 
      minQ <- pos[w,2]  # Qfm in second column 
      opt.edge <- pos[w,1]  # headv stored in first column 
      opt.est <- pos[w,3:5]
   }
   list(opt.edge=opt.edge, opt.est=opt.est, minQ=minQ)
}



##====================================================================
##           ATTACHING A LEAF TO A FOLIOX [TO A GIVEN EDGE]
## 
##  Given a leaf and an edge, attach the new leaf to the edge by
##  generating a new entry in a foliox
## 
##====================================================================

##--------------------------------------------------------------------
## Get prefixes and tail of headv accounting for subdivision
##  - get prefix to headv and the pos in the vector
##    [nb we don't know where it is in the pile of addresses]
##  - then go through again and check verts with common stem
##    tail is minimum of those bigger than headv 
##--------------------------------------------------------------------
prefix_and_tail <- function(headv,foliox){
   edges <- foliox$headv
   weights <- foliox$weight
   edge.stems <- floor(as.numeric(edges))  #get stems of all edges
   collection <- NULL
   tail.in.prefix <- FALSE   # whether or not tail occurs in prefix
   L <- length(foliox$address)
   for (i in 1:L) { # for each address 
      a1 <- stringr::str_split(foliox$address[i], pattern="-")[[1]] # split by -
      if (headv %in% a1) {
         break # stop when first one found
      }
   }
   pos <- match(headv, a1) # where is the vertex in the vector?
   if (pos==1) {# edge comes off reference
      prefix <- ""  # if pos-1 doesn't exist make it empty
   } else {
      p <- a1[1:(pos-1)]  # exclude the last one 
      prefix <- stringr::str_flatten(p, collapse="-")  # make prefix string
   }
   ## Now go again 
   for (i in 1:L) { # for each address 
      a <- stringr::str_split(foliox$address[i], pattern="-")[[1]] # split by -
      ## Look for common stem and collect
      if (floor(as.numeric(headv)) %in% floor(as.numeric(a[pos]))) {
         collection <- rbind(collection, a[1:pos]) 
      }
   }
   v <- collection[,pos]  # take column 'pos'
   w <- which(v>headv)  # which elements are bigger than headv?
   if (length(w)>0) {    # subdivisions found
      tail <- min(v[w])  # take smallest vertex label from collection
   } else {
      if (pos==1) { 
         tail <- 0  # must be the reference vertex
      } else {
         tail <- a1[(pos-1)]  # take previous element of selected address
         tail.in.prefix <- TRUE
      }
   }
#   catn("tail in prefix:", tail.in.prefix)
#   list(prefix=prefix, tail=as.character(tail)) 
   list(prefix=prefix, tail=as.numeric(tail), tail.in.prefix=tail.in.prefix) 
}

##--------------------------------------------------------------------
## Takes binary strings representing fractions
##        -- ie, taken from the RIGHT of a decimal point 
## Input: two binary strings with implied floating point to the left
##        MUST be STRINGS with no dots
## Output: string with the final carry attached at highest-value position
##          even if the carry is zero; NO DOT
##--------------------------------------------------------------------
avg_binary_fraction <- function(s1, s2){
   # Check valid input 
   valid_binary_string_input(s1)
   valid_binary_string_input(s2)
   
   ch1 <- stringr::str_split(s1,"")[[1]]
   ch2 <- stringr::str_split(s2,"")[[1]]
   lendiff <- length(ch1) - length(ch2)   # to pad with zeroes
   if (lendiff > 0)  # c1 is longer
      ch2 <- c(ch2, rep("0", lendiff))   # pad with 0
   else if (lendiff < 0) # c2 is longer
      ch1 <- c(ch1, rep("0", -lendiff))
   else { } # equal lengths; do nothing
   carry <- "0"
   result <- NULL
   for (i in length(ch1):1) { # go backwards 
      if (ch1[i]=="0" && ch2[i]=="0") {
         result <- c(carry, result)   # prepend the carry
         carry <- 0   # reset carry for next position
      } else if ((ch1[i]=="1" && ch2[i]=="0") || (ch1[i]=="0" && ch2[i]=="1")) {
         if (carry == "0") 
            result <- c("1", result)  # carry stays 0
         else # carry is 1
            result <- c("0", result)  # carry stays 1 
      } else { # case of both == 1
         result <- c(0, result)
         carry <- 1  # need to carry to next position 
      }
   }
   result <- c(carry, result)
   stringr::str_flatten(result)  # no dot; returns as string 
}
   
##--------------------------------------------------------------------
## Get bud by taking right side of vertex string (right of the dot).
## This allows later manipulation of strings instead of using numerical
##  operations (avoids floating point issues)
##  - input: vertex
##  - output: bud as string; leave out the dot 
##--------------------------------------------------------------------
get_bud <- function(vert){
   s <- as.character(vert)   # ensure it is a string 
   t <- stringr::str_split(s,"[.]")[[1]]
   if (length(t)<2) {  # no dot
      r <- 0
   } else { # if there's a dot, look at the right side
      if (t[2]=="")
         r <- 0
      else 
         r <- t[2]
   }
   as.character(r)
}

##--------------------------------------------------------------------
## Make a new name for a bud using binary average where needed
## input: vertex labels as numeric
## output: new vertex label as string 
##--------------------------------------------------------------------
generate_new_bud <- function(tailv, headv) {
   valid_positive("headv",headv)
   valid_positive("tailv",tailv)

   head.stem <- floor(headv)    # numeric 
   head.bud <- get_bud(headv)   # string

   tail.stem <- floor(tailv)
   tail.bud <- get_bud(tailv)
   if (tail.stem==head.stem) { # if head & tail on the same stem
      ba <- avg_binary_fraction(tail.bud, head.bud)
      new.vert <- as.numeric(paste(head.stem, ba, sep="."))   # create the new bud label 
   } else {  # tail is on a different stem
      new.vert <- as.numeric(paste(head.stem, ".1", head.bud, sep=""))
   }
   as.character(new.vert) # labels better as strings
}

##--------------------------------------------------------------------
## Add a single sequence to a database using Fitch-Margoliash criterion
## Input:
##  - seq: sequences in dnabin format 
##  - add.seq: one sequence in dnabin format
##  - foliox (to add leaf to)
## Output:
##  - the new foliox
##  - updated sequences 
## 
##--------------------------------------------------------------------
place_seq_FM_criterion <- function(seq, add.seq, foliox, loud=TRUE){ 
   next.name <-labels(add.seq)
   if (loud)
      catn("Sequence to attach: ", next.name)
   ## Infer placement of sequence/leaf in tree 
   ofm  <- optimal_fm_exhaustive(seq, add.seq, foliox)
   bhat <- ofm$opt.est[1] # b estimate 
   chat <- ofm$opt.est[2] # c estimate 
   emc <- ofm$opt.est[3]  # e-c estimate
   if (loud) {
      catn("Attached to: ", ofm$opt.edge)
      catn("Estimated distances (b, c, e-c): ", bhat, chat, emc)
   }
#   newfoliox <- add_a_leaf_to_foliox(ofm$opt.edge, foliox, next.name, bhat, chat, emc)
   newfoliox <- add_a_leaf_to_foliox(ofm$opt.edge, foliox, next.name, bhat, chat)
   updated.seq <- rbind(seq, add.seq) # add new sequences to existing ones
   list(foliox=newfoliox, 
        updated.seq=updated.seq) 
}

##--------------------------------------------------------------------
## Checking validity of user inputs 
## Input:
##  - user inputs
## Output:
##  - error message if incorrect input
##  - otherwise return TRUE
## 
##--------------------------------------------------------------------

valid_foliox <- function(foliox) {
   ## Get the name of the calling function
   calling_function <- as.character(sys.call(-1)[[1]])
   if (!inherits(foliox, "foliox")) {
       stop(paste("in function", calling_function,"\n foliox needs to be of class \"foliox\""), call. = FALSE)
   }
   return(TRUE)
}

valid_leafname <- function(leafname) {
   if (!inherits(leafname, "character")) {
      stop("leafname needs to be of class \"character\"")
   }
   return(TRUE)
}

valid_positive <- function(name, length) {
   if (!is.numeric(length) || length < 0) {
      stop(paste(name,"needs to be of class \"numeric\" and non-negative"))
   }
   return(TRUE)
}

valid_emc_and_chat <- function(foliox, headv, emc, chat) {
   e_arg <- emc + chat
   index <- which(foliox$headv == headv)
   if (foliox$weight[index] != e_arg) {
       stop(paste("The provided values of emc and chat are invalid.\n  emc and chat need to sum to \n  e=",foliox$weight[index], "which is the weight associated with head vertex", headv))
   }
   return(TRUE)
}

valid_vertex_in_folio <- function(foliox, vert, allow.zero=TRUE) {
    if (allow.zero) {
        if ( (!is.numeric(vert) && !is.character(vert)) || vert < 0) {
            stop("vertex needs to be a single numeric value greater than or equal to 0")
        }
        if (!(vert %in% foliox$headv) && !(vert==0)) {
            stop("vertex not found in foliox")
        } 
    } else {  # if zero not allowed 
        if ( (!is.numeric(vert) && !is.character(vert)) || vert < 0) {
            stop("vertex needs to be a single numeric value greater than 0")
        }        
        if (!(vert %in% foliox$headv)) {
            stop("Input is not a valid vertex in given foliox")
        }
    }
    return(TRUE)
}

valid_boolean <- function(bool) {
   if (!is.logical(bool)) {
      stop("Input must be a boolean (TRUE or FALSE)")
   }
   return(TRUE)
}

valid_subset <- function(subset, foliox) {
   taxa <- length(foliox$address)
   if (is.list(subset)) { # vector with elements of different type
      stop("List contains non-numeric elements.")
   }    
   if (is.numeric(subset)) { #
      if (length(subset) == 1) {
         if(subset > taxa || subset < 1) {
            stop("Index does not exist in the foliox.")
         }
         return(TRUE)
      } else {
         if(any(duplicated(subset))) {
            stop("List contains duplicate elements.")
         }
         
         if(any(subset > taxa) || any(subset < 1)) {
            stop("Index in the list does not exist in the foliox.")
         }
         return(TRUE)
      }
   } 
   stop("Input must be numeric (single value, vector, or list of numbers).")
}

valid_name <- function(names, foliox){
   if (is.character(names)) {
      if (all(names %in% foliox$name))
         return(TRUE)
   }
   stop("Input must be a character or vector of characters that match names in foliox.")
}

valid_newick <- function(tree.file, text) {
   ape.tree <- NULL
   
   if (tree.file != "") {
      ape.tree <- suppressWarnings(
         try(ape::read.tree(file = tree.file), silent = TRUE)
      )
   } else if (text != "") {
      ape.tree <- suppressWarnings(
         try(ape::read.tree(text = text), silent = TRUE)
      )
   }
   
   if (is.null(ape.tree) || inherits(ape.tree, "try-error")) {
      stop("Invalid Newick format.")
   }
   
   return(TRUE)
}

valid_binary_string_input <- function(str) {
    if (!is.character(str)) {
        stop("Error: input is not a string.")
    }
   # Check if the string contains a decimal point
   if (grepl("\\.", str)) {
      stop("Error: The string contains a decimal point.")
   }
   
   # Check if the string contains digits other than 0 or 1
   if (grepl("[^01]", str)) {
      stop("Error: The string contains digits other than 0 or 1.")
   }
   
   return(TRUE)
}


