##--------------------------------------------------------------------
## 
##   Stable Phylogenetic Leaf ADdress Encoding (splade) 
## 
##--------------------------------------------------------------------


##--------------------------------------------------------------------
#' Convert from `phylo` to `foliox`
#'
#' Convert a tree in `phylo` format (ape package) to a phylogenetic leaf
#' address (`foliox`) object.
#'
#' @param ape.tree tree as phylo
#' @param elabels vector for labelling edges; if none supplied, use
#'    numbering in ape's phylo
#' @return foliox object
#' @keywords phylo foliox
#' @export
#' @seealso \code{\link{foliox2phylo}, \link{foliox2apex}, \link{newick2foliox}, \link{egfoliox}}
#' @examples
#'
#'  ape.tree <- ape::rtree(16)  # make a random tree using ape
#'  foliox <- phylo2foliox(ape.tree)
#'  plot(foliox)
#'
#'  # Go from newick to phylo to foliox, then back to phylo
#'  newick_tree <- "( 1, (2, 8, 9, 10, (((3, 4, 7), 5), 6) ) );"
#'  atree <- ape::read.tree(text=newick_tree) # convert to phylo format
#'  foliox <- phylo2foliox(atree)  # convert to foliox
#'  foliox
#'  tree <- foliox2phylo(foliox)  # convert back to phylo
#'  plot(tree)
#'
phylo2foliox <- function(ape.tree, elabels = NULL) {
  edges <- ape.tree$edge
  Ntips <- length(ape.tree$tip.label)
  Nnodes <- ape.tree$Nnode
  Nedges <- nrow(edges)
  
  # Generate edge labels if not supplied
  if (is.null(elabels)) {
    edge.labels <- as.character(seq_len(Nedges))
  } else {
    if (length(elabels) != Nedges) {
      stop("Length of 'elabels' must match the number of edges in the tree.")
    }
    edge.labels <- as.character(elabels)
  }
  
  # Mapping from child node to its parent edge label and parent node
  # Use a list indexed by node number; note that ape node numbering uses
  # 1..Ntips for tips and (Ntips+1)..(Ntips+Nnodes) for internal nodes.
  child_to_parent_map <- vector("list", max(edges))
  for (i in seq_len(Nedges)) {
    child_node <- edges[i, 2]
    parent_node <- edges[i, 1]
    child_to_parent_map[[child_node]] <- list(
      label = edge.labels[i],
      parent = parent_node
    )
  }
  
  # Initialise addresses for tips (aligned by tip number)
  address <- character(Ntips)
  
  # Iterate through each tip and build its address by traversing upwards
  for (i in seq_len(Ntips)) {
    current_node <- i # Start at the tip node
    current_address_parts <- character(0)
    
    # Traverse up until we reach the root (a node with no parent in our map)
    while (!is.null(child_to_parent_map[[current_node]])) {
      edge_info <- child_to_parent_map[[current_node]]
      current_address_parts <- c(edge_info$label, current_address_parts)
      current_node <- edge_info$parent
    }
    address[i] <- paste(current_address_parts, collapse = "-")
  }
  
  p <- list(
    name = ape.tree$tip.label,
    address = address,
    headv = edge.labels, # headv in original seems to be all edge labels
    weight = ape.tree$edge.length
  )
  class(p) <- "foliox"
  return(p)
}

##--------------------------------------------------------------------
#' Convert `foliox` to ape's `phylo` Format with Extra Information
#'
#' Convert a `foliox` object to a list containing a tree in ape's `phylo` class
#' and additional information: labels for head vertices, interior node
#' labels, and tip numbers from the `foliox`.
#'
#' @param foliox phylogenetic leaf address object (foliox)
#' @param address.for.tips whether to use addresses as tip labels
#' @return A list including
#' \describe{
#'    \item{`ape`       }{ tree as phylo class} 
#'    \item{`head.label`}{ labels for head vertices}
#'    \item{`intr.label`}{ interior node labels}
#'    \item{`tip.nums`  }{ tip numbers from the foliox}
#' }
#' @keywords foliox phylo
#' @export
#' @seealso \code{\link{foliox2phylo}, \link{phylo2foliox},
#'    \link{newick2foliox}, \link{egfoliox}}
#' @examples
#'  apex <- foliox2apex(egfoliox_initial)
#'  apex$ape   # this is the tree as phylo object
#'  apex       # tree and extra other information
#' 
foliox2apex <- function(foliox,address.for.tips=FALSE){
   # Check valid input
   valid_foliox(foliox)
   valid_boolean(address.for.tips)
   
   r <- foliox2raw_edges(foliox)
   q <- resolve_rawedges(r)
   a <- edge2apex(q,foliox, address.for.tips) 
   a  # return the (extended) ape tree object with extra bits of info
}

##--------------------------------------------------------------------
#' Convert `foliox` Object to ape's `phylo` Format
#'
#' Convert phylogenetic leaf address (`foliox`) object to a tree in ape's
#' `phylo` format.
#'
#' @param foliox phylogenetic leaf address object (foliox)
#' @param address.for.tips whether to use addresses as tip labels
#' @return a tree as phylo
#' @export
#' @seealso \code{\link{foliox2apex}, \link{phylo2foliox}, \link{newick2foliox}, \link{egfoliox}}
#' @keywords foliox phylo
#' @examples
#'   foliox2phylo(egfoliox_initial)
foliox2phylo <- function(foliox,address.for.tips=FALSE){
   # Check valid input
   valid_foliox(foliox)
   valid_boolean(address.for.tips)
   
   r <- foliox2raw_edges(foliox)
   q <- resolve_rawedges(r)
   a <- edge2apex(q,foliox, address.for.tips)
   a$ape # just return the phylo object without other bits
}

##--------------------------------------------------------------------
#' Read a `newick` File and Convert to `foliox`
#'
#' Read a tree from a file in newick format and convert the tree to foliox.
#' @param tree.file file containing newick format tree
#' @param text newick as text
#' @return foliox object
#' @keywords foliox newick
#' @export
#' @seealso \code{\link{foliox2apex}, \link{phylo2foliox}, \link{foliox2phylo}}
#' @examples
#'  newick.tree.file <- system.file("extdata","tree-inferred-initial.newick",
#'                               package="splade")
#'  foliox <- newick2foliox(newick.tree.file)
#'  summary(foliox)
#'  filenamestem <- tempfile("tree-as-foliox")
#'  # filenamestem <- "tree-as-foliox"  # to save local copy 
#'  write_foliox(foliox, filenamestem) # write to files
#'
#'  newick_tree <- "( (1,2), ((3, 4), 5) );"
#'  newick2foliox(text=newick_tree)
#' 
newick2foliox <- function(tree.file="", text="") {
   # check valid newick
   valid_newick(tree.file, text)
   
   if (tree.file != "") { # newick file name entered [check file first]
      ape.tree <- ape::read.tree(tree.file)
      pl <- phylo2foliox(ape.tree)
   } else if (text != "") { # if newick text supplied 
      ape.tree <- ape::read.tree(text=text)
      pl <- phylo2foliox(ape.tree)
   } else {  # newick text entered
      pl <- NULL
   }
   return(pl)
}

##--------------------------------------------------------------------
#' Convert a jplace object to a foliox object
#'
#' This function reads a jplace file, extracts the reference tree and
#' phylogenetic placements, and integrates the placed sequences into a
#' foliox object. Leverages 'splade' functions
#' `phylo2foliox` and `add_a_leaf_to_foliox`.
#'
#' @param jplace.file Path to the jplace file.
#' @param use.original.tip.labels Whether to use original labels for tips 
#' @param max_placements_to_add Integer, optional. If provided, only the top
#'   `max_placements_to_add` placements (ranked by likelihood if available)
#'   will be added to the tree.
#' @return A `foliox` object representing the reference tree with placed sequences.
#' @importFrom ape read.dna read.tree
#' @importFrom stringr str_split_fixed
#' @importFrom treeio read.jplace as_tibble
#' @export
#' @examples
#' \dontrun{
#'   # Example usage:
#'   # jplace_file <- "path/to/your/placements.jplace"
#'   # result_foliox <- jplace2foliox(jplace_file)
#'   # print(result_foliox)
#' }
jplace2foliox <- function(jplace.file, use.original.tip.labels = TRUE, max_placements_to_add = NULL) {
  jplace_data <- treeio::read.jplace(jplace.file)
  ref_ape_tree <- jplace_data@phylo
  
  # The tibble-like placements object can be converted to a list of row-lists
  placements_tibble <- jplace_data@placements
  placements <- purrr::transpose(as.list(placements_tibble))
  
  if (is.null(ref_ape_tree) || is.null(placements)) { # Check 'placements' after potential conversion
    stop("Failed to read reference tree or placements from jplace file or conversion failed.")
  }
  
  # Ensure edge labels exist for downstream routines
  generate_edge_labels_from_ape_for_jplace(ref_ape_tree)
  
  initial_foliox <- phylo2foliox(ref_ape_tree)
  current_foliox <- initial_foliox
  num_placements_added <- 0
  
  if (length(placements) == 0) {
    return(initial_foliox)
  }
  
  # Process placements to extract relevant info and sort by likelihood
  processed_placements <- list()
  for (p_entry_list in placements) {
    best_p_option <- p_entry_list # Since each row is now a list with named fields
    # Each p_entry_list is a named list representing a row in the placements table
    query_names <- if (!is.null(best_p_option$name)) best_p_option$name else paste0("query_", length(processed_placements) + 1)

    processed_placements[[length(processed_placements) + 1]] <- list(
      name = query_names,
      info = best_p_option
    )
  }
  
  # If likelihood values exist, sort placements by likelihood (descending)
  if ("likelihood" %in% names(processed_placements[[1]]$info)) {
    processed_placements <- processed_placements[
      order(sapply(processed_placements, function(x) x$info$likelihood), decreasing = TRUE)
    ]
  }
  
  # Add placements to foliox
  for (placement_item in processed_placements) {
    if (!is.null(max_placements_to_add) && num_placements_added >= max_placements_to_add) {
      break
    }
    
    query_name <- placement_item$name 
    placement_info <- placement_item$info
    
    # jplace stores edge numbers as 0-based indexes (usually); convert to ape edge index
    jplace_edge_num <- placement_info$edge_num
    pendant_length <- placement_info$pendant_length
    distal_length <- placement_info$distal_length
    
    ape_edge_index <- jplace_edge_num + 1
    
    if (ape_edge_index > nrow(ref_ape_tree$edge) || ape_edge_index <= 0) {
      warning(paste("Invalid ape_edge_index:", ape_edge_index, "(from jplace_edge_num", jplace_edge_num, ") for query:", query_name, ". Skipping placement."))
      next
    }
    
    original_edge_length_in_ape <- ref_ape_tree$edge.length[ape_edge_index]
    
    # headv should be the numeric edge index for downstream functions
    headv_for_add_leaf_func <- ape_edge_index 
    
    calculated_chat <- original_edge_length_in_ape - distal_length
    
    # If chat <= 0, the placement is invalid (pendant + distal lengths exceed edge length), so skip it
    if (calculated_chat <= 0) {
      warning(paste("Calculated chat (", calculated_chat, ") is negative/degenerate for query:", query_name, ". Skipping placement."))
      next # Skip to the next placement
    }
    
    current_foliox <- add_a_leaf_to_foliox(
      headv = headv_for_add_leaf_func,
      foliox = current_foliox,
      leafname = query_name,
      bhat = pendant_length,
      chat = calculated_chat
    )
    num_placements_added <- num_placements_added + 1
  }
    
  return(current_foliox)
}

# --------------------------------------------------------------------
#' Convert a `foliox` object to a standard Newick string with branch lengths.
#'
#' This function converts a phylogenetic leaf address (`foliox`) object
#' into a Newick string suitable
#' Newick without explicit internal node labels, but including branch lengths).
#'
#' @param foliox A phylogenetic leaf address object (class `foliox`).
#' @return A character string representing the Newick tree.
#' @export
#' @examples
#' # Assume foliox_example is a valid foliox object
#' # foliox_example <- list(...)
#' # newick_output <- (foliox_example)
#' # cat(newick_output)
foliox2newick <- function(foliox) {
  # Convert the foliox object to an ape::phylo object
  ape_tree <- foliox2phylo(foliox)  
  # Convert the ape::phylo object to a Newick string
  newick_string <- ape::write.tree(ape_tree)
  return(newick_string)
}



##--------------------------------------------------------------------
#' Create a `foliox` From a Subset of Addresses
#'
#' This takes a `foliox` and the indices of a subset of addresses to extract
#' from the `foliox`, and creates a new `foliox` from it. For compatibility with
#' the original `foliox` the subset carries interior nodes that are unused
#' in the subset.
#'
#' @param foliox phylogenetic leaf address object (foliox)
#' @param subset vector of indices of addresses to extract or vector of names of taxa 
#' @return foliox object
#' @keywords foliox subtree
#' @export
#' @seealso \code{\link{foliox_clade}, \link{egfoliox}}
#' @examples
#'  egfoliox_initial  # inspect this example foliox
#'  foliox_subset(egfoliox_initial, c(1,3,4)) # rebuild foliox with three addresses
#'  foliox_subset(egfoliox_initial, c("t2","t18","t22","t23"))
foliox_subset<- function(foliox, subset){
   ## check valid inputs 
   valid_foliox(foliox)
   if (is.character(subset)) {    # input includes characters
      valid_name(subset,foliox)   # check valid names
      ni <- which(foliox$name %in% subset) # find name indices      
   } else {                       # input is indices 
      valid_subset(subset, foliox)# check valid indices of foliox
      ni <- subset                # input is list of indices
   }
   name.sub <- foliox$name[ni]     # get names of selected
   addr.sub <- foliox$address[ni]  # get addresses of selected

   vert.sub <- c()  # for subset of vertices
   for (i in ni) {
      seg <- unlist(stringr::str_split(foliox$address[i], "-"))
      vert.sub <- c(vert.sub, seg) # collect vertex segments 
   }
   vert.sub <- unique(vert.sub)    # remove duplicate vertices
   w <- which(foliox$headv %in% vert.sub) # where are the selected verts?

   weight.sub <- foliox$weight[w]  # get the weights of selected 
   headv.sub <- foliox$headv[w]    # reorder to match weights
   ## Return the reduced foliox
   p <- list(name=name.sub, address=addr.sub,
             headv=headv.sub, weight=weight.sub)
   class(p) <- "foliox"
   return(p)
}

##--------------------------------------------------------------------
#' Create a New `foliox` From a Clade Within a `foliox` 
#'
#' Extract a clade using the head vertex above it; return clade as a new
#'  `foliox`.
#'
#' @param foliox phylogenetic leaf address object (foliox)
#' @param headv label of head vertex at the clade to be extracted
#' @param remove.prefix exclude the prefix from addresses
#' @return foliox object
#' @keywords foliox clade subtree
#' @export
#' @seealso \code{\link{foliox_subset}, \link{egfoliox}}
#' @examples
#'  plot(egfoliox_initial) 
#'  fc <- foliox_clade(egfoliox_initial, 10) # extract clade with head vertex 10
#'  plot(fc)
foliox_clade <- function(foliox, headv, remove.prefix=TRUE) {
   ## Check for valid input
   valid_vertex_in_folio(foliox,headv)
   valid_foliox(foliox)
   
   getind <- NULL
   suffix <- NULL  # for collection of address suffixes
   prefix <- NULL  # to make the prefix string 
   for (i in 1:length(foliox$address)) {
      segment <- unlist(stringr::str_split(foliox$address[i], "-"))
      stems <- floor(as.numeric(segment))
      headstem <- floor(as.numeric(headv))
      if (headstem %in% stems) { # stem of head found among stems of address
         w <- which(stems %in% headstem)  # where's the stem of interest?
         l <- length(segment) # how many segments in the address?
         if (is.null(prefix)) { # if prefix not made yet, get prefix
            pref.vec <- segment[1:(w-1)]  # vector holding prefix vertices 
            prefix <- stringr::str_flatten(pref.vec, collapse="-")
         }
         if (headv >= segment[w]) {  # headv is closer to the ref
            getind <- c(getind, i) # collect the index of the address
            suffix <- c(suffix, stringr::str_flatten(segment[(w):l], collapse="-"))
         }
      }
   }
   if (headv=="0")
      ## trivial case where entire foliox is returned because
      ##  0 is the reference node at the "top" of the whole tree
      return(foliox) 
   else
      if (remove.prefix) {
         p <- foliox_subset(foliox, getind)
         catn("Removing prefix", prefix)
         k <- which(p$headv %in% pref.vec)  # to remove [since in prefix]
         p$address <- suffix
         p$headv <- p$headv[-k]
         p$weight <- p$weight[-k]
         return(p) 
      } else {
         return(foliox_subset(foliox, getind))
      }
}

##--------------------------------------------------------------------
#' A Plot Method for Class `foliox` 
#'
#' Plot a `foliox` by converting to `phylo` then using ape to render the tree.
#'
#' @param x phylogenetic leaf address object (foliox)
#' @param address.for.tips whether to use addresses as tip labels 
#' @param foliox.node.lab whether to add interior node labels from foliox
#' @param foliox.tip.lab whether to add tip labels from foliox
#' @param node.cex cex factor for foliox nodes 
#' @param tip.cex cex factor for foliox tips 
#' @param ... additional parameters to pass to ape's plot function 
#' @keywords foliox
#' @export
#' @seealso The plot function in ape: {ape::plot.phylo}
#' @examples
#'
#' plot(egfoliox_polytomy, type="cladogram", label.offset=0.005,
#'     show.tip.label=FALSE, foliox.tip.lab=TRUE, foliox.node.lab=FALSE)
#'
#' plot(egfoliox_subdivs, type="unrooted")
#' plot(egfoliox_initial, node.cex=0.75)
#' plot(egfoliox_updated, node.cex=0.75)
#' 
plot.foliox <- function(x, ..., 
                        address.for.tips=TRUE, 
                        foliox.node.lab=TRUE, foliox.tip.lab=FALSE, node.cex=0.8,
                        tip.cex=0.8) {
   foliox <- x
   tree.ex <- foliox2apex(foliox, address.for.tips)
   ape <- tree.ex$ape

#   if (pdfout) {
#      grDevices::pdf(file, width=pdfwh[1], height=pdfwh[2])
#   }
   graphics::par(mar=c(1,1,1,1))

   ape::plot.phylo(ape, ...)
   if (foliox.node.lab) { 
      ape::nodelabels(tree.ex$intr.label, cex=node.cex)
   }
   if (foliox.tip.lab) {
      ape::tiplabels(tree.ex$tip.nums, cex=tip.cex)
   }
#   if (pdfout){
#      grDevices::dev.off()
#   }
}

##--------------------------------------------------------------------
#' Print Method for Class `foliox` 
#'
#' Print `foliox` object.
#'
#' @param x phylogenetic leaf address object (foliox)
#' @param ... other print parameters
#' @keywords foliox 
#' @export
#' @examples
#'  print(egfoliox_polytomy) 
#'  print(egfoliox_subdivs)
#'  print(egfoliox_initial)
#'  print(egfoliox_updated)
#' 
print.foliox <- function(x, ...){
   foliox <- x
   catn("Name and address") 
   base::print(cbind(foliox$name, foliox$address),...)
   catn("Head vertex and weight")
   base::print(cbind(foliox$headv, foliox$weight),...)
}

##--------------------------------------------------------------------
#' Summary Method for Class `foliox` 
#'
#' Summarise `foliox` object.
#'
#' @param object phylogenetic leaf address object (foliox)
#' @param ... other parameters for summary of weights
#' @keywords foliox 
#' @export
#' @examples
#'  summary(egfoliox_polytomy)
#'  summary(egfoliox_subdivs)
#'  summary(egfoliox_initial)
#'  summary(egfoliox_updated)
#' 
summary.foliox <- function(object,...) {
   catn("\nfoliox object" ,deparse(substitute(object)))
   catn("                     Number of leaves : ", length(object$name))
   catn(" Number of vertices (including leaves): ", length(object$headv))
   catn("               Summary of edge weights: ")
   base::print(base::summary(object$weight,...))
   catn("")
}

##--------------------------------------------------------------------
#' Write `foliox` to Two Files
#'
#' Save a `foliox` object to two files with names starting with string
#'   supplied through `filenamestem`. The first file stores names and
#'   addresses, the second stores head vertices and weights (i.e.,
#'   distances). The two file extensions are `.foladdress` and
#'   `.folweight` respectively.
#' 
#' @param foliox phylogenetic leaf address object (foliox)
#' @param filenamestem stem of filename for the two files
#' @keywords foliox write
#' @export
#' @seealso \code{\link{read_foliox}}
#' @examples
#'  filenamestem <- tempfile("initfoliox")
#'  # filenamestem <- "initfoliox"  # to save local copy 
#'  write_foliox(egfoliox_initial, filenamestem)
#' 
write_foliox <- function(foliox, filenamestem=""){
   if (filenamestem=="") {
      print(foliox) 
   } else {
      sink(paste(filenamestem, ".foladdress", sep=""))
      catn("# Name  address") 
      m <- cbind(foliox$name, foliox$address)
      utils::write.table(format(m, justify="left"),
                  row.names=F, col.names=F, quote=F)
      sink()
      
      sink(paste(filenamestem,".folweight", sep=""))
      catn("# Head-vertex   weight")
      m <- cbind(as.character(foliox$headv), foliox$weight)
      utils::write.table(format(m, justify="left"),
                  row.names=F, col.names=F, quote=F)
      sink()
   }
}

##--------------------------------------------------------------------
#' Read `foliox` from two files
#'
#' Read a `foliox` object from two files. The first stores names and
#'    addresses, the second stores head vertices and weights, i.e.,
#'    distances.
#' 
#' @param filenamestem stem or basename of file (reads in two files)
#' @return foliox object 
#' @keywords foliox read
#' @export
#' @seealso \code{\link{write_foliox}}
#' @examples
#'  filenamestem <- tempfile("initfoliox")
#'  # filenamestem <- "initfoliox"  # to save local copy 
#'  write_foliox(egfoliox_initial, filenamestem)
#'  read_foliox(filenamestem)
#' 
read_foliox <- function(filenamestem) {
   addr.name <- paste(filenamestem, ".foladdress", sep="")
   weight.name <- paste(filenamestem, ".folweight", sep="")
   
   address <- utils::read.table(file=addr.name, header=F)
   names(address) <- c("name", "address")
   
   weight <- utils::read.table(file=weight.name, header=F)
   names(weight) <- c("headv", "weight")
   
   pp <- list(name=address$name, address=address$address,
              headv=weight$headv, weight=as.numeric(weight$weight))
   class(pp) <- "foliox"
   return(pp) 
}

##--------------------------------------------------------------------
#' Distance From Reference Node to a Vertex
#' 
#' Find distance from the reference node to any vertex, including a leaf.
#'
#' @param vert vertex label in foliox
#' @param foliox phylogenetic leaf address object (foliox)
#' @return distance computed from edge weights
#' @keywords foliox distance vertex
#' @export
#' @seealso \code{\link{distance_between}, \link{divergence_point}}
#' @examples
#'  distance_to_vertex(9,egfoliox_initial)
distance_to_vertex <- function(vert,foliox) {
   # Check for valid input
   valid_vertex_in_folio(foliox,vert)
   valid_foliox(foliox)
   
   edges <- foliox$headv
   weights <- foliox$weight
   edge.stems <- floor(as.numeric(edges))
   if (vert==0) {  # special case: start and end at ref
      return(0)
   } # otherwise continue...
   L <- length(foliox$address)
   for (i in 1:L) {
      a <- stringr::str_split(foliox$address[i], pattern="-")[[1]] # split by -
      if (vert %in% a) {
         break # stop when first one found 
      }
   }
   pos <- match(vert,a)  # where is vertex in the address?
   cw <- 0  # initialise cumulative weights 
   for (i in 1:pos) {
      ai <- as.numeric(a[i]) # ith vertex of selected address/prefix
      ## look for same stem [stem is floor(ai)] among all edge stems
      w <- which(edge.stems %in% floor(ai)) # which has same stem? 
      candidate.edges <- as.numeric(edges[w])
      candidate.weights <- weights[w]        # also take the weights
      for (j in 1:length(w)) { # for each edge with common stem 
         ## check if bigger than or equal to a[i]
         if (candidate.edges[j]>= ai) {   # for all edges with higher value
            cw <- cw + sum(candidate.weights[j])  # add up all weights
         }
      }
   }
   cw
}

##--------------------------------------------------------------------
#' Divergence point between two vertices
#' 
#' Find divergence point between two vertices in a `foliox` object. This is
#' the most recent common ancestor (MRCA) if the reference node is the
#' root of the tree.
#'
#' @param v1 the first vertex
#' @param v2 the second vertex
#' @param foliox phylogenetic leaf address object (foliox)
#' @return vertex label from foliox 
#' @keywords foliox divergence vertex
#' @export
#' @seealso \code{\link{distance_between}, \link{distance_to_vertex}}
#' @examples
#'  divergence_point(14,20,egfoliox_initial)
#' 
divergence_point <- function(v1,v2,foliox){
   # Check for valid input
   valid_foliox(foliox)
   valid_vertex_in_folio(foliox,v1)
   valid_vertex_in_folio(foliox,v2)
   
   if (v1==0 || v2==0) { # special case: both at reference
      return(0)  
   }
   L <- length(foliox$address)
   for (i in 1:L) {
      a1 <- as.numeric(stringr::str_split(foliox$address[i], pattern="-")[[1]]) # split by -
      if (v1 %in% a1) {
         break # stop when first one found 
      }
   }
   for (i in 1:L) {  # do same for second vertex
      a2 <- as.numeric(stringr::str_split(foliox$address[i], pattern="-")[[1]]) # split by -
      if (v2 %in% a2) {
         break # stop when first one found 
      }
   }
   dp <- 0  # divergence point init at ref =0 
   m <- min(length(a1),length(a2))
   for (i in 1:m) {
      if (a1[i]==a2[i]) {
         dp <- a1[i]   # <-- current div point
         if (a1[i]==v1 || a1[i]==v2
             || a2[i]==v1 || a2[i]==v2) { # stop if along the route
            break
         }
      } else {
         if (floor(a1[i]) == floor(a2[i])) { # compare stems
            dp <- max(a1[i], a2[i])   # take vert with bigger number
            ## This is critical for subdivided edges
         }
      }
   }
   dp
}

##--------------------------------------------------------------------
#' Compute Distance Between Vertices in `foliox`
#'
#' Compute the distance between two vertices in a `foliox` object. A vertex
#' can be a leaf (tip).
#' 
#' @param v1 the first vertex
#' @param v2 the second vertex
#' @param foliox phylogenetic leaf address object (foliox)
#' @return distance computed from edge weights
#' @keywords foliox distance
#' @seealso \code{\link{distance_to_vertex}, \link{divergence_point}}
#' @export
#' @examples
#'  distance_between(12,8,egfoliox_initial)
#' 
distance_between <- function(v1, v2, foliox) {
   # Check for valid input
   valid_foliox(foliox)
   valid_vertex_in_folio(foliox,v1)
   valid_vertex_in_folio(foliox,v2)
   
   dv1 <- distance_to_vertex(v1, foliox)
   dv2 <- distance_to_vertex(v2, foliox)
   dp <- divergence_point(v1,v2, foliox)
   ddp <- distance_to_vertex(dp, foliox)
   dv1 + dv2 - 2*ddp
}

##--------------------------------------------------------------------
#' Add a Leaf to a `foliox`
#'
#' Attach a leaf to a specified edge in a tree encoded as `foliox` object. The
#' function requires details about the subdivided edge. Note that this
#' adds a single leaf to the tree.
#'
#' @param headv head vertex of edge to attach to
#' @param foliox phylogenetic leaf address object (foliox)
#' @param leafname label of taxon at the leaf to be attached
#' @param bhat length of pendant edge to the leaf
#' @param chat length of subdivided edge from attachment point to tail of edge
# ' @param emc length of subdivided edge from attachment point to head of edge
#' @return foliox object
#' @keywords leaf foliox
#' @seealso \code{\link{place_seqs}}
#' @export
#' @examples
#'   add_a_leaf_to_foliox(10, egfoliox_initial, "new_taxon", 0.02, 0.03)
#'
add_a_leaf_to_foliox <- function(headv, foliox, leafname, bhat, chat) {
   # Check for valid input
   valid_foliox(foliox)
   valid_vertex_in_folio(foliox,headv,allow.zero=FALSE)
   valid_leafname(leafname)
   valid_positive("bhat",bhat)
   valid_positive("chat",chat)

   index <- which(foliox$headv == headv) # get index of headv
   e <- foliox$weight[index]  # length e of the target edge
   emc <- e - chat # e minus c [emc] 
   valid_positive("e - c",emc)  # check if e >= c 

   ## First, exclude degenerate cases in which two of the edges are zero
   if ((bhat==0 && chat==0) || (bhat==0 && emc==0) || (chat==0 && emc==0))
      stop("No new vertex formed.")

   ## Create a new foliox object: 
   new.foliox <- foliox        # Copy original foliox
   new.maxstem <- 1 + floor(max(as.numeric(foliox$headv))) # get next new stem number  
   pre <- prefix_and_tail(headv,foliox)  # get prefix and tail (corr to headv)
   ## Create the new address
   if (chat==0) { # boundary; attaching to the tail
       if (pre$tail.in.prefix) # tail is already in the prefix
           new.address <- paste(pre$prefix, new.maxstem, sep="-")
       else 
           new.address <- paste(pre$prefix, pre$tail, new.maxstem, sep="-")
   } else if (emc==0) {  # other boundary; include headv after prefex
      new.address <- paste(pre$prefix, headv, new.maxstem, sep="-")
   } else {  # not exactly on the head or tail; need new bud 
      newbud <- generate_new_bud(pre$tail, headv)   
      ## Edit the weight on the original headv: 
      new.foliox$weight[new.foliox$headv==headv] <- emc
      ## Add new bud as a new headv: 
      new.foliox$headv <- c(new.foliox$headv, newbud) 
      ## Add weight on (proximal) edge to new bud
      new.foliox$weight <- c(new.foliox$weight, chat)  
      ## construct new address including the new bud
      new.address <- paste(pre$prefix, newbud, new.maxstem, sep="-")
   }
   if (pre$prefix=="") { #blank means it joins ref ("0")
      ## In this case we don't want a "-" at the front 
      new.address <- substr(new.address, 2, nchar(new.address))
   }
   catn("New address is: ", new.address)
   ## Add the new leaf
   new.foliox$name <- c(new.foliox$name, leafname) # new taxon name for leaf
   new.foliox$address <- c(new.foliox$address, new.address) # append new address 
   new.foliox$headv <- c(new.foliox$headv, new.maxstem)
   new.foliox$weight <- c(new.foliox$weight, bhat) # weight on edge to new leaf
   
   new.foliox
}

##--------------------------------------------------------------------
#' Add New Sequences to Enlarge a Tree
#'
#' Enlarge a tree by adding new leaves using new sequences to be appended
#' to an existing set of sequences. The leaf placement algorithm uses the
#' Fitch-Margoliash criterion. The input tree can be either newick or
#' foliox. Return a `foliox`.
#' 
#' @param seq.file file containing sequences in phylip format
#' @param tree.file file containing newick or foliox tree inferred
#'    from the seq.file; if foliox, use filename stem
#' @param new.seq.file file containing new sequences to be added to the
#'    current data in seq.file
#' @param seq.format format of the sequence files, e.g. "sequential", or "fasta"
#' @param tree.format format of the input tree, i.e., "foliox" or "newick"
#' @return foliox object
#' @keywords sequences newick
#' @seealso \code{\link{add_a_leaf_to_foliox}}
#' @export
#' @examples
#' 
#'  # example (simulated) sequences in sequential format 
#'  sequences.initial <- system.file("extdata","sequences-initial.txt",
#'                                    package="splade")
#'  tree.initial <- system.file("extdata","tree-inferred-initial.newick",
#'                               package="splade")
#'  sequences.additional <- system.file("extdata","sequences-additional.txt",
#'                                      package="splade")
#'
#'  newfoliox <- place_seqs(seq.file=sequences.initial,
#'                          tree.file=tree.initial,
#'                          new.seq.file=sequences.additional,
#'                          tree.format="newick")
#' 
#'  folioxname <- tempfile("updated-foliox")
#'  # folioxname <- "updated-foliox"  # to save local copy
#'  write_foliox(newfoliox, folioxname)  # save it 
#'  plot(newfoliox)
#' 
place_seqs <- function(seq.file, tree.file, new.seq.file,
                             seq.format="sequential", tree.format="foliox") {
   ## Read in files and trees 
   seq.curr <- ape::read.dna(seq.file, format=seq.format)
   seq.to.add <- ape::read.dna(new.seq.file, format=seq.format)
   if (tree.format == "foliox") {
      foliox <- read_foliox(tree.file)
   } else { # assuming it is newick 
      tree.curr <- ape::read.tree(tree.file)
      foliox <- phylo2foliox(tree.curr) # first, convert to foliox
   }
   ## Add sequences using Fitch-Margoliash criterion 
   numnew <- dim(seq.to.add)[1]  # num seqs to add
   for (i in 1:numnew) {  # add in listed order
      aas <- place_seq_FM_criterion(seq.curr, seq.to.add[i,], foliox)
      seq.curr <- aas$updated.seq 
      foliox <- aas$foliox
   }
   foliox
}

##--------------------------------------------------------------------
#' Example `foliox` Data
#'
#' Hypothetical data in foliox format for illustrating the use of functions
#'  in the `splade` package. `egfoliox_subdivs` (`foliox` with subdivided edges);
#'  `egfoliox_polytomy` (`foliox` with polytomies); `egfoliox_initial` (an initial
#'  `foliox` before more leaves are added); `egfoliox_updated` (an updated version
#'  of `egfoliox_initial` with leaves added). 
#'
#' @name egfoliox
"egfoliox_subdivs"

#' @rdname egfoliox
"egfoliox_polytomy"

#' @rdname egfoliox
"egfoliox_initial" 

#' @rdname egfoliox
"egfoliox_updated"

