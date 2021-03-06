#' Wrapper for MAD function
#'
#' 
#' @keywords MAD, tree, phylogenetics, wrapper, oh
#' @export
#' @examples
#' oh.MAD()

oh.MAD <- function(tree = NULL,outfile = "rooted_tree.MAD.newick"){
	#Evaluate if is a tree already
	if( class(tree) == "phylo" ){
		t <- MAD(tree,output_mode = "newick")
		write(t,file = outfile)
		t <- read.tree(outfile)
		return(t)
	}else{
	#Read the tree
		t <- ape::read.tree(tree)
		t <- MAD(t, output_mode = "newick")
                write(t,file = outfile)
                t <- read.tree(outfile)
                return(t)
	}
}
