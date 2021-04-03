# This script explores 2-3 finger trees, using lambda.r
# (exploring typed structures similar to examples of 2-3 finger in haskell)
library(lambda.r)
library(igraph)
library(lambdass)  # shorter/nicer anonymous function syntax
library(magrittr)
library(rlist)
library(rstackdeque)
library(pryr)
library(memoise)


##############################
## Node type definitions
##############################

# generic node type - each node has a random "id" attribute 
# for later plotting and debugging
Node(...) %::% ... : list
Node(...) %as% { 
  res <- list(...)
  res@id <- paste(sample(letters, 4), collapse = "")
  res@cache <- new.env(parent = emptyenv())
  return(res)
}

# Node2 and Node3 node types
Node2(x, y) %::% a : a : list
Node2(x, y) %as% Node(x, y)

Node3(x, y, z) %::% a : a : a : list
Node3(x, y, z) %as% Node(x, y, z)


# basic constructor for inheriting (this should probably just inherit from Node, or others should directly
# inherit from node rather than this)
FingerTree(...) %::% ... : list
FingerTree(...) %as% {
  res <- list(...)
  attr(res, "id") <- paste(sample(letters, 4), collapse = "")
  return(res)
}

# empty node type
Empty() %::% FingerTree
Empty() %as% FingerTree(NULL)

# single-element node type
Single(x) %::% . : FingerTree
Single(x) %as% FingerTree(x)

# digits are like nodes, but they allow 1 to 4 elements
Digit(...) %::% ... : list
Digit(...) %as% {
  res <- list(...)
  attr(res, "id") <- paste(sample(letters, 4), collapse = "")
  res@cache <- new.env(parent = emptyenv())
  return(res)
}

# Deep is the main data type, with a prefix (digit), middle (fingertree of some type, either empty, single, or deep),
# and a suffix (digit)
Deep(prefix, middle, suffix) %::% Digit : FingerTree : Digit : FingerTree
Deep(prefix, middle, suffix) %as% {
  res<- FingerTree(prefix = prefix, middle = middle, suffix = suffix)   
  res@cache <- new.env(parent = emptyenv())
  res
}



##############################
## Monoid-annotation
## (actually, generalized, "reducer" annotation, and reduction functions that apply monoids (sorry, reducers),
## to sequences of tags, either from the left or the right)
##############################

# A reducer is a generalized monoid, since it's not actually required that the
# function be fully associative (which thus allows a "reduce_left" and "reduce_right" depending on
# if we want to treat it as left-associative or right-associative)
Reducer(f, i) %::% . : . : list
Reducer(f, i) %as% {
  list(f = f, i = i)
}

# # reducing leftward...
# # if we're reducing a Node2, we call the reducers' function in the correct grouping on the node's elements
# reduce_left(n, r) %::% Node2 : Reducer : .
# reduce_left(n, r) %as% {
#   cat("\n")
#   str(n)
#   r$f(r$f(r$i, n[[1]]), n[[2]]) # or: r$i %>% r$f(n[[1]]) %>% r$f(n[[2]])
# }
# 
# 
# # if we're reducing a Node3, we call the reducers' function in the correct grouping on the node's elements
# reduce_left(n, r) %::% Node3 : Reducer : .
# reduce_left(n, r) %as% {
#   r$f(r$f(r$f(r$i, n[[1]]), n[[2]]), n[[3]])
# }

# TODO: the cache doesn't seem to be working according to these timing tests,
# but I'm not sure why
reduce_left(n, r) %::% Node : Reducer: .
reduce_left(n, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(n@cache[[cache_value]])) { return(n@cache[[cache_value]]) }
  curr <- r$i
  for(el in n) {
    el_reduced <- reduce_left(el, r)
    curr <- r$f(curr, el_reduced)
  }

  n@cache[[cache_value]] <- curr
  return(curr)
}


# # reducing rightward...
# # if we're reducing a Node2, we call the reducers' function in the correct grouping on the node's elements
# reduce_right(n, r) %::% Node2 : Reducer : .
# reduce_right(n, r) %as% {
#   r$f(n[[1]], r$f(n[[2]], r$i))
# }

reduce_right(n, r) %::% Node : Reducer: .
reduce_right(n, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(n@cache[[cache_value]])) { return(n@cache[[cache_value]]) }
  curr <- r$i
  for(el in rev(n)) {
    el_reduced <- reduce_right(el, r)
    curr <- r$f(el_reduced, curr)
  }

  n@cache[[cache_value]] <- curr
  return(curr)
}


# # if we're reducing a Node3, we call the reducers' function in the correct grouping on the node's elements
# reduce_right(n, r) %::% Node3 : Reducer : .
# reduce_right(n, r) %as% {
#   r$f(n[[1]], r$f(n[[2]], r$f(n[[3]], r$i)))
# }

# small test code
# r1 <- Reducer(function(a, b) {b}, 0)
# n2 <- Node2(5, 7)
# n3 <- Node3(3, 7, 4)
# print(reduce_left(n2, r1)) # should be 7
# print(reduce_left(n3, r1)) # should be 4
# print(reduce_right(n2, r1)) # should be 0 (reduce isn't symmetric if the function isn't associative)
# print(reduce_right(n3, r1)) # should be 0
# 
# r2 <- Reducer(function(a, b) {a}, 0)
# n2 <- Node2(5, 7)
# n3 <- Node3(3, 7, 4)
# print(reduce_left(n2, r2)) # should be 0
# print(reduce_left(n3, r2)) # should be 0
# print(reduce_right(n2, r2)) # should be 5
# print(reduce_right(n3, r2)) # should be 3
# 
# r3 <- Reducer(`+`, 0)
# n2 <- Node2(5, 7)
# n3 <- Node3(3, 7, 4)
# print(reduce_left(n2, r3)) # should be 12
# print(reduce_left(n3, r3)) # should be 14
# print(reduce_right(n2, r3)) # should be 12
# print(reduce_right(n3, r3)) # should be 14


# reduce_left and reduce_right for empties and singles; these require utilizing the reducers identity element
reduce_left(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_left(e, r) %as% r$i    # it's just the identity

reduce_left(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_left(s, r) %as% {
  el_reduced <- reduce_left(s[[1]], r)
  r$f(r$i, el_reduced)    # it's just the identity with that element, identity first for left
  }

reduce_left(e, r) %::% Element : Reducer : . # an element can be reduced too
reduce_left(e, r) %as% r$f(r$i, e)


reduce_right(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_right(e, r) %as% r$i    # it's just the identity

reduce_right(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_right(s, r) %as% {
  el_reduced <- reduce_right(s[[1]], r)
  r$f(r$i, el_reduced)    # it's just the identity with that element, identity first for left
}

reduce_right(e, r) %::% Element : Reducer : . # an element can be reduced too
reduce_right(e, r) %as% r$f(r$i, e)




# reduce_left for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
# TODO: this is wrong for general finger trees, this assumes digits always contain data elements, which isn't true: they can contain 2-nodes
# and threenodes as well
# which means we need to distinguish the different cases
# and we need to create reduce_left and reduce_right for nodes
reduce_left(d, r) %::% Digit : Reducer : .
reduce_left(d, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(d@cache[[cache_value]])) { return(d@cache[[cache_value]]) }
  
  curr <- r$i
  for(el in d) {
    el_reduced <- reduce_left(el, r)
    curr <- r$f(curr, el_reduced)
  }

  d@cache[[cache_value]] <- curr
  return(curr)
}


# reduce_right for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
reduce_right(d, r) %::% Digit : Reducer : .
reduce_right(d, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(d@cache[[cache_value]])) { return(d@cache[[cache_value]]) }
  
  curr <- r$i
  for(el in rev(d)) {
    el_reduced <- reduce_right(el, r)
    curr <- r$f(el_reduced, curr)
  }

  d@cache[[cache_value]] <- curr
  return(curr)
}


# reduce_left for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_left(t, r) %::% Deep : Reducer : .
reduce_left(t, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(t@cache[[cache_value]])) { return(t@cache[[cache_value]]) }
  prefix_reduced <- reduce_left(t$prefix, r)
  middle_reduced <- reduce_left(t$middle, r)
  suffix_reduced <- reduce_left(t$suffix, r)
  answer <- reduce_left(Digit(prefix_reduced, middle_reduced, suffix_reduced), r)

  t@cache[[cache_value]] <- answer
  return(answer)
}

# reduce_right for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_right(t, r) %::% Deep : Reducer : .
reduce_right(t, r) %as% {
  cache_value <- paste0(as.character(r), collapse = "")
  if(!is.null(t@cache[[cache_value]])) { return(t@cache[[cache_value]]) }
  
  prefix_reduced <- reduce_right(t$prefix, r)
  middle_reduced <- reduce_right(t$middle, r)
  suffix_reduced <- reduce_right(t$suffix, r)

  answer <- reduce_right(Digit(prefix_reduced, middle_reduced, suffix_reduced), r)
  t@cache[[cache_value]] <- answer
  return(answer)
}


# some tests
# r1 <- Reducer(function(a, b) {b}, 0)
# r2 <- Reducer(function(a, b) {a}, 0)
# r3 <- Reducer(`+`, 0)
# 
# t2 <- Deep(Digit(1, 2, 3), Single(4), Digit(6, 7, 8))
# print(reduce_left(t2, r1))   # should be 8
# print(reduce_left(t2, r2))   # should be 0
# print(reduce_left(t2, r3))   # should be 31
# print(reduce_right(t2, r1))   # should be 0
# print(reduce_right(t2, r2))   # should be 1
# print(reduce_right(t2, r3))   # should be 31


#############################
## Adding entries to the 2-3 finger trees. Here, adding to the left, or adding to the right
#############################


# adding to the left or right for empties returns singles
add_left(e, el) %::% Empty : . : Single
add_left(e, el) %as% {
  Single(el)
}

add_right(e, el) %::% Empty : . : Single
add_right(e, el) %as% {
  Single(el)
}

# adding to a single resutls in a deep (with two digits, each with one element)
add_left(s, el) %::% Single : . : Deep
add_left(s, el) %as% {
  Deep(Digit(el), Empty(), Digit(s[[1]]))
}


add_right(s, el) %::% Single : . : Deep
add_right(s, el) %as% {
  Deep(Digit(s[[1]]), Empty(), Digit(el))
}


# adding to the left of a digit
# this is actually a pain, because we implement digits as lists with various attributes
# we can't just c() the element to the list, as we'd get a list(el, old_list), rather than list(el, old_list[[1]], old_list[[2]]), etc.
# list.prepend from the rlist package takes care of that.
# BUT, it strips off the attributes, including class and our random id. So we grab em, prepend, reset em.
add_left(d, el) %::% Digit : . : Digit
add_left(d, el) %as% {
  oldclasses <- class(d)
  oldid <- attr(d, "id")
  newd <- list.prepend(d, el)
  class(newd) <- oldclasses
  attr(newd, "id") <- oldid
  return(newd)
}

add_right(d, el) %::% Digit : . : Digit
add_right(d, el) %as% {
  oldclasses <- class(d)
  oldid <- attr(d, "id")
  newd <- list.append(d, el)
  class(newd) <- oldclasses
  attr(newd, "id") <- oldid
  return(newd)
}


# Just a helper that can add a collection of elements to the left or right
add_all_left(t, els) %::% FingerTree : . : FingerTree
add_all_left(t, els) %as% {
  for(el in rev(els)) {
    t <- add_left(t, el)
  }
  return(t)
}

add_all_right(t, els) %::% FingerTree : . : FingerTree
add_all_right(t, els) %as% {
  for(el in els) {
    t <- add_right(t, el)
  }
  return(t)
}

# adding to the left of a "deep" fingertree.
# this is where the magic happens; 
# if the prefix digit has 4 elements: then we push off the last to 
# be stored deeper along inside a Node3, and just store the new el and the remaining 1 from the digit in the prefix as a digit.
# otherwise: it's a simple add to the prefix digit
add_left(d, el) %::% Deep : . : Deep
add_left(d, el) %as% {
  if(length(d$prefix) == 4) {
    new_prefix <- Digit(el, d$prefix[[1]])
    new_middle_node <- Node3(d$prefix[[2]], d$prefix[[3]], d$prefix[[4]])
    new_middle <- add_left(d$middle, new_middle_node)
    return(Deep(prefix = new_prefix, middle = new_middle, suffix = d$suffix))
  } else {
    new_prefix <- add_left(d$prefix, el)
    return(Deep(prefix = new_prefix, middle = d$middle, suffix = d$suffix))
  }
}


# symmetric case for add_right
add_right(d, el) %::% Deep : . : Deep
add_right(d, el) %as% {
  if(length(d$suffix) == 4) {
    new_suffix <- Digit(d$suffix[[4]], el)
    new_middle_node <- Node3(d$suffix[[1]], d$suffix[[2]], d$suffix[[3]])
    new_middle <- add_right(d$middle, new_middle_node)
    return(Deep(prefix = d$prefix, middle = new_middle, suffix = new_suffix))
  } else {
    new_suffix <- add_right(d$suffix, el)
    return(Deep(prefix = d$prefix, middle = d$middle, suffix = new_suffix))
  }
}

###############
## Drawing trees with igraph
###############


# in order to convert the tree to an igraph object, we need an obvious way to determine that
# the node is an element (non-recursive case); this could also probably inherit from Node to grab it's random id
Element(x) %::% . : .
Element(x, value = x) %as% {
  res <- x
  res@value <- value
  attr(res, "id") <- paste(sample(letters, 4), collapse = "")
  return(res)
}

print.Element <- function(e) {
   cat("Data Element: \n")
   ecopy <- e
   class(ecopy) <- class(e)[class(e) != "Element"]
   attr(ecopy, "id") <- NULL
   attr(ecopy, "value") <- NULL
   print(ecopy)
   if(!is.null(attr(e, "value"))) {
     if(attr(e, "value") != e) {
       cat("Value: ")
       cat(attr(e, "value"))
       cat("\n")
     }
   }
   cat("\n")
 }

# a fancy recursive function that returns two data frames for building an igraph object out of;
# takes a fingertree, returns a list of edge_dataframe and node_dataframe with edge and node information
get_graph_df <- function(t) {
  
  EDGE_STACK <- rstack()
  NODE_STACK <- rstack()
  
  add_edges <- function(t) {

    if(t %isa% Empty) {
      parentid <- attr(t, "id")
      parenttype <- class(t)[1]
      parentlabel <- ""
      NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
    }
    else if(t %isa% Element) {
      parentid <- attr(t, "id")
      parenttype <- class(t)[1]
      parentlabel <- paste0(as.character(unlist(t)), collapse = ", ")
      if(!is.null(attr(t, "value"))) {
        if(any(attr(t, "value") != t)) {
          parentlabelValue <- paste0(as.character(unlist(attr(t, "value"))), collapse = ", ")
          parentlabel <- paste0(parentlabelValue, "\n", parentlabel)
        }
      }
      NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
    } else {
      if(!is.null(names(t))) {
        # rev() here and below determines the order of addition to the data and thus (apparently)
        # the node ordering 
        for(subthing_name in rev(names(t))) {
          subthing <- t[[subthing_name]]
          parentid <- attr(t, "id")
          childid <- attr(subthing, "id")
          EDGE_STACK <<- insert_top(EDGE_STACK, list(parent = parentid, child = childid, label = subthing_name))
          
          parenttype <- class(t)[1]
          parentlabel <- ""
          NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
          
          add_edges(subthing)
        }
        
      } else {
        index <- 1
        for(subthing in rev(t)) {
          parentid <- attr(t, "id")
          childid <- attr(subthing, "id")
          EDGE_STACK <<- insert_top(EDGE_STACK, list(parent = parentid, child = childid, label = index))
          
          parenttype <- class(t)[1]
          parentlabel <- ""
          NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
          
          add_edges(subthing)
          index <- index + 1
        }
      }
    }
    return(invisible())
  }
  
  add_edges(t)
  
  return(list(
    as.data.frame(EDGE_STACK, stringsAsFactors = FALSE),
    as.data.frame(NODE_STACK, stringsAsFactors = FALSE)
  ))
}

# plotting a tree with igraph, using the get_graph_df() helper
plot_tree <- function(t1, vertex.size = 4, edge.width = 1, label_edges = FALSE, title = NULL) {
  t1_edge_df <- get_graph_df(t1)[[1]]
  t1_node_df <- get_graph_df(t1)[[2]]
  t1_node_df$color <- NA
  t1_node_df$color[t1_node_df$type == "Element"] <- "#ffffb3"
  t1_node_df$color[t1_node_df$type == "Digit"] <- "#8dd3c7"
  t1_node_df$color[t1_node_df$type == "Deep"] <- "#bebada"
  t1_node_df$color[t1_node_df$type == "Empty"] <- "#fb8072"
  t1_node_df$color[t1_node_df$type == "Single"] <- "#80b1d3"
  t1_node_df$color[t1_node_df$type == "Node3"] <- "#fdb462"
  t1_node_df$color[t1_node_df$type == "Node2"] <- "#b3de69"

  g <- graph_from_data_frame(t1_edge_df, vertices = unique(t1_node_df), directed = TRUE)
  
  par(lheight = 0.3)
  plot(g, 
       layout = layout_as_tree(g), 
       #layout = layout.reingold.tilford, 
       vertex.label=V(g)$label, 
       #vertex.color = as.integer(as.factor(V(g)$type)),
       vertex.size = vertex.size,
       edge.arrow.size = 0.4,
       asp = 0.4, 
       edge.arrow.mode = 0,
       edge.width = edge.width,
       edge.label = ifelse(label_edges, t1_edge_df$label, ""),
       main = title,
       vertex.label.family = "Arial"
       )
}



# fill one and plot it!
# t1 <- Empty()
# for(i in seq(1, 45)) {
#   if(i %% 100 == 0) {print(i)} # print periodically
#   t1 <- add_left(t1, Element(i))
# }
# 
# plot_tree(t1)



##############################
## Splitting and concatenating - work in progress
##############################


# takes a list of elements such as: a, b, c, d, e, f, g
# returns a list of nodes such as: list(Node3(a, b, c), Node2(d, e), Node2(f, g))
# a pretty inefficient recursive implementation at the moment
nodes(l) %::% list : list
nodes(l) %as% {
  if(length(l) == 2) { return(list(
                                Node2( l[[1]], l[[2]] )
                              ))}
  if(length(l) == 3) { return(list(
                                Node3( l[[1]], l[[2]], l[[3]] )
                              ))}
  if(length(l) == 4) { return(list(
                                Node2( l[[1]], l[[2]] ),
                                Node2( l[[3]], l[[4]] )
                              ))}

  first = Node3( l[[1]], l[[2]], l[[3]] )
  rest = nodes(l[4:length(l)])
  rest = list.prepend(rest, first)
  return(rest)
}


## concatenation, depending on what kinds of finger trees we want to concatenate we do different things
# each concat function also takes a list of elements to smush between the two trees, useful for later functionality
app3(e, ts, xs) %::% Empty : list : FingerTree : FingerTree
app3(e, ts, xs) %as% add_all_left(xs, ts)

app3(xs, ts, e) %::% FingerTree : list : Empty : FingerTree
app3(xs, ts, e) %as% add_all_right(xs, ts)

app3(x, ts, xs) %::% Single : list : FingerTree : FingerTree
app3(x, ts, xs) %as% add_left(add_all_left(xs, ts), x)

app3(xs, ts, x) %::% FingerTree : list : Single : FingerTree
app3(xs, ts, x) %as% add_right(add_all_right(xs, ts), x)

# the toughy is concatenating two deep trees, a recursive operation
# still needs testing & further work
app3(xs, ts, ys) %::% Deep : list : Deep : FingerTree
app3(xs, ts, ys) %as% {
  Deep(xs$prefix,
       app3(xs$middle, 
            nodes(c(xs$suffix, ts, ys$prefix)), 
            ys$middle),
       ys$suffix)
}

concat(xs, ys) %::% FingerTree : FingerTree : FingerTree
concat(xs, ys) %as% {
  app3(xs, list(), ys)
}

as.FingerTree(l) %::% . : FingerTree
as.FingerTree(l) %as% {
  l <- as.list(l)
  t <- Empty()
  for(el in l) {t <- add_right(t, Element(el))}
  return(t)
}

as.FingerTree(l, v) %::% . : . : FingerTree
as.FingerTree(l, v) %as% {
  l <- as.list(l)
  v <- as.list(v)
  if(length(l) != length(v)) {
    stop("length of entries and values lists given to as.FingerTree not equal.")
  }
  t <- Empty()
  for(i in 1:length(l)) {
    el <- l[[i]]
    value <- v[[i]]
    t <- add_right(t, Element(el, value = value))
  }
  return(t)
}


# ## just for figure development
# t1 <- Empty()
# for(i in sample(toupper(c(letters[1:12], "L")))) {
#   t1 <- add_left(t1, Element(i))
# }
# 
# # manually exchange a 3-node with a 2-node for illustration; 
# # normally 2-nodes are only created during merge operations
# t1$middle$prefix[[1]] <- Node2(Element("B"), Element("F"))
# 
# plot_tree(t1, vertex.size = 8, edge.width = 1.5)



abcs <- as.FingerTree(as.list(letters[1:12]))
xyzs <- as.FingerTree(as.list(letters[16:26]))

#plot_tree(abcs)
#plot_tree(xyzs)

all <- concat(abcs, xyzs)
plot_tree(all, vertex.size = 9, title = "all!")



indices <- sample(1:26)
mix26 <- as.FingerTree(letters, indices)
plot_tree(mix26, vertex.size = 9, title = "valueed")


catter <- Reducer({a; b} %->% {
  Element(paste0(a, b))
  }, Element(""))
print(reduce_right(mix26, catter))



valueMinner <- Reducer({a; b} %->% {
  if(attr(a, "value") < attr(b, "value")) {a} else {b}
}, Element(Inf))
test <- reduce_left(mix26, valueMinner)
print(test)



valueSummer <- Reducer({a; b} %->% {
  Element(paste0(a, b), value = attr(a, "value") + attr(b, "value"))
}, Element("", value = 0) )
test <- as.FingerTree(1:10)
um <- reduce_left(test, valueSummer)
#str(test)
um2 <- reduce_left(test, valueSummer)


# minner <- Reducer({a; b} %->% {
#   cat("\n")
#   str(b)
#   if(a > b) {a} else {b}
# }, Inf)
# test <- reduce_left(mix26, minner)
# cat("-----")
# str(test)



# 
# collector <- Reducer(c, Element(list()))
# consonants <- Reducer(function(a, b) {
#                         vowels <- c("a", "e", "i", "o", "u")
#                         c(a[!a %in% vowels], b[!b %in% vowels])
#                       }, 
#                       Element(c()))
# 
# #plot_tree(mix26)
# print(reduce_left(mix26, consonants) %>% unlist()) 


#### this doesn't work... 

# fs <- as.FingerTree(list(
#   Element(abs),
#   Element(log),
#   Element(sqrt)
# ))
# 
# applyer <- Reducer(function(f1, f2) {
#   function(...) {
#       f1(f2(...))
#     }
#   }, 
# I)
# f_all <- reduce_left(fs, applyer)
# 
# x <- c(3, -5, 2, -4, 1)
# abs(log(sqrt(x)))
# sqrt(log(abs(x)))
# f_all(x)
