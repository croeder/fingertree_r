# This script explores 2-3 finger trees, using lambda.r
# (exploring typed structures similar to examples of 2-3 finger in haskell)
library(lambda.r)
library(magrittr)
library(rlist)
library(rstackdeque)
library(pryr)
library(igraph)

##############################
## Node type definitions
##############################

# generic node type - each node has a random "id" attribute 
# for later plotting and debugging
Node(...) %::% ... : list
Node(...) %as% { 
  res <- list(...)
  attr(res, "id") <- paste(sample(letters, 4), collapse = "")
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
  return(res)
}

# Deep is the main data type, with a prefix (digit), middle (fingertree of some type, either empty, single, or deep),
# and a suffix (digit)
Deep(prefix, middle, suffix) %::% Digit : FingerTree : Digit : FingerTree
Deep(prefix, middle, suffix) %as% {
  FingerTree(prefix = prefix, middle = middle, suffix = suffix)   
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

# reducing leftward...
# if we're reducing a Node2, we call the reducers' function in the correct grouping on the node's elements
reduce_left(n, r) %::% Node2 : Reducer : .
reduce_left(n, r) %as% {
  r$f(r$f(r$i, n[[1]]), n[[2]])
}


# if we're reducing a Node3, we call the reducers' function in the correct grouping on the node's elements
reduce_left(n, r) %::% Node3 : Reducer : .
reduce_left(n, r) %as% {
  r$f(r$f(r$f(r$i, n[[1]]), n[[2]]), n[[3]])
}


# reducing rightward...
# if we're reducing a Node2, we call the reducers' function in the correct grouping on the node's elements
reduce_right(n, r) %::% Node2 : Reducer : .
reduce_right(n, r) %as% {
  r$f(n[[1]], r$f(n[[2]], r$i))
}


# if we're reducing a Node3, we call the reducers' function in the correct grouping on the node's elements
reduce_right(n, r) %::% Node3 : Reducer : .
reduce_right(n, r) %as% {
  r$f(n[[1]], r$f(n[[2]], r$f(n[[3]], r$i)))
}

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
reduce_left(s, r) %as% r$f(r$i, s[[1]])    # it's just the identity with that element, identity first for left

reduce_right(e, r) %::% Empty : Reducer : .   # if it's an empty tree...
reduce_right(e, r) %as% r$i    # it's just the identity

reduce_right(s, r) %::% Single : Reducer : .   # if it's a single element...
reduce_right(s, r) %as% r$f(r$i, s[[1]])    # it's just the identity with that element, identity first for left




# reduce_left for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
reduce_left(d, r) %::% Digit : Reducer : .
reduce_left(d, r) %as% {
  curr <- r$i
  for(el in d) {
    curr <- r$f(curr, el)
  }
  return(curr)
}

# reduce_right for digits, which can have 1 to 4 elements; again we just call the reducer function with the right grouping
reduce_right(d, r) %::% Digit : Reducer : .
reduce_right(d, r) %as% {
  curr <- r$i
  for(index in rev(seq(1, length(d)))) {
    el <- d[[index]]
    curr <- r$f(el, curr)
  }
  return(curr)
}


# reduce_left for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_left(t, r) %::% Deep : Reducer : .
reduce_left(t, r) %as% {
  prefix_reduced <- reduce_left(t$prefix, r)
  middle_reduced <- reduce_left(t$middle, r)
  suffix_reduced <- reduce_left(t$suffix, r)
  return(reduce_left(Digit(prefix_reduced, middle_reduced, suffix_reduced), r))
}

# reduce_right for deep nodes: recursively reduce, then reduce the reductions (I'm cheating by putting them into a digit and then 
# reducing that)
reduce_right(t, r) %::% Deep : Reducer : .
reduce_right(t, r) %as% {
  prefix_reduced <- reduce_right(t$prefix, r)
  middle_reduced <- reduce_right(t$middle, r)
  suffix_reduced <- reduce_right(t$suffix, r)
  return(reduce_right(Digit(prefix_reduced, middle_reduced, suffix_reduced), r))
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
    t <- add_left(t, Element(el))
  }
  return(t)
}

add_all_right(t, els) %::% FingerTree : . : FingerTree
add_all_right(t, els) %as% {
  for(el in els) {
    t <- add_right(t, Element(el))
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
Element(x) %as% {
  res <- x
  attr(res, "id") <- paste(sample(letters, 4), collapse = "")
  return(res)
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
      NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
    } else {
      for(subthing in t) {
        parentid <- attr(t, "id")
        childid <- attr(subthing, "id")
        EDGE_STACK <<- insert_top(EDGE_STACK, list(parent = parentid, child = childid))
        
        parenttype <- class(t)[1]
        parentlabel <- ""
        NODE_STACK <<- insert_top(NODE_STACK, list(node = parentid, type = parenttype, label = parentlabel))
        
        add_edges(subthing)
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
plot_tree <- function(t1, vertex.size = 4, edge.width = 1) {
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
  
  plot(g, 
       layout = layout_as_tree(g), 
       #layout = layout.reingold.tilford, 
       vertex.label=V(g)$label, 
       #vertex.color = as.integer(as.factor(V(g)$type)),
       vertex.size = vertex.size,
       edge.arrow.size = 0.4,
       asp = 0.4, 
       edge.arrow.mode = 0,
       edge.width = edge.width)
}



# fill one and plot it!
t1 <- Empty()
for(i in seq(1, 45)) {
  if(i %% 100 == 0) {print(i)} # print periodically
  t1 <- add_left(t1, Element(i))
}

plot_tree(t1)



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

app3(s, ts, xs) %::% Single : list : FingerTree : FingerTree
app3(s, ts, xs) %as% add_left(add_all_left(xs, ts), s[[1]])

app3(xs, ts, s) %::% FingerTree : list : Single : FingerTree
app3(xs, ts, s) %as% add_right(add_all_right(xs, ts), s[[1]])

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




## just for figure development
t1 <- Empty()
for(i in sample(toupper(letters[1:6]))) {
  t1 <- add_left(t1, Element(i))
}

plot_tree(t1, vertex.size = 8, edge.width = 1.5)








