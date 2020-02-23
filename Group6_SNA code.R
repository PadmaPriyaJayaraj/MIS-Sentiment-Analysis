install.packages("dplyr")
library(dplyr)
install.packages("igraph")
library(igraph)

setwd("E:/Desktop/MIS")
scripts <- read.csv("1234.csv")

scripts <- scripts %>% select(speaker, listener)
scripts <- scripts %>% filter(listener != "")

length(unique(scripts$speaker))
unique(scripts$speaker)
length(unique(scripts$listener))
unique(scripts$listener)

scripts <- scripts %>% select(speaker, listener)

conversations <- scripts %>% group_by(speaker, listener) %>% summarise(counts = n())


nodes <- c(as.character(conversations$speaker), as.character(conversations$listener))
nodes <- unique(nodes)

my_graph <- graph_from_data_frame(d=conversations, vertices=nodes, directed=FALSE)
my_graph

V(my_graph)$name
E(my_graph)

plot(my_graph, vertex.label.color = "black")
plot(my_graph, vertex.label.color = "black", layout = layout_in_circle(my_graph))
plot(my_graph, vertex.label.color = "black", layout = layout_with_fr(my_graph))
plot(my_graph, vertex.label.color = "black", layout = layout_as_tree(my_graph))
w1 <- E(my_graph)$counts

plot(my_graph, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = sqrt(w1),  
     layout = layout_nicely(my_graph))

my_graph_2more_conv <- delete_edges(my_graph, E(my_graph)[counts < 15])
plot(my_graph_2more_conv, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = sqrt(E(my_graph_2more_conv)$counts),
     layout = layout_nicely(my_graph_2more_conv))


g <- graph_from_data_frame(conversations, directed = TRUE)
g
is.directed(g)

plot(g, 
     vertex.label.color = "black", 
     edge.color = 'orange',
     vertex.size = 0,
     edge.arrow.size = 0.03,
     layout = layout_nicely(g))

neighbors(g, 'Harry Potter', mode = c('all'))
neighbors(g, 'Harry Potter', mode = c('in'))
neighbors(g, 'Harry Potter', mode = c('out'))

n1 <- neighbors(g, 'Harry Potter', mode = c('out'))
n2 <- neighbors(g, 'Dumbledore', mode = c('in'))
intersection(n1, n2)

farthest_vertices(g)
get_diameter(g)  

ego(g, 2, 'Harry Potter', mode = c('out'))
ego(g, 2, 'Harry Potter', mode = c('in'))


g.outd <- degree(g, mode = c("out"))
g.outd

which.max(g.outd)

g.b <- betweenness(g, directed = TRUE)
g.b

plot(g, 
     vertex.label.color = 'black',
     edge.color = 'black',
     vertex.size = sqrt(g.b) / 1.2,
     edge.arrow.size = 0.03,
     layout = layout_nicely(g))


g184 <- make_ego_graph(g, 2, nodes = 'Ron', mode = c("all"))[[1]]
g184
dists <- distances(g184, "Harry Potter")

colors <- c("black", "blue", "orange", "red", "green")
V(g184)$color <- colors[dists+1]
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05)

