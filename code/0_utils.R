qgraph_to_ggplot <- function(x, thresh = .3){
  temp_a <- x$graphAttributes$Nodes
  temp_e <- x$Edgelist
  temp_l <- x$layout %>%
    as_tibble()
  
  output <- temp_e %>%
    as_tibble() %>%
    mutate(x = temp_l$V1[from],
           y = temp_l$V2[from],
           xend = temp_l$V1[to],
           yend = temp_l$V2[to],
           label_a = temp_a$names[from],
           label_b = temp_a$names[to],
           fill = temp_a$color[from])
  
  output <- output %>%
    filter(weight > thresh) %>%
    ggplot(aes(x = x, 
               xend = xend, 
               y = y, 
               yend = yend)) +
    geom_curve(aes(size = abs(weight),
                   color = weight),
               alpha = .75,
               curvature = .2) +
    geom_label(aes(fill = fill,
                   label = mstrwrp(label_a, 20)), cex = 3) +
    geom_label(aes(
      x = xend,
      y = yend,
      fill = fill,
      label = mstrwrp(label_b, 20)), cex = 3) +
    theme_void() +
    scale_color_gradient2(low = "darkred", mid = "white", high = "darkgreen") +
    scale_size_area() +
    scale_x_continuous(expand = c(.05,.05)) +
    scale_y_continuous(expand = c(.05,.05)) +
    theme(legend.position = "none")
  
  return(output)
}