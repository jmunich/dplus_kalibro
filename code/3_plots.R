source("code/2_network.R", encoding = "UTF-8")
library("tikzDevice")


# Sample ------------------------------------------------------------------


ages <- data_recode %>%
  select(d_age) %>%
  mutate(d_age = parse_number(d_age)) %>%
  summary()

agessd <- data_recode %>%
  select(d_age) %>%
  mutate(d_age = parse_number(d_age)) %>%
  unlist() %>%
  sd(na.rm = TRUE)

sexes <- data_recode %>%
  select(d_sex) %>%
  group_by(d_sex) %>%
  summarise(cnt = n())

plt_demo <- data_recode %>%
  select(d_sex, d_age) %>%
  mutate(d_age = parse_number(d_age),
         d_sex = ifelse(d_sex == "muž", "Men", "Women")) %>%
  na.omit() %>%
  ggplot(aes(x = d_age)) +
  geom_histogram(show.legend = FALSE, color = "black", fill = "lightgrey") +
  facet_wrap(~d_sex) +
  theme_classic() +
  theme(panel.grid.minor = element_line(),
        panel.grid.major.x = element_line()
  ) +
  labs(y = "\nCount", x = "Age\n") 

plt_location <- data_recode %>%
  select(i_school_org, i_town_size) %>%
  group_by(i_school_org, i_town_size) %>%
  summarise(count = n()) %>%
  na.omit() %>%
  mutate(i_school_org = case_when(i_school_org == "Církev" ~ "Church",
                                  i_school_org == "Obec/stát" ~ "State",
                                  i_school_org == "Soukromý subjekt" ~ "Private"),
         i_town_size = case_when(grepl("Město", i_town_size) ~ "Town\n(up to 100 000 citizens)",
                                 grepl("Velkoměsto", i_town_size) ~ "City",
                                 grepl("Vesnice", i_town_size) ~ "Village")
         ) %>%
  na.omit() %>%
  ggplot(aes(y = i_school_org, x = i_town_size, label = count, fill = count)) +
  geom_tile(show.legend = FALSE, color = "black") +
  geom_text(color = "black", cex = 5) +
  theme_classic() +
  labs(y = "School type\n", x = "\nLocation") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  coord_fixed()

plt_school <- data_recode %>%
  select(i_school, q_level) %>%
  group_by(i_school, q_level) %>%
  summarise(count = n()) %>%
  na.omit() %>%
  mutate(i_school = case_when(i_school == "Gymnázium" ~ "Gymnasium",
                                  i_school == "Jiná střední škola" ~ "High school",
                                  i_school == "Základní škola" ~ "Elementary school"),
         q_level = case_when(grepl("doktorský", q_level) ~ "PhD",
                                 grepl("magisterské", q_level) ~ "Master's degree",
                                 grepl("nižší", q_level) ~ "No university degree",
                                 grepl("bakalářské", q_level) ~ "Bachelor's degree"),
         q_level = ordered(q_level, c("PhD", "Master's degree", "Bachelor's degree", "No university degree"))
         ) %>%
  na.omit() %>%
  ggplot(aes(y = i_school, x = q_level, label = count, fill = count)) +
  geom_tile(show.legend = FALSE, color = "black") +
  geom_text(color = "black", cex = 5) +
  theme_classic() +
  labs(y = "School type\n", x = "\nHighest achieved education of the teacher") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  coord_fixed()

plt_regions <- data_recode %>%
  group_by(i_region) %>%
  mutate(cnt = n(),
         i_region = ifelse(is.na(i_region), "Unknown", i_region)) %>%
  ungroup() %>%
  arrange(cnt) %>%
  mutate(o_region = ordered(i_region, unique(i_region))) %>%
  ggplot(aes(x = o_region)) +
  geom_bar(stat = "count",
           color = "black",
           fill = "lightgrey") +
  coord_flip() +
  theme_classic() +
  theme(panel.grid.minor = element_line(),
        panel.grid.major.x = element_line()
        ) +
  labs(y = "\nCount", x = "Region\n") 


# Items -------------------------------------------------------------------


dat_ent <- data_entropy %>%
  mutate(lab = paste0("Item ", var, "\nEntropy = ", entropies, "\nIQR = ", IQRS),
         lab = ordered(lab, lab))


plt_entropies <- data_entropy %>%
  arrange(entropies) %>%
  group_by(IQRS) %>%
  mutate(tok = 1:n()) %>%
  ungroup() %>%
  ggplot(aes(x = order(entropies), 
             y = entropies,
             color = ordered(IQRS),
             label = var)) +
  geom_text(vjust = -1, color = "black", show.legend = FALSE) +
  geom_point(cex = 5) +
  theme_classic() +
  scale_color_manual(values = c("darkred", "pink", "darkgreen")) +
  theme(legend.position = "bottom") +
  theme(panel.grid.minor.y = element_line(),
        panel.grid.major.y = element_line(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "Items oredered by Shannon entropy", y = "Shannon entropy", color = "Interquartile range") +
  lims(y = c(0.75, 1.75))

plt_hist_a <- data_use %>%
  `names<-`(as.character(1:ncol(.))) %>%
  mutate_all(as.numeric) %>%
  mutate_all(function(x){ifelse(is.na(x), 6, x)}) %>%
  pivot_longer(-0) %>%
  left_join(dat_ent, c("name" = "var")) %>%
  filter(as.numeric(name) < 17) %>%
  mutate(value = as.character(value),
         value = ifelse(value == "6", "Missing", value)) %>%
  ggplot(aes(x = value)) +
  geom_bar(stat = "count",
           color = "black",
           fill = "lightgrey") +
  facet_wrap(~lab, 
             ncol = 4) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  lims(y = c(0, 400))
  

plt_hist_b <- data_use %>%
  `names<-`(as.character(1:ncol(.))) %>%
  mutate_all(as.numeric) %>%
  mutate_all(function(x){ifelse(is.na(x), 6, x)}) %>%
  pivot_longer(-0) %>%
  na.omit() %>%
  mutate(value = ordered(value)) %>%
  left_join(dat_ent, c("name" = "var")) %>%
  filter(as.numeric(name) > 16) %>%
  mutate(value = as.character(value),
         value = ifelse(value == "6", "Missing", value)) %>%
  ggplot(aes(x = value)) +
  geom_bar(stat = "count",
           color = "black",
           fill = "lightgrey") +
  facet_wrap(~lab, 
             ncol = 4) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  labs(y = "\nCount", x = "Value\n")  +
  lims(y = c(0, 400))


# Networks ----------------------------------------------------------------


data_weights_cmat <- tibble(type = "1. Spearman correlation coefficients", 
                            x = as.numeric(cmat[lower.tri(cmat)]))
data_weights_net <- tibble(type = "2. EBICglasso estimates (non-zero edges)", 
                           x = as.numeric(net_0$graph[lower.tri(net_0$graph)]))

plt_coeff <- ggplot(bind_rows(data_weights_cmat, data_weights_net) %>%
         filter(x != 0),
       aes(x = x)) +
  geom_histogram(color = "black", fill = "lightgrey", bins = 30) +
  facet_wrap(~type) +
  labs(x = "Edge weight", y = "Count\n") +
  theme_classic() +
  theme(panel.grid.minor.y = element_line(),
        panel.grid.major.y = element_line()
  )

labs <- names_old[names(data_use)] %>%
  gsub("(.+\\[)(.+)(\\])", "\\2", .)

var_groups <- names_old[names(data_use)] %>%
  gsub("[0-9]{2}\\. (.+)( \\[.+)(\\])", "\\1", .)

set.seed(1)

g_0 <- qgraph(net_0$graph,
              layout = "spring",
              legend.cex = 0.28,
#              nodeNames = labs,
              labels = keeps,
              shape = "square",
              groups = var_groups[as.numeric(keeps)],
              legend = FALSE,
              label.cex = 1.8,
              vsize = 5,
#              GLratio = 1.25,
              palette = "pastel")

ga <- qgraph(cmat %>%
               `diag<-`(0),
             layout = g_0$layout,
             legend.cex = 0.28,
             #              nodeNames = labs,
             labels = keeps,
             shape = "square",
             groups = var_groups[as.numeric(keeps)],
             legend = FALSE,
             label.cex = 1.8,
             vsize = 5,
             #              GLratio = 1.25,
             palette = "pastel")


# Bootnet -----------------------------------------------------------------


bootplot <- plot(bootstraps, order = "sample")

gpl <- ggplot_build(bootplot)

d_a <- gpl$data[[1]]
d_b <- gpl$data[[2]]
d_c <- gpl$data[[3]]

plt_boot_1 <- d_a %>%
  group_by(y) %>%
  arrange(x) %>%
  mutate(type = c("xmin", "xmax")) %>%
  select(y, x, type) %>%
  pivot_wider(id_cols = y, 
              names_from = type, 
              values_from = x) %>%
  left_join(d_b, "y") %>%
  arrange(y) %>%
  ggplot(aes(x = x, 
             y = y, 
             xmin = xmin, 
             xmax = xmax)) +
  geom_ribbon(aes(color = "Bootstrap 95\\% confidence interval"), fill = "lightgrey") +
  geom_point(data = d_c %>% arrange(y), aes(x = x, y = y, color = "Sample"), inherit.aes = FALSE) +
  geom_path(aes(color = "Bootstrap mean")) +
  theme_classic() +
  scale_y_continuous(limits = c(1, max(d_a$y))) +
  scale_color_manual(values = c("grey", "black", "darkred")) +
  theme(panel.grid.minor.x = element_line(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom"
  ) +
  labs(x = "Edge weight",
       y = "Edges ordered by sample estimate",
       color = "")

data_boot_2 <- d_a %>%
  group_by(y) %>%
  arrange(x) %>%
  mutate(type = c("xmin", "xmax")) %>%
  select(y, x, type) %>%
  pivot_wider(id_cols = y, 
              names_from = type, 
              values_from = x) %>%
  left_join(d_b %>% mutate(label = gpl$layout$panel_params[[1]]$y$get_labels()), "y") %>%
  ungroup() %>%
  arrange(y) %>%
  mutate(label = gsub("--", "-", label),
         label = ordered(label, label)) %>%
  filter(abs(xmin) > .15|abs(xmax) > .15)

plt_boot_2 <- data_boot_2 %>%
  ggplot(aes(x = x, 
             y = label, 
             xmin = xmin, 
             xmax = xmax)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_segment(aes(color = "Bootstrap 95\\% confidence interval", 
                   x = xmin, 
                   xend = xmax, 
                   y = label, 
                   yend = label)) +
  geom_point(data = d_c %>% 
               mutate(label = gpl$layout$panel_params[[1]]$y$get_labels(),
                      label = gsub("--", "-", label),
                      label = ordered(label, levels(data_boot_2$label))) %>% 
               filter(label %in% data_boot_2$label), 
             aes(x = x, y = label, color = "Sample"), 
             inherit.aes = FALSE) +
  geom_point(aes(color = "Bootstrap mean")) +
  theme_classic() +
  scale_color_manual(values = c("lightgrey", "black", "darkred")) +
  theme(panel.grid.minor.x = element_line(),
        panel.grid.major.x = element_line(),
        legend.position = "bottom"
  ) +
  labs(x = "Edge weight",
       y = "Edges ordered by sample estimate\n",
       color = "")

edgesel <- data_boot_2$label %>%
  as.character() %>%
  strsplit("-") %>%
  lapply(function(x){`names<-`(x, c("x", "y"))}) %>%
  bind_rows()

temp_mat <- matrix(0, nrow = ncol(net_0$graph), ncol = ncol(net_0$graph)) %>%
  `rownames<-`(rownames(net_0$graph)) %>%
  `colnames<-`(rownames(net_0$graph))

temp_mat[as.matrix(edgesel)] <- net_0$graph[as.matrix(edgesel)]

g_1 <- qgraph(temp_mat,
              layout = g_0$layout,
              directed = FALSE,
              legend.cex = 0.28,
              #              nodeNames = labs,
              labels = keeps,
              shape = "square",
              groups = var_groups[as.numeric(keeps)],
              legend = FALSE,
              label.cex = 1.8,
              vsize = 5,
              #              GLratio = 1.25,
              palette = "pastel")


# Save tex ----------------------------------------------------------------


tikz(file = "plots/report/plt_demo.tex",
     height = 4, 
     width = 6)
plt_demo
dev.off()

tikz(file = "plots/report/plt_regions.tex",
     height = 4, 
     width = 6)
plt_regions
dev.off()

tikz(file = "plots/report/plt_locations.tex",
     height = 4, 
     width = 4)
plt_location
dev.off()

tikz(file = "plots/report/plt_school.tex",
     height = 4, 
     width = 6)
plt_school
dev.off()

tikz(file = "plots/report/plt_hist_a.tex",
     height = 8, 
     width = 6)
plt_hist_a
dev.off()

tikz(file = "plots/report/plt_hist_b.tex",
     height = 8, 
     width = 6)
plt_hist_b
dev.off()

tikz(file = "plots/report/plt_entropies.tex",
     height = 4, 
     width = 6)
plt_entropies
dev.off()

tikz(file = "plots/report/plt_g0.tex",
     height = 6, 
     width = 6)
qgraph(ga)
dev.off()

tikz(file = "plots/report/plt_g1.tex",
     height = 6, 
     width = 6)
qgraph(g_0)
dev.off()

tikz(file = "plots/report/plt_g2.tex",
     height = 6, 
     width = 6)
qgraph(g_1)
dev.off()

tikz(file = "plots/report/plt_boot1.tex",
     height = 6, 
     width = 6)
plt_boot_1
dev.off()

tikz(file = "plots/report/plt_boot2.tex",
     height = 6, 
     width = 6)
plt_boot_2
dev.off()


tikz(file = "plots/report/plt_coeff.tex",
     height = 4, 
     width = 6)
plt_coeff
dev.off()


# Save png ----------------------------------------------------------------


png(file = "plots/readme/plt_demo.png",
    units = "in", 
    res = 1000,
    height = 4,
    width = 6)
plt_demo
dev.off()

png(file = "plots/readme/plt_regions.png",
    units = "in",
    res = 1000,
    height = 4, 
    width = 6)
plt_regions
dev.off()

png(file = "plots/readme/plt_locations.png",
    units = "in",
    res = 1000,
    height = 4, 
    width = 4)
plt_location
dev.off()

png(file = "plots/readme/plt_school.png",
    units = "in",
    res = 1000,
    height = 4, 
    width = 6)
plt_school
dev.off()

png(file = "plots/readme/plt_hist_a.png",
    units = "in",
    res = 1000,
    height = 8, 
    width = 6)
plt_hist_a
dev.off()

png(file = "plots/readme/plt_hist_b.png",
    units = "in",
    res = 1000,
    height = 8, 
    width = 6)
plt_hist_b
dev.off()

png(file = "plots/readme/plt_entropies.png",
    units = "in",
    res = 1000,
    height = 4, 
    width = 6)
plt_entropies
dev.off()

png(file = "plots/readme/plt_g0.png",
    units = "in",
    res = 1000,
    height = 6, 
    width = 6)
qgraph(ga)
dev.off()

png(file = "plots/readme/plt_g1.png",
    units = "in",
    res = 1000,
    height = 6, 
    width = 6)
qgraph(g_0)
dev.off()

png(file = "plots/readme/plt_g2.png",
    units = "in",
    res = 1000,
    height = 6, 
    width = 6)
qgraph(g_1)
dev.off()

png(file = "plots/readme/plt_boot_1.png",
    units = "in",
    res = 1000,
    height = 6, 
    width = 6)
plt_boot_1
dev.off()

png(file = "plots/readme/plt_boot_2.png",
    units = "in",
    res = 1000,
    height = 6, 
    width = 6)
plt_boot_2
dev.off()

png(file = "plots/readme/plt_g2.png",
    units = "in",
    res = 1000,
    height = 6, 
    width = 6)
qgraph(g_1)
dev.off()


png(file = "plots/readme/plt_coeff.png",
    units = "in",
    res = 1000,
    height = 4, 
    width = 6)
plt_coeff
dev.off()


folder <- "plots/report/"

for (x in list.files(folder, pattern = "*.tex")) {
  # full path to file
  file <- paste(folder, "/", x, sep = "")
  # full path to temp file
  temp <- paste(folder, "/", "temp.tex", sep = "")
  # rename source file to temp
  file.rename(file, temp)
  # read input file in correct encoding
  input <- readLines(temp, encoding = "cp1252")
  # convert input to UTF-8
  output <- iconv(input, from = "cp1252", to = "UTF8")
  # write output with original filename
  writeLines(input, con = file(file, encoding = "UTF8"))
  # remove temp file
  file.remove(temp)
  rm(input, output)
}