source("code/0_utils.R", encoding = "UTF-8")
source("code/1_data.R", encoding = "UTF-8")
library("bootnet")
library("psych")
library("qgraph")

data_network <- data_use %>%
  `names<-`(as.character(1:ncol(.)))

entropy <- function(target) {
  freq <- table(target)/length(target)
  vec <- as.data.frame(freq)[,2]
  vec<-vec[vec>0]
  -sum(vec * log2(vec))
}

entropies <- sapply(data_network, entropy) %>%
  round(3)

IQRS <- sapply(data_network, function(x){IQR(x, na.rm = TRUE)})

keeps <- names(entropies)[! (entropies < 1.3 & IQRS == 0)]

data_entropy <- tibble(var = names(entropies),
                       entropies = entropies,
                       IQRS = IQRS)

data_network_use <- data_network[,keeps]

cmat <- cor(data_network_use %>%
              mutate_all(as.numeric),
            use = "pairwise.complete.obs",
            method = "spearman")

net_0 <- estimateNetwork(data = data_network_use %>%
                           mutate_all(as.numeric) %>%
                           `names<-`(as.character(1:ncol(.))),
                         missing = "pairwise",
                         corMethod = "spearman",
                         default = "EBICglasso")

set.seed(108)

bootstraps <- bootnet(net_0, 
                 nBoots = 1000,
                 type = "nonparametric",
                 nCores = parallel::detectCores())
