# library(SoloTonic, lib.loc = "../data/SoloTonic")
# files_to_source <- list.files(path = "./R/SoloTonic", pattern = "*.R", full.names = TRUE)
# lapply(files_to_source, source)

# source("./R/GeneTonic-pkg.R")
# source("./R/gs_cluster.R")
# require(expm)

#' @export
GetSubNetwork <- function(string_db, STRING_IDs) {
  string_subgraph <- string_db$get_subnetwork(STRING_IDs)
  string_subgraph <- igraph::simplify(string_subgraph)
}

#' @export
ChooseInflation <-
  function(cur_subnetwork,
           mcl_inflation_param = c(1.5, 2, 2.5, 5, 10, 15, 20),
           # mcl_inflation_param = c(1.5),
           size_limits = c(5, 50)
  ) {
    GetNumClustersWithinSize <- function(string_obj) {
      n_in_limit <-
        sum(
          table(string_obj$membership) >= size_limits[1] &
            table(string_obj$membership) <= size_limits[2]
        )
      return(n_in_limit)
    }
    
    cat("\nmcl \n")
    
    mcl_results <-
      mclapply(mcl_inflation_param, function(x)
        #SoloTonic::cluster_markov(
        GeneTonic::cluster_markov(
       # cluster_markov(
          cur_subnetwork,
          allow_singletons = T,
          mcl_inflation = x
        ), mc.cores =  parallel::detectCores() )
    
    
    cat("Finished running MCL clustering in parallel at",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        "\n")
    
    optimal_inflation_ind <-
      which.max(sapply(mcl_results, function(x)
        GetNumClustersWithinSize(x)))
    print(paste0(
      'tested inflation:',
      paste0(mcl_inflation_param, collapse = ','),
      ' best = ',
      mcl_inflation_param[optimal_inflation_ind]
    ))
    optimal_mcl_clusters <- mcl_results[[optimal_inflation_ind]]
    
    optimal_mcl_clusters_list <-
      lapply(unique(optimal_mcl_clusters$membership), function(x)
        optimal_mcl_clusters$names[optimal_mcl_clusters$membership == x])
    
    return(optimal_mcl_clusters_list)
  }

# cat("run mcl")
# RunMCL <-
#   function(string_db,
#            genes_to_incl,
#            iter_limit = 2,
#            cur_iter = 1,
#            size_limits = c(5, 50)) {
#     if (cur_iter <= iter_limit) {
#       cat("\nCurrent iter: ", cur_iter)
#       
#       cur_subnetwork <- GetSubNetwork(string_db, genes_to_incl)
#       cur_clusters_list <-
#         ChooseInflation(cur_subnetwork, size_limits = size_limits)
#       
#       if (length(cur_clusters_list) == 1) {
#         print('MCL failed to Re-cluster')
#         return(list())
#       }
#       
#       cur_clusters_sizes <- purrr::map_int(cur_clusters_list, length)
#       overlimit_clusters <-
#         cur_clusters_list[cur_clusters_sizes > size_limits[2]]
#       print(paste0(length(overlimit_clusters), ' overlimit clusters'))
#       
#       within_limit_clusters <-
#         cur_clusters_list[cur_clusters_sizes <= size_limits[2] &
#                             cur_clusters_sizes >= size_limits[1]
#         ]
#       print(paste0(length(within_limit_clusters), ' within-limit clusters'))
#       
#       if (length(overlimit_clusters) > 0) {
#         return(c(
#           within_limit_clusters,
#           unlist(
#             mclapply(overlimit_clusters, function(x)
#               RunMCL(
#                 string_db = string_db,
#                 genes_to_incl = x,
#                 iter_limit = iter_limit,
#                 cur_iter = cur_iter + 1,
#                 size_limits = size_limits
#               ),
#               mc.cores = parallel::detectCores()),
#             recursive = F
#           )
#         ))
#       } else {
#         return(within_limit_clusters)
#       }
#     } else {
#       print(paste0('Iteration Limit Reached'))
#       return(list())
#     }
#   }

