cat("run mcl")

#' @export
RunMCL <-
  function(string_db,
           genes_to_incl,
           iter_limit = 2,
           cur_iter = 1,
           size_limits = c(5, 50)) {
    if (cur_iter <= iter_limit) {
      cat("\nCurrent iter: ", cur_iter)
      
      cur_subnetwork <- GetSubNetwork(string_db, genes_to_incl)
      cur_clusters_list <-
        ChooseInflation(cur_subnetwork, size_limits = size_limits)
      
      if (length(cur_clusters_list) == 1) {
        print('MCL failed to Re-cluster')
        return(list())
      }
      
      cur_clusters_sizes <- purrr::map_int(cur_clusters_list, length)
      overlimit_clusters <-
        cur_clusters_list[cur_clusters_sizes > size_limits[2]]
      print(paste0(length(overlimit_clusters), ' overlimit clusters'))
      
      within_limit_clusters <-
        cur_clusters_list[cur_clusters_sizes <= size_limits[2] &
                            cur_clusters_sizes >= size_limits[1]
        ]
      print(paste0(length(within_limit_clusters), ' within-limit clusters'))
      
      if (length(overlimit_clusters) > 0) {
        return(c(
          within_limit_clusters,
          unlist(
            mclapply(overlimit_clusters, function(x)
              RunMCL(
                string_db = string_db,
                genes_to_incl = x,
                iter_limit = iter_limit,
                cur_iter = cur_iter + 1,
                size_limits = size_limits
              ),
              mc.cores = parallel::detectCores()),
            recursive = F
          )
        ))
      } else {
        return(within_limit_clusters)
      }
    } else {
      print(paste0('Iteration Limit Reached'))
      return(list())
    }
  }

