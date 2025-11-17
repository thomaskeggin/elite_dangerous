# shortlist systems within a specified radius using EDSM data ------------------


sys_within_radius <-
  function(sys_id,     # EDSM system ID
           distance,  # radius in LY
           sys_dist_matrix){
    
    sys_id_char <- as.character(sys_id)
    
    shortlist <- sys_dist_matrix[sys_id_char,][sys_dist_matrix[sys_id_char,]<distance]
    
    shortlist <-
      data.frame(id = as.numeric(names(shortlist)),
                 distance  = shortlist)
    
  }
