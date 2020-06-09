oecd.tiva = function(io, indicator = c()){
  # Checking for correct inputs
  if(class(io) != "InputOutput") stop("io must be of class InputOutput. See ?as.inputoutput")
  # Checking for a balanced inputoutput system
  if(check.RS(io) == FALSE) stop('io must be a balanced system. See ?check.RS')
  
  # Setting up an object to store the results
  ind = list()
  RS = io$RS_label
  
  regions = unique(RS[, 1])
  R = length(regions)
  sectors = unique(RS[, 2])
  S = length(sectors)
  if(R < 2) stop('Trade in value added requires at least two regions')
  regions_f = unique(io$f_label[1, ])
  nf = length(unique(io$f_label[2, ]))
  
  
  # Checking for regions in final demand and exports
  if(!all(regions == regions_f)) stop('The region column in RS_label must match the region row in f_label')
  is_E = FALSE
  if(!is.null(io$E_label)){
    is_E = TRUE
    regions_E = unique(io$regions_E[1,] )
    if(!all(regions == regions_E)) stop('The region column in RS_label must match the region row in E_label')
  }
  is_M = FALSE
  if(!is.null(io$M_label)){
    is_M = TRUE
  }
  
  if(is_E == TRUE){
    nE = length(unique(io$E_label[2, ]))
  }
  
  # Checking for acceptable indicators
  approved = c('PROD', 'VALU', 'EXGR_INT', 'EXGR_FNL', 'EXGR', 'IMGR_INT', 'IMGR_FNL', 'IMGR',
               'BALGR', 'PROD_VASH', 'EXGRpSH', 'IMGRpSH', 'EXGR_DVA', 'EXGR_DDC','EXGR_IDC',
               'EXGR_RIM', 'EXGR_FVA', 'IMGR_DVA', 'REII', 'EXGR_DVASH', 'EXGR_INTDVASH',
               'EXGR_FNLDVASH', 'EXGR_DVAFXSH', 'EXGR_BSCI', 'EXGR_DVApSH', 'EXGR_INTDVApSH',
               'EXGR_FVASH', 'EXGR_TDVAIND', 'EXGR_TFVAIND', 'IMGR_DVASH', 'IMGINT_REII')
  for(i in 1:length(indicator)){
    if(!indicator[i] %in% approved){
      cat('\nYou can choose any combination of the following indicators: \n\n' )
      cat(approved)
      cat('\n\n')
      stop(paste0('"', indicator[i], '"', ' is not an available indicator'))
    } 
  }
  
  
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  ## Structural Indicators
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  
  
  # ===================== #
  ## PROD - Gross output ##
  # ===================== #
  if('PROD' %in% indicator){
    PROD = vector('list', R)
    names(PROD) = regions
    for(r in 1:R){
      PROD[[r]] = matrix(io$X[RS[,1] == regions[r]])
      rownames(PROD[[r]]) = sectors
      colnames(PROD[[r]]) = 'total'
    }
    ind$PROD = PROD
  }
  
  # ==================== #
  ## VALU - Value added ##
  # ==================== #
  if('VALU' %in% indicator){
    VALU = vector('list', R)
    names(VALU) = regions
    
    one = matrix(1, nrow = dim(io$V)[1])
    W = t(t(one) %*% io$V)
    for(r in 1:R){
      VALU[[r]] = matrix(W[RS[,1] == regions[r]])
      rownames(VALU[[r]]) = sectors
      colnames(VALU[[r]]) = 'total'
    }
    ind$VALU = VALU
  }
  
  # ======================================= #
  ## EXGR_INT - Intermediate gross exports ##
  # ======================================= #
  if('EXGR_INT' %in% indicator){
    EXGR_INT = vector('list', R)
    names(EXGR_INT) = regions
    
    for(r in 1:R){
      region = regions[r]
      partners = setdiff(regions, region)
      i_Z = which(RS[,1] == region)
      for(p in 1:length(partners)){
        partner = partners[p]
        j_Z = which(RS[,1] == partner)
        temp = apply(io$Z[i_Z, j_Z], 1, sum)
        EXGR_INT[[r]] = cbind(EXGR_INT[[r]], temp)
      }
      rownames(EXGR_INT[[r]]) = sectors
      colnames(EXGR_INT[[r]]) = partners
    }
    ind$EXGR_INT = EXGR_INT
  }
  
  # ================================ #
  ## EXGR_FNL - Gross final exports ##
  # ================================ #
  if('EXGR_FNL' %in% indicator){
    EXGR_FNL = vector('list', R)
    names(EXGR_FNL) = regions
    
    for(r in 1:R){
      region = regions[r]
      partners = setdiff(regions, region)
      i_Z = which(RS[,1] == region)
      for(p in 1:length(partners)){
        partner = partners[p]
        j_f = which(io$f_label[1,] == partner)
        temp = apply(as.matrix(io$f[i_Z, j_f]), 1, sum)
        if(is_E == TRUE){
          j_E = which(io$E_label[1,] == partner)
          E = io$E
          temp = temp + apply(as.matrix(io$E[i_Z, j_E]), 1, sum)
        }
        EXGR_FNL[[r]] = cbind(EXGR_FNL[[r]], temp)
      }
      rownames(EXGR_FNL[[r]]) = sectors
      colnames(EXGR_FNL[[r]]) = partners
    }
    ind$EXGR_FNL = EXGR_FNL
  }
  
  # ====================== #
  ## EXGR - Gross exports ##
  # ====================== #
  if('EXGR' %in% indicator){
    EXGR = vector('list', R)
    names(EXGR) = regions
    
    for(r in 1:R){
      region = regions[r]
      partners = setdiff(regions, region)
      i_Z = which(RS[,1] == region)
      for(p in 1:length(partners)){
        partner = partners[p]
        j_Z = which(RS[,1] == partner)
        j_f = which(io$f_label[1,] == partner)
        temp = apply(io$Z[i_Z, j_Z], 1, sum)
        temp = temp + apply(as.matrix(io$f[i_Z, j_f]), 1, sum)
        if(is_E == TRUE){
          j_E = which(io$E_label[1,] == partner)
          E = io$E
          temp = temp + apply(as.matrix(io$E[i_Z, j_E]), 1, sum)
        }
        EXGR[[r]] = cbind(EXGR[[r]], temp)
      }
      rownames(EXGR[[r]]) = sectors
      colnames(EXGR[[r]]) = partners
    }
    ind$EXGR = EXGR
  }
  
  # ========================== #
  ## IMGR_INT - Gross imports ##
  # ========================== #
  if('IMGR_INT' %in% indicator){
    IMGR_INT = vector('list', R)
    names(IMGR_INT) = regions
    
    for(r in 1:R){
      region = regions[r]
      partners = setdiff(regions, region)
      j_Z = which(RS[, 1] == region)
      for(p in 1:length(partners)){
        partner = partners[p]
        i_Z = which(RS[, 1] == partner)
        temp = apply(io$Z[i_Z, j_Z], 1, sum)
        IMGR_INT[[r]] = cbind(IMGR_INT[[r]], temp)
      }
      rownames(IMGR_INT[[r]]) = sectors
      colnames(IMGR_INT[[r]]) = partners
    }
    ind$IMGR_INT = IMGR_INT
  }
  
  # ====================== #
  ## IMGR_FNL - Gross imports ##
  # ====================== #
  if('IMGR_FNL' %in% indicator){
    IMGR_FNL = vector('list', R)
    names(IMGR_FNL) = regions
    
    for(r in 1:R){
      region = regions[r]
      partners = setdiff(regions, region)
      j_Z = which(RS[, 1] == region)
      j_f = which(io$f_label[1, ] == region)
      for(p in 1:length(partners)){
        partner = partners[p]
        i_Z = which(RS[, 1] == partner)
        temp = apply(io$f[i_Z , j_f], 1, sum)
        if(is_M == TRUE){
          temp = temp + io$M[j_Z]
        }
        IMGR_FNL[[r]] = cbind(IMGR_FNL[[r]], temp)
      }
      rownames(IMGR_FNL[[r]]) = sectors
      colnames(IMGR_FNL[[r]]) = partners
    }
    ind$IMGR_FNL = IMGR_FNL
  }
  
  # ====================== #
  ## IMGR - Gross imports ##
  # ====================== #
  if('IMGR' %in% indicator){
    IMGR = vector('list', R)
    names(IMGR) = regions
    
    for(r in 1:R){
      region = regions[r]
      partners = setdiff(regions, region)
      j_Z = which(RS[, 1] == region)
      j_f = which(io$f_label[1, ] == region)
      for(p in 1:length(partners)){
        partner = partners[p]
        i_Z = which(RS[, 1] == partner)
        temp = apply(io$Z[i_Z, j_Z], 1, sum)
        temp = temp + apply(io$f[i_Z , j_f], 1, sum)
        if(is_M == TRUE){
          temp = temp + io$M[j_Z]
        }
        IMGR[[r]] = cbind(IMGR[[r]], temp)
      }
      rownames(IMGR[[r]]) = sectors
      colnames(IMGR[[r]]) = partners
    }
    ind$IMGR = IMGR
  }
  
  # ============================= #
  ## BALGR - gross trade balance ##
  # ============================= #
  if('BALGR' %in% indicator){
    BALGR = vector('list', R)
    names(BALGR) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    imgr = oecd.tiva(io, 'IMGR')$IMGR
    for(r in 1:R){
      BALGR[[r]] = apply(exgr[[r]] - imgr[[r]], 2, sum)
    }
    ind$BALGR = BALGR
  }
  
  # ==================================================== #
  ## PROD_VASH - value added as a share of gross output ##
  # ==================================================== #
  if('PROD_VASH' %in% indicator){
    PROD_VASH = vector('list', R)
    names(PROD_VASH) = regions
    
    valu = oecd.tiva(io, 'VALU')$VALU
    prod = oecd.tiva(io, 'PROD')$PROD
    for(r in 1:R){
      PROD_VASH[[r]] = valu[[r]]/prod[[r]]*100
      PROD_VASH[[r]][is.na(PROD_VASH[[r]])] = 0
    }
    ind$PROD_VASH = PROD_VASH
  }
  
  # ===================================================== #
  ## EXGRpSH - Gross exports: partner shares by industry ##
  # ===================================================== #
  if('EXGRpSH' %in% indicator){
    EXGRpSH = vector('list', R)
    names(EXGRpSH) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    d = dim(exgr[[1]])
    for(r in 1:R){
      denom = matrix(apply(exgr[[r]], 1, sum), nrow = d[1], ncol = d[2])
      EXGRpSH[[r]] = exgr[[r]]/denom * 100
    }
    ind$EXGRpSH = EXGRpSH
  }
  
  # ===================================================== #
  ## IMGRpSH - Gross exports: partner shares by industry ##
  # ===================================================== #
  if('IMGRpSH' %in% indicator){
    IMGRpSH = vector('list', R)
    names(IMGRpSH) = regions
    
    imgr = oecd.tiva(io, 'IMGR')$IMGR
    d = dim(imgr[[1]])
    for(r in 1:R){
      denom = matrix(apply(imgr[[r]], 1, sum), nrow = d[1], ncol = d[2])
      IMGRpSH[[r]] = imgr[[r]]/denom * 100
    }
    ind$IMGRpSH = IMGRpSH
  }
  
  
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  ## Indicators based on value added, gross exports, and gross imports
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  
  
  # ========================================================= #
  ## EXGR_DVA - domestic value added ontent of gross exports ##
  # ========================================================= #
  if('EXGR_DVA' %in% indicator){
    EXGR_DVA = vector('list', R)
    names(EXGR_DVA) = regions
    
    v = oecd.tiva(io, 'PROD_VASH')$PROD_VASH
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    
    index = 1:S - S
    inner_index = dim(exgr[[1]])[2]
    for(r in 1:R){
      index = index + S
      for(j in 1:inner_index){
        temp = t(t(v[[r]]) %*% io$L[index, index] %*% diag(exgr[[r]][,j])/100)
        EXGR_DVA[[r]] = cbind(EXGR_DVA[[r]], temp)
      }
      colnames(EXGR_DVA[[r]]) = regions[-r]
      rownames(EXGR_DVA[[r]]) = sectors
    }
    ind$EXGR_DVA = EXGR_DVA
  }
  
  # ========================================================================= #
  ## EXGR_DDC - direct domestic indusry value added content of gross exports ##
  # ========================================================================= #
  if('EXGR_DDC' %in% indicator){
    EXGR_DDC = vector('list', R)
    names(EXGR_DDC) = regions
    
    v = oecd.tiva(io, 'PROD_VASH')$PROD_VASH
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    
    index = 1:S - S
    for(r in 1:R){
      index = index + S
      L_c = solve(diag(S) - io$A[index, index])
      L_c = diag(diag(L_c))
      v_hat = diag(c(v[[r]]))
      exgr_cw = apply(exgr[[r]], 1, sum)
      EXGR_DDC[[r]] = v_hat %*% L_c %*% matrix(exgr_cw)/100
      colnames(EXGR_DDC[[r]]) = 'total'
      rownames(EXGR_DDC[[r]]) = sectors
    }
    ind$EXGR_DDC = EXGR_DDC
  }
  
  # ======================================================= #
  ## EXGR_IDC - Indirect domestic content of gross exports ##
  # ======================================================= #
  if('EXGR_IDC' %in% indicator){
    EXGR_IDC = vector('list', R)
    names(EXGR_IDC) = regions
    
    v = oecd.tiva(io, 'PROD_VASH')$PROD_VASH
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    exgr_ddc = oecd.tiva(io, 'EXGR_DDC')$EXGR_DDC
    
    index = 1:S - S
    for(r in 1:R){
      index = index + S
      L_c = solve(diag(S) - io$A[index, index])
      diag(L_c) = 0
      v_hat = diag(c(v[[r]]))
      exgr_cw = apply(exgr[[r]], 1, sum)
      EXGR_IDC[[r]] = v_hat %*% L_c %*% matrix(exgr_cw)/100 - exgr_ddc[[r]]
      colnames(EXGR_IDC[[r]]) = 'total'
      rownames(EXGR_IDC[[r]]) = sectors
    }
    ind$EXGR_IDC = EXGR_IDC
  }
  
  # ====================================================================== #
  ## EXGR_RIM - re-imported domestic value added content of gross exports ##
  # ====================================================================== #
  if('EXGR_RIM' %in% indicator){
    EXGR_RIM = vector('list', R)
    names(EXGR_RIM) = regions
    
    exgr_dva = oecd.tiva(io, 'EXGR_DVA')$EXGR_DVA
    exgr_ddc = oecd.tiva(io, 'EXGR_DDC')$EXGR_DDC
    exgr_idc = oecd.tiva(io, 'EXGR_IDC')$EXGR_IDC
    
    index = 1:S - S
    for(r in 1:R){
      index = index + S
      
      exgr_dva_cw = apply(exgr_dva[[r]], 1, sum)
      EXGR_RIM[[r]] = exgr_dva_cw - exgr_ddc[[r]] - exgr_idc[[r]]
      colnames(EXGR_RIM[[r]]) = 'total'
      rownames(EXGR_RIM[[r]]) = sectors
    }
    ind$EXGR_RIM = EXGR_RIM
  }
  
  # ========================================================= #
  ## EXGR_FVA - foregin value added content of gross exports ##
  # ========================================================= #
  if('EXGR_FVA' %in% indicator){
    EXGR_FVA = vector('list', R)
    names(EXGR_FVA) = regions
    
    v = oecd.tiva(io, 'PROD_VASH')$PROD_VASH
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    
    index = 1:S - S
    for(r in 1:R){
      index = index + S
      exgr_cw = apply(exgr[[r]], 1, sum)
      L = io$L[index, index]
      EXGR_FVA[[r]] = diag(c(v[[r]])) %*% L %*% exgr_cw/100
      colnames(EXGR_FVA[[r]]) = 'total'
      rownames(EXGR_FVA[[r]]) = sectors
    }
    ind$EXGR_FVA = EXGR_FVA
  }
  
  # ========================================================= #
  ## IMGR_DVA - domestic value added ontent of gross imports ##
  # ========================================================= #
  if('IMGR_DVA' %in% indicator){
    IMGR_DVA = vector('list', R)
    names(IMGR_DVA) = regions
    
    v = oecd.tiva(io, 'PROD_VASH')$PROD_VASH
    imgr = oecd.tiva(io, 'IMGR')$IMGR
    
    index = 1:S - S
    inner_index = dim(imgr[[1]])[2]
    for(r in 1:R){
      index = index + S
      for(j in 1:inner_index){
        temp = t(t(v[[r]]) %*% io$L[index, index] %*% diag(imgr[[r]][,j])/100)
        IMGR_DVA[[r]] = cbind(IMGR_DVA[[r]], temp)
      }
      colnames(IMGR_DVA[[r]]) = regions[-r]
      rownames(IMGR_DVA[[r]]) = sectors
    }
    ind$IMGR_DVA = IMGR_DVA
  }
  
  # ========================================= #
  ## REII - recreational equipment inc inc   ##
  #       - re-exoirted intermediate imports  #
  # ========================================= #
  if('REII' %in% indicator){
    REII = vector('list', R)
    names(REII) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    
    index_row = 1:S - S
    for(r in 1:R){
      index_row = index_row + S
      L = io$L[index_row, index_row]
      exgr_cw = apply(exgr[[r]], 1, sum)
      
      index_col = 1:S - S
      A_c = matrix(0, nrow = S, ncol = S)
      for(j in 1:R){
        index_col = index_col + S
        if(j == r){next}
        A_c = A_c + io$A[index_row, index_col]
      }
      REII[[r]] = A_c %*% L %*% exgr_cw
      rownames(REII[[r]]) = sectors
      colnames(REII[[r]]) = 'total'
    }
    ind$REII = REII
  }
  
  # ========================================================== #
  ## EXGR_DVASH - Domestic value added share of gross exports ##
  # ========================================================== #
  if('EXGR_DVASH' %in% indicator){
    EXGR_DVASH = vector('list', R)
    names(EXGR_DVASH) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    exgr_dva = oecd.tiva(io, 'EXGR_DVA')$EXGR_DVA
    for(r in 1:R){
      EXGR_DVASH[[r]] = matrix(apply(exgr_dva[[r]], 1, sum) / apply(exgr[[r]], 1, sum) * 100)
      EXGR_DVASH[[r]][is.nan(EXGR_DVASH[[r]])] = 0
      colnames(EXGR_DVASH[[r]]) = 'total'
      rownames(EXGR_DVASH[[r]]) = sectors
    }
    ind$EXGR_DVASH = EXGR_DVASH
  }
  
  # ======================================================================================= #
  ## EXGR_INTVASH - domestic value added in exports of intermediate products share of exgr ##
  # ======================================================================================= #
  if('EXGR_INTDVASH' %in% indicator){
    EXGR_INTDVASH = vector('list', R)
    names(EXGR_INTDVASH) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    
    for(r in 1:R){
      region = regions[r]
      partners = setdiff(regions, region)
      i_Z = which(RS[,1] == region)
      exgr_intdva = matrix(rep(0,S))
      for(p in 1:length(partners)){
        partner = partners[p]
        j_Z = which(RS[,1] == partner)
        j_f = which(io$f_label[1,] == partner)
        exgr_intdva = exgr_intdva + apply(io$Z[i_Z, j_Z], 1, sum)
      }
      exgr_cw = apply(exgr[[r]], 1, sum)
      
      EXGR_INTDVASH[[r]] = exgr_intdva/exgr_cw
      EXGR_INTDVASH[[r]][is.na(EXGR_INTDVASH[[r]])] = 0
      colnames(EXGR_INTDVASH[[r]]) = 'total'
      rownames(EXGR_INTDVASH[[r]]) = sectors
    }
    ind$EXGR_INTDVASH = EXGR_INTDVASH
  }
  
  # ================================================================================= #
  ## EXGR_FNLDVASH - domestic value added in exports of final products share of exgr ##
  # ================================================================================= #
  if('EXGR_FNLDVASH' %in% indicator){
    EXGR_FNLDVASH = vector('list', R)
    names(EXGR_FNLDVASH) = regions
    
    int = oecd.tiva(io, 'EXGR_INTDVASH')$EXGR_INTDVASH
    d   = oecd.tiva(io, 'EXGR_DVASH')$EXGR_DVASH
    
    for(r in 1:R){
      EXGR_FNLDVASH[[r]] = d[[r]] - int[[r]]
    }
    ind$EXGR_FNLDVASH = EXGR_FNLDVASH
  }
  
  # ==================================================================================== #
  ## EXGR_DVAFXSH - Domestic value added emobdied in foreign exports as share of global ##
  # ==================================================================================== #
  if('EXGR_DVAFXSH' %in% indicator){
    EXGR_DVAFXSH = vector('list', R)
    names(EXGR_DVAFXSH) = regions
    
    exgr_bsci = oecd.tiva(io, 'EXGR_BSCI')$EXGR_BSCI
    for(r in 1){
      exgr_holder = matrix(rep(0,S))
      bsci_holder = matrix(rep(0,S))
      for(p in 1:R){
        if(r == p){next}
        exgr_holder = exgr_holder + exgr[[p]]
        bsci_holder = bsci_holder + exgr_bsci[[p]]
      }
      EXGR_DVAFXSH[[r]] = bsci_holder/exgr_holder
      colnames(EXGR_DVAFXSH[[r]]) = sectors
      rownames(EXGR_DVAFXSH[[r]]) = 'total'
    }
    ind$EXGR_DVAFXSH = EXGR_DVAFXSH
  }
  
  # ===================================================================== #
  ## EXGR_DVApSH - Domestic  value added in gross exports partner shares ##
  # ===================================================================== #
  if('EXGR_DVApSH' %in% indicator){
    EXGR_DVApSH = vector('list', R)
    names(EXGR_DVApSH) = regions
    
    exgr_dva = oecd.tiva(io, 'EXGR_DVA')$EXGR_DVA
    
    for(r in 1:R){
      top = exgr_dva[[r]]
      bot = matrix(apply(top, 1, sum), nrow = S, ncol = (R-1))
      
      EXGR_DVApSH[[r]] = top/bot*100
      colnames(EXGR_DVApSH[[r]]) = regions[-r]
      rownames(EXGR_DVApSH[[r]]) = sectors
    }
    ind$EXGR_DVApSH = EXGR_DVApSH
  }
  
  # ========================================================================================== #
  ## EXGR_INTDVApSH - domestic value added in exports of intermediate products partner shares ##
  # ========================================================================================== #
  if('EXGR_INTDVApSH' %in% indicator){
    EXGR_INTDVApSH = vector('list', R)
    names(EXGR_INTDVApSH) = regions
    
    exgr_int = oecd.tiva(io, 'EXGR_INT')$EXGR_INT
    
    for(r in 1:R){
      top = exgr_int[[r]]
      bot = matrix(apply(top, 1, sum), nrow = S, ncol = (R-1))
      
      EXGR_INTDVApSH[[r]] = top/bot*100
      colnames(EXGR_INTDVApSH[[r]]) = regions[-r]
      rownames(EXGR_INTDVApSH[[r]]) = sectors
    }
    ind$EXGR_INTDVApSH = EXGR_INTDVApSH
  }
  
  # ========================================================= #
  ## EXGR_FVASH - foreign value added share of gross exports ##
  # ========================================================= #
  if('EXGR_FVASH' %in% indicator){
    EXGR_FVASH = vector('list', R)
    names(EXGR_FVASH) = regions
    
    exgr     = oecd.tiva(io, 'EXGR')$EXGR
    exgr_fva = oecd.tiva(io, 'EXGR_FVA')$EXGR_FVA
    for(r in 1:R){
      top = matrix(apply(exgr[[r]], 1, sum))
      EXGR_FVASH[[r]] = top/exgr_fva[[r]] * 100
      EXGR_FVASH[[r]][is.na(EXGR_FVASH[[r]])] = 0
      colnames(EXGR_FVASH[[r]]) = 'total'
      rownames(EXGR_FVASH[[r]]) = sectors
    }
    ind$EXGR_FVASH = EXGR_FVASH
  }
  
  # ============================================================================ #
  ## EXGR_TDVAIND - industry domestic value added contribution to gross exports ##
  # ============================================================================ #
  if('EXGR_TDVAIND' %in% indicator){
    EXGR_TDVAIND = vector('list', R)
    names(EXGR_TDVAIND) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    exgr_dva = oecd.tiva(io, 'EXGR_DVA')$EXGR_DVA
    
    for(r in 1:R){
      top = matrix(apply(exgr_dva[[r]], 1, sum))
      bot = matrix(apply(exgr[[r]], 1, sum))
      EXGR_TDVAIND[[r]] = top/bot*100
      EXGR_TDVAIND[[r]][is.na(EXGR_TDVAIND[[r]])] = 0
      colnames(EXGR_TDVAIND[[r]]) = 'total'
      rownames(EXGR_TDVAIND[[r]]) = sectors
    }
    ind$EXGR_TDVAIND = EXGR_TDVAIND
  }
  
  # =========================================================================== #
  ## EXGR_TFVAIND - foreign domestic value added contribution to gross exports ##
  # =========================================================================== #
  if('EXGR_TFVAIND' %in% indicator){
    EXGR_TFVAIND = vector('list', R)
    names(EXGR_TFVAIND) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    exgr_fva = oecd.tiva(io, 'EXGR_FVA')$EXGR_FVA
    
    for(r in 1:R){
      top = exgr_fva[[r]]
      bot = matrix(apply(exgr[[r]], 1, sum))
      EXGR_TFVAIND[[r]] = top/bot*100
      EXGR_TFVAIND[[r]][is.na(EXGR_TFVAIND[[r]])] = 0
      colnames(EXGR_TFVAIND[[r]]) = 'total'
      rownames(EXGR_TFVAIND[[r]]) = sectors
    }
    ind$EXGR_TFVAIND = EXGR_TFVAIND
  }
  
  # ========================================================== #
  ## IMGR_DVASH - Domestic value added share of gross exports ##
  # ========================================================== #
  if('IMGR_DVASH' %in% indicator){
    IMGR_DVASH = vector('list', R)
    names(IMGR_DVASH) = regions
    
    imgr = oecd.tiva(io, 'IMGR')$IMGR
    imgr_dva = oecd.tiva(io, 'IMGR_DVA')$IMGR_DVA
    for(r in 1:R){
      IMGR_DVASH[[r]] = matrix(apply(imgr_dva[[r]], 1, sum) / apply(imgr[[r]], 1, sum) * 100)
      IMGR_DVASH[[r]][is.nan(IMGR_DVASH[[r]])] = 0
      colnames(IMGR_DVASH[[r]]) = 'total'
      rownames(IMGR_DVASH[[r]]) = sectors
    }
    ind$IMGR_DVASH = IMGR_DVASH
  }
  
  # ====================================================================== #
  ## IMGINT_REII - re-exported imports as % of total intermediate imports ##
  # ====================================================================== #
  if('IMGINT_REII' %in% indicator){
    IMGINT_REII = vector('list', R)
    names(IMGINT_REII) = regions
    
    imgr_int = oecd.tiva(io, 'IMGR_INT')$IMGR_INT
    exgr     = oecd.tiva(io, 'EXGR')$EXGR
    
    index_col = 1:S - S
    for(r in 1:R){
      index_col = index_col + S
      index_row = 1:S - S
      bot = matrix(apply(imgr_int[[r]], 1, sum))
      right = matrix(apply(exgr[[r]], 1, sum))
      temp = matrix(rep(0, S))
      for(p in 1:R){
        index_row = index_row + S
        if(p == r){next}
        temp = temp + io$A[index_row, index_col] %*% io$L[index_col, index_col] %*% right
      }
      IMGINT_REII[[r]] = temp / bot
      colnames(IMGINT_REII[[r]]) = 'total'
      rownames(IMGINT_REII[[r]]) = sectors
    }
    ind$IMGINT_REII = IMGINT_REII
  }
  
  
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  ## Complex indicators
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  # --------------------------------------------------------------------------------------------- #
  
  # ================================================== #
  ## EXGR_BSCI origin of value added in gross exports ##
  # ================================================== #
  if('EXGR_BSCI' %in% indicator){
    EXGR_BSCI = vector('list', R)
    names(EXGR_BSCI) = regions
    
    exgr = oecd.tiva(io, 'EXGR')$EXGR
    for(r in 1:R){
      exgr[[r]] = apply(exgr[[r]], 1, sum)
    }
    exgr = matrix(unlist(exgr))
    vhat = diag(c(unlist(oecd.tiva(io, 'PROD_VASH')$PROD_VASH)))
    L = io$L
    
    temp = vhat %*% L %*% exgr/100
    index = 1:S - S
    for(r in 1:R){
      index = index + S
      EXGR_BSCI[[r]] = matrix(temp[index])
      rownames(EXGR_BSCI[[r]]) = sectors
      colnames(EXGR_BSCI[[r]]) ='total'
    }
    ind$EXGR_BSCI = EXGR_BSCI
  }
  
  
  ###########
  # Print   #
  # the     #
  # results #
  # right   #
  # about   #
  # now     #
  ###########
  ind
}








