#I need all the models I'm going to test

#All string interactions: variables
inter <- function(variable){
  int <- apply(t(combn(variable, 2)), 1, paste, collapse=":")
  #on enlève shad des interactions
  int <- int[!str_detect(int, "shad")]
  return(int)
}

model <- function(clim=clim, sol=sol, NCI=NCI){
  #Blocks
  var <- c("clim", "sol", "comp", "shad")
  
  
  
  var_inter <- c(var, inter(var))
  
  
  a=crossing(clim = c(T, F),sol = c(T, F), comp = c(T, F), shad = c(T, F), 
             `clim:sol` = c(T, F),  `clim:comp` = c(T, F), `sol:comp` = c(T, F)) %>% 
    filter(!(clim== 0 & sol == 0  & comp == 0))
  
  rows_keep <- NULL
  
  for(i in 1:nrow(a)){
    keep=T
    l=colnames(a)[as.logical(as.data.frame(a[i,]))]
    interaction <- l[str_detect(l, ":")]
    no_interaction <-l[!str_detect(l, ":")] 
    
    if(length(interaction) !=0 ){
      for(j in 1:sum(str_detect(l, ":"))){ #j=1
        inter_split <- str_split(interaction[j], ":")[[1]] 
        if(sum(str_detect(inter_split, paste(no_interaction, collapse = "|")))!=2){
          keep=F
        }
      }
    }
    rows_keep=c(rows_keep, keep)
  }
  
  b=bind_cols(a, keep=rows_keep) %>% 
    filter(keep) %>% 
    dplyr::select(-keep)
  
  var_decomp <- list(comp=NCI, 
                     sol= sol,
                     clim=clim, 
                     shad="Shading")
  
  
  models=list()
  for(l in 1:nrow(b)){
    #Simple effect
    var_no_int <- colnames(b[l,])[as.logical(b[l,])][!str_detect(colnames(b[l,])[as.logical(b[l,])], pattern =":")]
    
    var_no_int2 <- NULL
    
    for(i in var_no_int){
      var_no_int2 <- c(var_no_int2, var_decomp[[i]])
    }
    
    #Adding interactions
    var_int <- colnames(b[l,])[as.logical(b[l,])][str_detect(colnames(b[l,])[as.logical(b[l,])], pattern =":")]
    
    var_int2= NULL
    
    if(length(var_int)!=0){
      int_split <- str_split(var_int, ":")
      
      for(i in 1:length(int_split)){
        
        eff_inter <- int_split[[i]]
        
        var_eff <- NULL
        
        for(j in eff_inter){
          var_eff <- bind_rows(var_eff, data.frame(group=j, var=var_decomp[[j]]))
        }
        
        g1 <- filter(var_eff, group==unique(var_eff$group)[1])$var
        g2 <- filter(var_eff, group==unique(var_eff$group)[2])$var
        
        group_var <- expand_grid(l1 = g1, l2 = g2) %>% 
          mutate(int=paste(l1, l2, sep=":")) %>% 
          dplyr::select(int) %>% 
          as.vector()
        
        var_int2 <- c(var_int2, group_var$int)
      }
    }
    
    
    variable_full <- c("ldbh", "IldbhE2", var_no_int2, unique(var_int2)) #we concatenate inter, and add the effects that are always present
    
    models[[l]] <- variable_full
    
  }
  
  return(models)
}

#Create lines for a priori distributions
create_prior <- function(d){ #enter d with column ESS, Estimate, variable
  prior <- eval(call("prior", call("normal", d[1,]$Estimate, d[1,]$sd), coef=d[1,]$variable)) #Initiate the first line, then loop over the rest
  if(nrow(d)>1){
    for(i in 2:nrow(d)){
      prior <- c(prior, eval(call("prior", call("normal", d[i,]$Estimate, d[i,]$sd), coef=d[i,]$variable)))
    }
  }
  return(prior)
}

