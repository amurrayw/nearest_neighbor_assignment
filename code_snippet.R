load(file="NN.RData")



###################TODO: Need to restrict the used.neighbors vector to only include the 65 variables from the sample.
############We don't care if a nearest neighbor has been used up by a variable outside of the sample.

assign.neighbors <- function(in.sample=FALSE, NN, rand=FALSE, sample.indx=which(s>0)){

    if(!is.logical(in.sample)){
        nearest.neigh.in.samp <- NN$nn.index[in.sample, ]
        nearest.neigh.dist <- NN$nn.dist[in.sample, ]

    }
    else{
        nearest.neigh.in.samp <- NN$nn.index
        nearest.neigh.dist <- NN$nn.dist
    }

    ## Keeps track of which variables have already been assigned as neighbors.
    ###TODO: Change this bit here.
    used.neighbors <- unique(nearest.neigh.in.samp[, 1])


    
    #Forbids sample from being a neighbor.
    used.neighbors <- unique(c(used.neighbors, sample.indx))
    
    ## Keeps track of which variables still need assigned neighbors due to conflicts.

##    need.resolved <- which(duplicated(nearest.neigh.in.samp[,1])|
  ##                         duplicated(nearest.neigh.in.samp[,1], fromLast=TRUE))
    
    need.resolved <- which(duplicated(nearest.neigh.in.samp[, 1]))



    positions.filled <- unique(c(which(!duplicated(nearest.neigh.in.samp[,1])), sample.indx))

    
    ## Keeps track of which variable has been assigned which neighbor.
    ## A 0 denotes a variable with no assigned neighbor.
    assignment.vec <- rep.int(0, times=nrow(nearest.neigh.in.samp))

    print("used.neighbors")
    print((used.neighbors))

    print("positions.filled")
    print((positions.filled))
    

#    print("assign. pre")
#    print(assignment.vec)
    assignment.vec[positions.filled] <- used.neighbors


#    print("assignment.vec")
#    print(assignment.vec)
    
    ## Randomly reorders order of conflict resolution if desired.
    if(rand==TRUE){
        need.resolved <- need.resolved[sample(1:length(need.resolved), size=length(need.resolved))]
    }


    print("need.resolved")
    print(need.resolved)
    
    return(resolve.conflict(need.resolved, used.neighbors,
                            nearest.neigh.in.samp, nearest.neigh.dist, assignment.vec))
}


resolve.conflict <- function(need.resolved, used.neighbors, nearest.neigh.in.samp,
                             nearest.neigh.dist, assignment.vec){

    if(length(need.resolved>0)){

        currently.resolving <- need.resolved[1]
        nearest.neighbors <- nearest.neigh.in.samp[currently.resolving,]
        
        ## Selects first matching nearest neighbor not yet used.
        to.assign <-nearest.neighbors[!(nearest.neighbors %in% used.neighbors)][1]


#        print(nearest.neighbors[!(nearest.neighbors %in% used.neighbors)][1])
        
        ## Assigning selected nearest neighbor.
        assignment.vec[currently.resolving] <- to.assign
        
        ## Adding now used neighbor to used list.
        used.neighbors <- c(used.neighbors, to.assign)

        ##removing solved neighbor.
        need.resolved <- need.resolved[-1]

        return(resolve.conflict(need.resolved, used.neighbors, nearest.neigh.in.samp,
                                nearest.neigh.dist, assignment.vec))
    }
    else{
        return(assignment.vec)
    }
}

resulting.assingment <- assign.neighbors(NN=NN, sample.indx=NULL)



test.case <- cbind(c(1,3,3,2,5), c(3,2,4,5,1))

assign.neighbors(NN=list(nn.index=test.case, nn.dist=test.case))


##load(file="NN.RData")
