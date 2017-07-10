## Code written by Alexander Murray-Watters

## Function: assign.neighbors() = Assigns unique nearest neighbors.

## Inputs:

## in.sample either a Boolean which says to not run the
## procedure on a specified subset of the NN matrix, or a vector
## specifying which row positions to include from the NN matrix.

## NN = the nearest neighbor matrix.

## rand = a Boolean specifying if
## neighbor conflict resolution should be done in a random order.

## Sample.indx = specifies which variables were part of the sample, and
## so shouldn't be used to fill in nearest neighbors.

## Outputs: Returns a vector containing the resulting neighbor
## assignments. The position of each number specifies the variable
## receiving the nearest neighbor. The vector can also include NAs due
## to an inability to find an unused nearest neighbor to assign.

assign.neighbors <- function(NN, rand=FALSE, sample.indx=which(s>0)){

    nearest.neigh.in.samp <- NN$nn.index
    nearest.neigh.dist <- NN$nn.dist

    ## If we want to restrict the procedure to just the in sample
    ## variables (so that we don't use up neighbors that are a nearest
    ## match for something outside the sample), follow the "else" branch.
    if(is.null(sample.indx)){
        ## Keeps track of which variables have already been assigned as neighbors.
        used.neighbors <- unique(nearest.neigh.in.samp[, 1])

        ## Keeps track of which variables still need assigned neighbors due to conflicts. 
        need.resolved <- which(duplicated(nearest.neigh.in.samp[, 1]))

        ## Keeps track of which positions have been filled by a nearest neighbor. 
        positions.filled <- unique(c(which(!duplicated(nearest.neigh.in.samp[,1]))))
    }else{
        used.neighbors <- unique(nearest.neigh.in.samp[sample.indx, 1])
        need.resolved <- which(duplicated(nearest.neigh.in.samp[sample.indx, 1]))
        positions.filled <- unique(c(which(!duplicated(nearest.neigh.in.samp[sample.indx,1]))))
    }
    
    ## Keeps track of which variable has been assigned which neighbor.
    ## A 0 denotes a variable with no assigned neighbor.
    assignment.vec <- rep.int(0, times=nrow(nearest.neigh.in.samp))

    assignment.vec[positions.filled] <- used.neighbors

                                        #Forbids sample from being a neighbor.
    used.neighbors <- unique(c(used.neighbors, sample.indx))
    
    ## Randomly reorders order of conflict resolution if desired.
    if(rand==TRUE){
        need.resolved <- need.resolved[sample(1:length(need.resolved), size=length(need.resolved))]
    }

    return(resolve.conflict(need.resolved, used.neighbors,
                            nearest.neigh.in.samp, nearest.neigh.dist, assignment.vec))
}


##Function: resolve.conflict() = Recursively resolves conflicts due to duplicate nearest neighbors.

## Inputs: 
## Outputs:Vector containing assignments.


resolve.conflict <- function(need.resolved, used.neighbors, nearest.neigh.in.samp,
                             nearest.neigh.dist, assignment.vec){

    if(length(need.resolved)==0){
        return(assignment.vec)
    }
    else{
        currently.resolving <- need.resolved[1]
        nearest.neighbors <- nearest.neigh.in.samp[currently.resolving,]
        
        ## Selects first matching nearest neighbor not yet used.
        to.assign <-nearest.neighbors[!(nearest.neighbors %in% used.neighbors)][1]
        
        ## Assigning selected nearest neighbor.
        assignment.vec[currently.resolving] <- to.assign
        
        ## Adding now used neighbor to used list.
        used.neighbors <- c(used.neighbors, to.assign)

        ##removing solved neighbor.
        need.resolved <- need.resolved[-1]

        return(resolve.conflict(need.resolved, used.neighbors, nearest.neigh.in.samp,
                                nearest.neigh.dist, assignment.vec))
    }
}


## Runs test cases to make sure function actually works.
run.test.cases <- function(){
    test.case.1 <- cbind(c(1,3,3,2,5), c(3,2,4,5,1))

    test.1.result <- all.equal(c(1,3,4,2,5), assign.neighbors(NN=list(nn.index=test.case.1,
                                                            nn.dist=test.case.1), sample.indx=NULL))


    test.case.2 <- cbind(c(1,2,3), c(3,2,1))

    
    test.2.result <- all.equal(c(1, 0, 0), assign.neighbors(NN=list(nn.index=test.case.2,
                                                            nn.dist=test.case.2), sample.indx=1))

    test.3.result <- sum(replicate(n=1000, sum(assign.neighbors(NN, rand=TRUE) ==
                                                    assign.neighbors(NN, rand=TRUE))==1933))==1000
    
    return(list(test.1.result=test.1.result, test.2.result=test.2.result,
                test.3.result=test.3.result))
    
}

run.test.cases()


## Actually running the code on the data set.

load(file="NN.RData")

resulting.assignment <- assign.neighbors(NN=NN, sample.indx=which(s>0))

