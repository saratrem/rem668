# Sockeye SR.R
# This code estimates Ricker and Larkin parameters for multiple sockeye stock-recruit datasets and plots results
# Date: 
# Programmer: 

# DATA SECTION
# Read all data here and calculate constants

file.name <- "Sockeye.data"
data      <- read.table(file=file.name, header=TRUE, sep="\t")
stocks    <- unique( data$Stock )
nstock    <- length( stocks )

# PROCEDURE SECTION

# create a function to estimate parameters and fitted values based on the Ricker model
"Ricker" <- function(){
  # Fit linear model to estimate alpha and beta in log( R/S ) = alpha - beta * S
  
  # create a list to fill with outputs from each stock
  fit <- list()    
  
  # loop over stocks
  for( i in 1:nstock ){
    # identify parts of data file that corresponds to this stock
    stock.id   <- data$Stock == stocks[i]
    # separate out stock-specific recruits and spawners
    R          <- data$Recruits[ stock.id ]
    S          <- data$Spawners[ stock.id ]
    fit[[i]]   <- lm( log( R/S ) ~ S )   # linear regression on transformed Ricker model
    fit[[i]]$r <- exp( fit[[i]]$fitted.values ) * S # back-transform to original Ricker model
  }
  
  # return findings so they are preserved outside the function
  return( fit )
}

# create a function to estimate parameters and fitted values based on the Ricker model

# GRAPHICS SECTION
