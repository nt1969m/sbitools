CFD.fin <- function( csv ) {
  csv[ ,1] <- gsub( "/" ,"-" ,csv[ ,1] )
  csv[ ,4] <- as.integer( gsub( "," ,"" ,csv[ ,4] ) )
  csv[ ,5] <- as.integer( gsub( "," ,"" ,csv[ ,5] ) )
  csv[ ,6] <- as.integer( gsub( "," ,"" ,csv[ ,6] ) )
#  print( paste0("証拠金の増減=" ,sum( csv[ ,5] ,na.rm=T ) - sum( csv[ ,4] ,na.rm=T ) ) )
  print( paste0(
     colnames(csv)[5]
    ," - "
    ,colnames(csv)[4]
    ," = "
    ,sum( csv[ ,5] ,na.rm=T ) - sum( csv[ ,4] ,na.rm=T ) ) )

  return( csv )
}

