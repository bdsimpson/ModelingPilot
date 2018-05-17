#################################################################
#  utilities.R
#
#  General-purpose utility functions
#
#################################################################

#### load or install function
loadOrInstall <- function(myPackages){
  for(myPackage in myPackages){
    if(!require(package=myPackage, character.only=TRUE)){
      install.packages(package=myPackage, character.only=TRUE)
      loadOrInstall(package=myPackage, character.only=TRUE)
    }
  }
} # end of loadOrInstall()


### JDBC connection to Teradata
teradataJDBCconnect <- function(JAR_DIR, username, password, LDAP=TRUE){
  # Location of Teradata JDBC driver jar files
  JAR_DIR <- path.expand(JAR_DIR)
  
  .jaddClassPath(paste(JAR_DIR, "terajdbc4.jar", sep=""))
  .jaddClassPath(paste(JAR_DIR, "tdgssconfig.jar", sep=""))
  drv <- JDBC(driverClass="com.teradata.jdbc.TeraDriver")
  
  conString <- paste0("jdbc:teradata://143.30.195.74",ifelse(LDAP,"/LOGMECH=LDAP"))
  tconn <- dbConnect(drv, conString, username, password)
  
  return(tconn)
}
