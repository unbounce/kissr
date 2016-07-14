# Attribute (Event and Property) Explorer Functions

# Use GetAttributeList() to get full list of properties and events from KM
# Example: attributesList <- GetAttributeList()

# Use GetAttributeIndex() by feeding it a single event/property Display Name to
# get the corresponding index to be used with KissReports
# Example: contactCreatedIndex <- GetAttributeIndex("Contact created")

# Outputs full list of KM events and properties as a df
# No parameters needed

GetAttributeList <- function(){
  # Get list of events from KM
  eventsList <- read(KissEvents())
  eventsList$metric_type <- "event"
  # Get list of properties from KM
  propertiesList <- read(KissProperties())
  propertiesList$metric_type <- "property"
  # Union both tables
  fullAttributeList <- rbind(eventsList,propertiesList)

  return(fullAttributeList)
}

# Outputs event/property index used as paramenter in KissReport function
GetAttributeIndex <- function(attributeName,
                              productId = "6581c29e-ab13-1030-97f2-22000a91b1a1"){
  fullAttributeList <- GetAttributeList()

  attributeIndex <- dplyr::filter(fullAttributeList, display_name == attributeName &
                             product_id == productId)

  if(nrow(attributeIndex) == 1){
    attributeIndex <- as.numeric(attributeIndex$index)
    return(attributeIndex)
  } else if (nrow(attributeIndex) > 1){
    stop(paste0("More than one attribute with that name.\n",
                "Use GetAttributeList() for full list of attributes  (display_name column)."))
  } else {
    stop(paste0("Attribute name not in table.\n",
                "Check attribute spelling by using GetAttributeList()\n",
                "for full list of attributes (display_name column)."))
  }
}

# Output event/property name from index number
GetAttributeName <- function(attributeIndex,
                              productId = "6581c29e-ab13-1030-97f2-22000a91b1a1"){
  fullAttributeList <- GetAttributeList()
  attributeName <- dplyr::filter(fullAttributeList,index == attributeIndex &
                            product_id == productId)

  if(nrow(attributeName) >= 1){
    return(attributeName[,c("display_name","metric_type")])
  } else {
    stop(paste0("Attribute index not in table.\n",
                "Check full attribute list by using GetAttributeList()."))
  }
}
