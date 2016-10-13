library(analogsea)

find_snapshots <- function(name) {
  imgs <- images(public = F)
  imgData <- data.frame(id=unname(sapply(imgs,`[[`,"id")),
                        name=names(imgs),
                        created_at = as.Date(unname(sapply(imgs,`[[`,"created_at"))))
  imgData <- imgData[order(imgData$created_at),]
  imgData[grepl(name,imgData$name),]

}

#' @export
edroplet_freeze <- function(name) {
  droplet_freeze(droplet = name)
}

#' @export
edroplet_thaw <- function(name) {
  imgData <- find_snapshots(name)
  latestImage <- as.character(imgData$name[nrow(imgData)])
  droplet_thaw(image = latestImage,name=name)
  snapshots <- find_snapshots(name)
  sapply(snapshots$id,image_delete)
}

# name <- "test-freeze"
#
# do_options(ssh_keys=c(42250,313661))
# do_options(region="ams3")
# droplet_new(name,image="coreos-beta")
#
# freeze(name)
# thaw(name)
#
# freeze(name)
# thaw(name)
#
# freeze(name)
# thaw(name)
#
# freeze(name)
# thaw(name)
#
# freeze(name)


#' Lists all objects in gloabal environment ordered by size
#' @export
memInfo <- function() {
  df <- data.frame(size=sapply(ls(globalenv()),FUN=function(x) object.size(get(x)),USE.NAMES = T))
  df <- df[rev(order(df)),,drop=F]
  format(df,big.mark=".")
}


#' @export
unloadAll <- function(maxUnloads=10) {

    basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

    package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

    package.list <- setdiff(package.list,basic.packages)

    if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE,unload=T)

    basic.namespaces=c("utils","grDevices","graphics","base","stats","methods","datasets","compiler","tools")
    for (i in 1:maxUnloads) {
        sapply(setdiff(loadedNamespaces(),basic.namespaces),FUN=function(x) {try(unloadNamespace(x))})
    }
}

