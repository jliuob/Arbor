insert_lr <- function(.data, plot, width,  side) {
  side <- match.arg(side, c("left", "right"))
  .data <- aplot:::as.aplot(.data)
  .data$n <- .data$n + 1
  
  new_col <- matrix(nrow=nrow(.data$layout), ncol=1)
  new_col[.data$main_row] <- .data$n
  
  if (side == "left") {
    .data$width <- c(width, .data$width)
    .data$layout <- cbind(new_col, .data$layout)
    .data$main_col <- .data$main_col + 1
  } else {
    .data$width <- c(.data$width, width)
    .data$layout <- cbind(.data$layout, new_col)
  }
  
  if (is.ggtree(plot)) { ## re-order based on the tree
    selected <- .data$layout[.data$main_row,]
    selected <- selected[!is.na(selected)]
    selected <- selected[selected != .data$n]
    for (i in selected) {
      if (is.coord_flip(.data$plotlist[[i]])) {
        xvar <- rvcheck::get_aes_var(.data$plotlist[[i]]$mapping, 'x')
        lvs <- rev(get_taxa_order(plot))
        
        axis_trans <- list(
          aes(x = factor(.data[[xvar]], 
                         levels = lvs)), ## c(.data[[xvar]][!.data[[xvar]] %in% lvs], lvs))),
          xlab(.data$plotlist[[i]]$labels$x)
        )
      } else {
        yvar <- rvcheck::get_aes_var(.data$plotlist[[i]]$mapping, 'y')
        lvs = rev(get_taxa_order(plot))
        
        axis_trans <- list(
          aes(y = factor(.data[[yvar]], 
                         levels = lvs)), ## c(.data[[yvar]][!.data[[yvar]] %in% lvs], lvs))),
          ylab(.data$plotlist[[i]]$labels$y)
        )
      }
      .data$plotlist[[i]] <- .data$plotlist[[i]] + axis_trans
    }
  }
  
  .data$plotlist[[.data$n]] = plot 
  .data
  
}

is.coord_flip <- function(p) {
  inherits(p, "gg") && inherits(p$coordinates, "CoordFlip")
}

get_taxa_order <- function (tree_view) {
  df <- tree_view$data
  with(df, {
    i = order(y, decreasing = T)
    label[i][isTip[i]]
  })
}