# PLOTTING
#================================================================================


#my_colors <- c("grey50", "#67A9CF", "#D6604D", "#74C476", "#E6AB02", "#B2182B")

mc <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
mc_fill <- mc

mc_fill[1] <- rethinking::col.desat(mc[1], .5)
mc_fill[2] <- rethinking::col.desat(mc[2], .5)
mc_fill[3] <- rethinking::col.desat(mc[3], .5)
mc_fill[4] <- rethinking::col.desat(mc[4], .4)
mc_fill[5] <- rethinking::col.desat(mc[5], .5)
mc_fill[6] <- rethinking::col.desat(mc[6], .5)
mc_fill[7] <- rethinking::col.desat(mc[7], .5)
mc_fill[8] <- rethinking::col.desat(mc[8], .5)


## lattice plotting options
#--------------------------------------------------------------------------------

my_settings <- list()
my_settings$axis.line$col <- NA
my_settings$strip.border$col <- NA
my_settings$strip.background$col <- NA
my_settings$layout.heights$strip = 1.4
my_settings$clip$panel = "off"

my_settings$layout.heights$key.axis.padding <- 0
my_settings$layout.heights$xlab <- 0
my_settings$layout.heights$bottom.padding <- 0.5
my_settings$layout.heights$top.padding <- 1
my_settings$layout.heights$key.padding <- 0
my_settings$layout.heights$axis.top <- 0
my_settings$layout.heights$main <- 0
my_settings$layout.heights$main.key.padding <- 0

my_settings$layout.widths$ylab <- 0
my_settings$layout.widths$axis.panel <- 10
my_settings$layout.widths$right.padding <- 0
my_settings$layout.widths$left.padding <- 1
my_settings$layout.widths$key.right <- 0
my_settings$layout.widths$axis.right <- 0
my_settings$layout.widths$ylab.right <- 0

my_settings$axis.text$col <- "black"

my_settings$axis.components$left$pad1 <- 0.5
my_settings$axis.components$left$pad2 <- 1.4
my_settings$axis.components$bottom$pad1 <- 0.5
my_settings$axis.components$bottom$pad2 <- 1.4

# tick marks
my_settings$axis.components$bottom$tck <- .5
my_settings$axis.components$left$tck   <- .5
my_settings$axis.components$top$tck    <- .5
my_settings$axis.components$right$tck  <- .5

# space between axis label and tick mark labels
my_settings$layout.widths$ylab.axis.padding <- 0.2
my_settings$layout.heights$axis.xlab.padding <- 0.2


my_settings$box.rectangle$col = 1
my_settings$box.umbrella$col = 1
my_settings$box.dot$col = 1
my_settings$plot.symbol$col = 1

# set font size and point size
my_settings$fontsize$text = 8
my_settings$fontsize$points = 3

my_settings$par.main.text$font = 2
my_settings$par.main.text$just = "left"
my_settings$par.main.text$x = grid::unit(5, "mm")

my_settings$par.sub.text$font = 1
my_settings$par.sub.text$cex = .8
my_settings$par.sub.text$col = "grey30"
my_settings$par.sub.text$just = "left" 
my_settings$par.sub.text$x = grid::unit(5, "mm")


## function for text with edges
#--------------------------------------------------------------------------------

panel.text_halo <- 
	function(x, y=NULL, labels, col='black', bg='white', 
	         current_fig.height=convertX(unit(1, "strheight", data = "A"), "inches", TRUE),
	         current_fig.width=convertX(unit(1, "strwidth", data = "A"), "inches", TRUE),
					 theta= seq(0, 2*pi, length.out=50), r=1, ... ) {
	  
		#xy <- xy.coords(x,y)
		yo <- r*current_fig.height
		xo <- r*current_fig.width
		
		# draw background text with small shift in x and y in background colour
		for (i in theta) {
			panel.text(x= x + cos(i)*xo, 
								 y= y + sin(i)*yo, labels, col=bg, ... )
		}
		# draw actual text in exact xy position in foreground colour
		panel.text(x, y, labels, col=col, ... )
	}


## functions for axes
#--------------------------------------------------------------------------------

axis_L <-
	function(side, ..., line.col)
	{
		if (side %in% c("left", "bottom")) {
			col <- trellis.par.get("axis.text")$col
			axis.default(side, ..., line.col = col)
			if (side == "bottom")
				grid::grid.lines(y = 0)
			if (side == "left")
				grid::grid.lines(x = 0)
		}
	}

axis_C <-
  function(side, ..., line.col)
  {
    if (side %in% c("left", "bottom", "top")) {
      col <- trellis.par.get("axis.text")$col
      axis.default(side, ..., line.col = col)
      if (side == "bottom")
        grid::grid.lines(y = 0)
      if (side == "left")
        grid::grid.lines(x = 0)
      if (side == "top")
        grid::grid.lines(y = 1)
    }
  }

axis_U <-
  function(side, ..., line.col)
  {
    if (side %in% c("left", "bottom", "right")) {
      col <- trellis.par.get("axis.text")$col
      axis.default(side, ..., line.col = col)
      if (side == "bottom")
        grid::grid.lines(y = 0)
      if (side == "left")
        grid::grid.lines(x = 0)
      if (side == "right")
        grid::grid.lines(x = 1)
    }
  }

axis_all <-
  function(side, ..., line.col)
  {
    if (side %in% c("left", "bottom", "top", "right")) {
      col <- trellis.par.get("axis.text")$col
      axis.default(side, ..., line.col = col)
      if (side == "bottom")
        grid::grid.lines(y = 0)
      if (side == "left")
        grid::grid.lines(x = 0)
      if (side == "top")
        grid::grid.lines(y = 1)
      if (side == "right")
        grid::grid.lines(x = 1)
    }
  }

axis_left = function(side, ..., line.col)
{
	if (side %in% "left") {
		col <- trellis.par.get("axis.text")$col
		axis.default(side, ..., line.col = col)
		if (side == "left")
			grid::grid.lines(x = 0)
	}
}

axis_bottom = function(side, ..., line.col)
{
	if (side %in% "bottom") {
		col <- trellis.par.get("axis.text")$col
		axis.default(side, ..., line.col = col)
		if (side == "bottom")
			grid::grid.lines(y = 0)
	}
}

axis_top = function(side, ..., line.col)
{
	if (side %in% "top") {
		col <- trellis.par.get("axis.text")$col
		axis.default(side, ..., line.col = col)
		if (side == "top")
			grid::grid.lines(y = 1)
	}
}

# new panel functions
#--------------------------------------------------------------------------------

# Dot diagram

panel.dotdiagram <- function(  x, 
                               x_anchor=0, y_anchor=0, n_bins=20, seq_min=min(x), seq_max=max(x), 
                               scale_y=1, set_pch=19, set_col=1, set_cex=1, set_alpha=1, vertical=F, 
                               leftwards=F, downwards=F, set_fontfamily=1, set_lwd=1){
	x_steps <- diff(c(seq_min, seq_max))/(n_bins-1)
	x_breaks <- seq(seq_min-x_steps/2, seq_max+x_steps/2, length.out=n_bins+1)
	counts <- as.numeric(table(cut(x, breaks = x_breaks, right = F)))
	dot_col_matrix <- matrix("NA", nrow=n_bins, ncol=max(counts))
	for(i in 1:nrow(dot_col_matrix)) {
		if(counts[i] >= 1) dot_col_matrix[i, 1:counts[i]] <- set_col
		}
	if(vertical==T){
		if(leftwards == F){
			panel.points(
				x = (rep(1:max(counts), each=n_bins)*scale_y) + x_anchor,
				y = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, 
				col=dot_col_matrix, cex=set_cex, alpha=set_alpha, fontfamily=set_fontfamily,
				lwd=set_lwd
			)		
			}
		if(leftwards == T){
			panel.points(
				x = -((rep(1:max(counts), each=n_bins)*scale_y)) + x_anchor,
				y = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, 
				col=dot_col_matrix, cex=set_cex, alpha=set_alpha, fontfamily=set_fontfamily,
				lwd=set_lwd
			)		
		}
	}
	if(vertical==F){
		if(downwards == F){
			panel.points(
				y = (rep(1:max(counts), each=n_bins)*scale_y) + y_anchor,
				x = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, 
				col=dot_col_matrix, cex=set_cex, alpha=set_alpha, fontfamily=set_fontfamily,
				lwd=set_lwd
			)		
			}
		if(downwards == T){
			panel.points(
				y = -((rep(1:max(counts), each=n_bins)*scale_y)) + y_anchor,
				x = rep(seq(seq_min, seq_max, length.out=n_bins), max(counts)), pch=set_pch, 
				col=dot_col_matrix, cex=set_cex, alpha=set_alpha, fontfamily=set_fontfamily,
				lwd=set_lwd
			)		
		}
	}
}


# lattice
#-------------------------------------------------------------------------------

# Nice arrows

panel.Arrows <- function (x0, y0, x1, y1, code = 2, arr.length = 0.4, arr.width = arr.length/2, 
                          arr.adj = 0.5, arr.type = "curved", segment = TRUE, 
                          col = "black", lcol = col, lty = 1, arr.col = lcol, 
                          lwd = 1, arr.lwd = lwd, ...) 
{
  if (arr.type == "simple") {
    panel.arrows(x0, y0, x1, y1, code = code, length = arr.length/2.54, 
                 lty = lty, col = col, lwd = lwd, ...)
    return()
  }
  if (arr.type == "none") {
    return()
  }
  if (arr.type == "T") {
    panel.arrows(x0, y0, x1, y1, code = code, 
                 length = arr.length/(2 * 2.54), lty = lty, angle = 90, col = col, lwd = lwd, 
                 ...)
    return()
  }
  if (segment) 
    panel.segments(x0, y0, x1, y1, col = lcol, lty = lty, lwd = lwd, 
                   ...)
  # user <- par("usr")
  pin <- par("din")
  # sy <- (user[4] - user[3])/pin[2]
  # sx <- (user[2] - user[1])/pin[1]
  
  user <- current.panel.limits(unit = "inch")
  # pcm <- c(lattice.getOption("layout.widths")$panel$x,
  #          lattice.getOption("layout.heights")$panel$x)
  sy <- (user$ylim[2] - user$ylim[1])/(pin[2])
  sx <- (user$xlim[2] - user$xlim[1])/(pin[1])
  
  angle <- atan((y1 - y0)/(x1 - x0) * sx/sy)/pi * 180
  angle[is.nan(angle)] <- 0
  angle[x1 < x0] <- 180 + angle[x1 < x0]
  xx <- x1
  yy <- y1
  if (sy < 0 & sx < 0) 
    angle <- angle + 180
  else if (sx < 0) 
    angle <- angle + 180
  if (code == 3) 
    lattice.Arrowhead(x0 = xx, y0 = yy, angle = angle, lcol = lcol, 
                      arr.col = arr.col, arr.adj = arr.adj, lty = lty, 
                      arr.length = arr.length, arr.width = arr.width, arr.type = arr.type, 
                      arr.lwd = arr.lwd, ...)
  if (code != 2) {
    angle <- 180 + angle
    xx <- x0
    yy <- y0
  }
  lattice.Arrowhead(x0 = xx, y0 = yy, angle = angle, lcol = lcol, arr.col = arr.col, 
                    arr.adj = arr.adj, lty = lty, arr.length = arr.length, 
                    arr.width = arr.width, arr.type = arr.type, arr.lwd = arr.lwd, 
                    ...)
}




lattice.Arrowhead <- function (x0, y0, angle = 0, arr.length = 0.4, arr.width = arr.length/2, 
                               arr.adj = 0.5, arr.type = "curved", lcol = "black", 
                               lty = 1, arr.col = lcol, arr.lwd = 2, npoint = 5, ...) 
{
  if (arr.type == "none") {
    return()
  }
  if (arr.type == "curved") {
    rad <- 0.7
    len <- 0.25 * pi
    mid <- c(0, rad)
    x <- seq(1.5 * pi + len, 1.5 * pi, length.out = npoint)
    rr <- cbind(mid[1] - rad * cos(x), mid[2] + rad * sin(x))
    mid <- c(0, -rad)
    x <- rev(x)
    rr <- rbind(rr, cbind(mid[1] - rad * cos(x), mid[2] - 
                            rad * sin(x)))
    mid <- c(rr[nrow(rr), 1], 0)
    rd <- rr[1, 2]
    x <- seq(pi/2, 3 * pi/2, length.out = 3 * npoint)
    rr <- rbind(rr, cbind(mid[1] - rd * 0.25 * cos(x), mid[2] - 
                            rd * sin(x)))
    rr[, 1] <- rr[, 1] * 2.6
    rr[, 2] <- rr[, 2] * 3.45
  }
  else if (arr.type == "triangle") {
    x <- c(-0.2, 0, -0.2)
    y <- c(-0.1, 0, 0.1)
    rr <- 6.22 * cbind(x, y)
  }
  else if (arr.type %in% c("circle", "ellipse")) {
    if (arr.type == "circle") 
      arr.width = arr.length
    rad <- 0.1
    mid <- c(-rad, 0)
    x <- seq(0, 2 * pi, length.out = 15 * npoint)
    rr <- 6.22 * cbind(mid[1] + rad * sin(x), mid[2] + rad * 
                         cos(x))
  }
  if (arr.adj == 0.5) 
    rr[, 1] <- rr[, 1] - min(rr[, 1])/2
  if (arr.adj == 0) 
    rr[, 1] <- rr[, 1] - min(rr[, 1])
  #user <- par("usr")
  pin <- par("din")
  pcm <- pin*2.54
  #sy <- (user[4] - user[3])/pcm[2]
  #sx <- (user[2] - user[1])/pcm[1]
  
  user <- current.panel.limits(unit="inch")
  # pcm <- c(lattice.getOption("layout.widths")$panel$x,
  #          lattice.getOption("layout.heights")$panel$x)
  sy <- (user$ylim[2] - user$ylim[1])/(pin[2])
  sx <- (user$xlim[2] - user$xlim[1])/(pin[1])
  nr <- max(length(x0), length(y0), length(angle), length(arr.length), 
            length(arr.width), length(lcol), length(lty), length(arr.col))
  if (nr > 1) {
    x0 <- rep(x0, length.out = nr)
    y0 <- rep(y0, length.out = nr)
    angle <- rep(angle, length.out = nr)
    arr.length <- rep(arr.length, length.out = nr)
    arr.width <- rep(arr.width, length.out = nr)
    lcol <- rep(lcol, length.out = nr)
    lty <- rep(lty, length.out = nr)
    arr.col <- rep(arr.col, length.out = nr)
  }
  RR <- rr
  for (i in 1:nr) {
    dx <- rr[, 1] * arr.length[i]
    dy <- rr[, 2] * arr.width[i]
    angpi <- angle[i]/180 * pi
    cosa <- cos(angpi)
    sina <- sin(angpi)
    RR[, 1] <- cosa * dx - sina * dy
    RR[, 2] <- sina * dx + cosa * dy
    RR[, 1] <- x0[i] + RR[, 1] * sx
    RR[, 2] <- y0[i] + RR[, 2] * sy
    panel.polygon(RR, col = arr.col[i], border = lcol[i], lty = lty[i], 
                  lwd = arr.lwd, ...)
  }
}


# ggplot theme
#--------------------------------------------------------

theme_ls <- function(){ 
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(

      #grid elements
      axis.line.x = element_line(colour = 'black', size = 0.25, lineend="round"),
      axis.line.y = element_line(colour = 'black', size = 0.25, lineend="round"),
      axis.ticks.x = element_line(colour = 'black', size = 0.25, lineend="round"),
      axis.ticks.y = element_line(colour = 'black', size = 0.25, lineend="round"),
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        size = 10,                #set font size
        margin = margin(b = 5),
        hjust = 0,                #left align
        vjust = 1),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        hjust = 0,                #left align
        margin = margin(b = 5),
        size = 10),               #font size
      
      plot.caption = element_text(           #caption
        size = 8,                 #font size
        hjust = 1),               #right align
      
      axis.title.x = element_text(             #axis titles
        margin = margin(t = 2),
        size = 10),               #font size
      axis.title.y = element_text(             #axis titles
        margin = margin(r = 2),
        size = 10, angle=90),               #font size
      
      axis.text.x = element_text(              #axis text
        margin = margin(t = 1),
        size = 8),
      axis.text.y = element_text(              #axis text
        margin = margin(r = 1),
        size = 8),
      
      strip.background = element_blank(),
      legend.position="none"
    )
}

# theme for dotplot (omits y-axis)

theme_dotplot <- function(){ 
  theme_ls() %+replace%
    
    theme(
      axis.line.y=element_line(colour=NA),
      axis.text.y=element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y=element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust=1, vjust=0),
      panel.grid = element_blank()
    )
}

theme_classic_ls <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                              base_rect_size = base_size/22) 
{
  theme_bw(base_size = base_size, 
           base_family = base_family, 
           base_line_size = base_line_size, 
           base_rect_size = base_rect_size) %+replace% 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black", 
                                   size = rel(1)), 
          legend.key = element_blank(), 
          strip.background = element_blank(), complete = TRUE)
}

    
numformat <- function(x, digits = 2, omit_zero=FALSE) { 
	ifelse(round(x,2)==1,
		   "1.0",
		   ifelse(x == 0 & omit_zero==TRUE, "\u2013",
		   {ncode <- paste0("%.", digits, "f")
		    sub("^(-?)0.", "\\1.", sprintf(ncode, x))}))
}
    
    
    
