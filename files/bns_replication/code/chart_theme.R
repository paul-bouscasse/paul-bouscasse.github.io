################################################################################
# THEME FOR CHARTS
# called by main_estimation.R and main_post.R
################################################################################

thick_line <- 1
thick_point <- 4
chart_width <- 6.5
chart_height <- 3.9
label_size <- 3
font_fam <- ""
mytheme <- theme(text = element_text(size=10, family=font_fam),
                 axis.title = element_blank(),
                 panel.background = element_rect(fill = NA),
                 panel.grid.major.y = element_line(colour="grey90"),
                 panel.border =  element_rect(fill = NA),
                 legend.box.margin = margin(-5,-5,-5,-5),
                 legend.key.height = unit(0.1, "lines"))
tit_theme <- theme(plot.title = element_text(size=10, family=font_fam,
                                             face='plain', hjust=.5))
col0 <- c("Black","grey70","Black","grey70","Black","grey70","Black","grey70")
typ0 <- c("solid","solid","dashed","dashed","dotted","dotted","twodash","twodash")
shape0 <- rep(32,length(col0))
mycol <- scale_colour_manual(values = col0)
mytyp <- scale_linetype_manual(values = typ0)
myshape <- scale_shape_manual(values = shape0)
