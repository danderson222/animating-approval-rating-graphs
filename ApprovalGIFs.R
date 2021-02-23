setwd("~/Coding/Blog/US Politics/TrumpApprovalRating")


# First we load the required packages. I include reasons for each package so you know
# The main ones in this case are the magick and gganimate packages
if(!require("tidyverse")) install.packages("tidyverse") # Our rock in data analysis (includes ggplot2)
if(!require("ggsci")) install.packages("ggsci") # Provides awesome color palettes
if(!require("gganimate")) install.packages("gganimate") # Makes animating ggplot graphs easy!!!!
if(!require("magick")) install.packages("magick") # One of my favourite packages ever. All about editing pictures, plots and making GIFs like magic

# First we load the data which is saved in my GitHub repository
# ADD LINK
df <- readRDS("data/CombinedPresidentialApproval.rds")

# First it should be known that we have a ton of information for Trump's approval rating because of the daily polling
# Let's cut this down to reduce the amount of data and the size of the Trump dataset
# We will cut every other day from Trump's approval ratings (which is fair given the lack of variation in the approval rating)
df.trump <- which(df$president == "Trump") # Figure out what rows contain Trump's data
toDelete <- seq(1, nrow(df[c(df.trump[1]:df.trump[1459]),]), 2) + 1716 # Pick every other number and add back the number of rows before the Trump data (1716)
df <- df[-toDelete, ] # Delete the rows identified
rm(df.trump, toDelete)

# Cut df.days by only days in office, president and rolling approval & limit it to first 4  (less than 1461 days)
df <- df %>% 
  mutate(days_in_office=as.numeric(days_in_office)) %>%
  select(president, term.start, days_in_office, rolling_approval) %>%
  filter(days_in_office<1461) %>% 
  na.omit(df)

# Let's try to plot the data to see how it shows up. For this I am just doing a simple ggplot
static.plot <- df %>% 
  ggplot(aes(x = days_in_office, y = rolling_approval, color = as.factor(president),
             text = paste(
               "President: ", president, " - ", round(rolling_approval, digits = 1), "%",
               sep = "")
  )) +
  ggsci::scale_color_simpsons() + # Love this color palette because it has a ton of colors
  geom_line(aes(group = president)) +
  scale_x_continuous(breaks = c(0, 400, 800, 1200, 1600)) +
  labs(x = "Day In Office", 
       y = "Approval Rating",
       title = "How have approval ratings changed by time in office within the first term?",
       color = "President") +
  theme(plot.title = element_text(face="bold", size =14), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12),
        legend.position = "bottom")
static.plot

# Now that we have decided on an animation, I create a function to run for each president's comparison.
# Note that this takes about two minutes to run on my machine. You can play with the frame rates, number of frames and the sizes as well to make it faster/ slower 
president_linecharts <- function(x) {
  
  # Vector of president names except Trump
  compare_presidents <- unique(x[order(x$term.start),]$president)[-c(1,14)]
  
  # A loop to produce ggplot2 graphs
  for (i in seq_along(compare_presidents)) {
    
    # make plots; note data = args in each geom
    plot <- x %>% 
      filter(president=='Trump' | president==compare_presidents[i]) %>% 
      ggplot(aes(x=days_in_office, y=rolling_approval, group=president, colour=president)) +
      geom_point(aes(group = seq_along(days_in_office)),
                 size = 1, alpha = 1, show.legend = FALSE) +
      geom_line(size = 2, show.legend = FALSE) +
      scale_color_manual(values = c("darkblue", "darkred")) +
      scale_x_continuous(breaks=c(200, 400, 600, 800, 1000, 1200, 1400)) +
      ylim(0,100) +
      labs(x = "Day in Office", 
           y = "Presidential Approval Rating",
           title = paste0("Trump's Approval Rating Compared to the First Term of \nEach President Dating Back to 1945"),
           subtitle = "Donald Trump's approval rating remains lower on average than any president in recent history \nduring their first term. Check out all the comparisons for the past 75 years!") +
      annotate(geom="text", x=c(1300, 1300), y=c(10,90), 
               label=c("Trump", compare_presidents[i]),
               color=c("darkred", "darkblue"),
               size = 10, fontface = 'bold', parse = TRUE) +
      theme_bw() +
      theme(plot.title = element_text(face="bold", size = 20), 
            plot.subtitle = element_text(face="bold", size = 12),
            axis.title.x = element_text(face="bold", size = 15),
            axis.title.y = element_text(face="bold", size = 15),
            legend.position = "none")
    
    # Animate the plot
    animated.plot <- plot + 
      transition_reveal(along = days_in_office)
    
    # Adjust the animation settings 
    animate(animated.plot, 
            width = 600, # 900px wide
            height = 400, # 600px high
            nframes = 30, # 30 frames
            fps = 10) # 10 frames per second
    
    # create folder to save the plots to
    if (dir.exists("animations")) { } 
    else {dir.create("animations")}
    
    # save plots to the 'output' folder
    anim_save(filename = paste0("animations/",
                                compare_presidents[i],
                                "_comparison.gif"))
    
    # print each plot to screen
    print(plot)
  }
}
president_linecharts(df)

gif_list <- list.files(path="animations", pattern = '*.gif', full.names = TRUE)
gif_list

# This takes ages to run!!!!!!
gif1 <- image_read(gif_list[12])
gif2 <- image_read(gif_list[5])
gif3 <- image_read(gif_list[8])
gif4 <- image_read(gif_list[7])
gif5 <- image_read(gif_list[9])
gif6 <- image_read(gif_list[6])
gif7 <- image_read(gif_list[3])
gif8 <- image_read(gif_list[11])
gif9 <- image_read(gif_list[2])
gif10 <- image_read(gif_list[4])
gif11 <- image_read(gif_list[1])
gif12 <- image_read(gif_list[10])
presidential_approval <- image_join(gif1, gif2, gif3, gif4, gif5, gif6,
                                    gif7, gif8, gif9, gif10, gif11, gif12)
presidential_approval
image_write(presidential_approval, path = "approvalcomparison.gif")
