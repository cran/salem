## ----setup, include = FALSE---------------------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE, warning = FALSE
)
knitr::opts_chunk$set(error = TRUE)

## ----libraries----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
library(salem)

## -----------------------------------------------------------------------------
if (!requireNamespace("sf", quietly = TRUE)) {
      knitr::knit_exit()
}

## ----eval=TRUE,fig.width=8, fig.height=8--------------------------------------
# Set up some common elements.



      p1 <- ggplot(salem_region)  +
             geom_sf_text(aes(label = TOWN_LABEL), color = "blue", size = 2, 
                              nudge_x = 5,
                               nudge_y = 5, na.rm = TRUE) 
        p2 <- scale_fill_manual(values = c( "grey", "red"), na.value = "white") 



## ----eval=TRUE----------------------------------------------------------------


newdata <- salem_region |> mutate(February.Any = February > 0, 
                                       March.Any = March > 0,
                                   April.Any = April > 0, May.Any = May > 0,
                                   June.Any = June > 0, July.Any = July > 0,
                                   August.Any = August > 0,
                                   September.Any = September >0,
                                   October.Any = October > 0,
                                   November.Any = November > 0,
                                   TOWN_LABEL = ifelse(n_accused == 0, 
                                                       NA,  TOWN_LABEL)
                                   )


## ----eval=TRUE,fig.width=6, fig.height=6--------------------------------------

      p1  + geom_sf(data = newdata,  
                    aes(geometry = geometry,  fill = February.Any
                        ), color = "black", 
                    size = .1)+
                   labs(title = "Location of Accusations in February") +
                   scale_fill_manual(values = c( "grey", "red"), na.value = "white",
                                     labels = c("None", "At least 1", "Never"))  +
                  theme(legend.title = element_blank(), legend.position = "bottom")

      p1  + geom_sf(data = newdata,  
                    aes(geometry = geometry,  fill = March.Any
                        ), color = "black", 
                    size = .1)+
                   labs(title = "Location of Accusations in March") +
                   scale_fill_manual(values = c( "grey", "red"), na.value = "white",
                                     labels = c("None", "At least 1", "Never"))  +
                  theme(legend.title = element_blank(), legend.position = "bottom")
      
      p1  + geom_sf(data = newdata,  
                    aes(geometry = geometry,  fill = April.Any
                        ), color = "black", 
                    size = .1)+
                   labs(title = "Location of Accusations in April") +
                  scale_fill_manual(values = c( "grey", "red"), na.value = "white",
                                    labels = c("None", "At least 1", "Never")) +
                  theme(legend.title = element_blank(), legend.position = "bottom")
      
      p1 +  geom_sf(data = newdata, 
                    aes(geometry = geometry, fill = July.Any), 
                    color = "black", size = .1) +
                    labs(title = "Location of Accusations in July") +
                    scale_fill_manual(values = c( "grey", "red"), na.value = "white",
                                      labels = c("None", "At least 1", "Never")) +
                  theme(legend.title = element_blank(), legend.position = "bottom")
  
      p1 + geom_sf(data = newdata,  
                   aes(fill = August.Any, geometry = geometry), 
                   color = "black", 
                   size = .1) +
                  labs(title = "Location of Accusations in August",
                       labels = c("None", "At least 1", "Never")) +
                  scale_fill_manual(values = c( "grey", "red"), na.value = "white",
                                    labels = c("None", "At least 1", "Never")) +
                  theme(legend.title = element_blank(), legend.position = "bottom")
      
       p1 + geom_sf(data = newdata,  
                   aes(fill = October.Any, geometry = geometry), 
                   color = "black",
                   size = .1) +
                   labs(title = "Location of Accusations in October",
                        ) +
                  scale_fill_manual(values = c( "grey", "red"), na.value = "white",
                                    labels = c("None", "At least 1", "Never"))  +
                  theme(legend.title = element_blank(), legend.position = "bottom") 
      p1 + geom_sf(data = newdata,  
                   aes(fill = November.Any, geometry = geometry), 
                   color = "black",
                   size = .1) +
                   labs(title = "Location of Accusations in November") +
                   scale_fill_manual(values = c( "grey", "red"), na.value = "white",
                                    labels = c("None", "At least 1", "Never"))  +
                   theme(legend.title = element_blank(), legend.position = "bottom")


