library(tidyverse)
library(pwt9)
library(ggmap)
library(sf)
library(ggthemes)

df_artists <- read_csv("../tidytuesday/data/2021/2021-01-12/artists.csv")     
df_artwork <- read_csv("../tidytuesday/data/2021/2021-01-12/artwork.csv")
sf_countries <- st_read("ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")


## countries <- df_artists %>%
##     mutate(country = str_extract(placeOfBirth, '\\b[^,]+$')) %>%
##     select(country) %>%
##     filter(!is.na(country)) 
## countries <- tibble(country = unique(countries$country))
## locations <- mutate_geocode(countries, country)
## write_csv(locations, "locations.csv")

sf_locations <- read_csv("locations.csv") %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords=c("lon", "lat"), crs=4326)

sf_locations$isocode <- sapply(1:length(sf_locations$country),
                               function(x){
                                   int <- st_intersection(sf_locations[x,], sf_countries)
                                   return(int$ISO_A3[1])
                               }) 

pwt <- pwt9.0 %>%
    filter(year == 2014) %>%
    select(isocode, hc, pop, rgdpe, rgdpna)

df <- df_artwork %>%
    select(id, artistId, year, acquisitionYear) %>%
    rename(artworkId = id, id = artistId) %>%
    left_join(df_artists) %>%
    mutate(country = str_extract(placeOfBirth, '\\b[^,]+$')) %>%
    left_join(sf_locations) %>%
    filter(!is.na(isocode), isocode != "-99") %>%
    left_join(pwt) %>%
    filter(isocode != "MSR", isocode != "GBR") %>%
    group_by(isocode) %>%
    summarize(worksPerCap = n()/pop[1],
              hc = unique(hc),
              gdp = unique(rgdpe)/unique(pop)) 

e <- lm(log(worksPerCap) ~ log(gdp), df) %>%
    coefficients()

text <- paste0("On average, a 1% increase in national income is associated \n with a ",
               round(e[2],1), "% increase in the number of artworks at the Tate")

p <-  ggplot(df, aes(x=log(gdp), y=log(worksPerCap))) +
    geom_point() +
    geom_smooth(method="lm", se=F, color="#268bd2") +
    theme_solarized() +
    xlab("GDP Per Capita (log)") +
    ylab("Artwork Per Capita in the Tate (log)") +
    ggtitle(label="Normal Art?", subtitle="Per Capita Artwork and National Income") +
    geom_text(x=8.6, y=5, label=text, color="#268bd2", size=4)
p

ggsave("elasticity.png", p)

sf_gravity <- sf_countries %>%
    rename(isocode = ISO_A3) %>%
    left_join(df) %>%
    filter(!is.na(worksPerCap))
    

p_gravity <- ggplot() +
    geom_sf(data=sf_gravity, aes(fill=log(worksPerCap)), color="#fdf6e3") +    
    geom_sf(data=sf_countries, alpha=0) + 
    scale_fill_gradient(
        low = "#fdf6e3",
        high = "#dc322f") +
    labs(fill="Works Per Capita (log)") + 
     ylim(c(-50, 70)) +
    theme_map() +
    ggtitle("Where are Tate Artworks Produced?",
            subtitle="Most of the Tate collection originanated in former British colonies and Western Europe") +
        theme(legend.position=c(0, 0),
          legend.direction="horizontal",
          legend.background = element_rect(fill = "#fdf6e3"),
          plot.background = element_rect(fill = "#fdf6e3",
                                         linetype="blank",
                                         color="#fdf6e3",
                                         size=0),
          plot.title=element_text(color="#586e75"),
          plot.subtitle=element_text(color="#586e75"),
          legend.text=element_text(color="#586e75"),
          legend.title = element_text(color="#586e75"),
          legend.key.size = unit(.5, 'cm')) 
p_gravity

ggsave("gravity_map.png", p_gravity)


