require(sf)
require(dplyr)

comuni_lomb.sf <- 
  read_sf("/Users/francesco/Downloads/Limiti01012020/Com01012020/Com01012020_WGS84.shp") %>%
  filter(COD_REG == 3)

comuni_lomb.sf$match <- tolower(comuni_lomb.sf$COMUNE)
comuni_lomb.sf$match <- gsub("è", "e", comuni_lomb.sf$match)
comuni_lomb.sf$match <- gsub("ò", "o", comuni_lomb.sf$match)
comuni_lomb.sf$match <- gsub("ù", "u", comuni_lomb.sf$match)


casi.df <- 
  read.delim("~/public_git/coronavirus-ita/lombardia_comune_data/casi.csv", 
             header=FALSE, stringsAsFactors = F)
casi.df <- casi.df[!duplicated(casi.df$V1),]

casi.df$match <- gsub("'$", "", tolower(casi.df$V1))

casi.df$match[!casi.df$match %in% comuni_lomb.sf$match]
casi.df$match[casi.df$match == "rodengo-saiano"] <- 
  "rodengo saiano"
casi.df$match[casi.df$match == "gabbioneta binanuova"] <- 
  "gabbioneta-binanuova"
casi.df$match[casi.df$match == "godiasco"] <- 
  "godiasco salice terme"
casi.df$match[casi.df$match == "gadesco pieve delmona"] <- 
  "gadesco-pieve delmona"
casi.df$match[casi.df$match == "roe' volciano"] <- 
  "roe volciano"

comuni_lomb.sf$covid19 <- 
  casi.df$V2[match(comuni_lomb.sf$match, casi.df$match)]

library(DBI)
library(RPostgreSQL)

conn <- dbConnect(
  drv = PostgreSQL(),
  dbname = "istat_sez2011",
  host = "localhost",
  port = "5432",
  user = "francesco",
  password = "")

options(scipen=999)

require(rpostgis)

sez2011_pop <-
  dbGetQuery(conn, 'SELECT sez2011, "P1" FROM sez2011.census_poly_wt_pop;')

sez2011_centroids <-
  pgGetGeom(conn, c("sez2011", "census_poly_centroids"))

sez2011_centroids$pop2011 <-
  sez2011_pop$P1[match(sez2011_centroids$sez2011 , sez2011_pop$sez2011)]
sez2011_centroids <- 
  sez2011_centroids[!is.na(sez2011_centroids$pop2011),]

require(sp)
res <- 
  over(sez2011_centroids, 
       as_Spatial(comuni_lomb.sf %>% st_transform(4326)))

sez2011_centroids$PRO_COM_T <- res$PRO_COM_T

res_lomb <- 
  sez2011_centroids@data %>%
  dplyr::filter(!is.na(PRO_COM_T)) %>%
  dplyr::group_by(PRO_COM_T) %>%
  dplyr::summarize(pop2011 = sum(pop2011))

comuni_lomb.sf$pop2011 <-
  res_lomb$pop2011[match(comuni_lomb.sf$PRO_COM_T, 
                         res_lomb$PRO_COM_T)]

comuni_lomb_simp.sf <- 
  rmapshaper::ms_simplify(input = as(comuni_lomb.sf, 'Spatial'), keep = 0.1) %>%
  st_as_sf()

require(ggplot2)
require(viridis)

province_lomb.sf <- 
  read_sf("/Users/francesco/Downloads/Limiti01012020/ProvCM01012020/ProvCM01012020_WGS84.shp") %>%
  filter(COD_REG == 3)

comuni_lomb_centroids.df <- 
  comuni_lomb_simp.sf %>%
  st_centroid() %>%
  # st_transform(4326) %>%
  st_coordinates() %>%
  data.frame()

comuni_lomb_centroids.df$label <- NA
comuni_lomb_centroids.df$label[comuni_lomb_simp.sf$COMUNE == "Codogno"] <- "Codogno"
comuni_lomb_centroids.df$label[comuni_lomb_simp.sf$COMUNE == "Nembro"] <- "Nembro"

require(ggrepel)

comuni_lomb_simp.sf$covid19[is.na(comuni_lomb_simp.sf$covid19)] <- 0
set.seed(28100)
ggsave(filename = "img.png", width = 9, height = 9,
ggplot(comuni_lomb_simp.sf) +
  geom_sf(aes(fill = covid19/(pop2011/10000)), colour = 'white', size = .01) +
  geom_sf(data = province_lomb.sf, fill = NA) +
  geom_label_repel(data = comuni_lomb_centroids.df, aes(x=X,y=Y,label=label),
                   force = 1300, alpha = .8, segment.alpha = .3) +
  scale_fill_distiller(palette = "Spectral") +
  # scale_fill_viridis() +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom') +
  labs(title = "Casi Covid-19 per 10,000 abitanti (22 Marzo 2020)",
       caption = "Source: Corriere della Sera (bit.ly/2Uc14nT)",
       fill = NULL, x = NULL, y = NULL))

comuni_lomb_simp.df <- 
  comuni_lomb_simp.sf %>%
  as.data.frame() %>%
  select(COD_RIP, COD_REG, COD_PROV, COD_CM, COD_UTS,
         PRO_COM, PRO_COM_T, COMUNE, COMUNE_A, CC_UTS,
         covid19, pop2011)

write.csv(comuni_lomb_simp.df, 
          file = "comuni_lomb_covid19-20200322.csv", 
          row.names = F)

  