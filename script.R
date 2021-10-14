library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(irlba)
library(ggrepel)
library(gridExtra)
library(igraph)
library(tidygraph)
library(ggraph)

tuesdata = tidytuesdayR::tt_load('2021-09-14')

bb = tuesdata$billboard
aud = tuesdata$audio_features

bb = bb %>%
	mutate(year = as.numeric(str_extract(week_id,"\\d+$")),
	decade = paste0(str_extract(year,"^\\d{3}"),"0's"),
	week_id = as.POSIXct(week_id,format = "%m/%d/%Y"))

bb_complete = left_join(bb,aud,by = c("song", "performer", "song_id"))
bb_complete = bb_complete %>%
	mutate(spotify_genre = ifelse(spotify_genre == "[]",NA,str_remove_all(spotify_genre,"\\[|\\]")))

bb_complete = bb_complete %>% 
group_by(song_id) %>% 
arrange(weeks_on_chart) %>% 
mutate(t = round(as.numeric(week_id - week_id[1])/(7*86400))) %>% 
ungroup()

bb_complete = bb_complete %>% mutate(is_christmas_week = (month(week_id)==12 & between(25-day(week_id),1,6)))

bb_complete = bb_complete %>% mutate(month = floor_date(as.Date(week_id),"month"))

## collab is feat.. or &|and not followed by the X's nor orchestra nor band
collab_regex = "(?i)\\s*((?<=[^a-zA-Z])(?:and|&|with)(?=[^a-zA-Z])(?!\\s*the\\s*\\S+s$)(?!.*(?:orchestra|band)$)(?!\\s*his\\s)|\\s(?:w/|[+])\\s|(?<=[^a-zA-Z])(?:feat|introd)\\S*|[,;](?!\\s*[js]r))\\s*"
bb_complete = bb_complete %>% mutate(collab = str_detect(performer, collab_regex))

df_genres_long = bb_complete %>% 
distinct(song_id,spotify_genre) %>% 
## There are a few songs that have 2 genre labels
group_by(song_id) %>% 
slice(1) %>% 
ungroup() %>% 
mutate(spotify_genre = str_remove_all(str_replace_all(spotify_genre,"[\'\"]\\s*,\\s*[\'\"]",","),"[\'\"]\\s*$|^\\s*[\'\"]"), row = row_number(),value = 1) %>% 
separate_rows(spotify_genre,sep = ",")
df_genres = df_genres_long %>% 
pivot_wider(names_from=spotify_genre,values_from = value,values_fill = 0) %>% 
select(-row)

df_genres_mat = df_genres %>% 
select(-song_id) %>% 
as.data.frame()
row.names(df_genres_mat) = df_genres$song_id

#genres_prop = colSums(df_genres_mat)/nrow(df_genres_mat)



cooc_genres = t(as.matrix(df_genres_mat)) %*% as.matrix(df_genres_mat)
# most common 100/50 genres
top100_ids = order(colSums(cooc_genres),decreasing = T)[1:100]
top50_ids = order(colSums(cooc_genres),decreasing = T)[1:50]
## genres that co-occur at least once with rock
rock_conn_ids = which(cooc_genres[,colnames(cooc_genres) == "rock"] > 0)
cooc_genres_100 = cooc_genres[top100_ids,top100_ids]
cooc_genres_50 = cooc_genres[top50_ids,top50_ids]
cooc_genres_rock = cooc_genres[rock_conn_ids, rock_conn_ids]

genres_nb_songs = data.frame(nb_songs = colSums(df_genres_mat),name = names(df_genres_mat))

# only keeping the genres that co-occur with rock
gr_rock = graph_from_adjacency_matrix(cooc_genres_rock,mode = "undirected",diag = FALSE, weighted = "nb")
gr_rock %>% as_tbl_graph() %>% activate(nodes) %>% mutate(m = node_coreness()) %>% as.data.frame() %>% arrange(desc(m))

grw = graph_from_adjacency_matrix(cooc_genres,mode = "undirected",diag = FALSE, weighted = "nb")
gr = graph_from_adjacency_matrix(cooc_genres,mode = "undirected",diag = FALSE)

genre_centrality  = gr %>% 
as_tbl_graph() %>% 
activate(nodes) %>% 
mutate(hub = centrality_hub(), 
		degree = centrality_degree(), 
		between = centrality_betweenness(), 
		core = node_coreness(),
		page_rank = centrality_pagerank(),
		closeness = closeness(.)) %>% 
as.data.frame() %>%
left_join(genres_nb_songs)

df_genres_long_complete = df_genres_long %>%
left_join(genre_centrality, by = c("spotify_genre" = "name")) %>%
select(-c(row,value))


df_genres_main = df_genres_long_complete %>%
group_by(song_id) %>%
summarize(spotify_genre_frequent = spotify_genre[spotify_genre!="adult standards"][which.max(nb_songs)],
		spotify_genre_hub = spotify_genre[which.max(hub)],
		spotify_genre_pagerank = spotify_genre[which.max(page_rank)],
		spotify_genre_least_frequent = spotify_genre[which.min(nb_songs)],
		spotify_genre_rare = (nb_songs<=200)) %>%
ungroup()

bb_complete = left_join(bb_complete, df_genres_main)

# bb_complete = bb_complete %>% select(-c(spotify_genre_frequent, spotify_genre_hub, spotify_genre_pagerank, spotify_genre_rare, spotify_genre_least_frequent))

theme_update(plot.title = element_text(hjust = 0.5))



## Position of songs when first entering Top 100

bb %>% 
	filter(weeks_on_chart == 1) %>%
	ggplot() + facet_grid(vars(decade)) + 	
	geom_density(aes(week_position,col=decade,fill=decade),show.legend = F) + 
	scale_y_continuous(breaks = c(0,0.05),labels = c("","0.05")) +
	ylab("") + 
	xlab("Position")+ 
	theme_classic() + 
	ggtitle("Flatten the curve:\nPosition of songs when they first enter Top 100")  + 
	theme(plot.title = element_text(hjust = 0.5)) 
	
ggsave("~/Projects/TidyTuesday/13092021/Plots/01_first_position.png",width = 6, height = 12)



## Has a Spotify genre depending on how old and how popular
bb_complete %>% 
mutate(genred = !is.na(spotify_genre),year = as.numeric(year)) %>% 
ggplot() + 
geom_bar(aes(x = year,fill = genred),position="fill") + 
scale_fill_brewer(palette = "Set1", labels = c("No","Yes"), name = "Has a spotify genre") + 
theme_classic() + 
ggtitle("Proportion of songs that have\na labelled genre in Spotify") + 
theme(plot.title = element_text(hjust = 0.5))
ggsave("~/Projects/TidyTuesday/13092021/Plots/hasspotifygenre.png",width = 6, height = 6)

bb_complete %>% mutate(genred = !is.na(spotify_genre)) %>% group_by(song_id) %>% summarize(genred = unique(genred),peak_position = min(peak_position),year = unique(year)) %>% ungroup() %>% ggplot() + geom_density(aes(peak_position,fill = genred),alpha = 0.5) + xlab("Peak position") + ylab("") + ggtitle("Peak position of songs that have/don't have\na labelled genre in Spotify") + scale_fill_brewer(palette = "Set1", name = "Has a Spotify genre", labels = c("No", "Yes"))
ggsave("~/Projects/TidyTuesday/13092021/Plots/hasspotifygenre_peakposition.png",width = 6, height = 6)



## Shape of progression 
bb_complete %>% group_by(song_id) %>% filter(any(peak_position == 1)) %>% ungroup() %>% ggplot(aes(x = t, y = week_position, col = decade)) + geom_line(aes(group = song_id),show.legend = FALSE, alpha = 0.3) + facet_grid(vars(decade)) + xlab("Weeks after entering Top 100") + ylab("Top 100 Position") + ggtitle("Billboard evolution of songs\nthat reached nº1") + xlim(c(0,75))

ggsave("~/Projects/TidyTuesday/Spotify_30days_Challenge/Plots/02_evolution_nb1s.png",width = 6, height = 6)



## Song danceability in billboard throughout time
bb_complete %>% 
mutate(flag = ifelse(danceability > median(danceability,na.rm = TRUE),"Danceable","Less danceable")) %>%  
select(week_id,peak_position,song_id, flag) %>% 
ggplot() + 
geom_tile(aes(x = week_id,y = peak_position,fill = flag)) + 
scale_y_reverse(breaks = c(1,25,50,75,100),labels = c("1","25","50","75","100")) + 
scale_fill_brewer(na.value = "white", palette = "Set2",name = "Song is rather") + 
xlab("Time") + 
ylab("Top 100 Position") + 
ggtitle("Danceability of songs in Billboard Top 100") + 
theme_classic()

ggsave("~/Projects/TidyTuesday/Spotify_30days_Challenge/Plots/03_song_danceability_landscape.png",width = 10, height = 4)



## All I wanto for Christmas is you
bb_christmas = bb_complete %>% 
filter(song_id == "All I Want For Christmas Is YouMariah Carey",is_christmas_week) %>% 
select(week_position,week_id)

bb_complete %>% 
filter(song_id == "All I Want For Christmas Is YouMariah Carey") %>% 
ggplot() + 
geom_line(aes(x = week_id, y =peak_position),col = "gray10",linetype = "dashed") + 
geom_point(aes(x = week_id, y =peak_position),alpha = 0.5) + 
scale_y_reverse(breaks = c(1,25,50,75,100),labels = c("1","25","50","75","100")) + 
theme_classic() + 
ylab("Top 100 Position") + xlab("Time") + 
ggtitle("Chart-breaking 'All I want for Christmas is You'\nby Mariah Carey") + geom_hline(aes(yintercept = 1), col = "lightgrey", linetype = "dotted") + 
geom_point(data = bb_christmas, aes(x = week_id, y = week_position),col = "red3", shape = 8, size = 4) + 
annotate("point", x = as.POSIXct(as.Date("01/01/2000","%d/%m/%Y")), y = 20, colour = "red3", size = 4, shape = 8) + 
annotate("text", label = "Christmas week", x = as.POSIXct(as.Date("01/10/2002","%d/%m/%Y")), y = 20) + 
theme(plot.title = element_text(hjust = 0.5)) 

ggsave("~/Projects/TidyTuesday/Spotify_30days_Challenge/Plots/04_all_i_want_for_christmas_is_you.png",width = 6, height = 6)



## Genres PCA baby
pca1 = prcomp_irlba(df_genres_mat, n = 3)

data = data.frame(obsnames=row.names(df_genres_mat), pca1$x)
datapc = data.frame(varnames=colnames(df_genres_mat), pca1$rotation)
mult_1_2 = min(
	(max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
        (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
        )
datapc_1_2 = transform(datapc,
     v1 = 0.7 * mult * (get("PC1")),
     v2 = 0.7 * mult * (get("PC2")))
         
mult_1_3 = min(
	(max(data[,"PC3"]) - min(data[,"PC3"])/(max(datapc[,"PC3"])-min(datapc[,"PC3"]))),
        (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
        )
datapc_1_3 = transform(datapc,
     v1 = 0.7 * mult * (get("PC1")),
     v2 = 0.7 * mult * (get("PC3")))
         
         
plot_1_2 = ggplot() + 
	#geom_text(data,aes(PC1,PC2),alpha=.4, size=3, aes(label=obsnames)) +
	geom_hline(aes(yintercept  = 0), size = 0.2) + 			
	geom_vline(aes(xintercept = 0), size = 0.2) +
    coord_equal() + 
    geom_label_repel(data = datapc_1_2, aes(x = v1, y = v2, label = varnames), segment.color = 'grey80') +
    geom_segment(data = datapc_1_2, aes(x = 0, y = 0, xend = v1, yend = v2), arrow = arrow()) +
    xlab("PC1") +
    ylab("PC2") +
    ggtitle("PCA biplot (PC1 vs PC2) of spotify genres\nbased on songs in Billboard Top 100 from 1958 to today")

plot_1_3 = ggplot() + 
	#geom_text(data,aes(PC1,PC2),alpha=.4, size=3, aes(label=obsnames)) +
	geom_hline(aes(yintercept  = 0), size = 0.2) + 			
	geom_vline(aes(xintercept = 0), size = 0.2) +
    coord_equal() + 
    geom_label_repel(data = datapc_1_3, aes(x = v1, y = v2, label = varnames), segment.color = 'grey80') +
    geom_segment(data = datapc_1_3, aes(x = 0, y = 0, xend = v1, yend = v2), arrow = arrow()) +
    xlab("PC1") +
    ylab("PC3") +
    ggtitle("PCA biplot (PC1 vs PC3) of spotify genres\nbased on songs in Billboard Top 100 from 1958 to today")

pca_plots = arrangeGrob(plot_1_2,plot_1_3,ncol = 2, nrow = 1)
ggsave("~/Projects/TidyTuesday/Spotify_30days_Challenge/Plots/05_pca_biplot_spotify_genres.png", pca_plots ,width = 24, height = 12)



## Collaborations / Featurings
bb_complete %>% 
mutate(collab = ifelse(collab,"Collaboration / Featuring","Solo artist or Band")) %>%
select(week_id,peak_position,song_id, collab) %>% 
ggplot() + 
geom_tile(aes(x = week_id,y = peak_position,fill = collab)) + 
scale_y_reverse(breaks = c(1,25,50,75,100),labels = c("1","25","50","75","100")) + 
scale_fill_brewer(na.value = "white", palette = "Set2",name = "") + 
xlab("Time") + 
ylab("Top 100 Position") + 
ggtitle("Featurings in Billboard Top 100 songs") + 
theme_classic() + 
theme(plot.title = element_text(hjust = 0.5)) 

ggsave("~/Projects/TidyTuesday/Spotify_30days_Challenge/Plots/06_featurings_landscape.png",width = 10, height = 4)

## Highest position a song reached depdending on when it entered Top 100
bb_complete %>% 
filter(between(year,1960,2020)) %>% 
group_by(song_id) %>% 
summarize(first_year = min(year,na.rm = T),min_position = min(peak_position,na.rm = T)) %>% 
ungroup() %>% 
mutate(min_position_group = case_when( min_position <=10 ~ "1-10" , between(min_position,11,30) ~ "11-30" , between(min_position,31,60) ~ "31-60", between(min_position,61,100)~"61-100")) %>% 
ggplot() + 
geom_bar(aes(first_year,fill = min_position_group),position = "fill") + 
xlab(" Date the song entered Top 100") + 
ylab("Proportion") + 
scale_fill_brewer(palette = "Set2",name = "Highest position\nthe song ever reached") + 
ggtitle("Highest position songs ever reached\ndepending on when they first entered Top 100")

ggsave("~/Projects/TidyTuesday/Spotify_30days_Challenge/Plots/07_highest_position_per_year.png",width = 6, height = 6)


## Genre. graph
fc = cluster_fast_greedy(grw)
B = modularity_matrix(grw, membership(fc))
coords = layout_with_fr(grw)
#plot(fc, grw, layout=coords)
#plot_dendrogram(fc)
#plot(grw, vertex.color=membership(fc), layout=coords)

## How does the number of songs labelled with a genre relate to the core
genre_centrality %>%
ggplot(aes(x = hub,y = nb_songs)) +
geom_point() +
geom_label_repel(aes(label = ifelse(nb_songs>=500,name,NA))) +
xlab("Genre hub centrality") +
ylab("Number of songs") +
ggtitle("How does a genre hub centrality relate to the number of songs labelled with the genre?") +
theme(plot.title = element_text(hjust = 0.5)) 

ggsave("~/Projects/TidyTuesday/Spotify_30days_Challenge/Plots/08_genre_hub_centrality_nb_songs.png",width = 20, height = 20)


## COmmunities of genres
## This takes too long to run
# comm1 = cluster_edge_betweenness(
  # gr,
  # weights = E(gr)$weight,
  # directed = FALSE,
  # edge.betweenness = TRUE,
  # merges = TRUE,
  # bridges = TRUE,
  # modularity = TRUE,
  # membership = TRUE
# )

# Main genre of top 1
bb_complete %>% filter(between(year,1958,1970)) %>%
filter(week_position <=1) %>%  distinct(song_id,year,week_id, spotify_genre_frequent) %>% add_count(spotify_genre_frequent)  %>% mutate(spotify_genre_frequent  = ifelse(n<20 ,"Other", spotify_genre_frequent)) %>% 
 ggplot() +
geom_bar(aes(x = year, fill = spotify_genre_frequent), position = "stack") + scale_fill_manual(values = c('brill building pop' = 'darkorange',`bubblegum pop` = 'plum1','classical performance' = 'lightgoldenrod1','doo-wop' = "purple",'easy listening' = 'pink','folk rock' = 'slateblue1','mellow gold' = 'goldenrod2','modern rock' = 'royalblue2','merseybeat' = 'darkseagreen1','motown' = 'firebrick3',`nashville sound` = 'lightgreen','Other' = 'dark grey',`rock`='red','rock-and-roll' = "orangered1", 'rhythm and blues' = 'mediumblue', 'soul' = 'olivedrab4'),na.value = "white",name = "Genre") + theme_classic() + ggtitle("Main spotify genre of Billboard Nº1 songs in the 1960's")

## Collaborations graph
df_feat = bb_complete %>% 
distinct(song_id,performer = str_remove_all(performer,"[()]")) %>% 
filter(str_detect(performer,collab_regex)) %>% 
separate_rows(performer,sep = collab_regex) %>% 
as.data.frame()

## Songs that stayed on a long time that were ever nb 1
bb_complete %>% group_by(song_id) %>% filter(any(peak_position == 1),any(t>1000)) %>% ungroup() %>% as.data.frame()

## Seasonal danceability/energy/valence

## 70's baby

