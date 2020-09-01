#### CODE FOR SORTING OUT AND SUMMARIZING CASA AUTOMATED OUTPUTS
packs <- c("dplyr", "tidyr" ,"broom")
sapply(packs, require, character.only = TRUE)


## Function to merge all the data files produced on the CASA python batch, and adds column with their respective file names with a desired name length

casa_wrangling<-function(wd, name_length){
  setwd(wd)
  casa_raw<-data.frame()
  for(i in 1:length(list.files(path="."))){
    t<-read.table(file=list.files(path=".")[i],
                h=F,
                nrow = length( readLines( list.files(path=".")[i] ) ) -1 )
    colnames(t) <- c("vcl", "vap", "vsl", "lin", "wob", "prog", "bcf")
    t<-mutate(t, video = substr(list.files(path=".")[i],1,name_length))
    casa_raw<-rbind(casa_raw,t)
  }
  return(casa_raw)
}

# wd              working directory where the casa files are stored
# name_lenth      number of characters taken from file name to make video_name column




## function that returns the proportion of motile sperm, number of tracks, and averages from only motile sperm for the parameters obtained from the CASA

casa_summary<-function(raw_data, min_vcl, min_vap, min_vsl){
  
  tracks <- raw_data %>% group_by(video) %>% dplyr:::summarise(sperm_tracks = length(vcl)) %>% as.data.frame()
  
  s1<-subset(raw_data, vcl > min_vcl)
  s2<-subset(s1, vap > min_vap)
  s3<-subset(s2, vsl > min_vsl)
  
  motile<-s3 %>% group_by(video) %>% dplyr:::summarise(motile_sperm = length (vcl)) %>% as.data.frame()
  
  long_tracks<-gather(tracks, "trait", "values", sperm_tracks)
  long_motile<-gather(motile, "trait", "values", motile_sperm)
  long_motile<-rbind(long_motile,long_tracks)
  motility<-long_motile %>% spread(trait,values) %>% group_by(video) %>% mutate(prop_mot = motile_sperm / sperm_tracks) %>% as.data.frame() %>% gather("casa_measure", "average_values", prop_mot, sperm_tracks)
  
  long_s3<-gather(s3, "casa_measure", "casa_value", vcl, vap, vsl, lin, wob, prog, bcf)
  
  summarized_casa<-long_s3 %>% group_by(video, casa_measure) %>% dplyr:::summarise(average_values = mean(casa_value)) %>% as.data.frame() %>% rbind (motility[,c(1,3,4)])

  wide_summarized_casa<-summarized_casa %>% spread(casa_measure,average_values)
  
  return(wide_summarized_casa)
}

# raw_data        an object generated from casa_wrangling
# min_vcl         minimum vcl to consider a motile sperm
# min_vap         minimum vap to consider a motile sperm
# min_vsl         minimum vsl to consider a motile sperm


t_0<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_0", name_length = 10) %>% mutate(sampling_time = 0)

t_15<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_15", name_length = 10) %>% mutate(sampling_time = 15)

t_30<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_30", name_length = 10) %>% mutate(sampling_time = 30)

t_45<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_45", name_length = 10) %>% mutate(sampling_time = 45)

t_60<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_60", name_length = 10) %>% mutate(sampling_time = 60)

setwd("~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses")

casa_data_raw <- rbind(t_0, t_15, t_30, t_45, t_60)

write.table(casa_data_raw, file = "Hasli15_raw_sperm_data.txt", sep = " ", dec = ".", row.names = FALSE, col.names = TRUE)

qua_0<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_0", name_length = 10) %>% casa_summary(min_vcl = 20, min_vap = 15, min_vsl = 10)  %>% mutate(sampling_time = 0)

qua_15<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_15", name_length = 10) %>% casa_summary(min_vcl = 20, min_vap = 15, min_vsl = 10)  %>% mutate(sampling_time = 15)

qua_30<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_30", name_length = 10) %>% casa_summary(min_vcl = 20, min_vap = 15, min_vsl = 10)  %>% mutate(sampling_time = 30)

qua_45<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_45", name_length = 10) %>% casa_summary(min_vcl = 20, min_vap = 15, min_vsl = 10)  %>% mutate(sampling_time = 45)

qua_60<-casa_wrangling(wd = "~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses/time_60", name_length = 10) %>% casa_summary(min_vcl = 20, min_vap = 15, min_vsl = 10)  %>% mutate(sampling_time = 60)

setwd("~/University_of_Neuchatel/Hasli_2015/Sperm_Video_analyses")

hasli_15_sperm_quality <- rbind(qua_0, qua_15, qua_30, qua_45, qua_60)

write.table(hasli_15_sperm_quality, file = "Hasli15_sperm_data.txt", sep = " ", dec = ".", row.names = FALSE, col.names = TRUE)



