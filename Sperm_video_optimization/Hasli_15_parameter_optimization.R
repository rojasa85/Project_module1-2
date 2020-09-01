par_opt<-read.table(file.choose(),h=T)

packages <- c("ggplot2", "dplyr", "tidyr" ,"scales" ,"broom" ,"gridExtra", "grid", "lattice", "vegan", "vegetarian", "ggmap", "maptools")

sapply(packages, library, character.only = TRUE)


vid<-factor(par_opt$sperm_video)

mot.plot<-ggplot(data = par_opt) +
  aes(
    x=ratio,
    y=prop.motile,
    color=factor(frames),
    shape=factor(frames)
  ) +
  xlab("Search ratio (pixels)") +
  ylab("Proportion of motile sperm") +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_line(stat="summary", fun.y="mean")

print(mot.plot)



vcl.plot<-ggplot(data = par_opt) +
  aes(
    x=ratio,
    y=vcl,
    color=factor(frames),
    shape=factor(frames)
  ) +
  xlab("Search ratio (pixels)") +
  ylab("Sperm Curvilinear Velocity (um/s)") +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_line(stat="summary", fun.y="mean")

print(vcl.plot)





tracks.plot<-ggplot(data = par_opt) +
  aes(
    x=ratio,
    y=sperm.track,
    color=factor(frames),
    shape=factor(frames)
  ) +
  xlab("Search ratio (pixels)") +
  ylab("Sperm tracks") +
  geom_point() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_line(stat="summary", fun.y="mean")

print(tracks.plot)








min_speed_opt<-read.table(file.choose(),h=T)

vcl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==50,], vcl)
print(vcl.dis)

vcl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==100,], vcl)
print(vcl.dis)

vcl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==150,], vcl)
print(vcl.dis)

vcl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==200,], vcl)
print(vcl.dis)

vcl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==250,], vcl)
print(vcl.dis)

vcl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==300,], vcl)
print(vcl.dis)



vsl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==50,], vsl)
print(vsl.dis)

vsl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==100,], vsl)
print(vsl.dis)

vsl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==150,], vsl)
print(vsl.dis)

vsl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==200,], vsl)
print(vsl.dis)

vsl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==250,], vsl)
print(vsl.dis)

vsl.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==300,], vsl)
print(vsl.dis)



vap.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==50,], vap)
print(vap.dis)

vap.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==100,], vap)
print(vap.dis)

vap.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==150,], vap)
print(vap.dis)

vap.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==200,], vap)
print(vap.dis)

vap.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==250,], vap)
print(vap.dis)

vap.dis<-qplot(data = min_speed_opt[min_speed_opt$Sperm_video==300,], vap)
print(vap.dis)




?qplot


