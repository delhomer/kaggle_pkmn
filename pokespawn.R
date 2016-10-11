################################################
#################### Warm-up ###################
################################################
setwd("C:/Users/Raphaël/Desktop/Kaggle") # Set the working directory path
librarypath = "C:/Users/Raphaël/Documents/R/library"
library("plotrix",lib=librarypath) # 'draw.circle' command
          
################################################
################# Data checking ################
################################################
pokespawn = read.csv("data/pokemon-spawns.csv",stringsAsFactors=F)
summary(pokespawn)
str(pokespawn)
# Replace '-1' value by NAs (before date transformations)
pokespawn$encounter_ms = replace(pokespawn$encounter_ms,which(pokespawn$encounter_ms==-1),NA)
# Change temporal data into dedicated date format
pokespawn$encounter_ms = as.POSIXct(pokespawn$encounter_ms/1000,origin="1970-01-01",tz="America/Los_Angeles")
pokespawn$disppear_ms = as.POSIXct(pokespawn$disppear_ms/1000,origin="1970-01-01",tz="America/Los_Angeles")
# Factorize s2_token argument
pokespawn$s2_token = factor(pokespawn$s2_token)
# Change Nidoran male and female names (encoding problem)
pokespawn[which(pokespawn$num==29),"name"] = "Nidoran f"
pokespawn[which(pokespawn$num==32),"name"] = "Nidoran m"
pokespawn$name = factor(pokespawn$name)
# Filter double records
pokespawn = unique(pokespawn)
names(pokespawn) = c(names(pokespawn)[1:2],"pkmn_id",names(pokespawn)[4:8])

################################################
############# Enhance data set #################
################################################
### Pokemon list (cf pokebip.com)
pokelist$type1 = pokelist$name
pokelist$type2 = pokelist$name
pokelist = data.frame(pkmn_id=1:151)
pokelist$name = c("Bulbasaur","Ivysaur","Venusaur","Charmander","Charmeleon","Charizard","Squirtle","Wartortle","Blastoise","Caterpie","Metapod","Butterfree","Weedle","Kakuna","Beedrill","Pidgey","Pidgeotto","Pidgeot",
                  "Rattata","Raticate","Spearow","Fearow","Ekans","Arbok","Pikachu","Raichu","Sandshrew","Sandslash","Nidoran f","Nidorina","Nidoqueen","Nidoran m","Nidorino","Nidoking","Clefairy","Clefable",
                  "Vulpix","Ninetales","Jigglypuff","Wigglytuff","Zubat","Golbat","Oddish","Gloom","Vileplume","Paras","Parasect","Venonat","Venomoth","Diglett","Dugtrio","Meowth","Persian",
                  "Psyduck","Golduck","Mankey","Primeape","Growlithe","Arcanine","Poliwag","Poliwhirl","Poliwrath","Abra","Kadabra","Alakazam","Machop","Machoke","Machamp","Bellsprout","Weepinbell","Victreebel",
                  "Tentacool","Tentacruel","Geodude","Graveler","Golem","Ponyta","Rapidash","Slowpoke","Slowbro","Magnemite","Magneton","Farfetch\'d","Doduo","Dodrio","Seel","Dewgong","Grimer","Muk","Shellder","Cloyster",
                  "Gastly","Haunter","Gengar","Onix","Drowzee","Hypno","Krabby","Kingler","Voltorb","Electrode","Exeggcute","Exeggutor","Cubone","Marowak","Hitmonlee","Hitmonchan","Lickitung","Koffing","Weezing","Rhyhorn","Rhydon",
                  "Chansey","Tangela","Kangaskhan","Horsea","Seadra","Goldeen","Seaking","Staryu","Starmie","Mr.Mime","Scyther","Jynx","Electabuzz","Magmar","Pinsir","Tauros","Magikarp","Gyarados","Lapras","Ditto","Eevee",
                  "Vaporeon","Jolteon","Flareon","Porygon","Omanyte","Omastar","Kabuto","Kabutops","Aerodactyl","Snorlax","Articuno","Zapdos","Moltres","Dratini","Dragonair","Dragonite","Mewtwo","Mew"
)
pokelist$type1 = c(rep("grass",3),rep("fire",3),rep("water",3),rep("insect",6),rep("normal",7),# -> 22: Fearow
                   rep("poison",2),rep("electric",2),rep("ground",2),rep("poison",6),rep("fairy",2),rep("fire",2),# -> 38 : Ninetales
                   rep("normal",2),rep("poison",2),rep("grass",3),rep("insect",4),rep("ground",2),rep("normal",2),# 53 -> Persian
                   rep("water",2),rep("fight",2),rep("fire",2),rep("water",3),rep("psy",3),rep("fight",3),rep("grass",3),rep("water",2),# 73 -> Tentacruel
                   rep("rock",3),rep("fire",2),rep("water",2),rep("electric",2),rep("normal",3),rep("water",2),rep("poison",2),rep("water",2),# 91 -> Cloyster
                   rep("ghost",3),"rock",rep("psy",2),rep("water",2),rep("electric",2),rep("grass",2),rep("ground",2),rep("fight",2),"normal",# 108 -> Lickitung
                   rep("poison",2),rep("ground",2),"normal","grass","normal",rep("water",6),"psy","insect","ice","electric","fire","insect","normal",rep("water",3),# 131 -> Lapras
                   rep("normal",2),"water","electric","fire","normal",rep("rock",5),"normal","ice","electric","fire",rep("dragon",3),rep("psy",2)
)
pokelist$type2 = c(rep("poison",3),"","","fly",rep("",5),"fly",rep("poison",3),rep("fly",3),"","",rep("fly",2),# -> 22: Fearow
                   rep("",8),"ground",rep("",2),"ground",rep("",4),# -> 38 : Ninetales
                   rep("fairy",2),rep("fly",2),rep("poison",3),rep("grass",2),rep("poison",2),rep("",4),# 53 -> Persian
                   rep("",8),"fight",rep("",6),rep("poison",5),# 73 -> Tentacruel
                   rep("ground",3),rep("",2),rep("psy",2),rep("steel",2),rep("fly",3),"","ice",rep("",3),"ice",# 91 -> Cloyster
                   rep("poison",3),"ground",rep("",6),rep("psy",2),rep("",5),# 108 -> Lickitung
                   rep("",2),rep("rock",2),rep("",8),"psy","fairy","fly","psy",rep("",5),"fly","ice",# 131 -> Lapras
                   rep("",6),rep("water",4),"fly","",rep("fly",3),rep("",2),"fly",rep("",2)
)
pkmn_types = unique(c(unique(pokelist$type2[-which(pokelist$type2=="")]),unique(pokelist$type1))) ; pkmn_types
pokelist$type1 = factor(pokelist$type1,levels=pkmn_types)
pokelist$type2 = factor(pokelist$type2,levels=pkmn_types)
pokelist$evol = c(rep(1:3,6),rep(1:2,5),rep(1:3,2),rep(1:2,4),rep(1:3,1),rep(1:2,7),rep(1:3,4),rep(1:2,1),rep(1:3,1),rep(1:2,3),1,rep(1:2,4),rep(1:3,1),1,rep(1:2,5),rep(1,3),rep(1:2,2),rep(1,3),rep(1:2,3),rep(1,7),rep(1:2,1),rep(1,2),1,rep(2,3),1,rep(1:2,2),rep(1,5),rep(1:3,1),rep(1,2))
pokelist$evolmax = c(rep(3,18),rep(2,10),rep(3,6),rep(2,8),rep(3,3),rep(2,14),rep(3,12),rep(2,2),rep(3,3),rep(2,6),1,rep(2,8),rep(3,3),1,rep(2,10),rep(1,3),rep(2,4),rep(1,3),rep(2,6),rep(1,7),rep(2,2),rep(1,2),rep(2,4),1,rep(2,4),rep(1,5),rep(3,3),rep(1,2))
str(pokelist)
### Spawning places
spawnloc = unique(pokespawn[,c("lng","lat")])
# Function giving the euclidian distance in degree of latitude/longitude
# San Francisco coordinates: lng=-122.42, lat=37.77
sfcoordinates = c(-122.42,37.77)
distance=function(candidateLng,candidateLat,ref=sfcoordinates){
  Vectorize(function(x,y){sqrt((x-ref[1])^2+(y-ref[2])^2)},vectorize.args=c("x","y"))(candidateLng,candidateLat)
}
spawnloc = cbind(spawnloc,disttosf=distance(spawnloc$lng,spawnloc$lat))
spawnloc = spawnloc[order(spawnloc$disttosf),]
spawnloc = cbind(loc_id=1:nrow(spawnloc),spawnloc)
### New pokespawn data set
pokespawn = merge(spawnloc,pokespawn)
pokespawn = merge(pokelist,pokespawn)
pokespawn = pokespawn[order(pokespawn$disppear_ms),]

################################################
############# Most viewed pokemons #############
################################################
spawn_pkmn = length(unique(pokespawn$name)) # Number of pokemon species
spawn_pkmn_prop = spawn_pkmn/nrow(pokelist)
pokelist$spawn_freq = 100*round(prop.table(table(factor(pokespawn$pkmn_id,levels=1:length(pokelist$pkmn_id)))),5)
unspawning_pkmn = pokelist[pokelist$spawn_freq==0,] ; unspawning_pkmn
spawning_pkmn = pokelist[pokelist$spawn_freq>0,]
mostviewed = spawning_pkmn[order(spawning_pkmn$spawn_freq,decreasing=T),]
pkmn_descr = function(id,tab=pokelist){
  unname(
    apply(tab[id,],1,function(x){
      type_str = x[which(names(x)=="type1")]
      if(!is.na(x[which(names(x)=="type2")])){
        type_str = paste(x[which(names(x)=="type1")],x[which(names(x)=="type2")],sep="/")
      }
      return(paste("Num.",x[which(names(x)=="id")]," - ",type_str," - evol:",x[which(names(x)=="evol")],"/",x[which(names(x)=="evolmax")],sep=""))
    })
  )
}
par(mfrow=c(1,1),mar=c(2,6,0,1))
barplot(mostviewed[1:10,"spawn_freq"],names.arg=mostviewed[1:10,"name"],las=2,cex.lab=0.5,cex.axis=0.5, horiz=T,xlim=c(0,30),col="lightblue",border="blue")
text(20,seq(0.5,10,1)+seq(0:9)*0.2,pkmn_descr(1:10,mostviewed),font=3,col="grey")
barplot(mostviewed[(nrow(mostviewed)-9):nrow(mostviewed),"spawn_freq"],names.arg=mostviewed[(nrow(mostviewed)-9):nrow(mostviewed),"name"],las=2,cex.lab=0.5,cex.axis=0.5, horiz=T,xlim=c(0,0.05),col="lightblue",border="blue")
text(0.03,seq(0.5,10,1)+seq(0:9)*0.2,pkmn_descr((nrow(mostviewed)-9):nrow(mostviewed),mostviewed),font=3,col="grey")

################################################
############# Geographical analysis ############
################################################
sfpokespawn = pokespawn[distance(pokespawn$lng,pokespawn$lat)<1,]
library("ggplot2","ggmap",lib=librarypath)
sfmap = ggmap(sfcoordinates)
# Plot the spawned pokemon in San Francisco bay
par(mar=c(3,4,1,1))
plotshift = 0.6
plot(x=sfpokespawn$lng,y=sfpokespawn$lat,ylim=c(sfcoordinates[2]-plotshift,sfcoordinates[2]+plotshift),xlim=c(sfcoordinates[1]-plotshift,sfcoordinates[1]+plotshift)
     ,col="red",xlab="Longitude",ylab="Latitude",pch=15,cex=0.25,xaxp=c(-122.9,-121.7,6),type="n")
points(x=sfpokespawn$lng,y=sfpokespawn$lat,ylim=c(sfcoordinates[2]-plotshift,sfcoordinates[2]+plotshift),xlim=c(sfcoordinates[1]-plotshift,sfcoordinates[1]+plotshift)
     ,col="red",xlab="Longitude",ylab="Latitude",pch=15,cex=0.25,xaxp=c(-122.9,-121.7,6))
points(sfcoordinates[1],sfcoordinates[2],pch=3)
draw.circle(sfcoordinates[1],sfcoordinates[2],1)

################################################
############### Temporal analysis ##############
################################################
# Time available for pokemon haunting (difference between scan and despawn)
pokespawn$availability = as.numeric(round(abs(difftime(pokespawn$encounter_ms,pokespawn$disppear_ms,units="secs"))))
par(mar=c(3,4,2,1))
hist(pokespawn$availability,breaks=seq(0,900,30),freq=T,xaxp=c(0,900,30),col="lightblue",border="blue",main="Histogram of available haunting time (in sec)")
maxhauntingtime = max(pokespawn$availability,na.rm=T) ; maxhauntingtime
# Haunting time seems to follow an uniform distribution (that's not really surprising...)
# Function checking equality between two dates (i.e. existence of a time difference of less than one second)
timecheck = function(date1,date2){
  abs(difftime(date2,date1,units="secs"))<1
}
# Check periodicity of spawning process (whatever the location site)
periodicity = c()
for(l in spawnloc$loc_id){
  # Consider only scans linked with l-th spawning location
  curloctab = pokespawn[pokespawn$loc_id==l & !is.na(pokespawn$encounter_ms), ]
  # If there is different scans AND if they correspond to different dates, check the periodicity
  if(nrow(curloctab)>1){
    if(length(unique(curloctab$encounter_ms))>1){
      # print(paste("loc_id:",l))
      # print(curloctab)
      periodicity = c( periodicity , as.numeric(difftime(curloctab[-1,"encounter_ms"],curloctab[1,"encounter_ms"],units="sec")) )
    }
  }
}
#