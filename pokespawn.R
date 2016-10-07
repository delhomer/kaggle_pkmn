librepos="C:/Users/Raphaël/Documents/R/library"
setwd("C:/Users/Raphaël/Desktop/Kaggle")

pokespawn = read.csv("data/pokemon-spawns.csv",stringsAsFactors=F)
summary(pokespawn)
str(pokespawn)
# Factorize s2_token argument
pokespawn$s2_token = factor(pokespawn$s2_token)
# Filter '-1' values in encounter column
pokespawn = pokespawn[-which(pokespawn$encounter_ms==-1),]
# Change temporal data into dedicated date format
pokespawn$encouter_ms = as.POSIXct(pokespawn$encounter_ms/1000,origin="1970-01-01",tz="America/Los_Angeles")
pokespawn$disppear_ms = as.POSIXct(pokespawn$disppear_ms/1000,origin="1970-01-01",tz="America/Los_Angeles")
# Change Nidoran male and female names (encoding problem)
pokespawn[which(pokespawn$num==29),"name"] = "Nidoran f"
pokespawn[which(pokespawn$num==32),"name"] = "Nidoran m"
pokespawn$name = factor(pokespawn$name)

### Most viewed pokemons
str(unique(pokespawn$name))
pokelist = pokespawn[match(unique(pokespawn$name),pokespawn$name),c("num","name")]
pokelist = pokelist[order(pokelist$num),]
pokelist = cbind(pokelist,prop.table(table(pokespawn$num)))[-3]
View(pokelist)
mostviewed = pokelist[order(pokelist$Freq,decreasing=T),]
barplot(mostviewed[1:20,"Freq"],names.arg=mostviewed[1:20,"name"],las=2,cex.lab=0.5,cex.axis=0.5)

### Geographical analysis
# San Francisco coordinates: lng=-122.42, lat=37.77
sfpokespawn = pokespawn[abs(pokespawn$lng+122.42)<0.75&abs(pokespawn$lat-37.77)<0.75,]
plot(x=sfpokespawn[,"lng"],y=sfpokespawn[,"lat"],ylim=c(37,38.5),xlim=c(-123.15,-121.65),pch=15,col="grey",cex=0.5)
plot(x=sfpokespawn[sfpokespawn$name=="Pidgey","lng"],y=sfpokespawn[sfpokespawn$name=="Pidgey","lat"],ylim=c(37,38.5),xlim=c(-123.15,-121.65),pch=15,col="grey",cex=0.5)

### Temporal analysis
# sfpokespawn