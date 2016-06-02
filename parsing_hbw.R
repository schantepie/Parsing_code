library(httr)
library(XML)
library(jpeg)


##################"RECUPERATION ET MISE EN FORME DES NOMS DE FAMILLES POUR PARSING DU NOM DE TOUTES LES ESPECES DU SITE ##########

doc<- htmlParse("http://www.hbw.com/family/home")
family=xpathSApply(doc, "//ul[contains(@class, 'families clearfix')]", xmlValue)
family=strsplit(family,"->")
family=do.call(c,family)
family=gsub("\\(","",family)
family=gsub(" ","-",family)
family=gsub('\\/',"",family)
family=gsub(")","",family)
family=gsub(",","",family)
family=gsub("Birds-of-paradise","Birds-paradise",family)
page_fam=paste("/family/",family,"#node_family_full_group_fam_view_vt_sp_table",sep="")

##################"PARSING DES NOMS SCIENTIFIQUES ET VERNACULAIRES DES ESPECES PRESENT SUR LE SITE

list_sp_scient=list()
list_sp_vern=list()
handle <- handle("http://www.hbw.com/") 

for(i in 1:length(page_fam)){
  
path=page_fam[i]
fam <- POST(handle = handle, path=path)
fami <- htmlParse(fam)

if (i==176){
  spe=xpathSApply(fami,"//table[contains(@class,'views-table cols-6')][1]",xmlValue)
  spe=strsplit(spe,"\n")[[1]]
  spe=spe[grep("[A-Za-z]",spe)]
  spe=spe[6:length(spe)]
  spe=matrix(spe,ncol=4,byrow=T)
  spe=gsub("'","",spe)
  spe=gsub("            ","",spe)
  spe=gsub("          ","",spe)
  spe=gsub(" ","-",spe)
  list_sp_scient[[i]]=matrix(paste(spe[,1],spe[,2],sep="-"),ncol=1)
  list_sp_vern[[i]]=matrix(spe[,3],ncol=1)
}else{
spe=xpathSApply(fami,"//span[contains(@class,'views-field views-field-field-sp-name-common')]",xmlValue)
spe=gsub("'","",spe)
spe=gsub("\\(","",spe)
spe=gsub(")","",spe)
spe=strsplit(spe," ")
spe=lapply(spe,function(x) x[which(nchar(x)>2)])
spe_scient=lapply(spe,function(x) paste(c(x[c(length(x)-1,length(x))]),collapse="-"))
spe_scient=do.call(rbind,spe_scient)
list_sp_scient[[i]]=spe_scient
spe_vern=lapply(spe,function(x) paste(c(x[-c(length(x)-1,length(x))]),collapse="-"))
spe_vern=do.call(rbind,spe_vern)
list_sp_vern[[i]]=spe_vern
}
}
list_sp_scient[[176]]

save.image("list-sp-all.Rdata")

aa=do.call(rbind,list_sp_scient)
bb=do.call(rbind,list_sp_vern)


write.csv(cbind(aa,bb),"spe_scient_vern_all.csv")

# checking code
# fp=rep(NA,205)
# for (i in 1:205) if(is.null(list_sp_scient[[i]])) fp[i]=i


################## COMPARAISON DU NOM SCIENTIF DES ESPECES DEJA UTLISEES ET SITE HBW POUR COMPILATION HTTP DES ESPECES

load("/media/mnhn/Leca/Migration/all_birds/full_list_jetz_hwb.Rdata")

names_hbw=full_list[,3:4]
colnames(names_hbw)=c("names_sp","name_pages")
head(names_hbw)
names=paste(names_hbw[,2],names_hbw[,1],sep="-")
table_link_sp=paste("species/",names,sep="")

# save(list="table_link_sp", file="table_link_all.Rdata")

##################"PARSING DES MAPS et MOVEMENTS

# load("/media/mnhn/Leca/Migration/all_birds/table_link_all.Rdata")

handle <- handle("http://www.hbw.com/") 
login <- list(
  name = "email_identification"
  ,pass = "lemotdepasse"
  ,form_id ="user_login"
)
response <- POST(handle = handle, path="user", body = login)

movement=list()
map=list()
sp_nam=list()
node=xmlNode("foo", attrs=c(a="1", b="my name"))

# for (i in 1:length(table_link_sp)){

for (i in 9952:9993){
  
path=table_link_sp[i]

if (i==8486) path="species/el-oro-parakeet-pyrrhura-orcesi"
if (i==9819) path=table_link_sp[9818]

if (i==9909) path="species/ruppells-vulture-gyps-rueppelli"
if (i==9952) path="species/hunstein-s-mannikin-lonchura-hunsteini"

path=gsub("ü","-",path)
path=gsub("ä","-",path)
path=gsub("ö","-",path)
path=gsub("of-paradise","paradise",path)
path=gsub("-like-","-",path)

response <- POST(handle = handle, path=path, body = login)
doc <- htmlParse(response)

sp_name=xpathSApply(doc,"//header/h1",xmlValue)
sp_nam[[i]]=sp_name
mapad=xpathSApply(doc, "//img[contains(@style, 'max-width')]",xmlGetAttr(node,name="src"))[[1]]
map_adress=xmlGetAttr(mapad,name="src")
map_binaire=content(GET(map_adress))

  if (is.array(map_binaire)){
    jpeg(file = "maplot.jpeg")
    frame()
    rasterImage(map_binaire, 0, 0, 1, 1)
    dev.off()
    jpg = readJPEG("maplot.jpeg", native=T) 
    map[[i]]=jpg
  }else{
    stop("dummy error")
}
}

http=paste("http://www.hbw.com/",table_link_sp,sep="")

save(list=c("map","sp_nam","http","full_list"),file="map_9001_9993.Rdata")


ggg # rep=rep(NA,1000)
# for (i in 1:1000)  if (map[[i]]=="Error") rep[i]=1




# if (i==352) path="species/yellow-breasted-bird-paradise-loboparadisea-sericea"
# if (i==380) path="species/pitta-ground-roller-atelornis-pittoides"
# if (i==506) path="species/crested-bird-paradise-cnemophilus-macgregorii"
# if (i==507) path="species/Lorias-Bird-paradise-Cnemophilus-loriae"
# if (i==1027) path="/species/st-helena-plover-charadrius-sanctaehelenae"
# if (i==1270) path="/species/thrush-mourner-schiffornis-turdina"
# if (i==2062) path="species/Twelve-wired-Bird-paradise-Seleucidis-melanoleucus"
# if (i==2093) path="species/Guianan-Cock-rock-Rupicola-rupicola"
# if (i==2094) path="species/Andean-Cock-rock-Rupicola-peruvianus"
# if (i==2195) path="species/King-Saxony-Bird-paradise-Pteridophora-alberti"
# if (i==2423) path="species/Standardwing-Bird-paradise-Semioptera-wallacii"
# if (i==2488) path="/species/retz-s-helmet-shrike-prionops-retzii"
# if (i==2586) path="/species/thrush-antpitta-myrmothera-campanisona"
# if (i==2951) path="species/Tit-Dacnis-Xenodacnis-parina"
# if (i==2984) path="species/Lark-Brushrunner-Coryphistera-alaudina"
# if (i==3059) path="species/Wren-Rushbird-Phleocryptes-melanops"
# if (i==3286) path="species/le-contes-thrasher-toxostoma-lecontei"
# if (i==3427) path="species/rio-de-janeiro-antwren-myrmotherula-fluminensis"
# if (i==3667) path="species/fernando-po-swift-apus-sladeniae"
# if (i==3798) path="species/Rio-de-Janeiro-Antbird-Cercomacra-brasiliana"
# if (i==3882) path="species/la-selle-thrush-turdus-swalesi"
# if (i==3932) path="species/Blue-Bird-paradise-Paradisaea-rudolphi"
# if (i==3955) "species/Thrush-Wren-Campylorhynchus-turdinus"
# if (i==5190) path="/species/stiles-s-tapaculo-scytalopus-stilesi"
# if (i==5323) path="/species/ruppells-parrot-poicephalus-rueppellii"
# if (i==5408) path="species/serra-do-mar-tyrannulet-phylloscartes-difficilis"
# if (i==6038) path="species/st-lucia-black-finch-melanospiza-richardsoni"
# if (i==6199) path="species/le-contes-sparrow-ammodramus-leconteii"
# if (i==6384) path="species/shelley-s-oliveback-nesocharis-shelleyi"
# if (i==6924) path="/species/ou-psittirostra-psittacea"
# if (i==7089) path="species/jameson-s-antpecker-parmoptila-jamesoni"
# if (i==7183) path="species/st-vincent-amazon-amazona-guildingii"
# if (i==7507) path="species/hunstein-s-mannikin-lonchura-hunsteini"
# if (i==7707) path="species/s-o-francisco-sparrow-arremon-franciscanus"
# if (i==7815) path="species/st-lucia-oriole-icterus-laudabilis"
# if (i==7840) path="species/st-lucia-warbler-dendroica-delicata"
# if (i==7975) path="species/la-sagras-flycatcher-myiarchus-sagrae"
# if (i==8225) path="species/st-Lucia-Amazon-Amazona-versicolor"
