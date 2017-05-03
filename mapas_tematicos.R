### mapas tematicos

rm(list=ls())
### o mapa foi carregado do site do IBGE em
### ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2014/PR/
download.file('ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2014/PR/pr_municipios.zip',
              destfile='pr_municipios.zip')
unzip('pr_municipios.zip')
dir()
file.info('pr_municipios.zip')
file.exists('pr_municipios.zip')
file.remove("pr_municipios.zip")
file.exists('pr_municipios.zip')



### salve os arquivos. Pelo menos aqueles com extensao .shp, .shx e .dbf

### carregando pacotes necessários
sapply(c('maptools','rgdal','sp','spdep'),require,char=T)

### lendo o mapa do Paraná
PR<-readOGR('.',layer='41MUE250GC_SIR')

### visualizando
plot(PR)

### atributos dos municipios (variáveis)
names(PR)
head(PR$CD_GEOCODM)

### bounding box
bbox(PR)

### atributos/elementos do objeto
names(attributes(PR))

### inspecionando os atributos (dados) dos munic?pios
dim(PR@data)

####
### Obtenção dos dados de densidade demográfica do PR
### - no site do IBGE <http://cidades.ibge.gov.br/download/mapa_e_municipios.php?lang=&uf=pr>

# Obtenção dos dados sobre analfabetismo no PR
# - no site do datasus <http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/censo/cnv/alfpr.def>

### lendo dados de desnidade demográfica de 2010
alf <- read.csv2("PR.IBGE.csv",h=T)
dim(alf)
alf[1:10,]
head(alf)
alf2<-alf[1:399,6]
names(alf2)
### Necessário colocar dados na mesma ordem do mapa

### extraindo o código de municipios dos dados
codmund <- alf$Código

### extraindo o código do mapa
cod6map <-PR@data[,2]

### rank dos códigos no mapa
rkmap <- rank(cod6map)

### códigos dos dados na ordem do mapa
ocodd <- codmund[order(codmund)[rkmap]]

### verificando se estão na mesma ordem
table(cod6map==ocodd)

### ordenando os dados de alfabetização
odat <- alf2[order(codmund)[rkmap] ]

### criando 5 categorias
cat5 <- findInterval(odat[], c(0, 25, 50, 100, 150))
table(cat5)

### cinco cores
cores5 <- rev(heat.colors(5))
cores5

par(mar=c(1.0,1.0,1.0,0))
plot(PR, col=cores5[cat5])
legend("topright", leglabs(c(0,25,50,100,150, 10000), "<", ">"),
       fill=cores5, bty="n")
title('\n Taxa de Densidade Demográfica \n do Paraná - 2010')


### data padronized
dados_pad <- data.frame(alf[,c(8)])

### neighboorhod list
pr.nb <- poly2nb(PR)

### calculing costs
lcosts <- nbcosts(pr.nb, dados_pad)

### making listw
nb.w <- nb2listw(pr.nb, lcosts, style="S")

### find a minimum spanning tree
mst.pr <- mstree(nb.w,1)

### the mstree plot
par(mar=c(0,0,0,0))
plot(mst.pr, coordinates(PR), col=2,
     cex.lab=.7, cex.circles=0.035, fg="blue")
plot(PR, border=gray(.5), add=TRUE)

### three groups with no restriction
group.res <- skater(mst.pr[,1:2], dados_pad, 4, 20, rep(1,nrow(PR@data)))

### groups frequency
table(group.res$groups)


### the skater plot, using other colors
plot(group.res, coordinates(PR), cex.circles=0.035, cex.lab=.7,
     groups.colors=colors()[(1:length(group.res$ed))*10])

### the Spatial Polygons plot
plot(PR, col=rev(heat.colors(5))[group.res$groups])


