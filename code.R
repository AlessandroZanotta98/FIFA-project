#IMPORTAZIONE DATASET


fifa <- read.csv(file="fifa20.csv")
dim(fifa)
str(fifa)
colnames(fifa)
#ESEGUI SELEZIONE COLONNE IN ORIDNE TUTTI I COMANDI
#togliamo tutte quelle colonne inutili per la nostra task

####### SELEZIONE COLONNE UTILI PER LE ANALISI######
library(tidyr)
#traduzione ruoli da inglese ad italiano
#la nostra task sarà quella di trovare un classifcatore che discrimini in base 
#agli attributi il giocatore in dei 4 ruoli, portiere difensore centrocampista attaccante
colnames(fifa)
as.matrix(colnames(fifa))
fifa <- as_tibble(fifa)
colnames(fifa)
#dacapire <- fifa[,c(29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,)]
fifa.new <- fifa[,-c(1,2,4,6,13,14,17,21,22,23,24,27,28,29,44)]
dim(fifa) 
str(fifa)
as.matrix(colnames(fifa))
work.rate <- fifa.new[,c(13)]
fifa.new2 <- fifa.new[,-c(13)]

View(fifa.new2)
as.matrix(colnames(fifa.new2))
anyNA(fifa.new2$team_position)
fifa.new3 <- fifa.new2[,-c(5,6,10,11,14,15,16)] #decidiamo di tenere player_positions

dim(fifa.new3)
View(fifa.new3)
as.matrix(colnames(fifa.new3))
fifa.new3 <- as_tibble(fifa.new3)
library(tidyr)
library(tidyverse)
# la colonna team_position si rifa al ruolo che il giocatore occupa all'interno della propria squadra
# non alla posizione in campo
#--------

###### PULIZIA DEL DATASET#####

# guardo quali sono i ruoli messi a disposiozione per ogni giocatore e mi rifaccio alla traduzione sopra riportata
#annoveriamo tra i difensori anche i terzini. Mentre CC,COC,CDC,ES,ED li possiamo considerare come centrocampisti.
# I restanti saranno attaccanti, non dimenticando GK come portieri
# GK --> POR
portieri <- c("GK")
# LB --> TS
# CB --> DC
# RB --> TD
#DF = Defender (difensori)
#SB = Side back (terzino)
#CB = Central back (dif. centrale)
#SW = Sweeper (libero)
difensori <- c("LB","CB","RB","DF","SB","CB","SW","LWB","RWB")
# CM --> CC
# CDM --> CDC
# CAM --> COC
# LM --> ES
# RM --> ED
#MF = midfielder (centrocampisti)
#AMF = Advanced midfielder (centrocampista avanzato/trequartista)
#SMF = Side midfielder (esterno di centrocampo)
#CMF = Central midfielder (centrocampista centrale)
#DMF = Defensive midfielder (centrocampista arretrato/mediano)
centrocampisti <- c("CM","CDM","LM","RM","MF","AMF","SMF","CMF","DMF","CAM","LW","RW")
# LW --> AS
# RW --> AD
# CF --> AT
# ST --> ATT
#FW = forward (attaccanti)
#ST = Striker (punta)
#CF = Central forward (centravanti)
#WF = Wing forward (ala)
attaccanti <- c("CF","ST","FW")
# mettiamo a posto i ruoli, le colonne ruolo presentano più ruoli per un giocatore
# e dobiamo inoltre ricondurci ad una delle tre cateogire tra "DIFENSORI" "CENTROCAMPISTI" "ATTACCANTI"
# Ci facciamo guidare dai nomi a disposizione per creare queste tre famose categorie. Il mio scopo
# è quello di creare un classificatore che in base agli attributi del giocatore mi dica la posizione che occupa
dim(fifa.new3)
#View(fifa.new3)
View(fifa.new3)
fifa.new3 <- separate(fifa.new3,player_positions,sep=",",into=c("ruol pr","ruol sec","ruol terz"))
unique(fifa.new3$`ruol pr`)
#RW"  "ST"  "LW"  "GK"  "CAM" "CB"  "CM"  "CDM" "CF"  "LB"  "RB"  "RM"  "LM"  "LWB" "RWB"

difensori.fifa <- which(fifa.new3$`ruol pr`%in%difensori)
centrocampisti.fifa <- which(fifa.new3$`ruol pr`%in%centrocampisti)
attaccanti.fifa <- which(fifa.new3$`ruol pr`%in%attaccanti)

fifa.new3[c(difensori.fifa),]$`ruol pr`="DIF"
fifa.new3[c(centrocampisti.fifa),]$`ruol pr`="CEN"
fifa.new3[c(attaccanti.fifa),]$`ruol pr`="ATT"
#View(fifa.new3)

fifa.new3 <- fifa.new3[,-c(8,9)]
#View(fifa.new3)
dim(fifa.new3)
length(which(fifa.new3$`ruol pr`=="GK")) 
length(which(fifa.new3$`ruol pr`=="DIF"))
length(which(fifa.new3$`ruol pr`=="CEN"))
length(which(fifa.new3$`ruol pr`=="ATT"))

dim(fifa)
View(fifa.new3)
ruoli.standard <- c("GK","DIF","CEN","ATT")
#mancano osservazioni, vediamo quali
#mancanti <- filter(fifa.new3,`ruol pr`%in% ruoli.standard==FALSE)
#View(mancanti)
#unique(mancanti$`ruol pr`)
#2036+5938+6862+3442
#SISTEMATE ANCHE LE ULTIME OSSERVAZIONI
#Ci sono variabili che aumentano che si riferiscono a valori 
#che possono migliorare durante la modalità del gioco 
#chiamata modalità carriera per il momento le teniamo fuori
#FIFA.NEW4 DATASET SU CUI LAVORARE (CREATO FIFA.NEW4 PERCHE
#VARIABILI TOLTE POTREBBERO SERVIRE)
str(fifa.new3[,c(56:81)])
View(fifa.new3)

fifa.lavoro <- fifa.new3[,c(56:81)]
fifa.lavoro$ls <- as.character(fifa.lavoro$ls)
fifa.lavoro$st <- as.character(fifa.lavoro$st)
fifa.lavoro$rs <- as.character(fifa.lavoro$rs)
fifa.lavoro$lw <- as.character(fifa.lavoro$lw)
fifa.lavoro$lf <- as.character(fifa.lavoro$lf)
fifa.lavoro$cf <- as.character(fifa.lavoro$cf)
fifa.lavoro$rf <- as.character(fifa.lavoro$rf)
fifa.lavoro$rw <- as.character(fifa.lavoro$rw)
fifa.lavoro$lam <- as.character(fifa.lavoro$lam)
fifa.lavoro$cam <- as.character(fifa.lavoro$cam)
fifa.lavoro$ram <- as.character(fifa.lavoro$ram)
fifa.lavoro$lm <- as.character(fifa.lavoro$lm)
fifa.lavoro$lcm <- as.character(fifa.lavoro$lcm)
fifa.lavoro$cm<-  as.character(fifa.lavoro$cm)
fifa.lavoro$rcm <- as.character(fifa.lavoro$rcm)
fifa.lavoro$rm <- as.character(fifa.lavoro$rm)
fifa.lavoro$lwb <- as.character(fifa.lavoro$lwb)
fifa.lavoro$ldm <- as.character(fifa.lavoro$ldm)
fifa.lavoro$cdm <- as.character(fifa.lavoro$cdm)
fifa.lavoro$rdm <- as.character(fifa.lavoro$rdm)
fifa.lavoro$rwb <- as.character(fifa.lavoro$rwb)
fifa.lavoro$lb <- as.character(fifa.lavoro$lb)
fifa.lavoro$lcb <- as.character(fifa.lavoro$lcb)
fifa.lavoro$cb <- as.character(fifa.lavoro$cb)
fifa.lavoro$rcb <- as.character(fifa.lavoro$rcb)
fifa.lavoro$rb <- as.character(fifa.lavoro$rb)

fifa.split <- fifa.lavoro
install.packages("splitstackshape")
library(splitstackshape)
fifa.splittato <- cSplit(fifa.split,splitCols = c(1:26),sep="+",direction = "wide")
#SPLITTO COLONNE CON cSplit

#View(fifa.splittato)
str(fifa.splittato)
i=1
fifa.splittato <- as.matrix(fifa.splittato)
dim(fifa.splittato)
cont <- matrix(nrow = 18278,ncol=26)
g=1
dim(fifa.splittato) #%% operatore per vedere se un numero è pari o dispari
for (i in 1:52){
  if (i %% 2 == 1){
  cont[,g] <- fifa.splittato[,i]+fifa.splittato[,i+1]
  g=g+1
  }

}
#View(fifa.lavoro)

fifa.pulito<- as.data.frame(cont)
colnames(fifa.pulito)[1:26]=colnames(fifa.lavoro)[1:26]
colnames(fifa.new3)[7]="RUOLO"
fifa.pulito <- cbind(fifa.pulito,fifa.new3[7])
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$ls <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$st <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rs <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lw <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lf <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$cf <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rf <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rw <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lam <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$cam <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$ram <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lcm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$cm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rcm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lwb <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$ldm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$cdm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rdm <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rwb <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lb <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$lcb <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$cb <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rcb <- 0
fifa.pulito[which(fifa.pulito$RUOLO=="GK"),]$rb <- 0
dim(fifa.pulito)
fifa.pulito <- fifa.pulito[,-c(27)]
colnames(fifa.pulito) #COLONNE MI DICONO IL POTENZIALE TOTALE DEL GIOCATORE
#QUANDO GIOCA IN UNA DETERMINATA POSIZIONE 
#DOPO AVER SISTEMATO IL DATASET POSSIAMO FINALMENTE COMINICARE CON LE NOSTRE ANALISI
######GESTIONE MISSING########
# CERCHIAMO ORA DI TRATTARE I MISSING, A UNA PRIMA OCCHIATA
# GIOCATORI COME MESSI CHE NON HANNO COME RUOLO QUELLO DEL PORTIERE (GK)
# HANNO PER GLI ATTRIBUTI RIFERITI A TALE RUOLO "NA" E' QUINDI RAGIONEVOLE
# SOSTITUIRE QUESTO NA CON 0, ANDIAMO ORA A VEDERE MEGLIO QUESTA SITUAZION
#ai portieri imputo valore zero per le abilità nei ruoli


fifa.new4 <- fifa.new3[,-c(56:81)]
fifa.new4 <- cbind(fifa.new4,fifa.pulito)
View(fifa.new4)
dim(fifa.new4)
colnames(fifa.new4)
install.packages("naniar")
install.packages("visdat")
install.packages("mice")


library("ggplot2")
library("tidyverse")
library("naniar")
library("visdat")
library("mice")

library(knitr)
library(gridExtra)

missing.values <- fifa.new4 %>%
  gather(key = "key", value = "val") %>%  #individuiamo i missing values
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 
dev.off()
missing.values  %>% kable()
missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity',fill="blue") +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  geom_col(aes(x=key,y=num.missing),fill="blue")+
  theme_dark()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#numero missing per ogni variabile tieni questo abbiamo quindi individuato
#le colonne con il maggior numero di missing


#stesssa cosa di su ma con percentuali, warn.large.data=FALSE
#mi consente di visualizzare i dati mancanti anche in percentuale
#vedo che gli NA sono concentrati maggioramente negli attributi 
#riferiti ai portieri
#E' lecito aspettarsi che gli NA negli attributi sono presenti nella maggior parte
# dei casi per quei giocatori che non ricoprono quel determinato ruolo
# ad esempio le abilità di un portiere saranno sostituite con un NA 
# per un attaccante stesso discorso analogo per i portieri stessi con 
#le abilità di un attaccante siamo di fronte a un caso di 
#NOT MISSING AT RANDOM, poichè la rilevazione dipende dal tipo 
#di giocatore considerato

#andiamo ora ripescare le colonne relative ai portieri

columns_na = c()

for (i in 1:dim(fifa.new4)[2]){
  if (any(is.na(fifa.new4[, i]))) columns_na = c(columns_na,i)
} #la funzione any mi dice se c'è almeno un valore true nella
#condzione posta al suo interno
#colonne con valori mancanti 10 11 12 13 14 15 16 17 18 19 20 21
#andiamo ora a vedere quali sono queste colonne e per che tipo 
#di giocatore mancano i dati

#View(fifa.new4)
colnames(fifa.new4[,c(10,11,12,13,14,15,16,17,18,19,20,21)])

#per ora ci concentriamo di piu sulle variabili riferite ai portieri
#(iniziano con gk) per trattare i missing


#anyNA(fifa.new4[,-c(10,11,12,13,14,15,16,17,18,19,20,21)]) corretto
#prendiamo solo i portieri e vediamo se per qualcuno mancano valori 
#riferiti alle abilità
colnames(fifa.new4)[7]="RUOLO"
portieri.abilita <-filter(fifa.new4,RUOLO=="GK") %>%
  select(gk_diving,gk_handling,gk_kicking,
                           gk_reflexes,gk_speed,gk_positioning) 
#View(portieri.abilita)
anyNA(portieri.abilita)
#non c'è nessun NA per i portieri, gli NA in tali attributi sono 
#dovuti al ruolo che il giocatore ha, di conseguenza 
#li possiamo ragionevolmente sostituire con 0. (un attaccante
#in porta avra abilità piuttosto basse)
fifa.new4[which(is.na(fifa.new4$gk_diving)),]$gk_diving=0
fifa.new4[which(is.na(fifa.new4$gk_handling)),]$gk_handling=0
fifa.new4[which(is.na(fifa.new4$gk_reflexes)),]$gk_reflexes=0
fifa.new4[which(is.na(fifa.new4$gk_speed)),]$gk_speed=0
fifa.new4[which(is.na(fifa.new4$gk_positioning)),]$gk_positioning=0
fifa.new4[which(is.na(fifa.new4$gk_kicking)),]$gk_kicking=0
#fifa.new4[which(is.na(fifa.new4$gk_diving)),]$gk_diving=0
#fifa.new4[which(is.na(fifa.new4$gk_diving)),]$gk_diving=0
giocatori.portieri <- filter(fifa.new4,RUOLO != "GK") %>%
  select(gk_diving,gk_handling,gk_kicking,
         gk_reflexes,gk_speed,gk_positioning)
#vediamo ora i restanti missing
gg_miss_var(fifa.new4)
#ne identifichiamo di nuovo le colonne
columns_na = c()

for (i in 1:dim(fifa.new4)[2]){
  if (any(is.na(fifa.new4[, i]))) columns_na = c(columns_na,i)
} 
columns_na
colnames(fifa.new4[,c(10,11,12,13,14,15)])

#colonne con NA 10 11 12 13 14 15
# vediamo se siamo in un caso analogo al precendente
# ma a parti invertite
giocatori.portieri <- filter(fifa.new4,RUOLO!="GK") %>%
  select(pace,shooting,passing,dribbling,defending,physic)
anyNA(giocatori.portieri)
#questi attributi sono NA solo per i portieri, sono infatti la velocità
# di corsa del giocatore, il tiro il passaggio, il dribbiling, la propensione
# a difendere, il fisico.
portieri.abilita <-filter(fifa.new4,RUOLO=="GK") %>%
  select(gk_diving,gk_handling,gk_kicking,
         gk_reflexes,gk_speed) 
fifa.new4[which(is.na(fifa.new4$pace)),]$pace=0
fifa.new4[which(is.na(fifa.new4$shooting)),]$shooting=0
fifa.new4[which(is.na(fifa.new4$passing)),]$passing=0
fifa.new4[which(is.na(fifa.new4$dribbling)),]$dribbling=0
fifa.new4[which(is.na(fifa.new4$defending)),]$defending=0
fifa.new4[which(is.na(fifa.new4$physic)),]$physic=0

anyNA(fifa.new4)
#ORA CHE ABBIAMO GESTITO LA SITUAZIONE DEI MISSING DATA POSSIAMO
#CONCENTRARCI SULL'ANALISI ESPLORATIVA
#--------
#APPROCCIO SUPERVISED######
#AVENDO A DISPOSZIONE LA VIARIABILE ETICHETTA USEREMO UN APPROCCIO DI TIPO
# SUPERVISED

######ANALISI ESPLORATIVA
colnames(fifa.new4)
str(fifa.new4)
dim(fifa.new4)
table(fifa.new4$RUOLO)
anyNA(fifa.new4)
#vediamo come si distinguono i giocatori in base 
# a due variabili che stanno agli antipodi e potential distinti per ruolo
# come ad esempio defendding e attacking_positioning
library(ggplot2)
View(fifa.new4)
fifa.new4$mentality_positioning
fifa.new4$attacking_finishing
#con skill e ruoli difensivi vediamo che i punti rossi (gli attaccanti hanno valori
#bassi mentre i difensori hanno valori elevati)


a <- ggplot(fifa.new4,mapping = aes(y=attacking_finishing,x=attacking_short_passing,col=RUOLO))+
  geom_point()+
  theme_gray()+
  labs(title = "Andamento delle classi per attributi di attacco")

fifa.new4$defending_sliding_tackle

d <- ggplot(fifa.new4,mapping=aes(x=defending_sliding_tackle,y=defending_marking,col=RUOLO))+
  geom_point()+
  theme_light()+
  labs(title = "Andamento delle classsi per attributi di difesa")

d


data <- as.data.frame(table(fifa.new4$RUOLO))
data$Freq
data <- data %>% 
  arrange(desc(Freq)) %>%
  mutate(prop = Freq / sum(data$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
colnames(data)[1]="Ruolo"



#### SELEZIONE DELLE VARIABILI QUANTITATIVE####
colnames(fifa.new4)
str(fifa.new4)
fifa_role <- unlist(fifa.new4$RUOLO)
fifa_role <- as.factor(fifa_role)
colnames(fifa.new4)
fifa.data <- fifa.new4[,-c(7,56)]
dim(fifa.data)
colnames(fifa.data)
fifa.data <- fifa.data[,c(9:79)]
str(fifa.data)
dim(fifa.data)
anyNA(fifa.data)
colnames(fifa.data)

fifa.spider <- fifa.data[,c(47:71)]
fifa.spider <- cbind(fifa.spider,fifa_role)
dim(fifa.spider)
colnames(fifa.spider) #1 6 9 13 



#CLASSI DISTRIBUZIONE
ggplot(fifa.new4, aes(RUOLO)) + 
  geom_bar(aes(fill = ..count..)) + 
  ggtitle("Distribution based on General Playing Position")




library(gridExtra)
#sotto dataset per grafici

fifa.spider.DIF <- filter(fifa.spider,fifa_role=="DIF")
fifa.spider.CEN <- filter(fifa.spider,fifa_role=="CEN")
fifa.spider.ATT <- filter(fifa.spider,fifa_role=="ATT")

fifa.spider.DIF <- fifa.spider.DIF[c(1:7),]
fifa.spider.CEN <- fifa.spider.CEN[c(1:30),]
fifa.spider.ATT <- fifa.spider.ATT[c(1:30),]

fifa.grafico <-rbind(fifa.spider.ATT,fifa.spider.CEN,fifa.spider.DIF)

fifa.grafico
p1 <- ggplot(fifa.grafico,mapping = aes(x=st,y=rf,col=fifa_role))+
  geom_point()+
  theme_light()
p1
p2 <- ggplot(fifa.grafico,mapping=aes(x=cam,y=cm,col=fifa_role))+
  geom_point()
p2


world_data <- map_data("world")


p <- p + geom_polygon(data=nazionalità, aes(x=long, y=lat, group = RUOLO,fill=Total$value),colour="white") + 
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "legend" ,title = "Title", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())

nazionalità <- select(fifa,nationality)


####PCA COMPONENTI PRINCIPALI####
pca=prcomp(fifa.data,scale. = T) #scale standardizzazione tutte le varianze=1 e medie uguali a 0

dim(pca$x) #punteggi tutti i miei dati riscalati sui nuovi pesi delle variabili risultato combinazione lineare
row.names(pca$rotation) #pesi come se ciascuna variabile contribuisse a spiegare un tota di ogni PCA
summary(pca) 
dataset.pca=as.data.frame(pca$x[,1:8]) #tramite varianaza spiegata dalle componenti
disegno.pca=cbind(dataset.pca,fifa_role)
library(ggfortify)
colnames(fifa.data)
XPos = data.frame(fifa.data,fifa_role)

autoplot(pca, data = XPos, colour = "fifa_role", 
         loadings=TRUE, loadings.label = TRUE) + theme_bw()

#EDDA
set.seed(211)
(n<-nrow(dataset.pca))
test.set.labels<-sample(1:n,nrow(dataset.pca)*.2)
v <- nrow(dataset.pca[-test.set.labels,])
validation.piece <- c(30,40,50,60,100)
library(Rmixmod) 
??`Rmixmod-package`

best.cv <- Inf #valore iniziale così so che il prossimo calcolato sarà il piu basso
itermax <- 10
bics<-matrix(nrow=itermax,ncol=2)


CLASSIF@bestResult@criterionValue
CLASSIF@results[[1]]@criterionValue[2]

matrice <- matrix(NA,nrow=5,ncol=10)

for(i in 1:5){
  for(g in 1:itermax){
  CLASSIF<-mixmodLearn(dataset.pca[-test.set.labels,], fifa_role[-test.set.labels], 
                       models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                       criterion=c("CV","BIC"),nbCVBlocks=validation.piece[i])
  cv <- CLASSIF@bestResult@criterionValue[1]
  matrice[i,g] <- cv
  if (cv<best.cv){
    best.result <-CLASSIF
    best.cv <- CLASSIF@bestResult@criterionValue[1]
  }
}
}
best.result@nbCVBlocks 
best.result@bestResult@criterion

best.result@results

best.result@models@listModels  

nomi=CV =vector(length = length(best.result@models@listModels),mode="numeric")

for (i in 1: length(best.result@models@listModels)){
  ind = which(best.result@results [[i]] @model == best.result@models@listModels) #nomi uguali
  CV[ind] = best.result@results [[i]] @criterionValue [1]
  nomi[ind] <- best.result@models@listModels[ind]
}

length(CV)
length(nomi)

nomi <- (as.matrix(nomi))
dim(nomi)


CV <- (as.matrix(CV))
dim(CV)

CV <- cbind(nomi,CV)


CV <- as.data.frame(CV)



colnames(CV)=c("MODELLO","CV")    
library(tidyverse)

CV <- CV %>%arrange(CV)
CV$CV <- as.character(CV$CV)
CV$CV <- as.numeric(CV$CV)
CV$CV <- round(CV$CV,2)
p2 <- ggplot(CV,mapping=aes(x=MODELLO,y=CV,group=1))+
  theme(axis.text.x =element_text(angle=45,hjust=1))+
  theme(axis.text.y =element_text(angle=45,hjust=1))+
  geom_point(size=.1,col="red")+
  geom_line(color="red")  

p2+labs(title="Andamento CV nei modelli migliori")

PREDICTION<- mixmodPredict(data=dataset.pca[test.set.labels,], classificationRule=best.result["bestResult"])

PREDICTION@partition

PREDICTION["partition"]

(mean(as.integer(fifa_role[test.set.labels]) == PREDICTION["partition"])) 


library("caret")

#confusionMatrix(data, reference, positive = NULL, dnn = c("Prediction", "Reference"), ...
true <- as.factor(fifa_role[test.set.labels])
clust <- as.factor(PREDICTION@partition)
levels(clust) <- c("ATT","CEN","DIF","GK")

true

clust

confusionMatrix(clust, true)
  






#######88%###



#MDA###
set.seed(211)
(n<-nrow(dataset.pca))
test.set.labels<-sample(1:n,nrow(dataset.pca)*.2)
library(mclust)


G = 5; V =25; perm = sample(n) #SCELTA VALORI MASSIMI PER G E V
B = round(n/V); err1 = matrix(NA ,G,V)
B

dim(dataset.pca[-test.set.labels,])
# cerco numero gruppi ottimale in base al classification error rate
for (g in 1:G){
  for (v in 1:V){
    test.set.labels = perm[(B*(v-1)+1):(B*v)]
    mod = MclustDA(dataset.pca[test.set.labels ,], fifa_role[test.set.labels],G=g,modelNames='VVV')
    err1[g,v] = sum(predict(mod ,dataset.pca[test.set.labels,])$class != fifa_role[test.set.labels])/B
  }
}
min <- Inf
err1 <- as.matrix(err1)
medie.errori <- rowMeans(err1)
vec <- vector(mode="numeric",length = 2)

for(i in 1:25){
  for (g in 1:5){
    if (err1[g,i]<min){
      min <- err1[g,i]
      vec <- c(i,g)
    }
  }
}
min
vec #11 blocchi 5 gruppi abbiamo G e V
dim(err1)


gruppi <- as.data.frame(x = 1:5)
colnames(gruppi) <- c("gruppi")
medie.errori <- as.matrix(medie.errori)
errore.per.gruppi <- cbind(gruppi,medie.errori)
colnames(errore.per.gruppi)[2] <- c("media errore")
errore.per.gruppi <- errore.per.gruppi %>% arrange(desc(`media errore`))


ggplot(errore.per.gruppi,mapping=aes(x=gruppi,y=`media errore`))+
  geom_point(col="blue",size=.1)+
  geom_line(show.legend = TRUE,col="blue")+
  labs(title = "andamento media errore per gruppi MDA")

library(mclust)
errore.per.gruppi$`media errore`
#ogni volta il risultato cambia e voglio prendere il migliore con 5 gruppi
somma <- Inf
for(i in 1:10){
  mod = MclustDA(dataset.pca[-test.set.labels ,], fifa_role[-test.set.labels],G=5,modelNames='VVV',criterion="CV")
  cer = sum(predict(mod, dataset.pca[-test.set.labels,])$class != fifa_role[-test.set.labels])
  if (cer<somma){
    best.mod <- mod
    somma = cer
  }
}

summary(best.mod)
summary(best.mod)

#g=5


predict(best.mod, dataset.pca[test.set.labels ,])$class #calcolo sul test set

sum(predict(best.mod, dataset.pca[test.set.labels,])$class != fifa_role[test.set.labels])

true <- as.factor(fifa_role[test.set.labels])
clust <- predict(best.mod, dataset.pca[test.set.labels ,])$class 
levels(clust) <- c("ATT","CEN","DIF","GK")
true
clust
library(caret)
confusionMatrix(clust, true)






#ACCURACY DEL 89%##




labs()





####SELEZIONE DELLE VARIABILI TRAMITE CORRELAZIONE####
library(caret)
fifa_role <- unlist(fifa.new4$RUOLO)
fifa_role <- as.factor(fifa_role)
correlazione <- cor(fifa.data)
dim(fifa.data)
da.eliminare <- findCorrelation(correlazione,cutoff = 0.8)
colnames(fifa.data[,c(da.eliminare)])
validation.piece <- c(30,40,50,60,100)
fifa.correlazione <- fifa.data[,-c(da.eliminare)]
dim(fifa.correlazione)

colnames(fifa.correlazione)
library(ggcorrplot)

ggcorrplot(cor(fifa.correlazione))
ggcorrplot(correlazione,show.diag = FALSE,)+
  theme(axis.text.y = element_text(hjust = 0,size = 6))+
  theme(axis.text.x = element_blank())




#EDDA###
set.seed(211)
(n<-nrow(fifa.correlazione))
test.set.labels<-sample(1:n,nrow(fifa.correlazione)*.2)
library(Rmixmod) 
CV(CLASSIF)
??`Rmixmod-package`

best.cv <- Inf #valore iniziale così so che il prossimo calcolato sarà il piu basso
itermax <- 10
bics<-matrix(nrow=itermax,ncol=2)
matrice <- matrix(NA,nrow=5,ncol=10)
validation.piece <- c(30,40,50,60,100)

dim(fifa.correlazione)
for(i in 1:5){
  for(g in 1:itermax){
    CLASSIF<-mixmodLearn(fifa.correlazione[-test.set.labels,], fifa_role[-test.set.labels], 
                         models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                         criterion=c("CV"),nbCVBlocks=validation.piece[i])
    cv <- CLASSIF@bestResult@criterionValue[1]
    matrice[i,g] <- cv
    if (cv<best.cv){
      best.result <-CLASSIF
      best.cv <- CLASSIF@bestResult@criterionValue[1]
    }
  }
}

best.result@bestResult@model
matrice

nomi=CV =vector(length = length(best.result@models@listModels),mode="numeric")

for (i in 1: length(best.result@models@listModels)){
  ind = which(best.result@results [[i]] @model == best.result@models@listModels) #nomi uguali
  CV[ind] = best.result@results [[i]] @criterionValue [1]
  nomi[ind] <- best.result@models@listModels[ind]
}

length(CV)
length(nomi)

nomi <- (as.matrix(nomi))
dim(nomi)


CV <- (as.matrix(CV))
dim(CV)

CV <- cbind(nomi,CV)


CV <- as.data.frame(CV)

colnames(CV)=c("MODELLO","CV")    
library(tidyverse)

CV <- CV %>%arrange(CV)
CV$CV <- as.character(CV$CV)
CV$CV <- as.numeric(CV$CV)
CV$CV <- round(CV$CV,4)
CV$CV
p2 <- ggplot(CV,mapping=aes(x=MODELLO,y=CV,group=1))+
  theme_light()+
  theme(axis.text.x =element_text(angle=45,hjust=1))+
  theme(axis.text.y =element_text(angle=45,hjust=1))+
  geom_point(size=.1,col="green")+
  geom_line(color="green") 

p2+labs(title="Andamento CV")



#dopo ciclo risultato migliore con 30 blocchi (ciclo eseguito con i blocchi in preced)
best.result #gaussian_pk_L_Dk_A_Dk
best.cv
un <- unlist(best.result)
un
PREDICTION<- mixmodPredict(data=fifa.correlazione[test.set.labels,], classificationRule=best.result["bestResult"])

PREDICTION["partition"]
mean(as.integer(fifa_role[test.set.labels]) == PREDICTION["partition"]) 
#87%  di accuracy##

library("caret")

#confusionMatrix(data, reference, positive = NULL, dnn = c("Prediction", "Reference"), ...


true <- as.factor(fifa_role[test.set.labels])
clust <- as.factor(PREDICTION@partition)
levels(clust) <- c("ATT","CEN","DIF","GK")
true
clust
matr <- confusionMatrix(clust, true)


library(mclust)


colnames(fifa.correlazione)





#87%  di accuracy##





##MDA###
library(mclust)
set.seed(211)
(n<-nrow(fifa.correlazione))
test.set.labels<-sample(1:n,nrow(fifa.correlazione)*.2)

G = 9; V =25; perm = sample(n)
B = round(n/V); err = matrix(NA ,G,V)
B

for (g in 1:G){
  for (v in 1:V){
    test.set.labels = perm[(B*(v-1)+1):(B*v)]
    mod = MclustDA(fifa.correlazione[test.set.labels ,], fifa_role[test.set.labels],G=7,modelNames='VVV')
    err[g,v] = sum(predict(mod ,fifa.correlazione[test.set.labels,])$class != fifa_role[test.set.labels]) / B
  }
}

#G=7, CVBLOCKS=2 sono 2 sottoinsiemi
#G è andato mediamente così ogni V
min <- Inf
err <- c(0.1197 ,0.1098 ,0.1092 ,0.1077 ,0.1078 ,0.1069 ,0.1065 ,0.1073 ,0.1071) #media per riga
vec <- vector(mode="numeric",length = 2)
for(i in 1:25){
  for (g in 1:9){
    if (err[g,i]<min){
      min <- err[g,i]
      vec <- c(i,g)
    }
  }
}
min
vec
dim(err)
min(err)
somma <- Inf

gruppi.corr.mda <- as.data.frame(x=1:9)
gruppi.corr.mda <- cbind(gruppi.corr.mda,err)
gruppi.corr.mda <- gruppi.corr.mda %>% arrange(desc(err))
colnames(gruppi.corr.mda)[1] <- c("gruppi")

# Plotting the class error according to G

ggplot(gruppi.corr.mda,mapping=aes(x=gruppi,y=err))+
  geom_point(col="orange",size=.1)+
  geom_line(col="orange")+
  labs(title = "classification error per gruppi")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
  theme_gray()
# 7 GRUPPI TOP ogni volta il risultato cambia e voglio prendere il migliore calcolandolo sul training
for(i in 1:10){
  mod = MclustDA(fifa.correlazione[-test.set.labels ,], fifa_role[-test.set.labels],G=7,modelNames='VVV')
  cer = sum(predict(mod, fifa.correlazione[-test.set.labels,])$class != fifa_role[-test.set.labels])
  if (cer<somma){
    best.mod <- mod
    somma = cer
  }
}


summary(best.mod)
rowMeans(err) 



true <- as.factor(fifa_role[test.set.labels])
clust <- predict(best.mod, fifa.correlazione[test.set.labels ,])$class
levels(clust) <- c("ATT","CEN","DIF","GK")
true
clust
confusionMatrix(clust, true)
##89%###








