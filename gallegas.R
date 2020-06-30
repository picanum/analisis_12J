library(pdftools)
library(tm)
library(SnowballC)
library(stringr)
library(fastmatch)
library(wordcloud)
library(tidytext)
library(tesseract)
library(tidyverse)
library(patchwork)
library(ggwordcloud)
library(factoextra)

pp <- pdf_text("pp.pdf")
pp <- pp[-c(1:5)]
pp <- str_remove_all(pp, "-\r\n")
pp <- str_replace_all(pp, "\r\n      ", " ")
pp <- str_remove_all(pp, "            ")
pp <- str_replace_all(pp, "\r\n", " ")
pp <- str_remove_all(pp, "     ")
pp <- str_remove_all(pp, "PROGRAMA ELECTORAL. ELECCIÓNS AUTONÓMICAS 2020.")
pp <- str_remove_all(pp, "PRESENTACIÓN /")
pp <- str_remove_all(pp, "EIXO")
pp <- str_remove_all(pp, "III /")
pp <- str_remove_all(pp, "II /")
pp <- str_remove_all(pp, "I /")
pp <- str_remove_all(pp, "IV /")
pp <- str_remove_all(pp, "V /")

bng <- pdf_text("bng.pdf")
bng <- bng[-c(1:7)]
bng <- str_replace_all(bng, "\r\n", " ")
bng <- str_remove_all(bng, "Programa electoral do BNG. Galegas Xullo 2020")

pod <- pdf_text("podemos.pdf")
pod <- pod[-c(1:5)]
pod <- str_replace_all(pod, "\r\n", " ")

img_file <- pdftools::pdf_convert("psoe.pdf", format = 'tiff', pages = NULL, dpi = 100)
text <- ocr(img_file, engine = tesseract("spa"))
unlink(img_file)
write.csv(text,"psoe.csv",row.names = F)
psoe <- read.csv("psoe.csv", stringsAsFactors = F)[,1]
psoe <- str_remove_all(psoe, "A hora GC do cambio. Fai Galicia.")
psoe <- str_remove_all(psoe, "GC do cambio. Fai Galicia.")
psoe <- str_remove_all(psoe, "A hora\n.\nC do cambio. Bases programáticas\n")
psoe <- str_remove_all(psoe, "G ¡\ndo cambio.")
psoe <- str_remove_all(psoe, "A hora . C do cambio.")
psoe <- str_remove_all(psoe, "A hora\n\nG do cambio. Fai Galicia.\n")
psoe <- str_remove_all(psoe, "A hora")
psoe <- str_remove_all(psoe, "G do cambio. Fai Galicia.\n")
psoe <- str_remove_all(psoe, "\n.\n\nC do cambio. Bases programáticas\n")
psoe <- str_remove_all(psoe, "\nC do cambio. Fai Galicia.\n")
psoe <- str_remove_all(psoe, "SOE / Bases programáticas. Marzo 2020")
psoe <- str_remove_all(psoe, "\nPáxina")
psoe <- str_replace_all(psoe, "\n", " ")
psoe <- str_remove_all(psoe, "PSOE Bases programáticas. Marzo 2020")
psoe <- str_remove_all(psoe, "PSOE Documento de debate")

preprocesamiento <- function(x) removePunctuation(removeNumbers(tolower(x)))

stopwords_gl <- read.table(
  "https://raw.githubusercontent.com/apache/lucene-solr/master/lucene/analysis/common/src/resources/org/apache/lucene/analysis/gl/stopwords.txt",
                           encoding = "UTF-8", stringsAsFactors = F)
stopwords_gl <- stopwords_gl[,1]
stopwords_gl <- preprocesamiento(stopwords_gl)
stopwords_gl <- c(stopwords_gl, c("máis","ás","sen","etc","desta","destes","destas",
                                  "nestes", "onde", "sendo"))

preproc <- function(texto){

  texto2 <- preprocesamiento(texto)
  for(i in 1:length(stopwords_gl)) texto2 <- str_replace_all(texto2, paste0(" ",stopwords_gl[i]," "), " ")
  texto2 <- removeWords(texto2, stopwords("spanish"))
  texto2 <- str_remove_all(texto2, "’")
  texto2 <- str_remove_all(texto2, "–")
  texto2 <- str_remove_all(texto2, "”")
  texto2 <- str_remove_all(texto2, "“")
  texto2 <- str_remove_all(texto2, "€")
  texto2 <- str_remove_all(texto2, "¿")
  texto2 <- str_remove_all(texto2, "‘")
  texto2 <- str_remove_all(texto2, "«")
  texto2 <- str_remove_all(texto2, "»")
  texto2 <- str_remove_all(texto2, "•")
  texto2 <- str_remove_all(texto2, "…")
  texto2 <- str_remove_all(texto2, "¡")
  texto2 <- str_remove_all(texto2, "—")

  DTM1 <- DocumentTermMatrix(Corpus(VectorSource(t(texto2))))
  DTM1<-as.matrix(DTM1)
  DTM1<-as.data.frame(DTM1)
  return(DTM1)
}

d_pp <- preproc(pp)
tail(sort(apply(d_pp,2,sum)),20)

d_psoe <- preproc(psoe)
tail(sort(apply(d_psoe,2,sum)),20)

d_bng <- preproc(bng)
tail(sort(apply(d_bng,2,sum)),20)

d_pod <- preproc(pod)
tail(sort(apply(d_pod,2,sum)),20)


palabras <- c(colnames(d_pp), colnames(d_psoe), colnames(d_bng), colnames(d_pod))
palabras <- names(table(palabras))

palabrizador <- function(d, palabras = palabras){
  ret <- merge(data.frame(palabras = palabras), data.frame(palabras = colnames(d), freq = apply(d,2,sum)), all.x = T)
  return(ret)
}

d2pp <- palabrizador(d_pp, palabras)
d2psoe <- palabrizador(d_psoe, palabras)
d2bng <- palabrizador(d_bng, palabras)
d2pod <- palabrizador(d_pod, palabras)

d <- data.frame(palabras = d2pp[,1], pp = d2pp[,2], psoe = d2psoe[,2], bng = d2bng[,2], pod = d2pod[,2])

for(i in 2:5){
  d[which(is.na(d[,i])),i] <- 0
  d[,i] <- d[,i]/sum(d[,i])
}

#Con esto podemos saber qué posición ocupa cada término en el ranking de palabras más usadas por cada partido
d_rank <- d
for(i in 2:5){
  d_rank[,i] <- nrow(d) + 1 - rank(d_rank[,i], ties.method = "last")
}
row.names(d_rank) <- d_rank[,1]
#Por ejemplo, vamos a ver en qué posición está el término "covid" en los programas de los partidos gallegos:
d_rank["covid",]

g1 <- d %>% filter(pp >= quantile(d$pp,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pp)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PP") + theme(title = element_text(colour = "dodgerblue"),
                             plot.title = element_text(hjust = 0.5))
g2 <- d %>% filter(psoe >= quantile(d$psoe,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = psoe)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PSdeG-PSOE", subtitle = "(Bases programáticas)") + theme(title = element_text(colour = "red2"),
                               plot.title = element_text(hjust = 0.5),
                               plot.subtitle = element_text(hjust = 0.5))
g3 <- d %>% filter(bng >= quantile(d$bng,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = bng)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "BNG") + theme(title = element_text(colour = "darkblue"),
                                     plot.title = element_text(hjust = 0.5))
g4 <- d %>% filter(pod >= quantile(d$pod,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pod)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Galicia En Común-Anova Mareas") + theme(title = element_text(colour = "purple"),
                              plot.title = element_text(hjust = 0.5))

(g1 + g2) / (g3 + g4) + plot_annotation(title = "Términos más repetidos en los programas electorales de las elecciones gallegas del 12J de los principales partidos",
                                                    caption = "Fuente: programas electorales de PP, PSdeG-PSOE, BNG y Galicia En Común-Anova Mareas | @Picanumeros")
ggsave("wordcloud.png", dpi = 300)

d$media <- apply(d[,2:5],1,mean)
d_comp <- d[which(d$media > quantile(d$media, 0.95)),]

par(mfrow=c(2,2))
barplot(tail(sort(d_comp$pp/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pp/d_comp$media)],20),
        horiz = TRUE, las=1, xlim = c(0,4),
        main = "Palabras más repetidas en el programa del PP\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 4 programas")
barplot(tail(sort(d_comp$psoe/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$psoe/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,4),
        main = "Palabras más repetidas en el programa del PSdeG-PSOE\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 4 programas")
barplot(tail(sort(d_comp$bng/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$bng/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,4),
        main = "Palabras más repetidas en el programa del BNG\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 4 programas")
barplot(tail(sort(d_comp$pod/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pod/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,4),
        main = "Palabras más repetidas en el programa de Galicia En Común-Anova Mareas\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 4 programas")

rownames(d_comp) <- d_comp[,1]
#d_comp <- d_comp[-which(d_comp[,1] %in% c("galiza","galicia")),]   #Descomenta esta línea si quieres hacer los mismos gráficos sin las denominaciones de Galicia
colnames(d_comp)[2:5] <- c("PP","PSOE","BNG","GeC-AM")
res.pca <- FactoMineR::PCA(d_comp[,2:5])
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Fuente: programas electorales de PP, PSdeG-PSOE, BNG y Galicia En Común-Anova Mareas | @Picanumeros",
  x = "Componente 1 (68.5% de varianza explicada)", y = "Componente 2 (15.4% de varianza explicada)"
)
ggsave("comp1.png", dpi = 300, width = 8.4, height = 6.4)
ind <- get_pca_ind(res.pca)

fviz_pca_biplot(res.pca, repel = F) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Fuente: programas electorales de PP, PSdeG-PSOE, BNG y Galicia En Común-Anova Mareas | @Picanumeros",
  x = "Componente 1 (68.5% de varianza explicada)", y = "Componente 2 (15.4% de varianza explicada)"
)
ggsave("comp2.png", dpi = 300, height = 12, width = 14)

mds <- dist(t(d_comp[,2:5]))

fit <- cmdscale(mds,eig=TRUE, k=2)
puntos <- as.data.frame(fit$points)
ggplot(puntos, aes(x = V1, y = V2, label = row.names(fit$points))) + 
  geom_point() + geom_label(size = 6.5) +
  theme_classic(base_size = 15) + labs(x = "Dimensión 1 (44.3% suma autovalores)",
                                       y = "Dimensión 2 (33.2% suma autovalores)",
                                       title = "Representación bidimensional de los programas de cada partido mediante\nescalamiento multidimensional (MDS) según las distancias entre ellos",
                                       subtitle = "Distancias obtenidas a partir de la distancia euclídea entre frecuencias relativas de palabras",
                                       caption = "Fuente: programas electorales de PP, PSdeG-PSOE, BNG y Galicia En Común-Anova Mareas | @Picanumeros")

ggsave("mds.png", dpi = 300)
