library(pdftools)
library(tm)
library(SnowballC)
library(stringr)
library(fastmatch)
library(wordcloud)
library(tidytext)
library(tidyverse)
library(patchwork)
library(ggwordcloud)
library(factoextra)

psoe <- pdf_text("PROGRAMA-PSE-2020.pdf")
psoe <- psoe[-c(1:6,126:128)]
psoe <- str_replace_all(psoe, "\r\n   ", " ")
psoe <- str_replace_all(psoe, "-\r\n", "")
psoe <- str_replace_all(psoe, "- ", "")
psoe <- str_replace_all(psoe, "\r\n", " ")
psoe <- str_replace_all(psoe, "@", "o")
psoe <- str_remove_all(psoe, "\a")
psoe <- str_remove_all(psoe, "  ")
psoe <- str_remove_all(psoe, "Vinculado a ODS")
psoe <- str_remove_all(psoe, "Vin culado a ODS")
psoe <- str_remove_all(psoe, "Vincula do a ODS")
psoe <- str_remove_all(psoe, "Vinculado aODS")


pnv <- pdf_text("pnv.pdf")
pnv <- str_remove_all(pnv, "SARRERA")
pnv <- str_remove_all(pnv, "EMPLEO")
pnv <- str_remove_all(pnv, "INDUSTRIA E INTERNACIONALIZACIÓN")
pnv <- str_remove_all(pnv, "INNOVACIÓN Y EMPRENDIMIENTO")
pnv <- str_remove_all(pnv, "TRANSPORTE Y MOVILIDAD SOSTENIBLE")
pnv <- str_remove_all(pnv, "ALIMENTACIÓN, SECTOR FORESTAL Y DESARROLLO RURAL")
pnv <- str_remove_all(pnv, "TURISMO")
pnv <- str_remove_all(pnv, "COMERCIO Y HOSTELERÍA / CONSUMO")
pnv <- str_remove_all(pnv, "EDUCACIÓN")
pnv <- str_remove_all(pnv, "FORMACIÓN PROFESIONAL")
pnv <- str_remove_all(pnv, "UNIVERSIDADES E INVESTIGACIÓN")
pnv <- str_remove_all(pnv, "SALUD")
pnv <- str_remove_all(pnv, "INCLUSIÓN SOCIAL / SERVICIOS SOCIALES")
pnv <- str_remove_all(pnv, "VIVIENDA")
pnv <- str_remove_all(pnv, "JUVENTUD / DEPORTE")
pnv <- str_remove_all(pnv, "IGUALDAD DE GÉNERO")
pnv <- str_remove_all(pnv, "MAYORES")
pnv <- str_remove_all(pnv, "FAMILIAS E INFANCIA")
pnv <- str_remove_all(pnv, "CULTURA / EUSKERA")
pnv <- str_remove_all(pnv, "SEGURIDAD / JUSTICIA")
pnv <- str_remove_all(pnv, "CONVIVENCIA Y DERECHOS HUMANOS")
pnv <- str_remove_all(pnv, "INMIGRACIÓN / DIVERSIDAD")
pnv <- str_remove_all(pnv, "COOPERACIÓN AL DESARROLLO")
pnv <- str_remove_all(pnv, "ESTRATEGIA ENERGÉTICA")
pnv <- str_remove_all(pnv, "ACCIÓN POR EL CLIMA")
pnv <- str_remove_all(pnv, "ESTRATEGIA AMBIENTAL / BIODIVERSIDAD")
pnv <- str_remove_all(pnv, "ECONOMÍA CIRCULAR")
pnv <- str_remove_all(pnv, "EURORREGIÓN VASCA")
pnv <- str_remove_all(pnv, "EUSKADI-BASQUE COUNTRY")
pnv <- str_remove_all(pnv, "GESTIÓN TRANSPARENTE Y RESPONSABLE")
pnv <- str_remove_all(pnv, "AUTOGOBIERNO")
pnv <- str_remove_all(pnv, "COMPROMISOS E INICIATIVAS COMPROMISO ")
pnv <- str_remove_all(pnv, "COMPROMISOS E INICIATIVAS")
pnv <- str_remove_all(pnv, "COMPROMISO 0")
pnv <- str_remove_all(pnv, "COMPROMISO 1")
pnv <- str_remove_all(pnv, "PRINCIPIOS ")
pnv <- pnv[-c(1:7)]
pnv <- str_replace_all(pnv, "\r\n", " ")

bildu <- pdf_text("bildu.pdf")
bildu <- str_remove_all(bildu, "AVANZAR")
bildu <- str_remove_all(bildu, "DEFENDER")
bildu <- str_remove_all(bildu, "CUIDAR")
bildu <- str_remove_all(bildu, "MOVER")
bildu <- str_remove_all(bildu, "DIGNIFICAR")
bildu <- str_remove_all(bildu, "UNIR")
bildu <- str_remove_all(bildu, "ACTUAR")
bildu <- str_remove_all(bildu, "TEJER")
bildu <- str_replace_all(bildu, "\r\n", " ")
bildu <- str_remove_all(bildu, "\a")
bildu <- str_remove_all(bildu, "-       ")
bildu <- str_remove_all(bildu, "   ")
bildu <- str_remove_all(bildu, "PROGRAMA")
bildu <- str_remove_all(bildu, "ELECTORAL 2020")
bildu <- str_remove_all(bildu, "ARABA, BIZKAIA Y GIPUZKOA")

pod <- pdf_text("podemos.pdf")
pod <- str_remove_all(pod, "PROGRAMA AUTONÓMICO 2020")
pod <- str_replace_all(pod, "\r\n", " ")

img_file <- pdftools::pdf_convert("pp.pdf", format = 'tiff', pages = NULL, dpi = 100)
text <- ocr(img_file, engine = tesseract("spa"))
unlink(img_file)
write.csv(text, "pp.csv", row.names = F)
pp <- read.csv("pp.csv", stringsAsFactors = F)[,1]
pp <- str_replace_all(pp, "\n", " ")
pp <- pp[-c(1:4)]
pp <- str_remove_all(pp, "PP\\+Cs")
pp <- str_remove_all(pp, "6\\+Cs")
pp <- str_remove_all(pp, "6\\+cs")


preproc <- function(texto){
  preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))

  texto2 <- preprocesamiento(texto)
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
d_psoe <- preproc(psoe)
tail(sort(apply(d_psoe,2,sum)),20)

d_pnv <- preproc(pnv)
tail(sort(apply(d_pnv,2,sum)),20)
d_pnv <- d_pnv[,-which(colnames(d_pnv) %in% c("dad", "pro","ción"))]

d_pod <- preproc(pod)
tail(sort(apply(d_pod,2,sum)),20)

d_bildu <- preproc(bildu)
tail(sort(apply(d_bildu,2,sum)),20)
d_bildu <- d_bildu[,-which(colnames(d_bildu) %in% c("ción"))]

d_pp <- preproc(pp)
tail(sort(apply(d_pp,2,sum)),20)
d_pp <- d_pp[,-which(colnames(d_pp) %in% c("pora","dle","ala","alos","alas"))]

palabras <- c(colnames(d_psoe), colnames(d_pnv), colnames(d_pod), colnames(d_bildu), colnames(d_pp))
palabras <- names(table(palabras))

palabrizador <- function(d, palabras = palabras){
  ret <- merge(data.frame(palabras = palabras), data.frame(palabras = colnames(d), freq = apply(d,2,sum)), all.x = T)
  return(ret)
}

d2psoe <- palabrizador(d_psoe, palabras)
d2pnv <- palabrizador(d_pnv, palabras)
d2pod <- palabrizador(d_pod, palabras)
d2bildu <- palabrizador(d_bildu, palabras)
d2pp <- palabrizador(d_pp, palabras)

d <- data.frame(palabras = d2psoe[,1], psoe = d2psoe[,2], pnv = d2pnv[,2], pod = d2pod[,2],
                bildu = d2bildu[,2], pp = d2pp[,2])
for(i in 2:6){
  d[which(is.na(d[,i])),i] <- 0
  d[,i] <- d[,i]/sum(d[,i])
}

d_rank <- d
for(i in 2:6){
  d_rank[,i] <- nrow(d) + 1 - rank(d_rank[,i], ties.method = "last")
}
row.names(d_rank) <- d_rank[,1]
d_rank["pandemia",]

g1 <- d %>% filter(psoe >= quantile(d$psoe,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = psoe)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PSE-EE") + theme(title = element_text(colour = "red2"),
                               plot.title = element_text(hjust = 0.5))
g2 <- d %>% filter(pnv >= quantile(d$pnv,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pnv)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PNV") + theme(title = element_text(colour = "darkgreen"),
                             plot.title = element_text(hjust = 0.5))
g3 <- d %>% filter(bildu >= quantile(d$bildu,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = bildu)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "EH Bildu") + theme(title = element_text(colour = "chartreuse"),
                                     plot.title = element_text(hjust = 0.5))
g4 <- d %>% filter(pod >= quantile(d$pod,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pod)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "Elkarrekin Podemos-IU") + theme(title = element_text(colour = "purple"),
                                         plot.title = element_text(hjust = 0.5))
g5 <- d %>% filter(pp >= quantile(d$pp,(1-20/nrow(d)))) %>%
  ggplot(aes(label = palabras, size = pp)) + geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) + theme_minimal(base_size = 20) +
  labs(title = "PP + Cs") + theme(title = element_text(colour = "dodgerblue"),
                                                plot.title = element_text(hjust = 0.5))


(g1 + g2) / (g3 + g4) / g5 + plot_annotation(title = "Términos más repetidos en los programas electorales de las elecciones vascas del 12J de los principales partidos",
                                                    caption = "Fuente: programas electorales de PSE-EE, PNV, EH Bildu, Elkarrekin Podemos-IU y PP+Cs | @Picanumeros")

ggsave("wordcloud.png", dpi = 300)

d$media <- apply(d[,2:6],1,mean)
d_comp <- d[which(d$media > quantile(d$media, 0.95)),]

par(mfrow=c(3,2))
barplot(tail(sort(d_comp$psoe/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$psoe/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,5),
        main = "Palabras más repetidas en el programa del PSE-EE\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 5 programas")
barplot(tail(sort(d_comp$pnv/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pnv/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,5),
        main = "Palabras más repetidas en el programa del PNV\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 5 programas")
barplot(tail(sort(d_comp$bildu/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$bildu/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,5),
        main = "Palabras más repetidas en el programa de EH Bildu\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 5 programas")
barplot(tail(sort(d_comp$pod/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pod/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,5),
        main = "Palabras más repetidas en el programa de Elkarrekin Podemos-IU\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 5 programas")
barplot(tail(sort(d_comp$pp/d_comp$media),20),
        names.arg =  tail(d_comp$palabras[order(d_comp$pp/d_comp$media)],20),
        horiz = TRUE,las=1, xlim = c(0,5),
        main = "Palabras más repetidas en el programa de PP+Cs\nen comparación con el resto de partidos",
        xlab = "Nº de veces que aparece dividido entre la media de los 5 programas")

rownames(d_comp) <- d_comp[,1]
d_comp <- d_comp[-which(d_comp$palabras %in% c("eajpnv","podemosiu","elkarrekin",
                                               "socialista","socialistas")),]
colnames(d_comp)[2:6] <- c("PSOE","PNV","Elkarrekin Podemos-IU","EH Bildu","PP+Cs")
res.pca <- FactoMineR::PCA(d_comp[,2:6])
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE 
) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Fuente: programas electorales de PSE-EE, PNV, EH Bildu, Elkarrekin Podemos-IU y PP+Cs | @Picanumeros",
  x = "Componente 1 (76.3% de varianza explicada)", y = "Componente 2 (8.5% de varianza explicada)"
)
ggsave("comp1_sinpart.png", dpi = 300, width = 8.4, height = 6.4)

ind <- get_pca_ind(res.pca)

fviz_pca_biplot(res.pca, repel = F) + labs(
  title = "Análisis de componentes principales sobre los programas electorales,\nsegún frecuencia relativa de palabras utilizadas",
  caption = "Fuente: programas electorales de PSE-EE, PNV, EH Bildu, Elkarrekin Podemos-IU y PP+Cs | @Picanumeros",
  x = "Componente 1 (75.3% de varianza explicada)", y = "Componente 2 (8.9% de varianza explicada)"
)
ggsave("comp2_sinpart.png", dpi = 300, height = 12, width = 12)

mds <- dist(t(d_comp[,2:6]))

fit <- cmdscale(mds,eig=TRUE, k=2)
puntos <- as.data.frame(fit$points)
ggplot(puntos, aes(x = V1, y = V2, label = row.names(fit$points))) + geom_point() + geom_label() +
  theme_classic(base_size = 15) + labs(x = "Dimensión 1 (41.4% suma autovalores)",
                                       y = "Dimensión 2 (23.5% suma autovalores)",
                                       title = "Representación bidimensional de los programas de cada partido mediante\nescalamiento multidimensional (MDS) según las distancias entre ellos",
                                       subtitle = "Distancias obtenidas a partir de la distancia euclídea entre frecuencias relativas de palabras",
                                       caption = "Fuente: programas electorales de PSE-EE, PNV, EH Bildu, Elkarrekin Podemos-IU y PP+Cs | @Picanumeros") +
  scale_x_continuous(expand = c(0,0.003), limits = c(-0.015, 0.01)) +
  scale_y_continuous(limits = c(-0.015, 0.01))
ggsave("mds_sinpart.png", dpi = 300)
