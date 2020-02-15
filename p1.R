
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
rm(list=ls(all=TRUE))

list.of.packages <- c("scales", "car", "tidyverse", "ggplot2", "ggExtra", "readr", "rpart", "rpart.plot", "rattle", "corrgram", "corrplot", "tclust", "rstudioapi", "cluster", "fpc", "dplyr", "plotly", "kableExtra", "knitr")
lapply(list.of.packages, require, character.only = TRUE)

# le base de dados
BaseWine_Red_e_White <- read_delim("BaseWine_Red_e_White.csv",
                                   ";",
                                   escape_double = FALSE,
                                   col_types = cols(Vinho = col_factor(levels
                                                                       = c("WHITE", "RED"))),
                                   locale = locale(date_names = "pt",
                                                   decimal_mark = ",",
                                                   grouping_mark = "."),
                                   trim_ws = TRUE)

# remode coluna inutil id_vinho
BaseWine_Red_e_White <- subset( BaseWine_Red_e_White, select = -id_vinho )



# remove vinho tinto
BaseWine_Red_e_White <- BaseWine_Red_e_White %>% filter(Vinho == "WHITE")


# guarda copias originas do DB
#BaseWine_Red_e_White_original <- BaseWine_Red_e_White
#BaseWine_Red_e_White_original <- subset( BaseWine_Red_e_White_original, select = -Vinho )
#BaseWine_Red_e_White <- subset( BaseWine_Red_e_White, select = -Vinho )
BaseWine_Red_e_White <- subset( BaseWine_Red_e_White, select = -Vinho )




# remove duplicatas
BaseWine_Red_e_White <- BaseWine_Red_e_White %>% distinct()


# padroniza
BaseWine_Red_e_White <- BaseWine_Red_e_White %>%
  mutate_at(vars("fixedacidity", "volatileacidity", "citricacid",
                 "residualsugar", "chlorides", "freesulfurdioxide",
                 "totalsulfurdioxide", "density", "pH",
                 "sulphates", "alcohol"),
            rescale)

# remove outliers
for (x in c("fixedacidity", "volatileacidity", "citricacid",
                    "residualsugar", "chlorides", "freesulfurdioxide",
                    "totalsulfurdioxide", "density", "pH",
                    "sulphates", "alcohol")) {

  outliers <- boxplot(BaseWine_Red_e_White[[x]], plot=FALSE, range=3)$out

  if( !is.null(outliers) & length(outliers) > 0 ) {
    BaseWine_Red_e_White <- BaseWine_Red_e_White[-which(BaseWine_Red_e_White[[x]] %in% outliers),]
  }
}



anno = list(
  list(
    text = "Plot 1"
  ),
  list(
    text = "Plot 2"
  )
)



p1 <- plot_ly(x = ~ BaseWine_Red_e_White$fixedacidity, type = "histogram", name = "Com outliers")

outliers <- boxplot(BaseWine_Red_e_White$fixedacidity, plot=FALSE, range=3)$out

p2 <- plot_ly(x = ~ BaseWine_Red_e_White$fixedacidity, type = "histogram", name = "Sem outliers", marker = list(color = 'green'))

p <- subplot(p1, p2) %>% layout(title = "Acidez fixa")
p






# regressão linear
lm_modelo <- lm(quality ~ fixedacidity + volatileacidity + residualsugar + density + pH + sulphates + alcohol,
                data=BaseWine_Red_e_White)
summary(lm_modelo)

anova(lm_modelo)

par(mfrow = c(2,2))
plot(lm_modelo, which=c(1:4), pch =20)






BaseWine_Red_e_White$predicted <- round(predict(lm_modelo, BaseWine_Red_e_White))



matriz.de.confusão_tree<-table(BaseWine_Red_e_White$quality, BaseWine_Red_e_White$predicted)
summary(matriz.de.confusão_tree)



fourfoldplot(matriz.de.confusão_tree)


library(caret)

result <- confusionMatrix(
  data = factor(BaseWine_Red_e_White$predicted),
  reference = factor(BaseWine_Red_e_White$quality),
  dnn = c("Predito", "Encontrado"),
  mode = "everything")

result

MCpredTree<-table(BaseWine_Red_e_White$quality, predTree)
MCpredTree

diagonal <- diag(MCpredTree)
Acc_tree_teste <- sum(diagonal)/sum(MCpredTree)
print(Acc_tree_teste*100, digits=5)

print(prop.table(table(predTree,BaseWine_Red_e_White$quality),2),digits=2)





