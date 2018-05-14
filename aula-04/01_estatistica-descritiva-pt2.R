#' ---
#' title: "Estatística Descritiva & R (II)"
#' output:
#'   html_document:
#'     df_print: paged
#' ---
#' 
## ----"LocaleEncoding", message=FALSE, warning=FALSE, echo=FALSE----------
Sys.setlocale("LC_ALL", "pt_BR")
options(encoding = "UTF-8")

#' 
#' ## Continuação da Aula 03
#' 
#' Na aula 03 estudamos 2 medidas de tendência central e duas medidas de dispersão:
#' 
#' - Medidas de Tendência Central
#'     + Média
#'     + Mediana
#' 
#' - Medidas de Dispersão
#'     + Desvio Padrão
#'     + Desvio Absoluto da Mediana
#' 
#' Nesta aula continuaremos com mais algumas medidas de tendência central e de dispersão, bem como medidas de associação entre 2 variáveis
#' 
#' - Medidas de Tendência Central
#'     + Moda
#' 
#' - Medidas de Dispersão
#'     + Intervalo Interquartil
#' 
#' - Medidas de Associação entre 2 variáveis
#'     + Covariância
#'     + Correlação
#' 
#' Na próxima aula trabalharemos com a biblioteca gráfica `ggplot2` e exploraremos visualmente as Estatísticas Descritivas estudadas até então.
#' 
#' ### Dataset para práticas
#' 
#' Seguiremos com os dados de remuneração de servidores públicos federais no mês de Fevereiro de 2018, pós-processados para uso nesta aula.
#' 
## ----"Dataset", message=FALSE, warning=FALSE-----------------------------
library(tidyverse)

subset_salarios <- read_csv("aula-04/data/201802_dados_salarios_servidores.csv.gz") %>%
  filter(REMUNERACAO_REAIS > 900, !is.na(UF_EXERCICIO)) %>%
  select(ID_SERVIDOR_PORTAL, REMUNERACAO_REAIS, DESCRICAO_CARGO, DATA_INGRESSO_ORGAO, ORGSUP_EXERCICIO, DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO, UF_EXERCICIO)

head(subset_salarios, 20)

#' 
#' ## Medidas de Tendência Central
#' 
#' Medidas de posição apresentam o valor típico de uma variável
#' 
#' > Uma estimativa do valor esperado para a variável
#' 
#' ### Moda
#' 
#' Valor mais frequente em um conjunto. É uma estatística descritiva mais empregada em variáveis categóricas, mas também pode ser utilizada em variáveis numéricas (neste caso não correpondendo necessariamente a um valor central).
#' 
#' Exemplos:
#' 
## ------------------------------------------------------------------------
subset_salarios %>%
  count(DESCRICAO_CARGO) %>%
  arrange(desc(n)) %>%
  head(1)

#' 
## ------------------------------------------------------------------------
ocorrencias_remuneracoes <- table(subset_salarios$REMUNERACAO_REAIS)

(moda_salario <- as.numeric(names(sort(ocorrencias_remuneracoes, decreasing = TRUE))[1]))

#' 
## ------------------------------------------------------------------------
mode <- function(x) {
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}

subset_salarios %>%
  mutate(remuneracao = round(REMUNERACAO_REAIS, -2)) %>% # Arredonda para centenas
  count(remuneracao) %>%
  ggplot( aes(x = remuneracao, y = n)) +
  geom_col() +
  geom_vline(xintercept = mean(subset_salarios$REMUNERACAO_REAIS), color="red", size=0.5, linetype="dashed") +
  geom_vline(xintercept = median(subset_salarios$REMUNERACAO_REAIS), color="blue", size=0.5, linetype="dashed") +
  geom_vline(xintercept = mode(subset_salarios$REMUNERACAO_REAIS), color="darkgreen", size=0.5, linetype="dashed") +
  geom_point(data = filter(subset_salarios, REMUNERACAO_REAIS > 40000 ), mapping = aes(x=REMUNERACAO_REAIS), inherit.aes = FALSE, y=0, color="darkgreen", size=1, alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(subtitle = "Média em vermelho, Mediana em azul, Moda em verde") +
  theme_minimal()

#' 
#' E quando não há um valor mais frequente?
#' 
## ------------------------------------------------------------------------
set.seed(1234)
valores_aleatorios_uniforme <- runif(n = 300, min = 1000, max = 5000)

ocorrencias_valores_aleatorios <- table(valores_aleatorios_uniforme)

# Quantas ocorrências do valor mais frequente?
max(ocorrencias_valores_aleatorios)

#' 
## ------------------------------------------------------------------------
(moda_uniforme <- mode(valores_aleatorios_uniforme))

#' 
#' 
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00015.jpg)
#' 
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00016.jpg)
#' 
#' 
#' ## Medidas de Dispersão
#' 
#' ### Intervalo Interquartil
#' 
#' #### Percentis / Quantis
#' 
#' Medida que divide um conjunto ordenado em P% dos valores anteriores à medida e (100-P)% valores acima desta medida.
#' 
#' A mediana é o percentil 50%.
#' 
#' Quartis dividem o conjunto ordenado em 4 valores:
#' 
#' - Quartil 1 (25%)
#' - Quartil 2, a mediana
#' - Quartil 3 (75%)
#' 
#' Em R:
## ----eval=FALSE----------------------------------------------------------
## ## Default S3 method:
## quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE)

#' 
#' Exemplo:
## ------------------------------------------------------------------------
quantile(subset_salarios$REMUNERACAO_REAIS)

#' 
## ------------------------------------------------------------------------
summary(subset_salarios$REMUNERACAO_REAIS)

#' 
#' #### O intervalo interquartil
#' 
#' Diferença entre o Terceiro Quartil e o Primeiro Quartil. Também conhecido pela sigla IQR.
#' 
## ------------------------------------------------------------------------
IQR(subset_salarios$REMUNERACAO_REAIS)

#' 
#' Em datasets com dados distribuidos simetricamente em torno da mediana, o IQR corresponderá ao dobro do Desvio Absoluto da Mediana.
#' 
#' >> ATIVIDADE
#' 
#' __Atividade I__
#' 
#' Qual a razão entre o IQR e o Desvio Absoluto da Mediana da variável REMUNERACAO_REAIS?
#' 
## ------------------------------------------------------------------------
print("Atividade")

## Código aqui

#' 
#' __Atividade II__
#' 
#' Crie uma função para calcular o Desvio Absoluto da Mediana. 
#' Calcule o Desvio Padrão, o Desvio Absoluto da Mediana e o IQR do tempo de ingresso __em anos__ (obtido da variável DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO).
#' 
## ------------------------------------------------------------------------
print("Atividade")

## Código aqui

#' 
#' >> FIM ATIVIDADE
#' 
#' 
#' ## Medidas de Associação entre 2 variáveis
#' 
#' A covariância ajuda a entender como dois conjuntos estão relacionados entre si.
#' 
#' $$cov = \frac{\sum _{i=1}^{n}(x - \overline{x})(y - \overline{y})}{n - 1}$$
## ------------------------------------------------------------------------
cov(x = subset_salarios$REMUNERACAO_REAIS, y = 2018 - year( subset_salarios$DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))

#' 
## ------------------------------------------------------------------------
cov(x = subset_salarios$REMUNERACAO_REAIS, y = year( subset_salarios$DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))

#' 
#' Seu sinal determina se há uma relação positiva ou negativa, conforme visto nos dois exemplos anteriores. Seus valores, contudo, variam de $-\infty$ até $+\infty$, dependendo da magnitude dos valores relacionados.
#' 
#' A correlação é uma padronização da covariância a partir dos desvios padrão, forçando que seus valores variem de $-1$ a $+1$, permitindo assim interpretar também o coeficiente.
#' 
#' $$cor = \frac{\sum _{i=1}^{n}(x - \overline{x})(y - \overline{y})}{n - 1} \frac{1}{s_x \times s_y}$$
#' 
## ------------------------------------------------------------------------
cor(x = subset_salarios$REMUNERACAO_REAIS, y = 2018 - year( subset_salarios$DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))

#' 
#' Esta correlação é a correlação (linear) de Pearson. Estudaremos também outras correlações, não-lineares, quando estudarmos testes não-paramétricos.
#' 
#' ### Interpretação da correlação:
#' 
#' A correlação de Pearson é o cosseno entre o vetor de covariância de duas variáveis e o vetor dos desvios padrão multiplicados. Geometricamente:
#' 
#' - Se $\rho = 1$, o ângulo $\alpha = 0°$, os dois vetores são colineares (paralelos).
#' - Se $\rho = 0$, o ângulo $\alpha = 90°$, os dois vetores são ortogonais.
#' - Se $\rho = -1$, o ângulo $\alpha = 180°$, os dois vetores são colineares com sentidos opostos. 
#' 
#' E a interpretação usual é a seguinte:
#' 
#' - 0.9 para mais ou para menos indica uma correlação muito forte.
#' - 0.7 a 0.9 positivo ou negativo indica uma correlação forte.
#' - 0.5 a 0.7 positivo ou negativo indica uma correlação moderada.
#' - 0.3 a 0.5 positivo ou negativo indica uma correlação fraca.
#' - 0 a 0.3 positivo ou negativo indica uma correlação desprezível.
#' 
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00019.jpg)
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00020.jpg)
#' 
#' >> ATIVIDADE
#' 
#' - Calcule a correlação entre o ano de ingresso no órgão (DATA_INGRESSO_ORGAO) e ano de ingresso no serviço público (DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)
#' - Decomponha a correlação em 2 respostas, atribuindo a duas variáveis distintas: 
#'     + Se é positiva ou negativa
#'     + Qual o grau de correlação (de acordo com o texto de interpretação da correlação)
#' 
## ------------------------------------------------------------------------
print("Atividade")

## Código aqui

#' 
#' >> FIM ATIVIDADE
#' 
#' 
#' ## Outros problemas com estatísticas descritivas
#' 
#' - [Quarteto de Anscombe e a importância da visualização](https://pt.wikipedia.org/wiki/Quarteto_de_Anscombe)
#' - [Datasaurus Dozen](https://www.autodeskresearch.com/publications/samestats)
#' 
#' ## Mais sobre estatísticas descritivas
#' 
#' - [Unlearning Descriptive Statistics](http://debrouwere.org/2017/02/01/unlearning-descriptive-statistics/)
#' - [Modes, Medians and Means: A Unifying Perspective](http://www.johnmyleswhite.com/notebook/2013/03/22/modes-medians-and-means-an-unifying-perspective/)
