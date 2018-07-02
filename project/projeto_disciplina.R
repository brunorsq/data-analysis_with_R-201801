# Nomes: Bruno Rousenq Zeferino, Victor e Ithon

# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos

View(departments)
View(aisles)
View(products)
View(insta_orders)
View(insta_products)

#1 # Quantos dos produtos do cadastro nunca foram comprados?

products %>%
    filter(!(products$product_id %in% insta_products$product_id)) %>%
    count()

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

mercado <- merge(x = products, y = departments, by = "department_id")
        
mercado <- merge(x = mercado, y = aisles, by = "aisle_id")
View(mercado)

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

mercado %>%
    group_by(department,aisle)%>%
    summarise(count = n()) %>%
    arrange(desc(count))%>%
    head(10) -> depart_corred

View(depart_corred)

#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

insta_products%>%
    count() -> total_pedidos

merge(x = insta_products, y = mercado, by = "product_id") -> mercado_venda

mercado_venda %>%
    filter(paste(mercado_venda$department,mercado_venda$aisle) %in% paste(depart_corred$department,depart_corred$aisle)) %>%
    summarise(perc = (n()/total_pedidos)*100)

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

mercado_venda %>%
    filter((paste(mercado_venda$department,mercado_venda$aisle) %in% paste(depart_corred$department,depart_corred$aisle))&&(mercado_venda$aisle != 'missing' & mercado_venda$department != 'missing')) -> ped_prod_categ
View(ped_prod_categ)
#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   # Transforme as variáveis user_id, department e aisle em factor
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)

   # Este dataframe deverá ser utilizado em todas as atividades seguintes
products %>%
    inner_join(x = products, y = departments, by = "department_id") %>%
    inner_join(x = products, y = aisles, by = "aisle_id") %>%
    inner_join(x = products, y = ped_prod_categ, by = "product_id") %>%
    inner_join(x = ped_prod_categ, y = insta_orders, by = "order_id") %>%
    mutate(user_id = factor(user_id),
           department = factor(department),
           aisle = factor(aisle),
           order_hour_of_day = ordered(order_hour_of_day)) -> final
View(final)
#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
final %>%
    group_by(order_hour_of_day) %>%
    summarise(count = n_distinct(user_id)) %>%
    arrange(desc(count)) %>%
    head(5) -> top_hour
    

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)

final %>%
    filter(order_hour_of_day %in% top_hour$order_hour_of_day) %>%
    group_by(product_name) %>%
    summarise(count = n())%>%
    arrange(desc(count)) %>%
    head(15)

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.


#13 # Identifique, por usuário, o tempo médio entre pedidos


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado


#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.


#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências


#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 


#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

