# Market Basket Analysis


### Embora tenha mais de 20 anos, a Market Basket Analysis (MBA) (ou Regras da Associação de Mineração) ainda pode ser uma técnica muito útil para obter insights sobre grandes conjuntos de dados transacionais.

##### O exemplo clássico são os dados transacionais em um supermercado. Para cada cliente, sabemos quais produtos individuais (itens) ele colocou na cesta e comprou. Outros casos de uso para o MBA podem ser dados de cliques da web, arquivos de log e até mesmo questionários.
##### Com a análise da cesta de compras, podemos identificar itens que costumam ser comprados juntos. Normalmente, os resultados de um MBA são apresentados na forma de regras. As regras podem ser tão simples quanto {A ==> B}, quando um cliente compra o item A, ele tem (muito) probabilidade de comprar o item B. Regras mais complexas também são possíveis {A, B ==> D, F} , quando um cliente compra os itens A e B, é provável que compre os itens D e F.

##### No total, o conjunto de dados transacionais de futebol contém cerca de 18.000 registros. Obviamente, esses registros não incluem apenas transferências multimilionárias cobertas pela mídia, mas também todas as transferências de jogadores dos quais ninguém nunca ouviu falar.

# Como vamos aplicar o MBA?
##### No R, você pode usar o pacote arules para regras de associação de mineração / MBA.
##### Alternativamente, quando a ordem das transações é importante, você deve usar o pacote arulesSequences. ##### Depois de executar o algoritmo, obteremos alguns resultados interessantes.

##### Em R, os gráficos de rede podem ser bem visualizados usando o pacote visNetwork.

## Configurarando o diretório de trabalho
```{r}
setwd("D:/Projects/R/BigData_Analytics_R/Cap04")
getwd()
```
## Packages
```{r}
library(RSQLite)
library(dplyr)
library(tidyr)
library(arules)
library(arulesSequences)
library(readr)
library(stringr)
library(visNetwork)
library(igraph)
library(lubridate)
library(DT)
```
## Os dados estão disponíveis em um banco de dados SQLITE que pode ser baixado do kaggle.

### Conectando-se ao banco de dados
```{r}
con = dbConnect(RSQLite::SQLite(), dbname="database.sqlite")
```

### Obtendo a lista de tabelas
```{r}
alltables = dbListTables(con)
alltables
```
### Extraindo as tabelas

```{r}
players       = dbReadTable(con, "Player")
players_stats = dbReadTable(con, "Player_Attributes")
teams         = dbReadTable(con, "Team")
league        = dbReadTable(con, "League")
Matches       = dbReadTable(con, "Match")
```

### Substituindo espaço por sublinhado em nomes muito longos
```{r}
teams$team_long_name = str_replace_all(teams$team_long_name, "\\s", "_")
teams$team_long_name = str_replace_all(teams$team_long_name, "\\.", "_")
teams$team_long_name = str_replace_all(teams$team_long_name, "-", "_")
print(teams)
```
## Preparando os dados para minerar as regras de associação

### Os jogadores estão em colunas separadas, mas precisamos deles empilhados em uma coluna

```{r}
tmp = Matches %>% 
  select(
    season, 
    home_team_api_id, 
    home_player_1:home_player_11
  )%>%
  gather(
    player, 
    player_api_id, 
    -c(season, home_team_api_id)
  ) %>%
  group_by(player_api_id, home_team_api_id ) %>% 
  summarise(season = min(season))
```

### Juntando jogadores e dados do clube

```{r}
playerClubSequence = left_join(
  tmp,
  players
  ) %>% 
  left_join(
    teams, 
    by=c("home_team_api_id"="team_api_id")
  )
playerClubSequence = playerClubSequence %>% 
  filter(
    !is.na(player_name), !is.na(team_short_name)
  )  %>%
  arrange(
    player_api_id, 
    season
  )
```
### Adicionando um número sequencial por jogador
```{r}
playerClubSequence$seqnr = ave( playerClubSequence$player_api_id, playerClubSequence$player_api_id, FUN = seq_along)
playerClubSequence$size = 1
```

## Mineração de string com algoritmo cSPade do pacote arulesSequences

### Grava o conjunto de dados em um arquivo txt para fácil manipulação da função read_basket em arulesSequence para criar um objeto de transação
```{r}
write_delim( 
  playerClubSequence %>% select( c(player_api_id, seqnr, size, team_long_name)) ,
  delim ="\t", path = "player_transactions.txt", col_names = FALSE
  )
```

### Agora importamos as transações registradas no item anterior

```{r}
playerstrxs <- read_baskets("player_transactions.txt", sep = "[ \t]+",info =  c("sequenceID","eventID","size"))
summary(playerstrxs)
```
### Execute a mineração de sequência, por enquanto apenas com o comprimento de duas sequências
```{r}
playersClubSeq <- cspade(
  playerstrxs, 
  parameter = list(support = 0.00010, maxlen=2), 
  control   = list(verbose = TRUE)
)
summary(playersClubSeq)
```

### Fazendo Data Wrangling para colocar os resultados do cspade em um conjunto de dados organizado que é adequado para visNetwork. VisNetwork precisa de dois conjuntos de dados: um conjunto de dados com bordas "de -> para" e um conjunto de dados com nós exclusivos
```{r}
seqResult = as(playersClubSeq, "data.frame")
seqResult = seqResult %>% 
  mutate(
    sequence = as.character(sequence)
  )
seqResult = bind_cols(
  seqResult,
  as.data.frame(
    str_split_fixed(seqResult$sequence, pattern =",", 2), 
    stringsAsFactors = FALSE)
  )
seqResult$from = str_extract_all(seqResult$V1,"\\w+", simplify = TRUE)[,1] 
seqResult$to   = str_extract_all(seqResult$V2,"\\w+",simplify = TRUE)[,1]
seqResult$width = exp(3000*seqResult$support)
seqResult = seqResult %>% filter(V2 !="")
seqResult$title = paste(seqResult$sequence, "<br>", round(100*seqResult$support,2), "%")
seqResult$support_perc = paste(sprintf("%.4f", 100*seqResult$support), "%")
```

## Criando o dataframe com os nós
```{r}
nodes = unique(c(seqResult$from, seqResult$to))
nodesData = data.frame(id = unique(nodes), title = unique(nodes), label = unique(nodes), stringsAsFactors = FALSE) %>%
  left_join(CountryClub, by = c("id"="team_long_name")) %>% 
  rename(group = name)
```

### Calcula as medidas de centralidade entre as duas usando o igraph, para que possamos ter diferentes tamanhos de nós no gráfico da rede
```{r}
transferGraph = graph_from_data_frame(seqResult[,c(5,6)], directed = TRUE)
tmp = betweenness(transferGraph)
Clubs_betweenness = data.frame(id = names(tmp), value = tmp, stringsAsFactors = FALSE)
nodesData = nodesData %>% 
  left_join(Clubs_betweenness) %>%
  mutate(title = paste(id, "betweeness ", round(value))) %>%
  arrange(id)
```

## Criando a rede interativa

### Preparando o dataframe final e removendo duplicatas
```{r}
nodes = nodesData
nodes = nodes[!duplicated(nodes$id),]
```

### Cria a rede
```{r}
visNetwork(nodes, edges = seqResult, width = 900, height = 700) %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000)
  )
  
```
<img src ="https://github.com/pcastr/BigData_Analytics_R/blob/master/Cap04/files/img/nodes.png"/>

### Especificandi em um clube

<img src= "https://github.com/pcastr/BigData_Analytics_R/blob/master/Cap04/files/img/nodes_barcelona.png">


### Cria a tabela final para support analysis
```{r}
seqResult$Ntransctions = seqResult$support*10542
DT::datatable(
  seqResult[,c(5,6,9,10)], 
  rownames = FALSE,
  options = list(
    pageLength=25)
  )
```
<img src="https://github.com/pcastr/BigData_Analytics_R/blob/master/Cap04/files/img/search_barcelona.png">





