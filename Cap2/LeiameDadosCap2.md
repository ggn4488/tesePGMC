#--------------------------------------------------------
# __Descrição dos dados do Cap2__
#--------------------------------------------------------

1) _loadsny1518.csv_

Cargas horárias do NYISO para os anos de 2015 a 2018.

Colunas:

1: date: data da observação (formato dd/mm/aaaa).
2: day: dia da observação na base de dados, começando com 1 (inteiro).
3: yearday: dia do ano da observação (inteiro de 1 a 366).
4: month: mês do ano (inteiro de 1 a 12).
5: hour: hora do dia (inteiro de 1 a 24).
6: weekday: dia da semana (1 para domingo, até 7 para sábado).
7: season: estação do ano (1 = primavera, 2 = verão, 3 = outono, 4 = inverno).
8 a 11: seasondummy_1, seasondummy_2, seasondummy_3, seasondummy_4: variáveis dummy para estações do ano. 0 se não estiver na estação indicada na coluna; 1 se estiver.
12: holiday: dia de feriado (1 se sim, 0 se não).
13: dst: horário de verão (1 se sim, 0 se não).
14: trend: índice para a observação na base de dados (inteiro).
15: trendnorm: o mesmo índice, da coluna anterior, mas normalizado (subtraída a média e dividido pelo desvio padrão) (float)
16 a 25: CAPITL,CENTRL,DUNWOD,GENESE,HUDVL,LONGIL,MHKVL,MILLWD,NORTH,N.Y.C.,WEST: carga, em MW, para cada uma das zonas indicadas.

Cabeçalho: Sim.

Fonte dos dados: NEW YORK INDEPENDENT SYSTEM OPERATOR - NYISO. Real-time dashboard. 
Disponível em: https://www.nyiso.com/real-time-dashboard. Acesso em: 28 Jan. 2020.

(Obs.: os dados de carga estão no dashboard do NYISO, separados por zona de carga e período; aqui nós agregamos em um mesmo arquivo e acrescentamos os demais dados).
(Obs. 2: o site deixou de ser acessível do Brasil, por alguma razão).

2) _temps1518.csv_

Temperaturas horárias em 11 estações meteorológicas do estado de Nova Iorque para os anos de 2015 a 2018.

Colunas:

1 a 11: ALB,ART,BGM,BUF,HPN,JFK,LGA,MSS,RME,ROC,SYR: temperatura, em ºC, para cada uma das zonas indicadas.

Cabeçalho: Sim.

Fonte dos dados: NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION - NOAA. Abou our agency. 
Disponível em: https://www.noaa.gov/about-our-agency. Acesso em 26. Jan. 2020.

(Obs.: estes são os dados limpos. Foram feitas remoções de ruídos e duplicadas, além da imputação de valores faltantes.)
(Obs.2: os arquivos originais estão separados por estação do ano; aqui consolidamos em um único.)

3) _PopNys.xls_

População do estado de Nova Iorque, em número de habitantes.

Na planilha 1, cada municipalidade do Estado, com sua população, e o rótulo indicado a zona de carga do NYISO dentro de qual a cidade se localiza (onde há valores faltantes, 
a zona de carga é uma das que não consideramos na tese - LONGIL, DUNWOD, MILLWD, N.Y.C.).

Na planilha 2, as estatísticas descritivas de população por zona de carga.

Fonte dos dados: CUBIT. New York Cities by Population. 
Disponível em: $https://www.newyork-demographics.com/cities_by_population$. Acesso em 13 Ago 2021.
