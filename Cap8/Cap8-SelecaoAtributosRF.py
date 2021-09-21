# Importação dos pacotes

import pandas as pd
import numpy as np
from os import chdir
from sklearn.ensemble import RandomForestRegressor
from sklearn.feature_selection import SelectFromModel
from sklearn.model_selection import train_test_split

# Filtra dados de carga
def filtra_cargas(dados, zona):

    sub_dados = dados[dados['Zona'] == zona]
    sub_dados = sub_dados.reset_index()
    return sub_dados

# Gera matriz de dados de entrada com lags das cargas
def X_cargas_lag(cargas, lag):
    
    n_dias = cargas['Dias'].iloc[-1] - lag
    
    X = pd.DataFrame([])
    
    for i in range(lag):
        
        X_loc = cargas.loc[((cargas['Dias'] >= i+1) & (cargas['Dias'] <= (n_dias + i))), 'Carga']
        X_res = X_loc.values.reshape(n_dias, 24)
        X_res = pd.DataFrame(X_res)
        X = pd.concat([X.reset_index(drop = True), X_res.reset_index(drop = True)], axis = 1)
               
    return X

# Gera matriz de dados de entrada com componentes determinísticas
def X_deterministica(cargas):
    
    cargas = cargas.drop_duplicates(subset = 'Dias')
    cargas = cargas.reset_index()
    cargas = cargas.iloc[2:,]

    X_mes = pd.get_dummies(cargas['Mes'])
    X_diasemana = pd.get_dummies(cargas['DiaSemana'])
    X_estacao = pd.get_dummies(cargas['Estacao'])
    
    X_det = pd.concat([X_mes, X_diasemana, X_estacao], axis = 1)
    
    X_det = X_det.reset_index(drop = True)
    
    return X_det

# Filtra dados de temperatura
def filtra_temps(dados, estacoes):

    dados['Temperatura'] = dados[estacoes].mean(axis = 1)
    return dados

# Gera matriz de dados de entrada com lags das temperaturas
def lag_temps(temps, lag):

    n_dias = temps['Dias'].iloc[-1] - lag
    
    X = pd.DataFrame([])
    
    for i in range(lag):
        
        X_loc = temps.loc[((temps['Dias'] >= i+2) & (temps['Dias'] <= (n_dias + i + 1))), 'Temperatura']
        X_res = X_loc.values.reshape(n_dias, 24)
        X_res = pd.DataFrame(X_res)
        X = pd.concat([X, X_res], axis = 1)
               
    return X

# Gera matriz de dados de entrada com mínimas e máximas
def calcula_temp_min_max(temps):   
    
    tmin = temps.groupby("Dias")["Temperatura"].min()
    tmax = temps.groupby("Dias")["Temperatura"].max()
    
    tmin_hoje = tmin[1:-1]
    tmin_amanha = tmin[2:]
    tmax_hoje = tmax[1:-1]
    tmax_amanha = tmax[2:]
    
    tmin_hoje = tmin_hoje.reset_index(drop = True)
    tmin_amanha = tmin_amanha.reset_index(drop = True)
    tmax_hoje = tmax_hoje.reset_index(drop = True)
    tmax_amanha = tmax_amanha.reset_index(drop = True)
    
    X_temps_min_max = pd.concat([tmax_hoje, tmax_amanha, tmin_hoje, tmin_amanha], axis = 1)
    
    return X_temps_min_max

# Retorna vetor com dados de saída
def dados_saida(dados, hora, lag):
        
    y_loc = dados[dados['Dias'] > lag]
    y_bool = y_loc['Hora'].isin([hora])
    y_index = y_bool[y_bool == True]
    y = dados['Carga'][y_index.index]
    y = y.reset_index(drop = True)
    
    return y  

# Gera dados de entrada e saída
def gera_X_y(cargas, temps, zona, estacao, lag, hora):
    
    cargas_z = filtra_cargas(cargas, zona)
    X_carga_lag = X_cargas_lag(cargas_z, lag)
    
    nomes = []
    
    for i in range(1, 25):
        nomes.append("Carga(d-1)(t - {})".format(i))
        
    for i in range(1, 25):
        nomes.append("Carga(d)(t - {})".format(i))
            
    X_carga_lag.columns = nomes
    
    X_det = X_deterministica(cargas_z)
    
    nomes = []
    
    for i in range(1, 13):
        nomes.append("Mes_{}(d+1)".format(i))
        
    for i in range(1, 8):
        nomes.append("Dia_semana_{}(d+1)".format(i))
    
    nomes.extend(["Primavera(d+1)", "Verao(d+1)", "Outono(d+1)", "Inverno(d+1)"])
    
    X_det.columns = nomes
        
    temps_e = filtra_temps(temps, estacao)
    X_temps_lag = lag_temps(temps_e, lag)

    nomes = []
    
    for i in range(1, 25):
        nomes.append("Temp(d)({})".format(i))
    
    for i in range(1, 25):
        nomes.append("Temp(d+1)({})".format(i))
            
    X_temps_lag.columns = nomes

    X_temps_min_max = calcula_temp_min_max(temps_e)
    
    nomes = ['Temp_max(d)', 'Temp_max(d+1)', 'Temp_min(d)', 'Temp_max(d+1)']
    
    X_temps_min_max.columns = nomes
    
    y = dados_saida(cargas_z, hora, lag)
    
    y.name = "Carga(d+1)({})".format(hora)
    
    X = pd.concat([X_carga_lag, X_det, X_temps_lag, X_temps_min_max],
                  axis = 1)
    
    return X, y

def testelag_cargas(cargas, lag):

    n_dias = cargas['Dias'].iloc[-1] - lag
    
    X = pd.DataFrame([])
    
    for i in range(lag):
        
        X_loc = cargas.loc[((cargas['Dias'] >= i+2) & (cargas['Dias'] <= (n_dias + i + 1))), 'Time']
        X_res = X_loc.values.reshape(n_dias, 24)
        X_res = pd.DataFrame(X_res)
        X = pd.concat([X, X_res], axis = 1)
               
    return X

def testedados_saida(dados, hora, lag):
    
    y_loc = dados[dados['Dias'] > lag]
    y_bool = y_loc['Hora'].isin([hora])
    y_index = y_bool[y_bool == True]
    y = dados['Time'][y_index.index]
    
    return y  

chdir("C:/Users/Pc/Documents/GitHub/tesePGMC/Dados")

lag = 2

cargas = pd.read_csv("cargas_ny_1518_tidy.csv")
cargas['Zona'] = cargas['Zona'].replace(['HUD VL','MHK VL'],['HUDVL','MHKVL'])
zona_carga = cargas['Zona'].unique()
zona_carga = np.setdiff1d(zona_carga, ['LONGIL', 'MILLWD', 'N.Y.C.'])

temps = pd.read_csv("temps_ny_1518_tidy_wDays.csv")
temps_untidy = pd.read_csv("temps_ny_1518_untidy.csv")
temps_untidy['Dias'] = temps['Dias']

estacoes_zonas = [['BUF'],
                  ['ROC'],
                  ['BGM', 'ELM', 'SYR'],
                  ['PBG'],
                  ['ART', 'MSS', 'MSV', 'RME'],
                  ['ALB'],
                  ['POU', 'SWF']]
# loop em zonas e em horas
X, y = gera_X_y(cargas, temps_untidy, zona_carga[0], estacoes_zonas[2], lag, 24)

X_train, X_test, y_train,y_test = train_test_split(X,y,test_size=365, shuffle = False)

sel = SelectFromModel(RandomForestRegressor(n_estimators = 100))
sel.fit(X_train, y_train)

sel.get_support()

selected_feat= X_train.columns[(sel.get_support())]

len(selected_feat)

print(selected_feat)