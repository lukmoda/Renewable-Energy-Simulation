import numpy as np
import matplotlib.pylab as plt

pais = 'Usa'
#pais = str(raw_input("\nInsira o Pais: "))
data_lv = np.genfromtxt("Out_" + pais +'_lv.txt', usecols = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14), skip_header=2)
data_mv = np.genfromtxt("Out_" + pais +'_mv.txt', usecols = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14), skip_header=2)
data_hv = np.genfromtxt("Out_" + pais +'_hv.txt', usecols = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14), skip_header=2)  
m = len(data_lv)  #number of lines in table#

ano = np.zeros(m)
pop_lv = np.zeros(m)
pib_lv = np.zeros(m)
gastopc_lv = np.zeros(m)
consumo_lv = np.zeros(m)
bal_lv = np.zeros(m)
tec_lv = np.zeros(m)
pet_lv = np.zeros(m)
ren_lv = np.zeros(m)
custobar_lv = np.zeros(m)
custopet_lv = np.zeros(m)
custoren_lv = np.zeros(m)
custototal_lv = np.zeros(m)
util_lv = np.zeros(m)
total_lv = np.zeros(m)

pop_mv = np.zeros(m)
pib_mv = np.zeros(m)
gastopc_mv = np.zeros(m)
consumo_mv = np.zeros(m)
bal_mv = np.zeros(m)
tec_mv = np.zeros(m)
pet_mv = np.zeros(m)
ren_mv = np.zeros(m)
custobar_mv = np.zeros(m)
custopet_mv = np.zeros(m)
custoren_mv = np.zeros(m)
custototal_mv = np.zeros(m)
util_mv = np.zeros(m)
total_mv = np.zeros(m)

pop_hv = np.zeros(m)
pib_hv = np.zeros(m)
gastopc_hv = np.zeros(m)
consumo_hv = np.zeros(m)
bal_hv = np.zeros(m)
tec_hv = np.zeros(m)
pet_hv = np.zeros(m)
ren_hv = np.zeros(m)
custobar_hv = np.zeros(m)
custopet_hv = np.zeros(m)
custoren_hv = np.zeros(m)
custototal_hv = np.zeros(m)
util_hv = np.zeros(m)
total_hv = np.zeros(m)



for i in range (0,m):
    ano[i] = data_lv[i,0]
    pop_lv[i] = data_lv[i,1]
    pib_lv[i] = data_lv[i,2]
    gastopc_lv[i] = data_lv[i,3]
    consumo_lv[i] = data_lv[i,4]
    bal_lv[i] = data_lv[i,5]
    tec_lv[i] = data_lv[i,6]
    pet_lv[i] = data_lv[i,7]
    ren_lv[i] = data_lv[i,8]
    custobar_lv[i] = data_lv[i,9]
    custopet_lv[i] = data_lv[i,10]
    custoren_lv[i] = data_lv[i,11]
    custototal_lv[i] = data_lv[i,12]
    util_lv[i] = data_lv[i,13]
    total_lv[i] = data_lv[i,14]    
    pop_mv[i] = data_mv[i,1]
    pib_mv[i] = data_mv[i,2]
    gastopc_mv[i] = data_mv[i,3]
    consumo_mv[i] = data_mv[i,4]
    bal_mv[i] = data_mv[i,5]
    tec_mv[i] = data_mv[i,6]
    pet_mv[i] = data_mv[i,7]
    ren_mv[i] = data_mv[i,8]
    custobar_mv[i] = data_mv[i,9]
    custopet_mv[i] = data_mv[i,10]
    custoren_mv[i] = data_mv[i,11]
    custototal_mv[i] = data_mv[i,12]
    util_mv[i] = data_mv[i,13]
    total_mv[i] = data_mv[i,14]
    pop_hv[i] = data_hv[i,1]
    pib_hv[i] = data_hv[i,2]
    gastopc_hv[i] = data_hv[i,3]
    consumo_hv[i] = data_hv[i,4]
    bal_hv[i] = data_hv[i,5]
    tec_hv[i] = data_hv[i,6]
    pet_hv[i] = data_hv[i,7]
    ren_hv[i] = data_hv[i,8]
    custobar_hv[i] = data_hv[i,9]
    custopet_hv[i] = data_hv[i,10]
    custoren_hv[i] = data_hv[i,11]
    custototal_hv[i] = data_hv[i,12]
    util_hv[i] = data_hv[i,13]
    total_hv[i] = data_hv[i,14]

plt.figure(1)    
plt.xlabel('Ano')
plt.ylabel('Populacao')
plt.title('Evolucao da Populacao')
plt.plot (ano, pop_hv, marker = ".", color = "red", linestyle = "solid", label = "High Variant")
plt.plot (ano, pop_mv, marker = ".", color = "blue", linestyle = "solid", label = "Medium Variant")
plt.plot (ano, pop_lv, marker = ".", color = "green", linestyle = "solid", label = "Low Variant")
ax = plt.gca()
ax.autoscale_view()
plt.legend(loc="best")
plt.savefig(pais+"_pop.png")

plt.figure(2)    
plt.xlabel('Ano')
plt.ylabel('PIB (bilhoes de U$)')
plt.title('Evolucao do PIB')
plt.plot (ano, pib_mv, marker = ".", color = "blue", linestyle = "solid")
ax = plt.gca()
ax.autoscale_view()
plt.savefig(pais+"_pib.png")

plt.figure(3)    
plt.xlabel('Ano')
plt.ylabel('Consumo de Energia per capita (Koe/ano)')
plt.title('Evolucao do Consumo de Energia per capita')
plt.plot (ano, gastopc_mv, marker = ".", color = "blue", linestyle = "solid")
ax = plt.gca()
ax.autoscale_view()
plt.savefig(pais+"_consumopc.png")

plt.figure(4)    
plt.xlabel('Ano')
plt.ylabel('Consumo total de energia (TKoe/ano)')
plt.title('Evolucao do Consumo total de energia')
plt.plot (ano, consumo_hv, marker = ".", color = "red", linestyle = "solid", label = "High Variant")
plt.plot (ano, consumo_mv, marker = ".", color = "blue", linestyle = "solid", label = "Medium Variant")
plt.plot (ano, consumo_lv, marker = ".", color = "green", linestyle = "solid", label = "Low Variant")
ax = plt.gca()
ax.autoscale_view()
plt.legend(loc="best")
plt.savefig(pais+"_consumotot.png")
plt.figure(5)    
plt.xlabel('Ano')
plt.ylabel('Balanca Comercial (%)')
plt.title('Evolucao da Balanca Comercial')
plt.plot (ano, bal_mv, marker = ".", color = "blue", linestyle = "solid")
plt.plot (ano, [0]*m, color = "black", linestyle = "solid")
ax = plt.gca()
ax.autoscale_view()
plt.savefig(pais+"_bal.png")

plt.figure(6)    
plt.xlabel('Ano')
plt.ylabel('Evolucao Tecnologica')
plt.title('Evolucao Tecnologica')
plt.plot (ano, tec_hv, marker = ".", color = "blue", linestyle = "solid")
ax = plt.gca()
ax.autoscale_view()
plt.savefig(pais+"_tec.png")

plt.figure(7)    
plt.xlabel('Ano')
plt.title('Evolucao da Matriz Energetica')
plt.ylabel('Proporcao Fossil/Renovavel')
plt.plot (ano, pet_hv, marker = ".", color = "black", linestyle = "solid", label = "Fossil")
plt.plot (ano, ren_lv, marker = ".", color = "green", linestyle = "solid", label = "Renovavel")
ax = plt.gca()
ax.autoscale_view()
plt.legend(loc="best")
plt.savefig(pais+"_matr.png")

plt.figure(8)    
plt.xlabel('Ano')
plt.title('Evolucao do Preco do Barril de Petroleo')
plt.ylabel('Preco do Barril de Petroleo (U$)')
plt.yscale('log')
plt.plot (ano, custobar_mv, marker = ".", color = "blue", linestyle = "solid")
ax = plt.gca()
ax.autoscale_view()
plt.savefig(pais+"_barr.png")

plt.figure(9)    
plt.xlabel('Ano')
plt.title('Evolucao do Custo Relativo de Producao de Energia')
plt.ylabel('Custo Relativo de Producao de Energia (U$/Koe)')
plt.yscale('log')
plt.plot (ano, custopet_hv, marker = ".", color = "black", linestyle = "solid", label = "Fossil")
plt.plot (ano, custoren_hv, marker = ".", color = "green", linestyle = "solid", label = "Renovavel")
ax = plt.gca()
ax.autoscale_view()
plt.legend(loc="best")
plt.savefig(pais+"_custorel.png")

plt.figure(10)    
plt.xlabel('Ano')
plt.ylabel('Custo Total de Producao de Energia (bilhoes de U$/ano)')
plt.title('Evolucao do Custo Total de Producao de Energia')
plt.yscale('log')
plt.plot (ano, custototal_hv, marker = ".", color = "red", linestyle = "solid", label = "High Variant")
plt.plot (ano, custototal_mv, marker = ".", color = "blue", linestyle = "solid", label = "Medium Variant")
plt.plot (ano, custototal_lv, marker = ".", color = "green", linestyle = "solid", label = "Low Variant")
ax = plt.gca()
ax.autoscale_view()
plt.legend(loc="best")
plt.savefig(pais+"_custotot.png")

plt.figure(11)    
plt.xlabel('Ano')
plt.title('Evolucao da % do PIB destinada `a producao de energia')
plt.ylabel('% do PIB destinada `a producao de energia')
plt.yscale('log')
plt.plot (ano, util_hv, marker = ".", color = "red", linestyle = "solid", label = "PIB efetivo HV")
plt.plot (ano, util_mv, marker = ".", color = "blue", linestyle = "solid", label = "PIB efetivo MV")
plt.plot (ano, util_lv, marker = ".", color = "green", linestyle = "solid", label = "PIB efetivo LV")
plt.plot (ano, total_hv, marker = ".", color = "black", linestyle = "solid", label = "PIB total HV")
plt.plot (ano, total_mv, marker = ".", color = "cyan", linestyle = "solid", label = "PIB total MV")
plt.plot (ano, total_lv, marker = ".", color = "yellow", linestyle = "solid", label = "PIB total LV")
plt.plot (ano, [34]*m, color = "black", linestyle = "dotted", label = "PIB efetivo")
plt.plot (ano, [100]*m, color = "red", linestyle = "dotted", label = "PIB total")
ax = plt.gca()
ax.autoscale_view()
leg = plt.legend(loc=(0.65,0.51), prop={'size':10})
leg.get_frame().set_alpha(0.7)

plt.savefig(pais+"_final.png")

