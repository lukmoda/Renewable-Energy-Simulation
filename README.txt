  SIMULAÇÃO ECONÔMICA ENERGIAS RENOVÁVEIS  

  Esse arquivo README tem como finalidade especificar o propósito do programa, os parâmetros de entrada, os parâmetros de saída e as especificações necessárias para executá-lo corretamente.

  O QUE O PROGRAMA FAZ

  O programa consiste de um cenário em que a matriz energética transita de fóssil para renovável. Enquanto isso acontece, a população cresce, o PIB se altera, o custo do barril de petróleo aumenta, o custo de produção das energias renováveis diminui, o consumo de energia per capita varia... 
  Dependendo dos parâmetros de entrada, que se interrelacionam, o cenário final será diferente. Podemos ter diminuição do custo de produção das energias ao longo do tempo, ou um aumento ainda suportado pela economia, ou até um colapso total da mesma. 
  O programa em si é um loop no tempo (de 2018 até 2100, com TIMESTEP de 1 ano) composto por 8 subrotinas:
		
	  - setup -> lê o arquivo com os parametros de entrada, armazena nas variaveis e produz as condições iniciais;
	  - randomize -> randomiza alguns dos parametros de entrada, relevantes p/ gerar uma nova simulação;
	  - novos_param -> escreve os novos parametros em um novo arquivo;
     - trade_off -> calcula a nova proporção entre o uso das energias (fóssil x renovável);
     - avanco_tec -> calcula os novos parâmetros relacionados à tecnologia (Tec, Balança e Custo Ren. Ver detalhes abaixo);
     - economia -> calcula os novos parâmetros relacionados à economia (Consumo pc, Consumo total de energia, PIB e PIB eff, SupDef e Preço Barril. Ver detalhes abaixo);
     - resultado -> calcula os custos de produção de energia e os relaciona com o PIB e PIB eff;
     - escreve_arq -> escreve os resultados num arquivo

	A função "sinal" retorna -1 ou 1 dependendo de um numero pseudoaletório gerado, e é utilizada nos cálculos da balança comercial. A subrotina "flut_bar" também utiliza números pseudoaleatórios para gerar flutuações no preço do barril de petróleo.

  Com isso, é possível explorar as saídas e observar as dependências das condições iniciais adotadas, assim como os cenários produzidos.
  As condições iniciais são randomizadas paulatinamente, até que se alcance a condição de saída (colapso da economia em determinado ano) - dentro da precisão desejada. O programa futuramente será paralelizado utilizando MPI para amplificar a performance.

  PARÂMETROS DE ENTRADA (t = 0)

- País: nome do país;
- PIB: PIB total (U$);
- PIB eff: Porcentagem do PIB alocada para investimentos (0 a 1);
- Pet: fração do uso de energias fósseis (entre 0 e 1);
- Ren: fração do uso de energias renováveis (entre 0 e 1);
- SupDef: array de 2 valores, com o máximo superávit possível seguido do máximo déficit possível (0 a 1);
- Consumo pc: consumo de energia per capita (Koe/ano);
- Preço Barril: Preço do barril de petróleo (U$);
- Rendimento Barril: Quantidade necessária de barris de petróleo para gerar 1 Koe (barril/Koe);
- Custo Ren: Custo de produção das energias renováveis (num > 1 representando a proporção do custo inicial ren e fossil);
- Custo Min: Custo mínimo para produção de 1 Koe através de fontes renováveis (U$);
- Tec: Parâmetro de avanço tecnológico -> melhora a balança comercial e diminui o custo das en renovaveis (0 a 1000);
- Ass Soc: Parâmetro de assistência social -> incrementa o pârametro de Tecnologia (entre 0 e 1);
- Expo To: Expoente do decaimento da fração do uso de energias fósseis
- Expo Tec: Expoente relacionado ao incremento do parâmetro de avanço tecnológico;
- Expo Consumopc: Expoente relacionado ao incremento do consumo de energia per capita;
- Turning Pb: fracao a partir da qual o preço do petróleo não aumenta mais, ou seja, fica constante (0 a 1);
- Turning PetRen: fracao a partir da qual toda a producao de energia passa a ser renovavel (0 a 1, de preferencia proxima a 5%);
- Incremento Barr: incremento no custo do barril de petróleo, calculado com base na razão entre Pet atual e anterior (> 1);
- Incrementos AssSocTec: array de 5 números (0 a 1), relacionando incremento do parâmetro de tecnologia com o parâmetro de assistência social (cada valor está p/ um range de Ass Soc -> ( < 0.3, entre 0.3 e 0.6, entre 0.6 e 0.8, entre 0.8 e 0.9 e > 900); 
- Incrementos TecBal: array de 6 números (0 a 1), relacionando incremento da balança com o parâmetro de tecnologia (cada valor está p/ um range de Tec -> entre 300 e 500, entre 500 e 600, entre 600 e 700, entre 700 e 800 entre 800 e 900 e > 900);
- Incrementos TecCustoRen: array de 5 números (0 a 1), relacionando barateamento do custo das energias renováveis com o parâmetro de tecnologia (cada valor está p/ um range de Tec -> entre 500 e 600, entre 600 e 700, entre 700 e 800 entre 800 e 900 e > 900);
- Semente: Semente do gerador de numeros aleatorios p/ simulacao da balança comercial (INT);
- Faixa de randomização: módulo da variação máxima dos parâmetros a cada re-simulação (> 0);
- Limites: array de 2 valores, com a porcentagem do PIB considerada como colapso. Valores mínimo e máximo (aconselhável mantê-los próximos, atentando para a possibilidade de não existirem dependendo da simulação, resultando num loop infinito);
- Crash: Ano em que se deseja que a economia colapse

  PARÂMETROS DE SAÍDA 	

- População - > Isso não é simulado, os dados são tirados das Variants da tabela da ONU;
- PIB total (U$);
- Consumo de energia per capita (Koe/ano);
- Consumo total de energia (Koe/ano) -> Consumo per capita * População;
- Balança comercial (0 a 1) -> Indica o superávit ou déficit;
- Parâmetro de Avanço Tecnológico (0 a 1000);
- Fração do uso de energias fósseis (entre 0 e 1);
- Fração do uso de energias renováveis (entre 0 e 1);
- Preço do barril de petróleo (U$); 
- Custo de produção das energias fósseis (U$/Koe);
- Custo de produção das energias renováveis (U$/Koe);
- Custo de produção total para suprir a demanda energética (U$);
- Porcentagem do pib efetivo necessária para suprir a demanda energética (%);
- Porcentagem do pib total necessária para suprir a demanda energética (%)

  COMO EXECUTAR O PROGRAMA

 Para executar a simulação é necessário carregar o arquivo com os parâmetros acima (por default "Usa_param.txt", mas pode ter o nome que quiser contanto que você descomente no código fonte) e ter, na mesma pasta do código fonte, o arquivo txt com a evolução populacional (neste caso extraída das projeções da ONU, mas você pode usar o que quiser). IMPORTANTE: esse arquivo deve conter a população anual linha a linha, e ser nomeado da seguinte maneira:

	- País + _lv.txt - > se a projeção for "Low";
	- País + _mv.txt - > se a projeção for "Medium";
	- País + _hv.txt - > se a projeção for "High";   

 Por exemplo, "Usa_mv.txt" carregaria o array com a projeção da população dos Estados Unidos para a Medium Variant. IMPORTANTE: o nome do país deve conter exatamente três letras (exemplo: Usa, Fra, Ale, Can). Não deixe espaços em branco.
 
 O arquivo com os parâmetros deve vir com estes NA ORDEM especificada pelo README, cada um numa linha (sem pular linha), e os parâmetros que são arrays devem ter os valores separados por vírgula.

 Os resultados são gravados num arquivo nomeado como "Out + _País + _Variant.txt". Por exemplo, o arquivo de saída utilizando a High Variant do Brasil seria "Out_Bra_hv.txt".

 Os parâmetros que geraram o colapso de acordo com a condição pretendida ficam gravados num arquivo nomeado como "País + _new.txt".

 Em seguida basta usar o arquivo de saída para obter os plots, em algum software de sua preferência; neste caso, foi usada a biblioteca Matplotlib do Python (python plots.py).

Caso queira rodar a simulação e obter os plots com um só comando, basta rodar o script shell run.sh com ./run.sh (nesse caso os plots sairão feitos no matplotlib).

