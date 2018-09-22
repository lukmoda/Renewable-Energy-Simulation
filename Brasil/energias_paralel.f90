!Simulacao Energias Renovaveis
!Author: Lucas Moda

PROGRAM energias_paralel


IMPLICIT NONE

REAL*4, DIMENSION(:), ALLOCATABLE :: pop, &         !array da populacao
												 pib, &         !array do pib nominal total (U$)			
				  							    pib_util, &    !array do pib nominal utilizado pelo governo (U$) 
				   						    gasto_pc, &    !gasto de energia per capita (Koe/ano)
				   						    consumo, &     !consumo total de energia (Koe/ano)
				   						    uso_pet, &     !prop de energias fosseis (0 a 1)
				   						    uso_ren, &     !prop de energias renovaveis (0 a 1)
				   						    custo_bar, &   !preco do barril de petroleo (U$/barril)
												 custo_barnew, &			
												 custo_pet, &   !preco das en fosseis por Koe (U$/Koe)
												 custo_ren, &   !preco das en renovaveis por Koe (U$/Koe)
												 custo_total, & !preco necessario usando ambas energias p/abastecer
																	 !populacao por 1 ano (U$)	
												 porc_util, &	 !pctagem necessaria do pib utilizado pelo governo p/
																	 !suprir a demanda energetica		
												 porc_total, &  !pctagem necessaria do pib total p/ suprir 
																	 !a demanda energetica
											    balanca, &     !superavit ou deficit
												 ed_cie_tec, &  !indice de ciencia, educacao e tecnologia (0 a 1000)
												 ed_cie_tecnew													
																													   

INTEGER :: n,        &    !numero de timesteps
			  seed,	   &	  !semente p/ a geracao de numeros pseudoaleatorios
			  crash,    &    !ano em que a economia colapsa (input p/ a condicao de saida)
			  i, j, k 

!a maioria dos parametros a seguir deve ser importada de um arquivo (ver README)

CHARACTER(15) :: arq, arqnew      !nome dos arquivos

CHARACTER(3) :: pais, &  	!nome do pai's
 					 aux        !apenas uma string auxiliar p/ controlar os arquivos e prints das variants

REAL*4 :: pib_eff,      &  !porcentagem do pib usada pelo governo (0 a 1)
			 rend_bar,     &  !qtde de barris necessarios p produzir 1 Koe (barril/Koe)
			 prop_ren,     &  !num > 1 que relaciona a proporcao entre o custo inicial da prod de en fossil e ren
			 prop_rennew,  &
			 custo_min,    &   !custo minimo p/ a producao de 1 Koe utilizando en renovaveis 
			 custo_minnew, &
			 ass_social,   &  !parametro de assistencia social. Aumenta o indice de tecnologia c/ o tempo (0 a 1)
			 expo_to,      &  !expoente utilizado no calculo da subrotina trade off
			 expo_tonew,   &
          expo_tec,     &  !expoente utilizado no calculo da evolucao do parametro de tecnologia
			 expo_tecnew,  &
          expo_gasto,   &  !expoente utilizado no calculo da evolucao do consumo per capita
			 expo_gastonew, &
          turn_pb,      &  !fracao a partir da qual o custo do barril de petroleo se torna constante
			 turn_pbnew,   &
          turn_petren,  &  !fracao a partir da qual o uso de energias fosseis zera
			 turn_petrennew,   &
          inc_bar,      &  !num >1 utilizado no calculo da evolucao do preco do barril de petroleo
			 inc_barnew,   &
          randpar              !num >0 que representa o modulo da variacao maxima dos parametros


REAL*4, DIMENSION(2) :: supdef,       &         !array do superavit maximo e do deficit maximo
								supdefnew,    &
								lim							!array que define os limites superior e inferior p/ a precisao da simulacao 																	
REAL*4, DIMENSION(5) :: inc_asstec,      &  !array que relaciona o incremento no parametro de teconologia de 
								                    !acordo com o parametro de assistencia social
								inc_asstecnew,   &
                        inc_tecren,      &  !array que relaciona o decrescimo do custo das energias renovaveis de
                                            !acordo com o parametro de tecnologia
								inc_tecrennew

REAL*4, DIMENSION(6) :: inc_tecbal,    &     !array que relaciona o incremento da balanca de acordo com o parametro
 													      !de tecnologia
								inc_tecbalnew

LOGICAL :: cond !booleano p/ controlar a condicao adotada para termino da simulacao
				
cond = .FALSE.
!PRINT*,"Digite o nome do arquivo c/ os parametros de entrada: "
!READ*, arq

arq = 'Bra_param.txt'

n = 2100-2017 !definindo o timestep

ALLOCATE (pop(n)); ALLOCATE (uso_ren(n)); ALLOCATE (uso_pet(n))     !alocando todos os arrays
ALLOCATE (pib(n)); ALLOCATE (pib_util(n)); ALLOCATE (gasto_pc(n)); 
ALLOCATE (consumo(n)); ALLOCATE (custo_bar(n)); ALLOCATE (balanca(n)); 
ALLOCATE (custo_ren(n)); ALLOCATE (ed_cie_tec(n)); ALLOCATE (custo_total(n));
ALLOCATE (porc_util(n)); ALLOCATE (porc_total(n)); ALLOCATE (custo_pet(n));
ALLOCATE (custo_barnew(n)); ALLOCATE (ed_cie_tecnew(n))

DO j=1, 3

	CALL setup(arq, pais, aux, pib(1), pib_eff, uso_pet(1), uso_ren(1), supdef, gasto_pc(1), &
	         custo_bar(1), rend_bar, prop_ren, custo_min, ed_cie_tec(1), ass_social, expo_to, &
	         expo_tec, expo_gasto, turn_pb, turn_petren, inc_bar, inc_asstec, inc_tecbal, &
	         inc_tecren, seed, pop, consumo(1), custo_pet(1), custo_ren(1), custo_total(1), & 
				pib_util(1), porc_total(1), porc_util(1), randpar, lim, crash, n, j)
END DO

CALL randomize(supdef, custo_bar(1), prop_ren, custo_min, ed_cie_tec(1), expo_to, expo_tec, &
         	expo_gasto, turn_pb, turn_petren, inc_bar, inc_asstec, inc_tecbal, inc_tecren,   &
	     		custo_barnew(1), prop_rennew, custo_minnew, ed_cie_tecnew(1), expo_tonew, expo_tecnew, &
        	 	expo_gastonew, turn_pbnew, turn_petrennew, inc_barnew, supdefnew, inc_asstecnew, & 
		   	inc_tecrennew, inc_tecbalnew, seed, randpar)

CALL novos_param(arqnew, pais, aux, pib(1), pib_eff, uso_pet(1), uso_ren(1), supdefnew, gasto_pc(1), &
            custo_barnew(1), rend_bar, prop_rennew, custo_minnew, ed_cie_tecnew(1), ass_social, expo_tonew, &
            expo_tecnew, expo_gastonew, turn_pbnew, turn_petrennew, inc_barnew, inc_asstecnew, & 
				inc_tecbalnew, inc_tecrennew, seed, randpar, lim, crash)


DO WHILE (cond .EQV. .FALSE.) 
!DO k=1, 2
	DO j=1, 3
		
		CALL setup(arqnew, pais, aux, pib(1), pib_eff, uso_pet(1), uso_ren(1), supdefnew, gasto_pc(1), &
	         custo_barnew(1), rend_bar, prop_rennew, custo_minnew, ed_cie_tecnew(1), ass_social, expo_tonew, &
	         expo_tecnew, expo_gastonew, turn_pbnew, turn_petrennew, inc_barnew, inc_asstecnew, inc_tecbalnew, &
	         inc_tecrennew, seed, pop, consumo(1), custo_pet(1), custo_ren(1), custo_total(1), & 
				pib_util(1), porc_total(1), porc_util(1), randpar, lim, crash, n, j)	   


		DO i=2,n

			IF (j == 1) THEN
				
				CALL trade_off(uso_pet(1), uso_pet(i), uso_ren(i), expo_tonew, turn_petrennew, i)

				CALL avanco_tec (ed_cie_tecnew(1), ed_cie_tecnew(i-1), ed_cie_tecnew(i), ass_social, balanca(i), &  
					custo_minnew, custo_ren(i-1), custo_ren(i), expo_tecnew, inc_asstecnew, inc_tecrennew, inc_tecbalnew, i)

			END IF

			CALL economia(pop(i), pib(i), pib_eff, pib_util(i), pib_util(i-1), gasto_pc(1), gasto_pc(i), & 
						 consumo(i), custo_barnew(i), custo_barnew(i-1), rend_bar, custo_pet(i), balanca(i),  &
						 uso_pet(i), uso_pet(i-1), expo_gastonew, supdefnew, turn_pbnew, inc_barnew, seed, i, j)

			CALL resultado(pib_util(i), pib(i), consumo(i), uso_pet(i), uso_ren(i), custo_pet(i), & 
							custo_ren(i), custo_total(i), porc_util(i), porc_total(i), i)

		END DO

		CALL escreve_arq(pop, pib, gasto_pc, consumo, uso_pet, uso_ren, custo_barnew, custo_pet,  &
					custo_ren, custo_total, porc_util, porc_total, balanca, ed_cie_tecnew, pais, j, n)

		IF (porc_total(crash-2017) <= lim(1) .AND. porc_total(crash-2017) >= lim(2) .AND. j == 3) THEN
			cond = .TRUE.
		END IF


	END DO

	IF (cond .EQV. .FALSE.) THEN
			
		seed = seed + 1	
			
		CALL randomize(supdef, custo_bar(1), prop_ren, custo_min, ed_cie_tec(1), expo_to, expo_tec, &
        	expo_gasto, turn_pb, turn_petren, inc_bar, inc_asstec, inc_tecbal, inc_tecren,   &
	     	custo_barnew(1), prop_rennew, custo_minnew, ed_cie_tecnew(1), expo_tonew, expo_tecnew, &
        		expo_gastonew, turn_pbnew, turn_petrennew, inc_barnew, supdefnew, inc_asstecnew, & 
		  	inc_tecrennew, inc_tecbalnew, seed, randpar)

		CALL novos_param(arqnew, pais, aux, pib(1), pib_eff, uso_pet(1), uso_ren(1), supdefnew, gasto_pc(1), &
           custo_barnew(1), rend_bar, prop_rennew, custo_minnew, ed_cie_tecnew(1), ass_social, expo_tonew, &
           expo_tecnew, expo_gastonew, turn_pbnew, turn_petrennew, inc_barnew, inc_asstecnew, & 
			  inc_tecbalnew, inc_tecrennew, seed, randpar, lim, crash)

	END IF

END DO

CONTAINS


!define acrescimo ou decrescimo na variacao dos parametros (+ ou -)

FUNCTION sinal(seed)
    
	 IMPLICIT NONE
	 INTEGER, INTENT (inout) :: seed
	 INTEGER :: sinal
    
	 CALL srand(seed)
	 IF (rand() >= 0.5) THEN
		sinal = 1
    ELSE
		sinal = -1
	 END IF
    RETURN

END FUNCTION

!usada para as flutuacoes no preço do barril de petroleo

SUBROUTINE flut_bar(random, flut)
    
	 IMPLICIT NONE
	 REAL*4, INTENT(inout) :: random, flut

		CALL RANDOM_NUMBER(random) 
		IF (random >= 0. .AND. random <=0.1) THEN
			flut = 0.05
		 ELSEIF (random >= 0.1 .AND. random <=0.2) THEN
			flut = -0.05
		 ELSEIF (random >= 0.2 .AND. random <=0.25) THEN
			flut = 0.08
		 ELSEIF (random >= 0.25 .AND. random <=0.3) THEN
			flut = -0.08
		 ELSEIF (random >= 0.3 .AND. random <=0.4) THEN
			flut = -0.12
		 ELSEIF (random >= 0.4 .AND. random <=0.5) THEN
			flut = 0.12
		 ELSEIF (random >= 0.5 .AND. random <=0.6) THEN
			flut = -0.15
		 ELSEIF (random >= 0.6 .AND. random <=0.7) THEN
			flut = 0.15
		 ELSEIF (random >= 0.7 .AND. random <=0.8) THEN
			flut = -0.18
		 ELSEIF (random >= 0.8 .AND. random <=0.9) THEN
			flut = 0.18
		 ELSEIF (random >= 0.9 .AND. random <=0.95) THEN
			flut = -0.2
		 ELSEIF (random >= 0.95 .AND. random <=1.) THEN
			flut = 0.2
		 END IF
 
END SUBROUTINE   

!le os parametros de entrada do arquivo fonte e constroi as condicoes iniciais da simulacao
 
SUBROUTINE setup(arq, pais, aux, pib, pib_eff, uso_pet, uso_ren, supdef, gasto_pc, &
            custo_bar, rend_bar, prop_ren, custo_min, ed_cie_tec, ass_social, expo_to, &
            expo_tec, expo_gasto, turn_pb, turn_petren, inc_bar, inc_asstec, inc_tecbal, inc_tecren, &
            seed, pop, consumo, custo_pet, custo_ren, custo_total, pib_util, porc_total, porc_util, &
				randpar, lim, crash, n, j)

	IMPLICIT NONE
	CHARACTER(15), INTENT(inout) :: arq
	CHARACTER(3), INTENT(inout) :: pais, aux
	INTEGER, INTENT(inout) :: seed, crash, n
	INTEGER, INTENT(in) :: j
	REAL*4, INTENT(inout) :: pib, pib_eff, uso_pet, uso_ren, gasto_pc, custo_bar, rend_bar, &
                         prop_ren, custo_min, ed_cie_tec, ass_social, expo_to, expo_tec, &
                         expo_gasto, turn_pb, turn_petren, inc_bar, consumo, custo_pet, & 
								 custo_ren, custo_total, pib_util, porc_total, porc_util, randpar

	REAL*4, DIMENSION(2), INTENT(inout) :: supdef, lim
	REAL*4, DIMENSION(5), INTENT(inout) :: inc_asstec, inc_tecren
	REAL*4, DIMENSION(6), INTENT(inout) :: inc_tecbal
	REAL*4, DIMENSION(n), INTENT(inout) :: pop
	TYPE mytype !necessario para ler o arquivo de entrada, que tem datatypes distintos
 		CHARACTER(15) :: paisx
 	 	REAL*4 :: pibx
	   REAL*4 :: pibeffx
	   REAL*4 :: usopetx
	   REAL*4 :: usorenx
	   REAL*4 :: sup
	   REAL*4 :: def
	   REAL*4 :: gastopcx
	   REAL*4 :: custobarx
	   REAL*4 :: rendbarx
	   REAL*4 :: proprenx
	   REAL*4 :: custominx
	   REAL*4 :: edcietecx
	   REAL*4 :: asssocialx
	   REAL*4 :: expotox
	   REAL*4 :: expotecx
	   REAL*4 :: expogastox
	   REAL*4 :: turnpbx
	   REAL*4 :: turnpetrenx
	   REAL*4 :: incbarx
	   REAL*4 :: asstec1
	   REAL*4 :: asstec2
	   REAL*4 :: asstec3
	   REAL*4 :: asstec4
	   REAL*4 :: asstec5
	   REAL*4 :: tecbal1
	   REAL*4 :: tecbal2
	   REAL*4 :: tecbal3
	   REAL*4 :: tecbal4
	   REAL*4 :: tecbal5
	   REAL*4 :: tecbal6
	   REAL*4 :: tecren1
	   REAL*4 :: tecren2
	   REAL*4 :: tecren3
	   REAL*4 :: tecren4
	   REAL*4 :: tecren5
		INTEGER :: semente
		REAL*4 ::  randparx
		REAL*4 ::  limsupx
		REAL*4 ::  liminfx
		INTEGER :: crashx
	ENDTYPE mytype

	TYPE(mytype) :: tudo

	OPEN (FILE = arq, UNIT = 1, STATUS = "old", ACTION = "read") !lendo arquivo c/ os parametros
		READ(1, *) tudo
		CLOSE(1)
	
	!a seguir a atribuicao `as variaveis, utilizando OOP

   pais            = tudo%paisx
	pib             = tudo%pibx
	pib_eff         = tudo%pibeffx
	uso_pet         = tudo%usopetx
	uso_ren         = tudo%usorenx
	supdef(1)       = tudo%sup
	supdef(2)       = tudo%def
	gasto_pc        = tudo%gastopcx
   custo_bar       = tudo%custobarx
	rend_bar        = tudo%rendbarx
	prop_ren        = tudo%proprenx
	custo_min       = tudo%custominx
	ed_cie_tec      = tudo%edcietecx
	ass_social      = tudo%asssocialx
	expo_to         = tudo%expotox
	expo_tec        = tudo%expotecx
	expo_gasto      = tudo%expogastox
	turn_pb         = tudo%turnpbx
	turn_petren     = tudo%turnpetrenx
	inc_bar			 =	tudo%incbarx
	inc_asstec(1)   = tudo%asstec1
	inc_asstec(2)   = tudo%asstec2
	inc_asstec(3)   = tudo%asstec3
	inc_asstec(4)   = tudo%asstec4
	inc_asstec(5)   = tudo%asstec5
   inc_tecbal(1)   = tudo%tecbal1
	inc_tecbal(2)   = tudo%tecbal2
	inc_tecbal(3)   = tudo%tecbal3
	inc_tecbal(4)   = tudo%tecbal4
	inc_tecbal(5)   = tudo%tecbal5
	inc_tecbal(6)   = tudo%tecbal6
	inc_tecren(1)   = tudo%tecren1
	inc_tecren(2)   = tudo%tecren2
	inc_tecren(3)   = tudo%tecren3
	inc_tecren(4)   = tudo%tecren4
	inc_tecren(5)   = tudo%tecren5
   seed            = tudo%semente
   randpar         = tudo%randparx
	lim(1)   		 = tudo%limsupx
	lim(2)          = tudo%liminfx
	crash           = tudo%crashx 

	IF (j == 1) THEN
		aux = "_lv"
   ELSEIF (j == 2) THEN
		aux = "_mv"
	ELSEIF (j == 3) THEN
		aux = "_hv"
	END IF

	OPEN (FILE = pais//aux//".txt", UNIT = 2, STATUS = "old", ACTION = "read")
		READ (2,*) pop !lendo o array c/ as projecoes da populacao ate 2100 	
		CLOSE(2)

   !fazendo o setup p/ t=0

	pop = pop * 1000 !o arquivo da ONU da' a populacao em milhoes
	pib_util = pib * pib_eff
	consumo = gasto_pc * pop(1)
	custo_pet = custo_bar * rend_bar
	custo_ren = prop_ren*custo_pet
	custo_total = (uso_pet * consumo * custo_pet) + (uso_ren * consumo * custo_ren)
	porc_util  = custo_total/pib_util
	porc_total  = custo_total/pib
	
END SUBROUTINE setup

! randomiza os parametros dentro de uma faixa dada por randpar

SUBROUTINE randomize (supdef, custo_bar, prop_ren, custo_min, ed_cie_tec, expo_to, expo_tec, &
            expo_gasto, turn_pb, turn_petren, inc_bar, inc_asstec, inc_tecbal, inc_tecren,   &
	         custo_barnew, prop_rennew, custo_minnew, ed_cie_tecnew, expo_tonew, expo_tecnew, &
            expo_gastonew, turn_pbnew, turn_petrennew, inc_barnew, supdefnew, inc_asstecnew, & 
				inc_tecrennew, inc_tecbalnew, seed, randpar)

	IMPLICIT NONE
	INTEGER, INTENT(inout) :: seed
	REAL*4, INTENT(inout) :: custo_bar, prop_ren, custo_min, ed_cie_tec, expo_to, expo_tec, &
                            expo_gasto, turn_pb, turn_petren, inc_bar, randpar

	REAL*4, DIMENSION(2), INTENT(inout) :: supdef
	REAL*4, DIMENSION(5), INTENT(inout) :: inc_asstec, inc_tecren
	REAL*4, DIMENSION(6), INTENT(inout) :: inc_tecbal
	REAL*4, INTENT(inout) :: custo_barnew, prop_rennew, custo_minnew, ed_cie_tecnew, expo_tonew, &
                          expo_tecnew, expo_gastonew, turn_pbnew, turn_petrennew, inc_barnew 

	REAL*4, DIMENSION(2), INTENT(inout) :: supdefnew
	REAL*4, DIMENSION(5), INTENT(inout) :: inc_asstecnew, inc_tecrennew
	REAL*4, DIMENSION(6), INTENT(inout) :: inc_tecbalnew

	REAL*4, DIMENSION(5) :: asstec_aux, tecren_aux
	REAL*4, DIMENSION(6) :: tecbal_aux
	INTEGER :: i, sig !determina se o parametro vai aumentar ou diminuir (1 ou -1)

	CALL srand(seed)
   sig = sinal(seed)
	supdefnew(1) = supdef(1) + sig * (supdef(1) * rand()) 
	sig = sinal(seed)
	supdefnew(2) = supdef(2) + sig * (supdef(2) * rand())
   sig = sinal(seed)
	custo_barnew = custo_bar + sig * (custo_bar * (rand() * randpar))
   sig = sinal(seed)
	prop_rennew = prop_ren + sig * (prop_ren * (rand() * randpar))
	!IF (prop_rennew < 1) THEN
	!	prop_rennew = 1.5
	!END IF
   sig = sinal(seed)
	custo_minnew = custo_min + sig * (custo_min * (rand() * randpar))
   sig = sinal(seed)
	ed_cie_tecnew = ed_cie_tec + sig * (ed_cie_tec * (rand() * randpar))
   sig = sinal(seed)
	expo_tonew = expo_to + sig * (expo_to * (rand() * randpar))
   sig = sinal(seed)
	expo_tecnew = expo_tec + sig * (expo_tec * (rand() * randpar))
   sig = sinal(seed)
	expo_gastonew = expo_gasto + sig * (expo_gasto * (rand() * randpar))
   sig = sinal(seed)
	turn_pbnew = turn_pb + sig * (turn_pb * (rand() * randpar))
   sig = sinal(seed)
	turn_petrennew = turn_petren + sig * (turn_petren * (rand() * randpar))
   sig = sinal(seed)
	inc_barnew = inc_bar + sig * (inc_bar * (rand() * randpar))
	IF (inc_barnew < 1) THEN
		inc_barnew = 1.01
	END IF
   sig = sinal(seed)
	inc_asstecnew(1) = inc_asstec(1) + sig * (inc_asstec(1) * (rand() * randpar)) 
	inc_asstecnew(2) = inc_asstec(2) + sig * (inc_asstec(2) * (rand() * randpar)) 
	inc_asstecnew(3) = inc_asstec(3) + sig * (inc_asstec(3) * (rand() * randpar)) 
	inc_asstecnew(4) = inc_asstec(4) + sig * (inc_asstec(4) * (rand() * randpar)) 
	inc_asstecnew(5) = inc_asstec(5) + sig * (inc_asstec(5) * (rand() * randpar))  
	
	DO i=1,5
		IF (inc_asstecnew(i) < 0) THEN
			inc_asstecnew(i) = inc_asstec(i)
		END IF
	END DO  

	DO i=1,5
		asstec_aux(6-i) = MAXVAL(inc_asstecnew)
		inc_asstecnew(MAXLOC(inc_asstecnew)) = 0
	END DO

	inc_asstecnew = asstec_aux 

   sig = sinal(seed)
	inc_tecrennew(1) = inc_tecren(1) + sig * (inc_tecren(1) * (rand() * randpar)) 
	inc_tecrennew(2) = inc_tecren(2) + sig * (inc_tecren(2) * (rand() * randpar)) 
	inc_tecrennew(3) = inc_tecren(3) + sig * (inc_tecren(3) * (rand() * randpar)) 
	inc_tecrennew(4) = inc_tecren(4) + sig * (inc_tecren(4) * (rand() * randpar)) 
	inc_tecrennew(5) = inc_tecren(5) + sig * (inc_tecren(5) * (rand() * randpar)) 

	DO i=1,5	
		IF (inc_tecrennew(i) > 1) THEN
			inc_tecrennew(i) = inc_tecren(i)
		END IF
	END DO  

	DO i=1,5
		tecren_aux(i) = MAXVAL(inc_tecrennew)
		inc_tecrennew(MAXLOC(inc_tecrennew)) = 0
	END DO

	inc_tecrennew = tecren_aux 

	sig = sinal(seed)
	inc_tecbalnew(1) = inc_tecbal(1) + sig * (inc_tecbal(1) * (rand() * randpar))
	inc_tecbalnew(2) = inc_tecbal(2) + sig * (inc_tecbal(2) * (rand() * randpar))
	inc_tecbalnew(3) = inc_tecbal(3) + sig * (inc_tecbal(3) * (rand() * randpar))
	inc_tecbalnew(4) = inc_tecbal(4) + sig * (inc_tecbal(4) * (rand() * randpar))
	inc_tecbalnew(5) = inc_tecbal(5) + sig * (inc_tecbal(5) * (rand() * randpar))
	inc_tecbalnew(6) = inc_tecbal(6) + sig * (inc_tecbal(6) * (rand() * randpar)) 

	
	DO i=1,6
		IF (inc_tecbalnew(i) < 0) THEN
			inc_tecbalnew(i) = inc_tecbal(i)
		END IF  
	END DO

	DO i=1,6
		tecbal_aux(7-i) = MAXVAL(inc_tecbalnew)
		inc_tecbalnew(MAXLOC(inc_tecbalnew)) = 0
	END DO

	inc_tecbalnew = tecbal_aux 

END SUBROUTINE randomize


! escreve os novos parametros em novo arquivo, p/ poder comparar com parametros iniciais

SUBROUTINE novos_param(arqnew, pais, aux, pib, pib_eff, uso_pet, uso_ren, supdef, gasto_pc, &
            custo_bar, rend_bar, prop_ren, custo_min, ed_cie_tec, ass_social, expo_to, &
            expo_tec, expo_gasto, turn_pb, turn_petren, inc_bar, inc_asstec, inc_tecbal, inc_tecren, &
            seed, randpar, lim, crash)

	IMPLICIT NONE
	CHARACTER(15), INTENT(inout) :: arqnew
	CHARACTER(3), INTENT(inout) :: pais,aux
	INTEGER, INTENT(inout) :: seed, crash
	REAL*4, INTENT(inout) :: pib, pib_eff, uso_pet, uso_ren, gasto_pc, custo_bar, rend_bar, &
                         prop_ren, custo_min, ed_cie_tec, ass_social, expo_to, expo_tec, &
                         expo_gasto, turn_pb, turn_petren, inc_bar, randpar

	REAL*4, DIMENSION(2), INTENT(inout) :: supdef, lim
	REAL*4, DIMENSION(5), INTENT(inout) :: inc_asstec, inc_tecren
	REAL*4, DIMENSION(6), INTENT(inout) :: inc_tecbal

	OPEN (FILE = pais//"_new.txt", UNIT = 20, STATUS = "replace", ACTION = "write") !lendo arquivo c/ os parametros
			
		WRITE(20, "(A)"), pais
		WRITE(20, "(F15.0)"), pib
		WRITE(20, "(F4.2)"), pib_eff  
		WRITE(20, "(F3.1)"), uso_pet
		WRITE(20, "(F3.1)"), uso_ren
		WRITE(20, "(F5.3, A, F6.3)"), supdef(1), ", ", supdef(2) 
		WRITE(20, "(F5.0)"), gasto_pc
		WRITE(20, "(F5.2)"), custo_bar 
		WRITE(20, "(F11.9)"), rend_bar
		WRITE(20, "(F4.2)"), prop_ren
		WRITE(20, "(F4.2)"), custo_min 
		WRITE(20, "(F5.1)"), ed_cie_tec
		WRITE(20, "(F3.1)"), ass_social
		WRITE(20, "(F6.3)"), expo_to 
		WRITE(20, "(F7.0)"), expo_tec
		WRITE(20, "(F5.3)"), expo_gasto
		WRITE(20, "(F4.2)"), turn_pb 
		WRITE(20, "(F4.2)"), turn_petren
		WRITE(20, "(F4.2)"), inc_bar
		WRITE(20, "(F5.3, A, F5.3, A, F5.3, A, F5.3, A, F5.3)"), inc_asstec(1), ", ", inc_asstec(2), ", ", inc_asstec(3), &
																			", ", inc_asstec(4), ", ", inc_asstec(5) 
		WRITE(20, "(F5.3, A, F5.3, A, F5.3, A, F5.3, A, F5.3, A, F5.3)"), inc_tecbal(1), ", ", inc_tecbal(2), ", ", &
														inc_tecbal(3), ", ", inc_tecbal(4), ", ", inc_tecbal(5), ", ", inc_tecbal(6)
		WRITE(20, "(F5.3, A, F5.3, A, F5.3, A, F5.3, A, F5.3)"), inc_tecren(1), ", ", inc_tecren(2), ", ", inc_tecren(3), ", ", &
																										 inc_tecren(4), ", ", inc_tecren(5)
		WRITE(20, "(I8)"), seed
		WRITE(20, "(F4.2)"), randpar
		WRITE(20, "(F8.5, A, F8.5)"), lim(1), ", ", lim(2)   
		WRITE(20, "(I4)"), crash  		

		CLOSE(20)
	
	arqnew = pais//"_new.txt"

END SUBROUTINE novos_param


!calcula a nova proporcao do uso de energias - fossil x renovavel

SUBROUTINE trade_off (pet_ini, pet, ren, expo, petmin, n)

	IMPLICIT NONE
	REAL*4, INTENT (inout) :: pet_ini, pet, ren
   REAL*4, INTENT (inout) :: expo, petmin
	INTEGER, INTENT (in) :: n
		
	pet = pet_ini * EXP(expo * n)
	ren = 1-pet
   IF (pet <= petmin) THEN  !a partir de certa fracao (baixa) o petroleo se esgota
		pet = 0
		ren = 1
	END IF

END SUBROUTINE

!calcula o novo parametro de tecnologia e ve as implicacoes disso nos parametros afetados por ele

SUBROUTINE avanco_tec (ed_cie_tec_ini, ed_cie_tec_ant, ed_cie_tec, ass_social, balanca, custo_min, & 
							 custo_ren_ant, custo_ren, expo, asstec, tecren, tecbal, n)

	IMPLICIT NONE
	REAL*4, INTENT (inout) :: ed_cie_tec_ini, ed_cie_tec_ant, ed_cie_tec, ass_social, balanca, custo_min, & 
									  custo_ren_ant, custo_ren
	REAL*4, INTENT (inout) :: expo	
   REAL*4, DIMENSION(5), INTENT (inout) :: asstec, tecren
   REAL*4, DIMENSION(6), INTENT (inout) :: tecbal
	INTEGER, INTENT (in) :: n

   ed_cie_tec = ed_cie_tec_ini * EXP(ed_cie_tec_ini/expo * n)
	IF (ass_social < 0.3) THEN
		ed_cie_tec = ed_cie_tec + asstec(1)* ed_cie_tec_ant
	ELSEIF (ass_social > 0.3 .AND. ass_social < 0.6) THEN
		ed_cie_tec = ed_cie_tec + asstec(2)* ed_cie_tec_ant
	ELSEIF (ass_social > 0.6 .AND. ass_social < 0.8) THEN
		ed_cie_tec = ed_cie_tec + asstec(3)* ed_cie_tec_ant
	ELSEIF (ass_social > 0.8 .AND. ass_social < 0.9) THEN
		ed_cie_tec = ed_cie_tec + asstec(4)* ed_cie_tec_ant
	ELSE
		ed_cie_tec = ed_cie_tec + asstec(5)* ed_cie_tec_ant
	END IF

	IF (ed_cie_tec < 300.) THEN
		balanca = 0.
	ELSEIF (ed_cie_tec > 300. .AND. ed_cie_tec < 500.) THEN
		balanca = tecbal(1)
		custo_ren = custo_ren_ant * 0.99
	ELSEIF (ed_cie_tec > 500. .AND. ed_cie_tec < 600.) THEN
		balanca = tecbal(2)
		custo_ren = custo_ren_ant * tecren(1)
	ELSEIF (ed_cie_tec > 600. .AND. ed_cie_tec < 700.) THEN
		balanca = tecbal(3)
		custo_ren = custo_ren_ant * tecren(2)
	ELSEIF (ed_cie_tec > 700. .AND. ed_cie_tec < 800.) THEN
		balanca = tecbal(4)
		custo_ren = custo_ren_ant * tecren(3)
	ELSEIF (ed_cie_tec > 800. .AND. ed_cie_tec < 900.) THEN
		balanca = tecbal(5)
		custo_ren = custo_ren_ant * tecren(4)
	ELSE
		balanca = tecbal(6)
		custo_ren = custo_ren_ant* tecren(5)
	END IF

	IF (custo_ren < custo_min) THEN
		custo_ren = custo_min !a partir de um certo ponto nao sera mais possivel
                            !baratear o uso de energias renovaveis
	END IF

END SUBROUTINE

!calcula os novos parametros relacionados `a economia

SUBROUTINE economia (pop, pib_total, pib_eff, pib_util, pib_ant, gasto_ini, gasto_pc, consumo, custo_bar, & 
							custo_bar_ant, rend_bar, custo_pet, balanca, pet, pet_ant, expo, supdef, barmin, inc, seed, n, j)

	IMPLICIT NONE
	REAL*4, INTENT(inout) :: pop, pib_util, pib_total, pib_eff, pib_ant, gasto_ini, gasto_pc, consumo, custo_bar, &
									 custo_bar_ant, rend_bar, custo_pet, balanca, pet, pet_ant
   REAL*4, INTENT(inout) :: expo, barmin, inc
   REAL*4, DIMENSION(2), INTENT(inout) :: supdef
	INTEGER, INTENT(in) :: n, j
	INTEGER, INTENT(inout) :: seed
	REAL*4 :: random, flut

	IF (n == seed) THEN
		CALL RANDOM_SEED()
	END IF
	
	gasto_pc = gasto_ini * EXP(expo * n)
	consumo = pop * gasto_pc
   
	IF (j == 1) THEN
		
		CALL RANDOM_NUMBER(random) 
   	balanca = balanca + (random * (supdef(1)-supdef(2))) + supdef(2) !aleatorio entre sup e def
		pib_util = pib_ant + (pib_ant * balanca) 
		pib_total = pib_util/(pib_eff)

		IF (pet >= barmin) THEN
			CALL flut_bar(random, flut)
			custo_bar = custo_bar_ant * (inc*pet_ant/pet) + flut * custo_bar_ant
		ELSE 
			custo_bar = custo_bar_ant !a partir de um certo ponto o custo do petroleo ficara' fixo
		END IF
	
		custo_pet = custo_bar * rend_bar

	END IF

END SUBROUTINE

!relaciona o consumo de energia com o pib, prevendo colapso, aumento ou diminuicao do gasto necessario

SUBROUTINE resultado (pib_util, pib_total, consumo, pet, ren, custo_pet, & 
							 custo_ren, custo_total, porc_util, porc_total, n)

	IMPLICIT NONE
	REAL*4, INTENT (inout) :: pib_util, pib_total, consumo, pet, ren, custo_pet, & 
							        custo_ren, custo_total, porc_util, porc_total
	INTEGER, INTENT (in) :: n

	custo_total = (pet * consumo * custo_pet) + (ren * consumo * custo_ren) 
	porc_util = custo_total/pib_util
	porc_total = custo_total/pib_total

END SUBROUTINE

!gera o arquivo de saida com os resultados

SUBROUTINE escreve_arq (pop, pib, gasto_pc, consumo, uso_pet, uso_ren, custo_bar, custo_pet, custo_ren, &
					        custo_total, porc_util, porc_total, balanca, ed_cie_tec, pais, j, n)

	IMPLICIT NONE
	INTEGER, INTENT (inout) :: n
	INTEGER, INTENT (in) :: j
   CHARACTER(3), INTENT(inout) :: pais
	REAL*4, DIMENSION(n), INTENT (inout) :: pop, pib, gasto_pc, consumo, uso_pet, uso_ren, custo_bar,   &
					              custo_pet, custo_ren, custo_total, porc_util, porc_total, balanca, ed_cie_tec
   CHARACTER(3) :: aux
	INTEGER :: i

	IF (j == 1) THEN
		aux = "_lv"
   ELSEIF (j == 2) THEN
		aux = "_mv"
	ELSEIF (j == 3) THEN
		aux = "_hv"
	END IF

	OPEN (FILE = "Out_"//pais//aux//".txt", UNIT = 3, STATUS = "replace", ACTION = "write")
		WRITE (3, "(A, A, /)") " Ano        Pop         PIB     Gst pc   Cons      Bal    Tec   Pet    Ren", &  
								  "      Cst_bar  Cst_pet   Cst_ren    Cst_tot     % util    % total"
		DO i=1,n
	
			WRITE(3, "(I5, I13, F12.3, F8.1, F8.1, F9.3, I6, F7.2, F7.2, F11.2, F8.2, F10.2, F12.2, F11.2, F11.2)"), &
  		             (i+2017), INT(pop(i)), (pib(i)/10E9), gasto_pc(i), (consumo(i)/10E9), (balanca(i)*100), &
                   INT(ed_cie_tec(i)), uso_pet(i), uso_ren(i), custo_bar(i), custo_pet(i), &
						 custo_ren(i), (custo_total(i)/10E9), (porc_util(i)*100), (porc_total(i)*100)
		END DO
	
	CLOSE(3)
END SUBROUTINE

END PROGRAM energias_paralel
