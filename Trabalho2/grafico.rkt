#lang racket

(require examples)

;; --------------------------------------
;; Disciplina: Programação Funcional
;; Professor: Marco Aurélio Lopes Barbosa
;; --------------------------------------

;; =============================================
;; Nome: Gabriel Balancieri Perassoli RA: 135854
;; =============================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct registro (tipo quantidade) #:transparent)
;; Registro representa um tipo de registro e sua quantidade
;; tipo : String - qual é o tipo de registro
;; quantidade : Número - a quantidade de registros daquele tipo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Processar uma lista de strings e convertê-las em uma lista de registros.  A função processa-entrada divide cada linha
;; em tipo e quantidade, criando registros correspondentes a partir dessas partes.

;; Tipo de Dados
;; Linhas da tabela é uma lista de strings, onde cada string contém um tipo e uma quantidade de registros.

;; Especificação
;; List<String> -> List<Registro>
;; Processa uma lista de strings da tabela, onde cada string contém um tipo e uma quantidade de registros,
;; e retorna uma lista de instâncias da estrutura registro. Cada string é dividida em tipo e quantidade para criar registros.

;; Exemplos de uso
(examples
  (check-equal? (processa-entrada '("Uva 43" "Chocolate 15" "Morango 25"))
                (list (registro "Uva" 43) (registro "Chocolate" 15) (registro "Morango" 25)))
  (check-equal? (processa-entrada '("Baunilha 10" "Menta 5"))
                (list (registro "Baunilha" 10) (registro "Menta" 5)))
  (check-equal? (processa-entrada '("Amora 7"))
                (list (registro "Amora" 7)))
  (check-equal? (processa-entrada empty) empty))

(define (processa-entrada linhas)
  (cond
    [(empty? linhas) empty]
    [else 
     (define partes (string-split (first linhas)))
     (cons (registro (first partes) (string->number (second partes)))
           (processa-entrada (rest linhas)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Determinar a soma total de todos os elementos da coluna registro-quantidade da tabela.
;;
;; Tipo de Dados
;; Tabela é o resultado da função processa-entrada que é do tipo List<Registro>
;;
;; Especificação
;; List<Registro> -> NúmeroNatural
;; Determina a soma total de todas as quantidades de registros estrutura, tabela é um List<Registro> do tipo struct, logo registro-quantidade
;; são as quantidades de cada tipo de registro.

;; Exemplos de uso
(examples
  (check-equal? (total-registros empty) 0)
  (check-equal? (total-registros (list (registro "Baunilha" 10))) 10)
  (check-equal? (total-registros (list (registro "Baunilha" 10) (registro "Menta" 25))) 35)
  (check-equal? (total-registros (list (registro "Baunilha" 0) (registro "Menta" 100))) 100))

(define (total-registros tabela)
  (cond
    [(empty? tabela) 0]
    [else (+ (registro-quantidade (first tabela))
             (total-registros (rest tabela)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Determinar a porcentagem de um único registro.
;;
;; Tipo de Dados
;; Registro é a estrutura de entrada para obter a quantidade, tendo dois campos tipo e quantidade.
;; Total é um número inteiro positivo
;;
;; Especificação
;; Registro NúmeroNatural -> Número
;; Determina a porcentagem de um único registro, dada a estrutura registro-quantidade e o total, podemos calcular a porcentagem de um único registro.
;; Fazendo assim, ((registro-quantidade * 100) / total).

;; Exemplos de uso
(examples
  (check-equal? (calcular-porcentagem (registro "Baunilha" 10) 127) 8)
  (check-equal? (calcular-porcentagem (registro "Menta" 5) 110) 5)
  (check-equal? (calcular-porcentagem (registro "Chocolate" 25) 100) 25)
  (check-equal? (calcular-porcentagem (registro "Morango" 0) 100) 0)
  (check-equal? (calcular-porcentagem (registro "Uva" 15) 0) 0))

(define (calcular-porcentagem registro total)
  (if (zero? total)
      0
      (round (/ (* 100 (registro-quantidade registro)) total))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Determinar a lógica para decidir quantas barras devem ser geradas em uma linha.
;;
;; Tipo de Dados
;; Porcentagem é um número positivo inteiro
;;
;; Especificação
;; Porcentagem -> NúmeroNatural
;; Determina uma lógica para decidir a quantidade de barras que devem ser geradas para uma linha da tabela por exemplo "===" ou "=======". A lógica foi definida da
;; seguinte maneira, ((porcentagem / 10) * 3), essa é a quantidade de barras que devem ser geradas. E deve ser arredondada usando o ceiling.

;; Exemplos de uso
(examples
  (check-equal? (gerar-barra 0) "")
  (check-equal? (gerar-barra 8) "===")
  (check-equal? (gerar-barra 25) "=========")
  (check-equal? (gerar-barra 55) "==================")
  (check-equal? (gerar-barra 100) "=============================="))

(define (gerar-barra porcentagem)
  (make-string (* (ceiling (/ porcentagem 10)) 3) #\=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Descobrir qual é o maior tamanho de string da estrutura registro-tipo da tabela.
;;
;; Tipo de Dados
;; Tabela é o resultado da função processa-entrada que é do tipo List<Registro> 
;;
;; Especificação
;; List<Registro> -> NúmeroNatural
;; Descobre qual é o maior tamanho de string das linhas da estrutura registro-tipo da tabela. A função max-tam-tipo recebe uma lista
;; de registros e retorna o comprimento máximo das strings no campo registro-tipo. A função examina todos os registros na lista para encontrar
;; o comprimento máximo das strings.

;; Exemplos de uso
(examples
  (check-equal? (max-tam-tipo empty) 0)
  (check-equal? (max-tam-tipo (list (registro "Uva" 43) (registro "Groselha" 100) (registro "Limão" 40) (registro "Alho" 0))) 8)
  (check-equal? (max-tam-tipo (list (registro "Uva" 10)(registro "Laranja" 20))) 7)
  (check-equal? (max-tam-tipo (list (registro "Abacaxi" 5))) 7))

(define (max-tam-tipo tabela)
  (cond
    [(empty? tabela) 0]
    [else (max (string-length (registro-tipo (first tabela)))
               (max-tam-tipo (rest tabela)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Análise
;; Gerar uma representação visual de uma barra de gráfico, adiciona colchetes e um espaço ao redor da barra para concatenar a saída.
;;
;; Tipo de Dados
;; Registro é a estrutura de entrada, tendo dois campos tipo e quantidade 
;; Total é um número inteiro positivo
;;
;; Especificação
;; Registro Total -> String
;; Gera uma representação visual de uma barra de gráfico, logo recebe um registro e o total de registros, calcula a porcentagem do registro em relação ao total,
;; gerando a barra correspondente e retornando uma string concatenada que inclui a barra entre colchetes, seguida de um espaço.

;; Exemplos de uso
(examples
  (check-equal? (pega-barra (registro "Uva" 43) 183) " [=========] ")
  (check-equal? (pega-barra (registro "Chocolate" 15) 100) " [======] ")
  (check-equal? (pega-barra (registro "Morango" 25) 100) " [=========] ")
  (check-equal? (pega-barra (registro "Menta" 5) 20) " [=========] ")
  (check-equal? (pega-barra (registro "Baunilha" 0) 100) " [] "))

(define (pega-barra registro total)
  (string-append " [" (gerar-barra (calcular-porcentagem registro total)) "] "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Calcular o comprimento máximo da string que representa a barra de gráfico para todos os registros na tabela.
;;
;; Tipo de Dados
;; Tabela é o resultado da função processa-entrada que é do tipo List<Registro> 
;; Total é um número inteiro positivo
;;
;; Especificação
;; List<Registro> Número -> NúmeroNatural
;; Para cada registro na tabela, gera a barra correspondente utilizando a função pega-barra e retorna o comprimento máximo da string da barra entre todos os registros.

;; Exemplos de uso
(examples
  (check-equal? (max-tam-barra (list (registro "Uva" 43) (registro "Groselha" 100) (registro "Limão" 40) (registro "Alho" 0)) 183) 22)
  (check-equal? (max-tam-barra (list (registro "Uva" 50) (registro "Laranja" 30)) 80) 25)
  (check-equal? (max-tam-barra (list (registro "Abacaxi" 20)) 20) 34)
  (check-equal? (max-tam-barra empty 100) 0))

(define (max-tam-barra tabela total)
  (cond
    [(empty? tabela) 0]
    [else (max (string-length (pega-barra (first tabela) total))
               (max-tam-barra (rest tabela) total))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        
;; Análise
;; Constuir uma linha da tabela gerando um gráfico, concatenando o tipo do registro, espacos, a quantidade de barras, espacos da barra e a porcentagem.
;;
;; Tipo de Dados
;; Registro é a estrutura de entrada, tendo dois campos tipo e quantidade 
;; Total é um número inteiro positivo
;; Max-Tam-Tipo é um número inteiro positivo
;; Max-Tam-Barra é um número inteiro positivo
;;
;; Especificação
;; Registro Total Max-Tam-Tipo Max-Tam-Barra -> Linha do gráfico
;; Constroi uma linha do gráfico a partir da tabela, total, max-tam-tipo e max-tam-barra, concatenando o tipo do registro, espacos do tipo, quantidade de barras,
;; espacos da barra e porcentagem.

;; Exemplos de uso
(examples
  (check-equal? (linha-grafico (registro "Alho" 0) 183 8 19) "Alho     []                0%")
  (check-equal? (linha-grafico (registro "Menta" 100) 183 5 19) "Menta [==================] 55%")
  (check-equal? (linha-grafico (registro "Chocolate" 40) 183 9 19) "Chocolate [=========]       22%")
  (check-equal? (linha-grafico (registro "Morango" 0) 183 7 19) "Morango []                0%"))

(define (linha-grafico registro total max-tam-tipo max-tam-barra)
  (define porcentagem (calcular-porcentagem registro total))
  (define barra (gerar-barra porcentagem))
  (define tipo-length (string-length (registro-tipo registro)))
  (define tipo-espacos (make-string (max 0 (- max-tam-tipo tipo-length)) #\space))
  (define barras-append (string-append " [" barra "] "))
  (define barras-length (string-length barras-append))
  (define barras-espacos (make-string (max 0 (- max-tam-barra barras-length)) #\space))
  (string-append (registro-tipo registro)
                 tipo-espacos
                 barras-append
                 barras-espacos
                 (number->string porcentagem)
                 "%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Processar todas as linhas da tabela a partir da função linha-grafico, fazendo imprimir recursivamente todas as linhas e seus respectivos dados.
;;
;; Tipo de Dados
;; Tabela é o resultado da função processa-entrada que é do tipo List<Registro> 
;; Total é um número inteiro positivo
;; Max-Tam-Tipo é um número inteiro positivo
;; Max-Tam-Barra é um número inteiro positivo
;;
;; Especificação
;; Tabela Total Max-Tam-Tipo Max-Tam-Barra -> Linhas do gráfico
;; Processa todas as linhas da tabela a partir da função linha-grafico, fazendo que seja gerado recursivamente todas as linhas do gráfico com seus
;; respectivos dados, como o tipo, maior tamanho do tipo, quantidade de barras, maior tamanho das barras e porcentagem.

;; Exemplos de uso
(examples
  (check-equal? (gerar-linhas (list (registro "Uva" 43) (registro "Groselha" 100) (registro "Limão" 40) (registro "Alho" 0)) 183 8 19)
                (list "Uva      [=========]       23%"
                      "Groselha [==================] 55%"
                      "Limão    [=========]       22%"
                      "Alho     []                0%"))
  (check-equal? (gerar-linhas (list (registro "Uva" 50) (registro "Laranja" 30)) 80 7 19)
                (list "Uva     [=====================] 62%"
                      "Laranja [============]    38%"))
  (check-equal? (gerar-linhas (list (registro "Abacaxi" 20)) 20 7 19)
                (list "Abacaxi [==============================] 100%"))
  (check-equal? (gerar-linhas empty 100 0 19)
                empty))

(define (gerar-linhas tabela total max-tam-tipo max-tam-barra)
  (cond
    [(empty? tabela) empty]
    [else (cons (linha-grafico (first tabela) total max-tam-tipo max-tam-barra)
                (gerar-linhas (rest tabela) total max-tam-tipo max-tam-barra))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Determinar o comprimento máximo das barras no gráfico de barras. A função calcular-max-tam-barra chama a função max-tam-barra,
;; que realiza o cálculo do comprimento da barra  com base nos registros da tabela e no total de registros.
;;
;; Tipo de Dados
;; Tabela é o resultado da função processa-entrada que é do tipo List<Registro> 
;; Total é um número inteiro positivo
;;
;; Especificação
;; List<Registro> Total -> NúmeroNatural
;; Determina o comprimento máximo da string da barra de gráfico, considerando os registros fornecidos e o total.  A função calcular-max-tam-barra chama
;; max-tam-barra, que calcula o comprimento máximo necessário para a barra do gráfico com base na porcentagem dos registros em relação ao total. 
;; O comprimento máximo da barra é usado para garantir que todas as barras do gráfico sejam exibidas proporcionalmente e de forma consistente.

;; Exemplos de uso
(examples
  (check-equal? (calcular-max-tam-barra (list (registro "Uva" 43) (registro "Groselha" 100)) 183) 22)
  (check-equal? (calcular-max-tam-barra (list (registro "Uva" 50) (registro "Laranja" 30)) 80) 25)
  (check-equal? (calcular-max-tam-barra (list (registro "Abacaxi" 20)) 20) 34)
  (check-equal? (calcular-max-tam-barra empty 100) 0))

(define (calcular-max-tam-barra tabela total)
  (max-tam-barra tabela total))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Análise
;; Gerar o gráfico de barras para uma tabela de registros a partir da função grafico-barras.
;; Calcular o total de registros, o maior comprimento dos tipos e o comprimento máximo das barras.
;; Devolver uma lista de linhas do gráfico.
;;
;; Tipo de Dados
;; Tabela é o resultado da função processa-entrada que é do tipo List<Registro>
;;
;; Especificação
;; List<Registro> -> List<String>
;; Gera um gráfico de barras a partir de uma tabela de registros. Calcula o total de registros, o maior comprimento dos tipos
;; e o comprimento máximo das barras. Gera uma linha para cada registro com o nome, a barra gráfica e a porcentagem, retornando uma lista de strings.

;; Exemplos de uso
(examples
  (check-equal? (grafico-barras (list (registro "Uva" 43) (registro "Groselha" 100) (registro "Limão" 40) (registro "Alho" 0)))
                (list "Uva      [=========]          23%"
                      "Groselha [==================] 55%"
                      "Limão    [=========]          22%"
                      "Alho     []                   0%"))
  (check-equal? (grafico-barras (list (registro "Uva" 50) (registro "Laranja" 30)))
                (list "Uva     [=====================] 62%"
                      "Laranja [============]          38%"))
  (check-equal? (grafico-barras (list (registro "Abacaxi" 20)))
                (list "Abacaxi [==============================] 100%"))
  (check-equal? (grafico-barras empty)
                empty))

(define (grafico-barras tabela)
  (define total (total-registros tabela))
  (define max-tam (max-tam-tipo tabela))
  (define max-tam-barra (calcular-max-tam-barra tabela total))
  (cond
    [(zero? total) empty]
    [else (gerar-linhas tabela total max-tam max-tam-barra)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Função principal para executar todas as funções do programa
(define (main)
  (define entrada (port->lines (open-input-file "tabela.txt")))
  (define tabela (processa-entrada entrada))
  (define saida (grafico-barras tabela))
  (display-lines saida))

(main)
