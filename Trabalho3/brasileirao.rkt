#lang racket

(require examples)

;; --------------------------------------
;; Disciplina: Programação Funcional
;; Professor: Marco Aurélio Lopes Barbosa
;; --------------------------------------

;; =============================================
;; Nome: Gabriel Balancieri Perassoli RA: 135854
;; =============================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct desempenho (time pontos vitorias saldo) #:transparent)
;; Desempenho representa o desempenho que um time teve.
;; time: representa o nome do time
;; pontos: representa a pontuação do time
;; vitorias: representa o número de partidas vencidas
;; saldo: representar o saldo de gols do time

(struct resultado (anfitriao gols-a visitante gols-v) #:transparent)
;; Resultado representa o resultado de uma partida de um time.
;; anfitriao: representa o nome do time da casa
;; gols-a: representa o número de gols feito pelo anfitrião
;; visitante: representa o nome do time de fora
;; gols-v: representa o número de gols feito pelo visitante

;; Análise
;; Processar uma linha representando o resultado de uma partida e retorna uma instância da struct resultado.
;; 
;; Tipos de Dados
;; linha é do tipo string
;;
;; Especificação
;; String -> Struct<resultado>
;; Processa a linha que representa o resultado de uma partida e retorna uma instância da struct resultado.

(examples
 (check-equal? (processa-linha "Palmeiras 2 Corintias 1") (resultado "Palmeiras" 2 "Corintias" 1))
 (check-equal? (processa-linha "Santos 0 Coritiba 1") (resultado "Santos" 0 "Coritiba" 1)))

(define (processa-linha linha)
  (define partes (string-split linha))
  (resultado (first partes)
             (string->number (second partes))
             (third partes)
             (string->number (fourth partes))))

;; Análise
;; Processar todas as linhas que representa os resultados das partidas e retorna instâncias da struct resultado.
;;
;; Tipos de Dados
;; linhas é do tipo List<String>
;;
;; Especificação
;; List<String> -> List<resultado>
;; Processa todas as linhas da lista que representa todos os resultados das partidas e retorna as instâncias da struct resultado.

(examples
 (check-equal? (processa-entrada (list "Palmeiras 2 Corintias 1" "Avai 1 Corintias 0" "CRB 4 Sport 1"))
               (list (resultado "Palmeiras" 2 "Corintias" 1)
                     (resultado "Avai" 1 "Corintias" 0)
                     (resultado "CRB" 4 "Sport" 1)))
 (check-equal? (processa-entrada (list "Santos 0 Coritiba 1"))
               (list (resultado "Santos" 0 "Coritiba" 1))))

(define (processa-entrada linhas)
  (map processa-linha linhas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Descobrir os nomes dos times

;; Análise
;; Determinar se um elemento é igual ao outro.
;;
;; Tipos de Dados
;; x pode ser qualquer tipo de dado.
;;
;; Especificação
;; x -> Boolean
;; Determina se um elemento é igual ao outro comparando um valor com x e retorna #t se forem iguais, #f caso contrário.

(examples
  (check-equal? ((igual? 5) 5) #t)
  (check-equal? ((igual? 5) 3) #f)
  (check-equal? ((igual? "Olá") "Olá") #t)
  (check-equal? ((igual? "Olá") "Adeus") #f))

(define (igual? x)
  (define (comparar elem)
    (equal? x elem))
  comparar)

;; Análise
;; Verificar se determinado elemento ou item está em lst.
;;
;; Tipos de Dados
;; x é um elemento
;; lst é uma lista de elementos
;;
;; Especificação
;; x List<elemento> -> Boolen
;; Verifica se um determinado x (elemento ou item) está em lst, se estiver deve produzir #t, caso contrário produz #f.

(examples
 (check-equal? (na-lista? 5 (list 5 10 2 50)) #t)
 (check-equal? (na-lista? 3 (list 28 10 0 1 99 100)) #f)
 (check-equal? (na-lista? 1 (list 1)) #t))

(define (na-lista? x lst)
  (not (empty? (filter (igual? x) lst))))

;; Análise
;; Determinar se em lst tem elementos repetidos, se repetem os elementos são removidos.
;;
;; Tipos de Dados
;; lst é uma lista de elementos.
;;
;; Especificação
;; List<Elemento> -> List<Elemento>
;; Determina se em lst possui elementos repetidos, se tiver, remova-os para assim criar uma lista sem elementos repetidos.

(examples
 (check-equal? (remove-duplicada (list 5 1 10 15 1 1 9)) (list 5 1 10 15 9))
 (check-equal? (remove-duplicada (list 2 2 2 2)) (list 2))
 (check-equal? (remove-duplicada (list)) (list))
 (check-equal? (remove-duplicada (list 1 2 3 4)) (list 1 2 3 4)))

(define (remove-duplicada lst0)
  (define (iter lst acc)
    (cond
      [(empty? lst) acc]
      [(na-lista? (first lst) acc) (iter (rest lst) acc)]
      [else (iter (rest lst) (append acc (list (first lst))))]))
  (iter lst0 empty))

;; Análise
;; Descobrir os times presentes em uma lista de resultados.
;;
;; Tipos de Dados
;; resultados é uma lista com a estrutura resultado.
;;
;; Especificação
;; List<resultados> -> List<times>
;; Determina os times únicos que participam dos jogos, considere tanto os times anfitriões quanto os times visitantes,
;; removendo as duplicatas para garantir que cada time apareça apenas uma vez.

(examples
 (check-equal? (descobrir-times
                (list (resultado "Time A" 2 "Time B" 1)
                      (resultado "Time C" 0 "Time A" 3)
                      (resultado "Time B" 1 "Time C" 2)
                      (resultado "Time D" 2 "Time A" 2)))
               (list "Time A" "Time C" "Time B" "Time D")))

(define (descobrir-times resultados)
  (remove-duplicada
   (append (map resultado-anfitriao resultados)
           (map resultado-visitante resultados))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Calcular os pontos, número de vitórias e saldo de gols de um time por vez

;; Análise
;; Determinar se duas strings são iguais.
;;
;; Tipos de Dados
;; s1 é uma string
;; s2 é uma string
;;
;; Especificação
;; Determina se duas string são iguais, produzindo #t se for, caso contrário #f.

(examples
 (check-equal? (strings-iguais? "Ola" "Ola") #t)
 (check-equal? (strings-iguais? "Sim" "nao") #f)
 (check-equal? (strings-iguais?  "Pois e" "Pois e") #t))

(define (strings-iguais? s1 s2)
  (equal? (string->list s1) (string->list s2)))

;; Análise
;; Calcular o número de pontos dado um determinado time.
;;
;; Tipos de Dados
;; resultados é uma lista de estruturas resultado
;; time é uma string
;;
;; Especificação
;; List<resultados> time -> NúmeroInteiro
;; Calcular o número de pontos de um time dado um lista de resultados, ao final é gerado todos os pontos que aquele time
;; pontuou em diversas partidas.

(examples
 (check-equal? (calc-pontos (list (resultado "Time A" 2 "Time B" 1)
                                  (resultado "Time C" 0 "Time A" 3)
                                  (resultado "Time B" 1 "Time C" 2)
                                  (resultado "Time D" 2 "Time A" 2)) "Time A") 7)
 (check-equal? (calc-pontos (list (resultado "Time A" 1 "Time B" 0)
                                  (resultado "Time B" 2 "Time C" 1)) "Time B") 3))

;; Definição com o uso de acumulador usando foldl.
(define (calc-pontos resultados time)
  ;; acc - a soma dos pontos dos resultados, dado o time
  (define (iter r acc)
    (cond
      [(strings-iguais? (resultado-anfitriao r) time)
       (cond
         [(> (resultado-gols-a r) (resultado-gols-v r)) (+ acc 3)]
         [(= (resultado-gols-a r) (resultado-gols-v r)) (+ acc 1)]
         [else acc])]
      [(strings-iguais? (resultado-visitante r) time)
       (cond
         [(> (resultado-gols-v r) (resultado-gols-a r)) (+ acc 3)]
         [(= (resultado-gols-v r) (resultado-gols-a r)) (+ acc 1)]
         [else acc])]
      [else acc]))
  (foldl iter 0 resultados))

;; Análise
;; Determinar a quantidade vitórias que um time obteve a partir de uma lista de resultados.
;;
;; Tipos de Dados
;; resultado é uma lista de estruturas resultados
;; time é uma string
;;
;; Especificação
;; List<resultados> time -> NúmeroInteiro
;; Determina a quantidade de vitórias que um time obteve a partir de um lista de resultados, gerando assim
;; o número de vitórias.

(examples
 (check-equal? (calc-vitorias (list (resultado "Time A" 2 "Time B" 1)
                                    (resultado "Time C" 0 "Time A" 3)
                                    (resultado "Time B" 1 "Time C" 2)
                                    (resultado "Time D" 2 "Time A" 2)) "Time A") 2)
 (check-equal? (calc-vitorias (list (resultado "Time A" 1 "Time B" 0)
                                    (resultado "Time B" 2 "Time C" 1)) "Time B") 1))

;; Definição com o uso de acumuladores usando foldl
(define (calc-vitorias resultados time)
  ;; acc - é a soma de vitórias que um determinado time obteve.
  (define (iter r acc)
    (cond
      [(strings-iguais? (resultado-anfitriao r) time)
       (cond
         [(> (resultado-gols-a r) (resultado-gols-v r)) (+ acc 1)]
         [(= (resultado-gols-a r) (resultado-gols-v r)) (+ acc 0)]
         [else acc])]
      [(strings-iguais? (resultado-visitante r) time)
       (cond
         [(> (resultado-gols-v r) (resultado-gols-a r)) (+ acc 1)]
         [(= (resultado-gols-v r) (resultado-gols-a r)) (+ acc 0)]
         [else acc])]
      [else acc]))
  (foldl iter 0 resultados))

;; Análise
;; Determinar o número de saldo de gols de um time, dada uma lista de resultados.
;;
;; Tipos de Dados
;; resultados é uma lista de estruturas resultado
;; time é uma string
;;
;; Especificação
;; List<resultados> time -> NúmeroInteiro
;; Determina o saldo de gols de um determinado time, dada uma lista de resultados, o saldo de gols é calculado da seguinte
;; maneira, resultado-gols-a - resultado-gols-v ou vice-versa, assim produzindo o número total de saldo.

(examples
 (check-equal? (calc-saldo (list (resultado "Time A" 2 "Time B" 1)
                                 (resultado "Time C" 0 "Time A" 3)
                                 (resultado "Time B" 1 "Time C" 2)
                                 (resultado "Time D" 2 "Time A" 2)) "Time A") 4)
 (check-equal? (calc-saldo (list (resultado "Time A" 1 "Time B" 0)
                                 (resultado "Time B" 2 "Time C" 1)) "Time B") 0))

;; Definição com o uso de acumulador usando foldl.
(define (calc-saldo resultados time)
  ;; acc - o saldo de gols de resultados, dado o time.
  (define (iter r acc)
    (cond
      [(strings-iguais? (resultado-anfitriao r) time)
       (+ acc (- (resultado-gols-a r) (resultado-gols-v r)))]
      [(strings-iguais? (resultado-visitante r) time)
       (+ acc (- (resultado-gols-v r) (resultado-gols-a r)))]
      [else acc]))
  (foldl iter 0 resultados))

;; Análise
;; Calcular o desempenho total de todos os times a partir de uma lista de resultados.
;;
;; Tipos de Dados
;; resultados é uma lista de estruturas resultado
;;
;; Especificação
;; List<resultado> -> List<desempenho>
;; Calcular o desempenho total de todos os times a partir de uma lista de resultados. Recebe uma lista de resultados
;; e produz uma lista com o desempenho de cada time participante, considerando todos os jogos em que o time esteve envolvido.

(examples
 (check-equal? (desempenho-total
                 (list (resultado "Time A" 2 "Time B" 1)
                       (resultado "Time C" 0 "Time A" 3)
                       (resultado "Time B" 1 "Time C" 2)
                       (resultado "Time D" 2 "Time A" 2)))
               (list (desempenho "Time A" 7 2 4) 
                     (desempenho "Time C" 3 1 -2) 
                     (desempenho "Time D" 1 0 0) 
                     (desempenho "Time B" 0 0 -2)))
 (check-equal? (desempenho-total
                 (list (resultado "Time X" 1 "Time Y" 1)
                       (resultado "Time Z" 2 "Time X" 1)
                       (resultado "Time Y" 0 "Time Z" 3)))
               (list (desempenho "Time Z" 6 2 4)
                     (desempenho "Time X" 1 0 -1) 
                     (desempenho "Time Y" 1 0 -3)))
 (check-equal? (desempenho-total
                 (list (resultado "Team1" 3 "Team2" 2)
                       (resultado "Team2" 1 "Team3" 0)
                       (resultado "Team1" 2 "Team3" 1)))
               (list (desempenho "Team1" 6 2 2) 
                     (desempenho "Team2" 3 1 0)
                     (desempenho "Team3" 0 0 -2))))

(define (desempenho-total resultados)
  (define (iter time)
    (desempenho time
                (calc-pontos resultados time)
                (calc-vitorias resultados time)
                (calc-saldo resultados time)))
  (ordenar-desempenho
   (map iter (descobrir-times resultados))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Ordenar o desempenho dos times por pontos, vitórias e saldo de gols

;; Análise
;; Ordenar uma lista de desempenhos de times em ordem decrescente baseado em pontos, vitórias e saldo de gols.
;;
;; Tipos de Dados
;; desempenhos é uma lista de estruturas desempenho.
;;
;; Especificação
;; List<desempenho> -> List<desempenho>
;; Ordena a lista de desempenhos dos times em ordem decrescente baseado em pontos, vitórias e saldo de gols.
;; O critério de ordenação é primeiro pelos pontos, depois pelas vitórias e finalmente pelo saldo de gols.
;; 1. Pontos (decrescente)
;; 2. Vitórias (decrescente)
;; 3. Saldo de gols (decrescente)
;; 4. Ordem alfabética do nome dos times (ordem crescente).

;; Exemplos de uso
(examples
 (check-equal? (ordenar-desempenho
                 (list (desempenho "Team 1" 8 4 2)
                       (desempenho "Team 2" 8 4 3)
                       (desempenho "Team 3" 6 3 1)
                       (desempenho "Team 4" 6 3 2)))
               (list (desempenho "Team 2" 8 4 3)
                     (desempenho "Team 1" 8 4 2)
                     (desempenho "Team 4" 6 3 2)
                     (desempenho "Team 3" 6 3 1)))
 (check-equal? (ordenar-desempenho
                 (list (desempenho "Team Alpha" 7 3 4)
                       (desempenho "Team Beta" 7 3 3)
                       (desempenho "Team Gamma" 5 2 1)
                       (desempenho "Team Delta" 5 2 2)))
               (list (desempenho "Team Alpha" 7 3 4)
                     (desempenho "Team Beta" 7 3 3)
                     (desempenho "Team Delta" 5 2 2)
                     (desempenho "Team Gamma" 5 2 1))))

(define (ordenar-desempenho desempenhos)
  (sort desempenhos
        (lambda (d1 d2)
          (cond
            [(> (desempenho-pontos d1) (desempenho-pontos d2)) #t]
            [(< (desempenho-pontos d1) (desempenho-pontos d2)) #f]
            [(> (desempenho-vitorias d1) (desempenho-vitorias d2)) #t]
            [(< (desempenho-vitorias d1) (desempenho-vitorias d2)) #f]
            [(> (desempenho-saldo d1) (desempenho-saldo d2)) #t]
            [(< (desempenho-saldo d1) (desempenho-saldo d2)) #f]
            [else (string<? (string-downcase (desempenho-time d1))
                            (string-downcase (desempenho-time d2)))]))))

;; Análise
;; Determinar o maior comprimento das strings entre todos os times.
;;
;; Tipos de Dados
;; desempenhos é uma lista de estruturas desempenho.
;;
;; Especificação
;; List<desempenhos> -> NúmeroInteiro
;; Determina o comprimento máximo entre todos os nomes de times a partir de uma lista de desempenhos.

(examples
 (check-equal? (max-tam-time
                 (list (desempenho "Team A" 7 3 4)
                       (desempenho "Team Jota" 7 3 3)
                       (desempenho "Team Jorges" 5 2 1))) 11) 
 (check-equal? (max-tam-time
                (list (desempenho "Tigre" 8 2 0)
                      (desempenho "Leopardo" 3 1 0)
                      (desempenho "Onça" 1 1 0))) 8))

(define (max-tam-time desempenhos)
  (define (iter desempenhos acc)
    (cond
      [(empty? desempenhos) acc]
      [else
       (iter (rest desempenhos)
            (max acc (string-length (desempenho-time (first desempenhos)))))]))
  (iter desempenhos 0))

;; Análise
;; Determinar o comprimento máximo da representação em string dos valores de pontos de todos os times.
;;
;; Tipos de Dados
;; desempenhos é uma lista de estruturas desempenho.
;;
;; Especificação
;; List<desempenhos> -> NúmeroInteiro
;; Determinar o comprimento máximo da representação em string dos valores de pontos de todos os times,
;; a partir de uma lista de desempenhos, produzindo o comprimento máximo.

(examples
 (check-equal? (max-tam-pontos
                 (list (desempenho "Team A" -207 3 4)
                       (desempenho "Team Jota" 7 3 3)
                       (desempenho "Team Jorges" 5 2 1))) 4) 
 (check-equal? (max-tam-pontos
                (list (desempenho "Tigre" 8 2 0)
                      (desempenho "Leopardo" -5 1 0)
                      (desempenho "Onça" 1 1 0))) 2))

(define (max-tam-pontos desempenhos)
  (define (iter desempenhos acc)
    (cond
      [(empty? desempenhos) acc]
      [else
       (iter (rest desempenhos)
             (max acc (string-length (number->string (desempenho-pontos (first desempenhos))))))]))
  (iter desempenhos 0))

;; Análise
;; Determinar o comprimento máximo da representação em string dos valores de vitorias de todos os times.
;;
;; Tipos de Dados
;; desempenhos é uma lista de estruturas desempenho.
;;
;; Especificação
;; List<desempenhos> -> NúmeroInteiro
;; Determinar o comprimento máximo da representação em string dos valores de vitorias de todos os times,
;; a partir de uma lista de desempenhos, produzindo o comprimento máximo.

(examples
 (check-equal? (max-tam-vitorias
                 (list (desempenho "Team A" 7 23 4)
                       (desempenho "Team Jota" 7 3 3)
                       (desempenho "Team Jorges" 5 2 1))) 2) 
 (check-equal? (max-tam-vitorias
                (list (desempenho "Tigre" 8 2 0)
                      (desempenho "Leopardo" 3 1 0)
                      (desempenho "Onça" 1 2000 0))) 4))

(define (max-tam-vitorias desempenhos)
  (define (iter desempenhos acc)
    (cond
      [(empty? desempenhos) acc]
      [else
       (iter (rest desempenhos)
             (max acc (string-length (number->string (desempenho-vitorias (first desempenhos))))))]))
  (iter desempenhos 0))

;; Análise
;; Determinar o comprimento máximo da representação em string dos valores de saldo de todos os times.
;;
;; Tipos de Dados
;; desempenhos é uma lista de estruturas desempenho.
;;
;; Especificação
;; List<desempenhos> -> NúmeroInteiro
;; Determinar o comprimento máximo da representação em string dos valores de saldo de todos os times,
;; a partir de uma lista de desempenhos, produzindo o comprimento máximo.

(examples
 (check-equal? (max-tam-saldo
                 (list (desempenho "Team A" 7 2 10550)
                       (desempenho "Team Jota" 7 3 3)
                       (desempenho "Team Jorges" 5 2 22))) 5) 
 (check-equal? (max-tam-saldo
                (list (desempenho "Tigre" 8 2 0)
                      (desempenho "Leopardo" 3 1 -200)
                      (desempenho "Onça" 1 1 0))) 4))

(define (max-tam-saldo desempenhos)
  (define (iter desempenhos acc)
    (cond
      [(empty? desempenhos) acc]
      [else
       (iter (rest desempenhos)
             (max acc (string-length (number->string (desempenho-saldo (first desempenhos))))))]))
  (iter desempenhos 0))

;; Análise
;; Converter uma estrutura desempenho em uma string formatada. A string inclui o nome do time, alinhado à esquerda com um
;; comprimento específico, seguido pelos valores de pontos, vitórias e saldo, cada um alinhado à direita com os comprimentos
;; especificados.
;;
;; Tipos de Dados
;; d é uma estrutura de desempenho
;; max-len-time é um número inteiro
;; max-len-pontos é um número inteiro
;; max-len-vitorias é um número inteiro
;; max-len-saldo é um número inteiro
;;
;; Especificação
;; List<d> max-len-time max-len-pontos max-len-vitorias max-len-saldo -> String
;; Converte uma estrutura desempenho em uma string formatada, recebe uma estrutura de desempenho e um número inteiro que
;; representa o comprimento máximo do nome do time. Produz uma string onde:
;; - O nome do time é alinhado à esquerda e preenche até max-len-time caracteres.
;; - Os valores de pontos, vitórias e saldo são alinhados à direita e preenchem até max-len-pontos, max-len-vitorias,
;;   e max-len-saldo caracteres, respectivamente.

(examples
  (check-equal? (desempenho->string (desempenho "Short" 9 7 6) 9 2 2 2) "Short      9  7  6")
  (check-equal? (desempenho->string (desempenho "Flamengo" 3 1 -2) 10 3 3 3) "Flamengo     3   1  -2")
  (check-equal? (desempenho->string (desempenho "Fluminense" 12 3 -2) 16 2 2 2) "Fluminense       12  3 -2"))

(define (desempenho->string d max-len-time max-len-pontos max-len-vitorias max-len-saldo)
  (string-append
   (desempenho-time d)
   (make-string (- max-len-time (string-length (desempenho-time d))) #\space)
   " "
   (make-string (- max-len-pontos (string-length (number->string (desempenho-pontos d)))) #\space)
   (number->string (desempenho-pontos d))
   " "
   (make-string (- max-len-vitorias (string-length (number->string (desempenho-vitorias d)))) #\space)
   (number->string (desempenho-vitorias d))
   " "
   (make-string (- max-len-saldo (string-length (number->string (desempenho-saldo d)))) #\space)
   (number->string (desempenho-saldo d))))

;; Análise
;; Gerar uma lista de strings dos desempenhos com alinhamento.
;;
;; Tipos de Dados
;; desempenhos é uma lista de estruturas desempenho.
;; max-len-time é um número inteiro
;; max-len-pontos é um número inteiro
;; max-len-vitorias é um número inteiro
;; max-len-saldo é um número inteiro
;;
;; Especificação
;; desempenhos max-len-time max-len-pontos max-len-vitorias max-len-saldo -> List<String>
;; Gera uma lista de strings dos desempenhos com alinhamento dos nomes dos times, pontos, vitorias e saldo.

(examples
  (check-equal? (desempenhos-list (list (desempenho "TeamA" 12 5 3)) 10 2 2 2) (list "TeamA      12  5  3"))
  (check-equal? (desempenhos-list (list (desempenho "TeamB" 9 7 6) (desempenho "TeamC" 123 8 7)) 8 3 2 2)
                (list "TeamB      9  7  6" 
                      "TeamC    123  8  7"))
  (check-equal? (desempenhos-list (list (desempenho "Alpha" 45 12 9) (desempenho "Beta" 5 3 -4) (desempenho "Gamma" 100 15 10)) 10 3 2 3) 
                (list "Alpha       45 12   9" 
                      "Beta         5  3  -4" 
                      "Gamma      100 15  10")))

(define (desempenhos-list desempenhos max-len-time max-len-pontos max-len-vitorias max-len-saldo)
  (cond
    [(empty? desempenhos) empty]
    [else
     (cons (desempenho->string (first desempenhos) max-len-time max-len-pontos max-len-vitorias max-len-saldo)
           (desempenhos-list (rest desempenhos) max-len-time max-len-pontos max-len-vitorias max-len-saldo))]))

;; Análise
;; Exibir elementos da lista gerada a partir de desempenhos-list, imprimindo cada desempenho em uma nova linha até
;; que a lista esteja vazia.
;;
;; Tipos de Dados
;; desempenhos-list é uma lista de desempenhos.
;;
;; Especificação
;; desempenhos-list -> Void
;; Exibe os elementos da lista de strings desempenhos-list na saída padrão, com cada string sendo exibida em uma nova linha.
;; A função não retorna nenhum valor e termina quando todos os elementos da lista foram exibidos.

(define (desempenhos-string desempenhos-list)
  (cond
    [(empty? desempenhos-list) (void)] 
    [else
     (display (first desempenhos-list))
     (display "\n")
     (desempenhos-string (rest desempenhos-list))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Função principal para exibir os desempenhos
(define (classifica-times)
  (define entrada (port->lines (open-input-file "jogos.txt")))
  (define jogos (processa-entrada entrada))
  (define desempenho (desempenho-total jogos))
  (define max-len-time (max-tam-time desempenho))
  (define max-len-pontos (max-tam-pontos desempenho))
  (define max-len-vitorias (max-tam-vitorias desempenho))
  (define max-len-saldo (max-tam-saldo desempenho))
  (define list-desempenhos (desempenhos-list desempenho max-len-time max-len-pontos max-len-vitorias max-len-saldo))
  (desempenhos-string list-desempenhos))

;; Chamada da função principal
(classifica-times)