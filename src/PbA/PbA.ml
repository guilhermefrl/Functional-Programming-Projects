open Printf
let k1 = ref (9999999) (* Variavel que vai guardar o valor do numero de passos, vamos definir com um valor superior aos limites establecidos no enunciado *)
let r= ref "BAD LUCK" (* Variavel string caso não exista resultado possível*) 
let d = read_int() (* Ler e guardar valor inserido pelo utilizador/moshak *)
let ()=
  let rec solve d k = (*Função que resolve o problema *)
    if (d = 42 && (k < !k1)) then k1 := k; (*Se chegarmos ao valor 42 e este for menor que o valor atual de k1 então substituimos o valor da variavel *)
    if(d > 42) then (*Se a execução continuar a ser valida com o valor superior a 42 continuamos *)
      if ((d mod 2) = 0) then (solve (d/2)(k+1)); (*Se o valor for multiplo de 2, então chamamos a função devolvendo metade do valor ao utilizador e incrementando o numero de passos *)
      if ((((d mod 3) = 0) || ((d mod 4) = 0)) && ((d mod 10) * ((d/10) mod 10)) <> 0) then (solve (d - ((d mod 10) * ((d/10) mod 10)))(k+1));(*Se o valor for multiplo de 3 ou 4, então chamamos a função de novo, devolvendo ao utilizador o valor total, subtraindo o valor dos dois ultimos digitos multiplicados, este caso também exige mais a condição de nenhum dos dois ultimos digitos serem 0, pois se existir algum 0 nos dois ultimos digitos e chamarmos esta função como estamos a multiplicar um pelo outro e depois subtrair entramos num loop infinito*)
      if((d mod 5) = 0)  then (solve (d-42)(k+1)); (* Se o valor for multiplo de 2, então chamamos a função de novo, subtraindo 42 ao valor, e incrementando o numero de passos*)
in
      let ()= 
    solve d 0 in (* Chamamos a execução da função com o valor lido do utilizador e os passos a 0*)
    if(!k1 <> 9999999) then  (* Se o valor de k1 foi alterado então podemos dar print ao mesmo pois significa que atingimos o valor 42   *)
      printf("%d\n") !k1
    else
      printf("%s\n") !r;(* Se o valor de k1 permanecer inalterado então damos print á string indicando que não foi possivel chegar ao valor de 42  *)
      

(* Exemplo:

Valor de entrada: 210

1º - Vai ser chamada a função recursiva com o valor 210 e com 0 passos

2º - Como 210 é maior que 42 vão ser verificadas as regras (explicadas acima):

  210 |(regra 1) -> 105 |(regra 2) -> (Como a multiplicação dos dois últimos dígitos vai dar zero então não se pode efetuar a regra, pois vai ficar o mesmo número)
      |                 |(regra 3) -> 63 | (regra 2) -> 45 | (regra 2) -> 25 (Como 25<42 então a função recursiva não vai ser chamada)
      |                                                    | (regra 3) ->  3 (Como 3<42 então a função recursiva não vai ser chamada)
      |
      |(regra 2) -> (Como a multiplicação dos dois últimos dígitos vai dar zero então não se pode efetuar a regra, pois vai ficar o mesmo número)
      |
      |(regra 3) -> 168 |(regra 1) -> 84 | (regra 1) -> 42 (3 passos)
                        |                | (regra 2) -> 52 | (regra 1) -> 26 (Como 26<42 então a função recursiva não vai ser chamada)
                        |                                  | (regra 2) -> 42 (4 passos)
                        |
                        |(regra 2) -> 120 | (regra 1) -> 60 | (regra 1) -> 30 (Como 30<42 então a função recursiva não vai ser chamada)
                                          |                 | (regra 2) -> (Como a multiplicação dos dois últimos dígitos vai dar zero então não se pode efetuar a regra, pois vai ficar o mesmo número)
                                          |                 | (regra 3) -> 18 (Como 18<42 então a função recursiva não vai ser chamada)
                                          |
                                          | (regra 2) -> (Como a multiplicação dos dois últimos dígitos vai dar zero então não se pode efetuar a regra, pois vai ficar o mesmo número)
                                          | (regra 3) -> 78 | (regra 1) -> 39 (Como 39<42 então a função recursiva não vai ser chamada)
                                                            | (regra 2) -> 22 (Como 22<42 então a função recursiva não vai ser chamada)

3º - Cada vez que a função recursiva é chamada o número de passos vai ser incrementado em mais 1

4ª - Caso haja mais do que uma solução, vai ser guardado o menor número de passos.

  Neste exemplo, para chegar ao 42 foi necessário 3 ou 4 passos.
  Assim, vai ser escolhido os 3 passos.
*)