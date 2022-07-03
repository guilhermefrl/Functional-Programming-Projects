open Printf (* Abrir bibliotecas*)
let k1 = ref (0)  
let k2 = ref (0)
let()=
  let rec solve arr d k = (*Função do algoritmo Guloso com o array de moedas, o valor que deve ser calculado e 0*)
    if(d=0) then k1 := k;  (*Se d chegar a zero então retornamos o valor de passos em k*)
    if(d>0) then (*Se o dinheiro ainda for maior que 0 então continuamos a função*)
      let f = ref 0 in (*Definimos um flag f*)
      for jk=((Array.length arr)-1) downto 0 do (*Um for desde o tamanho do array até 0, começamos no maior elemento do array para o for ser mais eficiente*)
        if((arr.(jk))<=d && !f=0) (*Se o elemento atual do array for menor que o dinheiro e a flag igual a 0, então podemos subtrair o valor do elemento atual ao dinheiro senão baixamos uma posicão no array das moedas, assim indo para um valor inferior *)
        then( 
        (solve (arr) (d-(arr.(jk))) (k+1)); (*Podemos subtrair o valor do array ao dinheiro atual, logo voltamos a chamar a função, subtraindo o valor ao dinheiro total, e aumentando o numero de passos k*)
        f := 1 ); (*Se voltamos a chamar a função podemos terminar este for para aumentar a eficiência do algoritmo*)
      done; 
  in
  let rec dinamico arr d tam=(*Função do algoritmo dinamico com o array de moedas, o total das moedas e o valor que deve ser calculado*)
    let t = Array.make (tam+1) 99999 in (*Fazemos um array t com os campos todos a 99999*)
    t.(0) <- 0; (*Definimos primeiro elemento a 0*)

    for j=0 to tam do (*Fazemos um for desde 0 até ao valor que deve ser calculado, logo j vai representar o valor que queremos calcular*)
      for k=0 to (d-1) do (*Um segundo for para calcular o valor com todas as moedas existentes*)
        if arr.(k) <= j then ( (*Se a moeda k do array arr for menor ou igual que o valor que pretendemos realizar o troco então vamos buscar e guardar em aux o elemento j menos a moeda atual*)
          let aux = t.(j - arr.(k)) in (*Guardamos na variavel aux, a subtração do valor que queremos calcular com a moeda que estamos a usar*)
          if((aux <> 99999) && (aux+1 < t.(j)))
		  then (t.(j) <- aux+1); (*Se aux for diferente de 99999 e aux+1 for menor que a moeda atual então guardamos na posicao j de t o valor de aux+1*)
        );
      done; (*Fim do for interior*)
    done;(*Fim do for exterior*)
    if(t.(tam)=99999) then (k2 := 0) (*Se o elemento que corresponde ao valor que deve ser calculado for 9999 significa que não foi alterado logo o valor de k2 deve permanecer a 0*)
    else k2 := t.(tam) (*Se esse elemento foi alterado então o valor a retornar deve corresponder a esse valor*)
  in
  let d = read_int() in (*Ler o total de moedas e armazenar em d*)
  let arr =  Array.make d 0 in (*Fazer um array com d elementos*)
  let aux = ref 0 in (*Definir aux 0*)

  for i=0 to (d-1) do (*For para ler todas as moedas*)
    let j = read_int () in (*Ler as moedas*)
    arr.(i) <- j; (*Colocar o valor das moedas no array*)
  done; (*Fim do for*)

  let maior=arr.((Array.length arr)-1) in (*Descobrir a moeda maior introduzida*)
  let menor=((arr.(0))-1) in (*Descobrir a menor moeda introduzida*)
  let a = Array.make (maior*2) 0 in (*Fazer um array para os resultados do algoritmo guloso*)
  let b = Array.make (maior*2) 0 in(*Fazer um array para os resultados do algoritmo Dinâmico*)

  for j=menor to ((maior*2)-1) do  (*Realizar um for para calcular resultados, desde a moeda menor até duas vezes o tamanho da maior*)
    if(!aux=0) then ( (*Se houver algum valor diferente entre os arrays o valor de aux sai de 0*)
      let() = solve arr (j+1) 0 in (*Invocar o algoritmo guloso mandando o array com as moedas, o valor atual que queremos calcular, ou seja j+1, e o numero de passos ou seja 0*)
      a.(j) <- !k1; (*Colocar o valor do algoritmo guloso no array a na posicao j*)
      k1 :=0; (*Voltar a colocar k1 a zero*)
      let() = dinamico arr d (j+1) in(*Invocar o algoritmo dinamico mandando o array com as moedas, o total de moedas e o valor atual que queremos calcular, ou seja j+1, *)
      b.(j) <- !k2; (*Colocar o valor do algoritmo dinamico no array b na posicao j*)
    );
    if(a.(j) > b.(j)) then aux := (j+1) else(); (*Se o valor colocada em a for maior que em b então temos um caso em que ambos os algoritmos são diferentes logo paramos o for mudando o valor da variavel*)
  done;
  if(!aux <> 0) then printf("%d\n") !aux (*Se aux for diferente de 0 significa que temos um caso em que discordam logo imprimimos o valor de aux*)
  else printf("YES\n") (*Se os algoritmos nunca discordam então podemos imprimir YES*)

(*Algoritmo Dinamico baseado no seguinte link: https://dev.to/shivams136/leetcode-322-coin-change-solution-4kmd *)
(* Exemplo:

Moedas: 1, 4, 5

1º - Vai ser chamada a função do algoritmo dinâmico e do algoritmo guloso, num ciclo desde a moeda menor até ao dobro da moeda maior, neste caso, de 1 até 10.

    -> Na função do algoritmo guloso:
      1º - Vai ser chamada recursivamente a função, com o array das moedas (1,4,5), o valor que queremos calcular o troco, por exemplo 8, e o contador a 0.

      2º - Vamos começar pelo fim do array das moedas, pois é onde está a moeda maior.

      3º - Verificamos se a moeda que queremos testar é menor ou igual ao valor.

      4º - Cada vez que a função recursiva é chamada o contador vai ser incrementado em mais 1.

      Neste caso, a solução para o valor 8 seria:

      8-5 -> 3 | (As moedas 5 e 4 não vão funcionar pois são maiores que 3)
               | 3-1 -> 2 | (As moedas 5 e 4 não vão funcionar pois são maiores que 2)
                          | 2-1 -> 1 | (As moedas 5 e 4 não vão funcionar pois são maiores que 1)
                                     | 1-1 -> 0 (O contador vai ser igual a 4)

      Para o valor 8, a função do algoritmo guloso vai retornar 4.


   -> Na função do algoritmo dinâmico:
      1º - Vamos criar um array com o tamanho do valor que queremos calcular o troco, por exemplo 8, mais um, neste caso o tamanho do array seria 9.

      2º - Vamos colocar 0 na primeira posição, pois o troco mínimo para zero vai ser sempre zero.

      3º - Vamos preencher o resto do array com um número maior do que o valor que queremos calcular o troco, por exemplo 99999.

          |0|99999|99999|99999|99999|99999|99999|99999|99999|

      4º - Vamos fazer um ciclo desde 1 até ao valor que queremos calcular o troco, neste caso 8.

      5º - Vamos verificar para todas as moedas qual o menor troco possível, para cada elemento do array.

      Neste caso, a solução para o valor 8 seria:
      
        - Para a moeda 1:
          |0|1|2|3|4|5|6|7|8|

        - Para a moeda 4:
          |0|1|2|3|1|2|3|4|2| 

        - Para a moeda 5:
          |0|1|2|3|1|1|2|3|4|

        Logo o array final com o troco mínimo seria:
          |0|1|2|3|1|1|2|3|2|
                           ^
                           |
          Assim, para o valor 8, a função do algoritmo dinâmico vai retornar 2.

2º - Vão ser guardados os valores da função do algoritmo dinâmico e do algoritmo guloso em dois arrays.

  Neste caso, no ciclo de 1 a 10, os arrays vão ser os seguintes:

    |1|2|3|1|1|2|3|4|-|-| (Array do algoritmo guloso)
    |1|2|3|1|1|2|3|2|-|-| (Array do algoritmo dinâmico)
                   ^
                   |
    Assim, como na oitava posição os trocos são diferentes o resultado vai ser 8.
*)