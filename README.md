# Paradigmas de programação UFRPE
<p align="center">
  <img src="https://github.com/user-attachments/assets/157a5d93-aaa7-436b-a0c2-d3099d8fd18a" alt="image">
  <img src="https://github.com/user-attachments/assets/aa87609e-1904-4392-b7c7-17e657dbe9fd" alt="image">
</p>

# Descrição 

Este repositório se trata dos trabalhos realizados na matéria de Paradigmas de Programação ministrada pelo professor Sidney na UFRPE.

# Projeto
![Haskell](https://img.shields.io/badge/Haskell-%235e5086?style=for-the-badge&logo=haskell&logoColor=white)![Cabal](https://img.shields.io/badge/Cabal-%23007ACC?style=for-the-badge&logoColor=white)![Threepenny](https://img.shields.io/badge/Threepenny--GUI-%2331868f?style=for-the-badge)

# V.1

Este projeto usa programação funcional, especificamente a linguagem Haskell para criar uma versão do jogo 2048. Como pode ser vista no vídeo abaixo. O jogo é completamente funcional. Em seguida será implementada a interface gráfica para tornar o jogo mais acessível.
![projeto gif](https://github.com/user-attachments/assets/161fa44a-b87b-4310-9c9f-862e451f53ed) 

# V.1.1

A interface gráfica foi implementada usando a biblioteca Threepenny-GUI que é permite criar interfaces gráficas usando HTML, CSS e JavaScript, mas escrevendo todo o código em Haskell. A aplicação Haskell roda um servidor web local que renderiza a interface no navegador, utilizando HTML, CSS e Html no front-end, enquanto a lógica e os eventos são controlados no back-end Haskell.

De maneira geral, foi uma experiência bastante interessante utilizar a programação funcional do haskell junto a programação reativa do Threepenny-GUI. O resultado da aplicação pode ser visto abaixo.


![Vídeo-sem-título-‐-Feito-com-o-Clipchamp](https://github.com/user-attachments/assets/e8344381-bbf7-41e6-9599-11674bc80a80)


# Uso de IA 

## Ryan Eskinazi: 

Durante o desenvolvimento da funcionalidade de pontuação do jogo, implementei uma estratégia baseada na diferença entre a soma do tabuleiro antes e depois de um movimento. A lógica era:

```haskell
scoreFromBoards old new = sum (concat new) - sum (concat old)
```
Apesar de funcionar parcialmente, percebi que a pontuação não mostrava corretamente as "fusões", já que a função addRandomTile adicionava peças que alteravam a soma total.

Com ajuda do ChatGPT 4o, identifiquei que a forma certa era capturar os pontos durante a fusão das peças, reestruturando a função de movimentação para retornar o novo tabuleiro e a pontuação da jogada. A IA me ajudou a reorganizar a função moveLine para retornar um par (novaLinha, pontosObtidos) e integrar isso na UI.

A exibição do score foi implementada usando UI.span, e a atualização do texto após cada jogada foi feita com:

```haskell
void $ element scoreDisplay # set UI.text ("Score: " ++ show newScore)
```

O prompt usado foi: 
"estou trabalhando no desenvolvimento do jogo 2048 em haskell. calculei a pontuação com 'scoreFromBoards old new = sum (concat new) - sum (concat old)', mas o score não está correto depois de cada jogada. o que pode estar errado?" seguido do trecho de codigo que queria depurar

## Vinícius Ferraz:

Houveram três ocasiões que precisei usar o LLM. A primeira era pra mover as os numeros na matriz. A segunda era para a função do merge que une as entradas depois de ter lido o teclado. Nas duas situações o código gerado foi.

```
-- Rotaciona o tabuleiro 90 graus no sentido horário
rotateBoard :: Board -> Board
rotateBoard = reverse . transpose
  where transpose ([]:_) = []
        transpose x = map head x : transpose (map tail x)

-- Move e junta uma linha para a esquerda
moveLine :: [Int] -> [Int]
moveLine xs = let noZeros = filter (/= 0) xs
                  merged = merge noZeros
              in merged ++ replicate (boardSize - length merged) 0
  where
    merge (x:y:zs)
      | x == y = x*2 : merge zs
      | otherwise = x : merge (y:zs)
    merge xs = xsAdd commentMore actions
```

Depois tive que escolher o framework para a camada de aplicação (interface gráfica) do projeto. Neste caso, perguntei quais as opções mais usadas para criação de interfaces usando haskell. A LLM falou sobre as seguintes ferramentas

* Gtk2Hs (Haskell bindings for GTK) : Wrapper para a biblioteca GTK.Bastante madura e estável.Funciona bem para aplicações desktop.Sintaxe mais próxima de linguagens imperativas.
* Threepenny-GUI:Interface gráfica baseada em navegador.Gera uma GUI rodando localmente via servidor web.Muito funcional, idiomática em Haskell.Recomendado para projetos Haskell puros.
* Reflex FRP (Functional Reactive Programming):Abordagem reativa funcional.Muito poderosa, mas com curva de aprendizado alta.Usada com Reflex-DOM para GUIs web.

No primeiro momento tentei fazer o proejto usando o Gtk2Hs. No entanto, estava tendo dificuldades com a instalação do projeto e também com a troca de estados do tabuleiro após o movimento.
Em seguida, foi testado o Threepeenny-GUI que é uma biblioteca de interface gráfica muito interessante de haskell que roda a aplicação em um servidor javascript. Desta forma, é mais flexível e fácil de modificar. Isso se dá por que o Three-penny tem acesso a propriedades CSS.

# Como executar o código
Em primeiro lugar, se faz necessário ter instalado o Cabal e o GHCi. A documentação oficial de instalação pode ser encontrada em: https://www.haskell.org/cabal/ para o Cabal e https://www.haskell.org/ghc/ para o GHC e o GHCi.

1 - Clone o repositório no github
```
git clone https://github.com/Vinicius-O-Ferraz/paradigmas-de-programacao-ufrpe.git
```

2 - Em seguida, abra o código no editor de sua preferência. Abra o terminal e navegue a pasta projeto
```
cd .\projeto\
```

3 - Compile o projeto cabal. Esta etapa pode demorar um pouco
```
cabal build
```

4 - Execute o arquivo
```
cabal run
```
