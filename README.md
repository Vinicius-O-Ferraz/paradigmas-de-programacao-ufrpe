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

Ryan Eskinazi: 

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

