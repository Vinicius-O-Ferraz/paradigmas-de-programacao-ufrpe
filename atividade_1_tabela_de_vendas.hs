

-- tabelas de vendas pega do github da disciplina
vendas :: Int -> Int
vendas 0 = 20
vendas 1 = 50
vendas 2 = 30
vendas 3 = 50
vendas n = n * 2

-- implementação de uma função que adiciona espaço n vezes a uma string
addEspaco :: Int -> Int -> String
addEspaco semana venda =
  show semana ++ replicate (8 - length (show semana)) ' ' ++ show venda

-- tabela de vendas até a semana n
tabela :: Int -> IO ()
tabela n = do
  putStrLn "Semana   Venda"
  mapM_ (\i -> putStrLn $ addEspaco i (vendas i)) [0..n]

-- somatório de vendas para a semana n 
totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = totalVendas (n - 1) + vendas n

-- média de vendas até a semana n
mediaVendas :: Int -> Float
mediaVendas n = fromIntegral (totalVendas n) / fromIntegral (n + 1)

main :: IO ()
main = do
  -- Teste para a semana três
  let semana = 3
  tabela semana
  putStrLn $ "Total: " ++ show (totalVendas semana)
  putStrLn $ "Média: " ++ show (mediaVendas semana)


  -- teste para semana 10

  -- let semana = 10
  -- tabela semana
  -- putStrLn $ "Total: " ++ show (totalVendas semana)
  -- putStrLn $ "Média: " ++ show (mediaVendas semana)
