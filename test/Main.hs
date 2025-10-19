module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 20 "test n > s" ~?= "          test n > s",
      alinearDerecha 20 "  espaciosDelante" ~?= "     espaciosDelante",
      alinearDerecha 20 "espaciosDetras  " ~?= "    espaciosDetras  ",
      alinearDerecha 3 "hola" ~?= "hola",
      alinearDerecha 0 "test" ~?= "test",
      alinearDerecha 5 "" ~?= "     "
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 2 (+ 10) [1, 2, 3] ~?= [1, 2, 13],
      actualizarElem (-2) (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 4 (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 28 (+ 10) [1..29] ~?= [1..28] ++ [39],
      actualizarElem 0 (++ "x") ["a", "b", "c"] ~?= ["ax", "b", "c"],
      actualizarElem 1 (const 99) [1, 2, 3] ~?= [1, 99, 3],
      actualizarElem 0 id [42] ~?= [42],
      actualizarElem 0 (+ 1) [] ~?= []
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (vacio 2 (2, 10))
        ~?= [ Casillero infinitoNegativo 2 0 0,
              Casillero 2 6 0 0,
              Casillero 6 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 4 (-3, 0))
        ~?= [ Casillero infinitoNegativo (-3) 0 0,
              Casillero (-3) (-2.25) 0 0,
              Casillero (-2.25) (-1.5) 0 0,
              Casillero (-1.5) (-0.75) 0 0,
              Casillero (-0.75) 0 0 0,
              Casillero 0 infinitoPositivo 0 0
            ],
      casilleros (vacio 5 (10, 20))
        ~?= [ Casillero infinitoNegativo 10 0 0,
              Casillero 10 12 0 0,
              Casillero 12 14 0 0,
              Casillero 14 16 0 0,
              Casillero 16 18 0 0,
              Casillero 18 20 0 0,
              Casillero 20 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
      h1 = vacio 2 (2, 10)
      h2 = vacio 4 (-3, 0)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h1)
            ~?= [ Casillero infinitoNegativo 2 1 100,
                  Casillero 2 6 0 0,
                  Casillero 6 10 0 0,
                  Casillero 10 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 (agregar 1 h1))
            ~?= [ Casillero infinitoNegativo 2 1 50,
                  Casillero 2 6 1 50,
                  Casillero 6 10 0 0,
                  Casillero 10 infinitoPositivo 0 0
                ],
          casilleros (agregar (-2.5) (agregar (-2) (agregar (-1) (agregar (-0.5) h2))))
            ~?= [ Casillero infinitoNegativo (-3) 0 0,
                  Casillero (-3) (-2.25) 1 25,
                  Casillero (-2.25) (-1.5) 1 25,
                  Casillero (-1.5) (-0.75) 1 25,
                  Casillero (-0.75) 0 1 25,
                  Casillero 0 infinitoPositivo 0 0
                ],
          casilleros (agregar 1 (agregar 12 h1))
            ~?= [ Casillero infinitoNegativo 2 1 50,
                  Casillero 2 6 0 0,
                  Casillero 6 10 0 0,
                  Casillero 10 infinitoPositivo 1 50
                ],
          casilleros (agregar 5.99 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 1 100,
                  Casillero 6 infinitoPositivo 0 0
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      histograma 1 (1, 100) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 1 (1, 100)))),
      histograma 2 (0, 10) [] ~?= vacio 2 (0, 10),
      histograma 3 (0, 6) [0, 1, 2, 3, 4, 5, 6]
        ~?= agregar 6 (agregar 5 (agregar 4 (agregar 3 (agregar 2 (agregar 1 (agregar 0 (vacio 3 (0, 6)))))))),
      histograma 5 (-10, 10) [-15, -5, 0, 5, 15]
        ~?= agregar 15 (agregar 5 (agregar 0 (agregar (-5) (agregar (-15) (vacio 5 (-10, 10))))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 6 (agregar 5 (agregar 3 (agregar 1 (agregar (-1) (vacio 3 (0, 6)))))))
        ~?= [ Casillero infinitoNegativo 0.0 1 20,
              Casillero 0.0 2.0 1 20,
              Casillero 2.0 4.0 1 20,
              Casillero 4.0 6.0 1 20,
              Casillero 6.0 infinitoPositivo 1 20
            ],
      length (casilleros (vacio 10 (0, 100))) ~?= 12,
      casCantidad (casilleros (agregar 5 (vacio 1 (0, 10))) !! 1) ~?= 1
    ]

testsRecr :: Test
testsRecr =
  test
    [ recrExpr (\_ -> 1) (\_ _ -> 1)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (Const 5) ~?= 1,
      recrExpr (\_ -> 1) (\_ _ -> 1)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (\_ r1 _ r2 -> 1 + r1 + r2)
               (Suma (Const 3) (Mult (Const 2) (Rango 4 6))) ~?= 5,
      recrExpr (\x -> x) (\x y -> x + y)
               (\e1 r1 e2 r2 -> r1 + r2)
               (\e1 r1 e2 r2 -> r1 - r2)
               (\e1 r1 e2 r2 -> r1 * r2)
               (\e1 r1 e2 r2 -> r1 / r2)
               (Suma (Const 3) (Const 7)) ~?= 10,
      recrExpr id (\x y -> (x + y) / 2)
               (\_ r1 _ r2 -> r1 + r2)
               (\_ r1 _ r2 -> r1 - r2)
               (\_ r1 _ r2 -> r1 * r2)
               (\_ r1 _ r2 -> r1 / r2)
               (Mult (Rango 2 4) (Const 5)) ~?= 15
    ]

testsFold :: Test
testsFold =
  test
    [ foldExpr (\x -> x) (\x y -> (x + y) / 2) (+) (-) (*) (/) (Const 5) ~?= 5,
      foldExpr (\x -> x) (\x y -> (x + y) / 2) (+) (-) (*) (/) (Suma (Const 2) (Const 3)) ~?= 5,
      foldExpr (\x -> x) (\x y -> (x + y) / 2) (+) (-) (*) (/) (Resta (Const 10) (Const 5)) ~?= 5,
      foldExpr (\x -> x) (\x y -> (x + y) / 2) (+) (-) (*) (/) (Mult (Const 1) (Const 5)) ~?= 5,
      foldExpr (\x -> x) (\x y -> (x + y) / 2) (+) (-) (*) (/) (Div (Rango 0 10) (Const 1)) ~?= 5,
      foldExpr Const Rango Suma Resta Mult Div (Suma (Const 3) (Const 2)) ~?= Suma (Const 3) (Const 2),
      foldExpr (\x -> 1) (\_ _ -> 1) (\x y -> x + y) (\x y -> x + y) (\x y -> x + y) (\x y -> x + y)
               (Div (Suma (Const 1) (Const 2)) (Mult (Const 3) (Const 4))) ~?= 4,
      foldExpr (\_ -> 0) (\_ _ -> 0) (\x y -> 1 + x + y) (\x y -> 1 + x + y)
               (\x y -> 1 + x + y) (\x y -> 1 + x + y)
               (Resta (Const 1) (Suma (Const 2) (Const 3))) ~?= 2
    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Const 7) genFijo) ~?= 7.0,
      fst (eval (Rango 1 5) genFijo) ~?= 3.0,
      fst (eval (Resta (Const 10) (Const 3)) genFijo) ~?= 7.0,
      fst (eval (Mult (Const 2) (Const 3)) genFijo) ~?= 6.0,
      fst (eval (Div (Const 10) (Const 2)) genFijo) ~?= 5.0,
      fst (eval (Mult (Rango 2 4) (Rango 3 5)) genFijo) ~?= 12.0,
      fst (eval (Div (Suma (Const 10) (Const 20)) (Const 3)) genFijo) ~?= 10.0,
      fst (eval (Suma (Mult (Const 2) (Const 3)) (Resta (Const 10) (Const 5))) genFijo) ~?= 11.0
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [ -- Test con generador fijo (siempre devuelve el valor medio)
      let expr = eval (Const 5)
          (hist, _) = armarHistograma 3 10 expr genFijo
          cs = casilleros hist
      in (length cs == 5) ~?= True,

      -- Test con rango usando generador fijo
      let expr = eval (Rango 10 20)
          (hist, _) = armarHistograma 5 100 expr genFijo
          cs = casilleros hist
      in (length cs == 7) ~?= True,

      -- Test con expresión compleja
      let expr = eval (Suma (Const 10) (Rango 0 10))
          (hist, _) = armarHistograma 4 50 expr genFijo
          cs = casilleros hist
      in (length cs == 6) ~?= True,

      -- Test verificando que todos los valores con genFijo caen en el mismo casillero
      let expr = eval (Const 100)
          (hist, _) = armarHistograma 2 20 expr genFijo
          cs = casilleros hist
          cantidades = map casCantidad cs
      in sum cantidades ~?= 20
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [ -- Test con constante
      let (hist, _) = evalHistograma 3 10 (Const 42) genFijo
          cs = casilleros hist
      in (length cs == 5) ~?= True,

      -- Test con rango
      let (hist, _) = evalHistograma 5 100 (Rango 0 100) genFijo
          cs = casilleros hist
      in (length cs == 7) ~?= True,

      -- Test con suma
      let (hist, _) = evalHistograma 4 50 (Suma (Const 10) (Const 20)) genFijo
          cs = casilleros hist
          total = sum (map casCantidad cs)
      in total ~?= 50,

      -- Test con expresión compleja
      let (hist, _) = evalHistograma 3 30 (Mult (Rango 1 3) (Const 2)) genFijo
          cs = casilleros hist
      in (sum (map casCantidad cs) == 30) ~?= True,

      -- Test verificando distribución con semilla fija
      let (hist, _) = evalHistograma 5 100 (Rango 10 20) (genNormalConSemilla 42)
          cs = casilleros hist
          cantidadTotal = sum (map casCantidad cs)
      in cantidadTotal ~?= 100
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0",
      mostrar (Const 42.0) ~?= "42.0",
      mostrar (Rango 1 10) ~?= "1.0~10.0",
      mostrar (Div (Const 10) (Suma (Const 2) (Const 3)))
        ~?= "10.0 / (2.0 + 3.0)",
      mostrar (Div (Div (Const 100) (Const 10)) (Const 2))
        ~?= "100.0 / 10.0 / 2.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]