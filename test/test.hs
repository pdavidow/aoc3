import Test.Tasty
import Test.Tasty.HUnit

import AOC3 (mkClaim, linearAreaOverlap)

main = defaultMain tests
  
tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests = testGroup "Unit tests" $

    [ testGroup "AOC3" $ 
        [ testCase "1" $
            let
                c1 = mkClaim 1 (1,3) (4,4)
                c2 = mkClaim 2 (3,1) (4,4)
                c3 = mkClaim 3 (5,5) (2,2)
            in
                linearAreaOverlap [c1, c2, c3] @?= 4

        ,  testCase "2" $
            let
                c1 = mkClaim 1 (1,3) (4,4)
                c2 = mkClaim 2 (3,1) (4,4)
                c3 = mkClaim 3 (5,5) (2,2)
                c4 = mkClaim 3 (3,5) (3,3)                    
            in
                linearAreaOverlap [c1, c2, c3, c4] @?= 6              
        ]
    ]