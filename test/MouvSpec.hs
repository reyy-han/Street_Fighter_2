module  MouvSpec where

import Test.Hspec
import Test.QuickCheck

import TypesSpec
import Model


instance Arbitrary Jeu where
    arbitrary = do 
        jeu <- genJeuEnCoursOk 
        return jeu


moveLeftSpec1 = do
    describe "Left" $ do
        it "moveLeft - TEST 1" $ 
            moveLefti 1 10 (EnCours (Comb (Coord 0 2) (Rect (Coord 40 2) 120 300) D (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
            `shouldBe` (EnCours (Comb (Coord 0 2) (Rect (Coord 40 22) 120 300) G (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 

moveLeftSpec2 = do
    describe "Left" $ do
        it "moveLeft - TEST 2" $ 
            moveLefti 1 10 (EnCours (Comb (Coord 150 2) (Rect (Coord 190 2) 120 300) D (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
            `shouldBe` (EnCours (Comb (Coord 140 2) (Rect (Coord 180 22) 120 300) G (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 

postMoveLeftAndRightSpec = do
    describe "Left" $ do
        it "moveLeft and moveRight are correctly done" $ property $
            \jeu -> postMoveLeft 1 10 jeu && postMoveLeft 2 10 jeu && postMoveRight 1 10 jeu && postMoveRight 2 10 jeu 

--(EnCours (Comb (Coord 3 424) (Composite (fromList [Rect (Coord 43 444) 120 300,Rect (Coord 3 444) 66 40])) H (Ok 94) (Bonus 16 36) 3) (Comb (Coord 254 174) (Composite (fromList [Rect (Coord 269 194) 170 300,Rect (Coord 399 194) 50 110])) D (Ok 76) (Bonus 26 34) 48) (Zone 4346 1874))

moveRightSpec1 = do
    describe "Right" $ do
        it "moveRight - TEST 1" $ 
            moveRighti 1 10 (EnCours (Comb (Coord 0 2) (Rect (Coord 40 2) 120 300) G (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
            `shouldBe` (EnCours (Comb (Coord 10 2) (Rect (Coord 50 22) 120 300) D (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 

moveRightSpec2 = do
    describe "Right" $ do
        it "moveRight - TEST 2" $ 
            moveRighti 1 10 (EnCours (Comb (Coord 150 2) (Rect (Coord 190 2) 120 300) D (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
            `shouldBe` (EnCours (Comb (Coord 160 2) (Rect (Coord 200 22) 120 300) D (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
        

movePropSpec1 = do
    describe "Left & Right" $ do
        it "moveLeft can be undone by moveRight" $ property $
            \jeu  -> prop_moveleft_unmove_right 1 10 jeu && prop_moveleft_unmove_right 2 10 jeu


moveUpSpec1 = do
    describe "Up" $ do
        it "moveUp - TEST 1" $ 
            moveUpi 1 30 (EnCours (Comb (Coord 0 480) (Rect (Coord 40 480) 120 300) D (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
            `shouldBe` (EnCours (Comb (Coord 0 450) (Rect (Coord 40 450) 120 300) H (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 

moveUpSpec2 = do
    describe "Up" $ do
        it "moveUp - TEST 2" $ 
            moveUpi 1 10 (EnCours (Comb (Coord 150 150) (Rect (Coord 190 130) 120 300) D (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
            `shouldBe` (EnCours (Comb (Coord 150 145) (Rect (Coord 190 125) 120 300) H (Ok 20) Max 0) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
     
     
chuteSpec = do
    describe "Chute" $ do
        it "preserves the invariant" $ property $
            \jeu -> post_inv_chute 1 jeu