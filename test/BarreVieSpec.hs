module BarreVieSpec where

import Test.Hspec
import Test.QuickCheck

import TypesSpec
import Model


genCombOk2 :: Gen Combattant
genCombOk2 = do
    x <- choose (0,1330)
    y <- choose (155,480)
    let pos = Coord x y
    hitboxc <- genHitboxOk
    facec <-  elements [B,H,D,G]
    let etatx = Ok 100
    laps <- choose (0,100)
    bonus <- choose (0,99)
    let jauge = Bonus bonus laps
    gravite <- choose (0,50)
    return $ Comb pos hitboxc facec etatx jauge gravite

genJeuOK2 :: Gen Jeu
genJeuOK2 = do
    joueur1 <- genCombOk2
    joueur2 <- genCombOk2
    zone <- genZoneOk
    return $ EnCours joueur1 joueur2 zone


property_inv_setVieOk :: Int -> Integer -> Property
property_inv_setVieOk id modif = forAll genJeuOK2 $ prop_post_inv_setVie id modif


setVieSPec = do
    describe "Set Barre de vie" $ do
        it "preserves the invariant" $ property $ 
            \id modif -> property_inv_setVieOk id modif

property_inv_setVieOk2 :: Int -> Integer -> Property
property_inv_setVieOk2 id modif = forAll genJeuEnCoursOk $ post_setVie id modif

post_setVieSpec = do 
    describe "set Barre de vie" $ do
        it "still coherent" $ property $
            \id -> property_inv_setVieOk2 id 20


-- Extensions regain d'energie
prop_postMajTourSpec = do
    describe "Regain de Vie" $ do
        it "the counter is only incremented if the player's life has not changed since the last turn" $ property $
            \nv_life old_life cpt -> prop_postMajTour nv_life old_life cpt

property_gain_VieOk ::  Int -> Integer -> Integer -> Integer -> Property
property_gain_VieOk id condition nb_tour gain = forAll genJeuOK2 $ prop_post_gain_vie id condition nb_tour gain

prop_post_gain_vieSpec = do 
    describe "Regain de Vie" $ do
        it "only gives life under certain conditions" $ property $
            \id condition nb_tour gain -> property_gain_VieOk id condition nb_tour gain