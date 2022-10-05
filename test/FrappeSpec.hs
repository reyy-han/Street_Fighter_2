module  FrappeSpec where

import Data.Sequence as Seq

import Test.Hspec
import Test.QuickCheck

import TypesSpec
import Model


instance Arbitrary Jeu where
    arbitrary = do 
        jeu <- genJeuEnCoursOk 
        return jeu

genJaugeOk :: Gen Jauge
genJaugeOk = do 
    laps <- choose (0,100)
    bonus <- choose (0,99)
    return $ Bonus bonus laps

property_inv_augmenteJauge :: Property
property_inv_augmenteJauge = forAll genJaugeOk $ prop_inv_jauge 

prop_inv_JaugeSpec = do 
    describe "Jauges" $ do
        it "preserves the invariant" $ property property_inv_augmenteJauge 

prop_post_augmenteJaugeSpec = do 
    describe "Jauges" $ do
        it "Increasing gauge keeps the gauges coherent and preserves the invariant of EtatCombattant and Jeu" $ property $
            \jeu -> post_augmenteBonus jeu


prop_pre_jauge_max = do
    describe "Jauges" $ do
        it "ensures that the Jauge is full in the exemple " $ 
            is_jauge_max 1 (EnCours (Comb (Coord 0 2) (Rect (Coord 40 2) 120 300) G (Ok 20) Max 10) (Comb (Coord 90 45) (Rect (Coord 130 45) 10 20) D (Ok 50) Max 5) (Zone 1520 840)) 
            `shouldBe` True

prop_pre_ken_frappe_ryuSpec = do 
    describe "Frappes" $ do
        it "ensures that ken hit ryu in the exemple" $
            ken_frappe_ryu (EnCours (Comb (Coord 0 480) (Rect (Coord 40 500) 120 300) D (Ok 20) Max 10)(Comb (Coord 120 480) (Composite (fromList[(Rect (Coord 160 500) 200 300), (Rect (Coord 120 500) 66 40) ])) G (Ok 50) Max 5) (Zone 1520 840)) 
           `shouldBe` True

post_frappeTouchSpec = do
    describe "Frappes" $ do
        it "reduce life when one of the player hits the other" $ property $
            \jeu perte -> post_frappeTouche perte jeu

prop_inv_post_frappeTouchSpec = do
    describe "Frappes" $ do
        it "preserves the invariant" $ property $
            \jeu perte -> prop_post_inv_frappeTouche perte jeu


prop_post_coup_special_ou_nonSpec = do
    describe "Frappes - Jauges" $ do
        it "preserves the fact that the special hit is usable only if the gauge is full" $ property $
            \id iscoupspecial jeu -> prop_post_coup_special_ou_non id iscoupspecial jeu

prop_post_frappe_specialSpec = do 
    describe "Frappes - Jauges" $ do
        it  "Reset Bonus after Special Hit" $ property $
            \id iscoupspecial jeu -> prop_post_reset_jauge id iscoupspecial jeu

all_prop_post_JaugeSpec = do 
    describe "Frappes - Jauges" $ do
        it  "tests all prop of gauges" $ property $
            \id iscoupspecial jeu ->  prop_post_coup_special_ou_non id iscoupspecial jeu && prop_post_reset_jauge id iscoupspecial jeu


prop_post_retourNormalSpec = do 
    describe "Retour de Frappes" $ do
        it "ensures that the player returns to their normal position" $ property $ 
            \id jeu -> prop_post_retourNormal id jeu