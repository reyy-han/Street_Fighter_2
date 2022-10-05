module TypesSpec where

import Test.Hspec
import Test.QuickCheck

import Model

prop_zone :: Zone -> Bool
prop_zone (Zone x y) = x >= 1520 && y >= 850

zoneSpecInit = do 
    describe "initZone" $ do
        it "preserves the invariant" $ property $
            \x y -> post_inv_initZoneXY x y

coordSpecInit = do
    describe "initCoord" $ do
        it "preserves the invariant" $ property $ 
            forAll (oneof [return (Zone 1520 900)               -- oneof tire un des return aléatoirement
                     , return (Zone 5000 5250)
                     , return (Zone 6000 800)]) $ -- quelque soit le return choisi
      
            \z -> post_init_Coord 1 z && post_init_Coord 2 z

genHitboxOk :: Gen Hitbox
genHitboxOk = do
    x <- choose (0,1320)
    y <- choose (0, 640)
    let c = Coord x y
    h <- elements [(hitboxRepos_Pied c), (hitboxRightPied c), (hitboxLeftPied c), (hitboxRightPoing c), (hitboxLeftPoing c)]
    return h
    --return $ initHitbox id 

property_inv_genHitboxOk :: Property
property_inv_genHitboxOk = forAll genHitboxOk $ prop_inv_hitbox1

hitboxSpecGenOk = do
    describe "genHitboxOk - non vide" $ do
        it "generates hitboxs that satisfy their invariant" $
            property property_inv_genHitboxOk

genZoneOk :: Gen Zone
genZoneOk = do
        x <- choose (1520, 5000)
        y <- choose (840, 5000)
        return $ initZoneXY x y

property_inv_genZoneOk :: Property
property_inv_genZoneOk = forAll genZoneOk $ prop_inv_Zone

zoneSpecGenOk = do
    describe "genZoneOk" $ do 
        it "generates zones that satisfy invariant" $ property property_inv_genZoneOk

property_inv_genHitboxZoneOk :: Hitbox -> Property
property_inv_genHitboxZoneOk h = forAll genZoneOk $ (prop_inv_hitbox h)

hitboxSpecInit = do
    describe "initHitbox" $ do
        it "preserves the invariant" $ 
            forAll (oneof [return 1, return 2]) $
            \id -> property $ property_inv_genHitboxZoneOk (initHitbox id) 


genCombOk :: Gen Combattant
genCombOk = do
    x <- choose (0,1330)
    y <- choose (155,480)
    let pos = Coord x y
    hitboxc <- elements [(hitboxRepos_Pied pos), (hitboxRightPied pos), (hitboxLeftPied pos), (hitboxRightPoing pos), (hitboxLeftPoing pos)]
    facec <-  elements [B,H,D,G]
    vie <- choose (1,100)
    let etatx = Ok vie
    laps <- choose (0,100)
    bonus <- choose (0,99)
    let jauge = Bonus bonus laps
    gravite <- choose (0,50)
    return $ Comb pos hitboxc facec etatx jauge gravite

property_inv_genCombOk :: Property
property_inv_genCombOk = forAll genCombOk $ prop_inv_comb

combSpecGenOk = do
    describe "genCombOk" $ do 
        it "generates combattants that satisfy invariant" $ property property_inv_genCombOk

property_inv_genCombZoneOk :: Combattant -> Property
property_inv_genCombZoneOk comb = forAll genZoneOk $ (prop_inv_comb_in_zone comb)

combSpecInit = do
    describe "initComb" $ do 
        it "preserves the invariant" $ 
            forAll (oneof [return 1, return 2]) $
                \id -> property $ property_inv_genCombZoneOk (initPerso id) 

genJeuEnCoursOk :: Gen Jeu 
genJeuEnCoursOk = do 
    joueur1 <- genCombOk
    joueur2 <- genCombOk
    zone <- genZoneOk
    return $ EnCours joueur1 joueur2 zone

property_inv_genJeuEnCoursOk :: Property
property_inv_genJeuEnCoursOk = forAll genJeuEnCoursOk $ prop_inv_jeu

jeuEnCoursSpecGenOk = do
    describe "genJeuEnCoursOk" $ do 
        it "generates games that satisfy invariant" $ property property_inv_genJeuEnCoursOk
