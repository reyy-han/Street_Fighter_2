module  HitboxSpec where

import Test.Hspec
import Test.QuickCheck

import Model

instance Arbitrary Coord where
    arbitrary = do 
        x <- choose (0,1520)
        y <- choose (0, 840)
        return $ Coord x y

instance Arbitrary Zone where
    arbitrary = do 
        l <- choose (1520, 10000)
        h <- choose (840, 8000)
        return $ Zone l h

instance Arbitrary Hitbox where
    arbitrary = do 
        c <- arbitrary
       -- l <- choose (0,1520)
       -- h <- choose (0,840)
        h <- elements [(hitboxRepos_Pied c), (hitboxRightPied c), (hitboxLeftPied c), (hitboxRightPoing c), (hitboxLeftPoing c)]
        return h

hitboxReposPiedSpecInit = do
    describe "update hitbox 1" $ do
        it "preserves the invariant and modify correctly the hitbox" $ property $
            \c z -> post_HitboxRepos_Pied z c

hitboxReposPoingSpecInit = do
    describe "update hitbox 2" $ do
        it "preserves the invariant and modify correctly the hitbox" $ property $
            \c z -> post_HitboxPoing z c

hitboxReposLeftPoingSpecInit = do
    describe "update hitbox 2 - Poing Gauche" $ do
        it "preserves the invariant and modify correctly the hitbox" $ property $
            \c z -> post_HitboxLeftPoing z c

hitboxReposRightPoingSpecInit = do
    describe "update hitbox 2 - Poing Droit" $ do
        it "preserves the invariant and modify correctly the hitbox" $ property $
            \c z -> post_HitboxRightPoing z c

hitboxReposRightPiedSpecInit = do
    describe "update hitbox 2 - Pied Droit" $ do
        it "preserves the invariant and modify correctly the hitbox" $ property $
            \c z -> post_HitboxRightPied z c


hitboxReposLeftPiedSpecInit = do
    describe "update hitbox 2 - Pied Gauche" $ do
        it "preserves the invariant and modify correctly the hitbox" $ property $
            \c z -> post_HitboxLeftPied z c


hitboxSetXSpec = do
    describe "set Hitbox" $ do
        it "set the hitbox - TEST" $ 
            (setXHitbox (Rect (Coord 0 0) 10 10) 10) `shouldBe` (Rect (Coord 10 0) 10 10) 

hitboxSetXPropSpec1 = do
    describe "set Hitbox" $ do
        it "set can be undone" $ property $
            \h i -> prop_set_unsetX h i
hitboxSetXPropSpec2 = do 
    describe "set Hitbox" $ do
        it "set correct" $ property $
            \h i -> post_setXHitbox h i

hitboxSetYSpec = do
    describe "set Hitbox" $ do
        it "set the hitbox - TEST" $ 
            (setYHitbox (Rect (Coord 0 0) 12 50) 10) `shouldBe` (Rect (Coord 0 10) 12 50) 

hitboxSetYPropSpec1 = do
    describe "set Hitbox" $ do
        it "set can be undone" $ property $
            \h i -> prop_set_unsetY h i

hitboxSetYPropSpec2 = do 
    describe "set Hitbox" $ do
        it "set correct" $ property $
            \h i -> post_setYHitbox h i