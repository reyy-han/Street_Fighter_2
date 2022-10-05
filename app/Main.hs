{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

------
import Mouse (Mouse)
import qualified Mouse as M

import Data.Maybe
------

import qualified Debug.Trace as T

import Model (Jeu)
import qualified Model as M

loadBackground :: Renderer-> FilePath -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path path_over tmap smap = do
  tmap1 <- TM.loadTexture rdr path (TextureId "background") tmap
  tmap' <- TM.loadTexture rdr path_over (TextureId "gameover") tmap1
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1520 840)
  let sprite' = S.defaultScale $ S.addImage sprite $ S.createImage (TextureId "gameover") (S.mkArea 0 0 1520 890)
  let smap' = SM.addSprite (SpriteId "background") sprite' smap
  return (tmap', smap')

loadPersosKen :: Renderer-> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPersosKen rdr path0 path1 path2 path3 path4 path5 tmap smap = do
  tmap0 <- TM.loadTexture rdr path0 (TextureId "perso_Ken_droite") tmap
  tmap1 <- TM.loadTexture rdr path1 (TextureId "perso_Ken_gauche") tmap0
  tmap2 <- TM.loadTexture rdr path2 (TextureId "perso_Ken_coup_droit") tmap1
  tmap3 <- TM.loadTexture rdr path3 (TextureId "perso_Ken_coup_gauche") tmap2
  tmap4 <- TM.loadTexture rdr path4 (TextureId "perso_Ken_pied_droit") tmap3
  tmap5 <- TM.loadTexture rdr path5 (TextureId "perso_Ken_pied_gauche") tmap4
  let sprite0 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso_Ken_droite") (S.mkArea 0 0 200 325)
  let sprite1 = S.defaultScale $ S.addImage sprite0 $ S.createImage (TextureId "perso_Ken_gauche") (S.mkArea 0 0 200 325)
  let sprite2 = S.defaultScale $ S.addImage sprite1 $ S.createImage (TextureId "perso_Ken_coup_droit") (S.mkArea 0 0 285 300)
  let sprite3 = S.defaultScale $ S.addImage sprite2 $ S.createImage (TextureId "perso_Ken_coup_gauche") (S.mkArea 0 0 285 300)
  let sprite4 = S.defaultScale $ S.addImage sprite3 $ S.createImage (TextureId "perso_Ken_pied_droit") (S.mkArea 0 0 263 310)
  let sprite5 = S.defaultScale $ S.addImage sprite4 $ S.createImage (TextureId "perso_Ken_pied_gauche") (S.mkArea 0 0 263 310)
  let smap' = SM.addSprite (SpriteId "perso_Ken") sprite5 smap
  return (tmap5, smap')

loadPersosRyu :: Renderer-> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPersosRyu rdr path0 path1 path2 path3 path4 path5 tmap smap = do
  tmap0 <- TM.loadTexture rdr path0 (TextureId "perso_Ryu_droite") tmap
  tmap1 <- TM.loadTexture rdr path1 (TextureId "perso_Ryu_gauche") tmap0
  tmap2 <- TM.loadTexture rdr path2 (TextureId "perso_Ryu_coup_droit") tmap1
  tmap3 <- TM.loadTexture rdr path3 (TextureId "perso_Ryu_coup_gauche") tmap2
  tmap4 <- TM.loadTexture rdr path4 (TextureId "perso_Ryu_pied_droit") tmap3
  tmap5 <- TM.loadTexture rdr path5 (TextureId "perso_Ryu_pied_gauche") tmap4
  let sprite0 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso_Ryu_droite") (S.mkArea 0 0 200 325)
  let sprite1 = S.defaultScale $ S.addImage sprite0 $ S.createImage (TextureId "perso_Ryu_gauche") (S.mkArea 0 0 200 325)
  let sprite2 = S.defaultScale $ S.addImage sprite1 $ S.createImage (TextureId "perso_Ryu_coup_droit") (S.mkArea 0 0 285 300)
  let sprite3 = S.defaultScale $ S.addImage sprite2 $ S.createImage (TextureId "perso_Ryu_coup_gauche") (S.mkArea 0 0 285 300)
  let sprite4 = S.defaultScale $ S.addImage sprite3 $ S.createImage (TextureId "perso_Ryu_pied_droit") (S.mkArea 0 0 263 310)
  let sprite5 = S.defaultScale $ S.addImage sprite4 $ S.createImage (TextureId "perso_Ryu_pied_gauche") (S.mkArea 0 0 263 310)
  let smap' = SM.addSprite (SpriteId "perso_Ryu") sprite5 smap 
  return (tmap5, smap')

-- permet de mettre a jour l'image du sprite en fonction de la direction du joueur
updateImagePerso :: Maybe M.Direction -> Sprite -> Sprite
updateImagePerso d sprite = 
  case d of
    (Just M.D) -> (S.changeImage sprite 0) 
    (Just M.G) -> (S.changeImage sprite 1) 
    (Just M.BD) -> (S.changeImage sprite 2) 
    (Just M.BG) -> (S.changeImage sprite 3) 
    (Just M.HD) -> (S.changeImage sprite 4) 
    (Just M.HG) -> (S.changeImage sprite 5) 
    Nothing -> sprite
    otherwise -> sprite

-- section de vie --
loadLife :: Renderer-> FilePath -> FilePath -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadLife rdr path_gris path_jaune path_logo tmap smap = do
  tmap0 <- TM.loadTexture rdr path_gris (TextureId "barre_de_vie") tmap
  tmap1 <- TM.loadTexture rdr path_jaune (TextureId "vie_ryu") tmap0
  tmap2 <- TM.loadTexture rdr path_jaune (TextureId "vie_ken") tmap1
  tmap' <- TM.loadTexture rdr path_logo (TextureId "logo") tmap2
  let sprite0 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "barre_de_vie") (S.mkArea 0 0 1520 50)
  let sprite1 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "vie_ryu") (S.mkArea 0 0 735 30) --655 lorsque la vie est a 100
  let sprite2 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "vie_ken") (S.mkArea 0 0 735 30)
  let sprite3 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "logo") (S.mkArea 0 0 50 50)
  let smap0 = SM.addSprite (SpriteId "barre_de_vie") sprite0 smap
  let smap1 = SM.addSprite (SpriteId "vie_ryu") sprite1 smap0
  let smap2 = SM.addSprite (SpriteId "vie_ken") sprite2 smap1
  let smap' = SM.addSprite (SpriteId "logo") sprite3 smap2
  return (tmap', smap')

updateLife :: Maybe Integer -> Sprite -> Sprite
updateLife sante sprite = 
  case sante of
    (Just sante) -> (S.scale sprite  (fromIntegral ((sante * 735)`div` 100 )) (fromIntegral 30))
    Nothing -> sprite

-- section des jauges --
loadJauge :: Renderer -> FilePath -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadJauge rdr path_vert path_rouge tmap smap = do
  tmap0 <- TM.loadTexture rdr path_vert (TextureId "jauge_ryu") tmap
  tmap1 <- TM.loadTexture rdr path_vert (TextureId "jauge_ken") tmap0
  tmap2 <- TM.loadTexture rdr path_rouge (TextureId "jauge_remplie_ryu") tmap1
  tmap' <- TM.loadTexture rdr path_rouge (TextureId "jauge_remplie_ken") tmap2
  let sprite0 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "jauge_ryu") (S.mkArea 0 0 10 200) 
  let sprite1 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "jauge_ken") (S.mkArea 0 0 10 200)
  let sprite2 = S.defaultScale $ S.addImage sprite0 $ S.createImage (TextureId "jauge_remplie_ryu") (S.mkArea 0 0 10 200) 
  let sprite3 = S.defaultScale $ S.addImage sprite1 $ S.createImage (TextureId "jauge_remplie_ken") (S.mkArea 0 0 10 200)
  let smap0 = SM.addSprite (SpriteId "jauge_ryu") sprite2 smap
  let smap' = SM.addSprite (SpriteId "jauge_ken") sprite3 smap0
  return (tmap', smap')

updateJauge :: Maybe M.Jauge -> Sprite -> Sprite
updateJauge mb sprite = 
  case mb of
    Just bonus -> 
      case bonus of
        M.Max -> S.changeImage ((S.scale sprite (fromIntegral 10) (fromIntegral 200))) 1
        M.Bonus nb laps -> if (S.getCurrentIndex sprite) == 0 
          then (S.scale sprite (fromIntegral 10) (fromIntegral ((nb * 200)`div` 100 )))
          else S.changeImage ((S.scale sprite (fromIntegral 10) (fromIntegral ((nb * 200)`div` 100 )))) 0
    Nothing -> sprite

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Street Fighter" $ defaultWindow { windowInitialSize = V2 1520 890 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "Images_street_fighter/bmp/background2.bmp"
                                          "Images_street_fighter/bmp/YouWin.bmp"
                                           TM.createTextureMap SM.createSpriteMap
  -- chargement de la section Barre de vie
  (tmap1, smap1) <- loadLife renderer "Images_street_fighter/bmp/Carré_gris.bmp"
                                      "Images_street_fighter/bmp/jaune.bmp"
                                      "Images_street_fighter/bmp/logo_street_fighter.bmp"
                                       tmap smap
   -- chargement des Jauges
  (tmap2, smap2) <- loadJauge renderer "Images_street_fighter/bmp/vert.bmp"
                                       "Images_street_fighter/bmp/rouge.bmp"
                                       tmap1 smap1
  -- chargement du personnage Ken
  (tmap', smap') <- loadPersosKen renderer "Images_street_fighter/bmp/ge-street-fighter-ken-vers-la-droite.bmp"
                                            "Images_street_fighter/bmp/personnage-street-fighter-ken.bmp"
                                            "Images_street_fighter/bmp/coupdepoingOrange.bmp"
                                            "Images_street_fighter/bmp/coupdepoingOrange-vers-la-gauche.bmp"
                                            "Images_street_fighter/bmp/coupdepiedOrange.bmp"
                                            "Images_street_fighter/bmp/coupdepiedOrange-vers-la-droite.bmp"
                                             tmap2 smap2
  -- chargement du personnage Ryu
  (tmap'', smap'') <- loadPersosRyu renderer "Images_street_fighter/bmp/ge-street-fighter-ryu-vers-la-droite.bmp"
                                            "Images_street_fighter/bmp/personnage-street-fighter-ryu-removebg-preview.bmp"
                                            "Images_street_fighter/bmp/coupdepoingBlanc.bmp"
                                            "Images_street_fighter/bmp/coupdepoingBlanc-vers-la-gauche.bmp"
                                            "Images_street_fighter/bmp/coupdepiedBlanc.bmp"
                                            "Images_street_fighter/bmp/coupdepiedBlanc-vers-la-gauche.bmp"
                                             tmap' smap'
  -- initialisation de l'état du jeu
  let jeu = M.initJeu
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap'' smap'' kbd jeu 100 100 0 0 
------------------------------


-----------------------------
--------------------------------------------------------------------------------------------------------------------

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Jeu -> Integer -> Integer -> Integer -> Integer -> IO ()    
gameLoop frameRate renderer tmap smap kbd jeu@(M.GameOver id) vieRyu vieKen tourRyu tourKen = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd

  clear renderer
  --- display background
  S.displaySprite renderer tmap (S.moveTo (S.changeImage (SM.fetchSprite (SpriteId "background") smap) 1)
                                  (fromIntegral 0) 
                                  (fromIntegral 0) )   
  if id == 1 then S.displaySprite renderer tmap (S.moveTo (updateImagePerso (M.getDirection 1 jeu) (SM.fetchSprite (SpriteId "perso_Ryu") smap))
                                  (fromIntegral 660) 
                                  (fromIntegral 480))
    else S.displaySprite renderer tmap (S.moveTo (updateImagePerso (M.getDirection 2 jeu) (SM.fetchSprite (SpriteId "perso_Ken") smap)) 
                                  (fromIntegral 660) 
                                  (fromIntegral 480))    
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' (M.safe_augmenteBonus jeu) vieRyu vieKen tourRyu tourKen)

gameLoop frameRate renderer tmap smap kbd jeu vieRyu vieKen tourRyu tourKen = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd

  clear renderer
  --- display background
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "background") smap)
                                  (fromIntegral 0) 
                                  (fromIntegral 50) )
  --- display de la barre de vie
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "barre_de_vie") smap) 
    -- Bare de vie de Ryu (a gauche) -- 
  let ryu_life = M.getLife 1 jeu

  S.displaySprite renderer tmap (S.moveTo (updateLife ryu_life (SM.fetchSprite (SpriteId "vie_ryu") smap))
                                  (fromIntegral (0 + (735 * (100 - (fromJust ryu_life))) `div` 100) ) -- permet de bien placer la barre de vie en fonction de la sante
                                  (fromIntegral 10))  
    -- Bare de vie de Ken (a droite) -- 
  let ken_life = M.getLife 2 jeu
  S.displaySprite renderer tmap (S.moveTo (updateLife ken_life (SM.fetchSprite (SpriteId "vie_ken") smap))
                                  (fromIntegral 785) 
                                  (fromIntegral 10))
    -- LOGO du Jeu --
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "logo") smap)
                                  (fromIntegral 735) 
                                  (fromIntegral 0))
  -- display des Jauges
    -- Jauge Ryu     
  S.displaySprite renderer tmap (S.moveTo (updateJauge (M.getBonus 1 jeu)(SM.fetchSprite (SpriteId "jauge_ryu") smap)) 
                                  (fromIntegral 10) 
                                  (fromIntegral 70))
    -- Jauge Ken
  S.displaySprite renderer tmap (S.moveTo (updateJauge (M.getBonus 2 jeu) (SM.fetchSprite (SpriteId "jauge_ken") smap) )
                                  (fromIntegral 1500) 
                                  (fromIntegral 70))                    
  --- display perso Ken
  S.displaySprite renderer tmap (S.moveTo (updateImagePerso (M.getDirection 2 jeu) (SM.fetchSprite (SpriteId "perso_Ken") smap)) 
                                  (fromIntegral $ fromJust (M.px 2 jeu)) 
                                  (fromIntegral ((fromJust $ M.py 2 jeu) + 50)))                               
  --- display perso Ryu

  S.displaySprite renderer tmap (S.moveTo (updateImagePerso (M.getDirection 1 jeu) (SM.fetchSprite (SpriteId "perso_Ryu") smap))
                                  (fromIntegral (fromJust $ M.px 1 jeu)) 
                                  (fromIntegral ((fromJust $ M.py 1 jeu) + 50))) 
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  --putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  --putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let jeu1 = M.gameStep jeu kbd' deltaTime
  let jeu2 = M.gameStepCoup jeu1 kbd' deltaTime
  
  -- extension ajout de vie --
  let new_ryu_life = M.getLife 1 jeu2
  let new_ken_life = M.getLife 2 jeu2
  let tourRyu' = M.majTour vieRyu  new_ryu_life tourRyu
  let tourKen' = M.majTour vieKen new_ken_life tourKen
  let jeu3 = M.condition_gain_vie 1 500 tourRyu' 15 jeu2
  let jeu' = M.condition_gain_vie 2 500 tourKen' 15 jeu3

  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' (M.safe_augmenteBonus jeu') 
    (fromJust (M.getLife 1 jeu'))  (fromJust (M.getLife 2 jeu')) tourRyu' tourKen')
  
--------------------------------------------------------------------------------------------------------------------
  