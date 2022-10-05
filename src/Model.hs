module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import Data.Sequence
import qualified Data.Sequence as Seq

data Zone = Zone Integer Integer
instance Show Zone where 
  show (Zone x y) = "(Zone " ++ (show x) ++ " " ++  (show y) ++ ")"
instance Eq Zone where
  Zone x y == Zone a b = x == a && y == b

data Coord = Coord Integer Integer
instance Show Coord where 
  show (Coord x y) = "(Coord " ++ (show x) ++ " " ++ (show y) ++ ")"
instance Eq Coord where
  Coord x y == Coord a b = x == a && y == b

data Direction = H | B | G | D 
              | HD | HG | BD | BG -- Ajout au sujet 
  deriving (Show)
instance Eq Direction where
  D == D = True
  G == G = True
  H == H = True
  B == B = True
  HD == HD = True
  HG == HG = True
  BD == BD = True
  BG == BG = True
  _ == _ = False

data Mouvement = Mouv Direction Integer
instance Show Mouvement where 
  show (Mouv d x) = "Mouvement " ++ (show d) ++ " " ++ (show x)

data Hitbox = Rect Coord Integer Integer -- Rect coordonnees_du_rectangle largeur hauteur
            | Composite (Seq Hitbox)
  deriving (Show)
instance Eq Hitbox where
  (Rect (Coord x1 y1) largeur1 hauteur1) == (Rect (Coord x2 y2) largeur2 hauteur2) = x1 == x2 && y1 == y2 && largeur1 == largeur2 && hauteur1 == hauteur2
  Composite sh1 == Composite sh2 = sh1 == sh2 
  _ == _ = False

data EtatCombattant = Ko  
                    | Ok Integer  -- sante actuelle
instance Show EtatCombattant where 
  show Ko = "Ko"
  show (Ok x) = "Ok " ++ (show x)
instance Eq EtatCombattant where
  Ko == Ko = True
  Ok x == Ok v = x == v

data Jauge = Max
          | Bonus Integer Integer-- Bonus (bonus actuel) laps (si bonus == 100 alors Jauge passe a Max / le bonus augmente lorsque laps arrive à 100)
instance Show Jauge where 
  show Max = "Max"
  show (Bonus bonus laps) = "Bonus " ++ (show bonus) ++ " " ++ (show laps)
instance Eq Jauge where
  Max == Max = True
  Bonus b1 l1 == Bonus b2 l2 = b1 == b2 && l1 == l2

data Combattant = Comb {
                          positionc :: Coord
                        , hitboxc :: Hitbox
                        , facec :: Direction
                        , etatx :: EtatCombattant
                        , bonus :: Jauge        -- ou Integer
                        , gravite :: Integer    -- aide pour les sauts et les chutes
}
  deriving (Show)

instance Eq Combattant where
  (Comb p1 h1 f1 e1 b1 g1) == (Comb p2 h2 f2 e2 b2 g2) = p1 == p2 && h1 == h2 && f1 == f2 && e1 == e2 && b1 == b2 && g1 == g2

data Jeu = GameOver Integer  -- le numero du joueur vainqueur
          | EnCours {
              joueur1 :: Combattant
            , joueur2 :: Combattant
            , zoneJeu :: Zone
          }
  deriving (Show)

instance Eq Jeu where
  (EnCours j1 j2 z) == (EnCours j1' j2' z') = j1==j1' && j2==j2' && z==z'
  GameOver x == GameOver v = x==v

-------------------------------------------------------------------------------------------------------------
---- Initialisation des personnages et du jeu ---- 
--- # Invariant Coord et initCoord --------------------------------------------------------------------------
-- verifie que la coordonee n'ai pas de valeurs negatives
prop_inv_coord :: Coord -> Bool
prop_inv_coord (Coord x y) = x >= 0 && y >= 0

pre_init_coord :: Integer -> Integer -> Zone -> Bool
pre_init_coord x y (Zone l h) = x <= l && y <= h 

-- verifie que la fonction initCoord initialise les coordonnees en respectant les proprietes demandees par les invariants
post_init_Coord :: Int -> Zone -> Bool
post_init_Coord id z@(Zone l h) = (pre_id id) && (pre_init_coord x y z) && (prop_inv_coord c)  where c@(Coord x y) = initCoord id

-- verifie que le point coord donne en parametre se situe bien à l'interieur de la zone z
prop_inv_coord_in_zone :: Coord -> Zone -> Bool
prop_inv_coord_in_zone c@(Coord x y) (Zone l h) | prop_inv_coord c = x <= l && y <= h  

initCoord :: Int -> Coord 
initCoord 1 = initCoordRyu
initCoord 2 = initCoordKen

initCoordKen :: Coord
initCoordKen = Coord 1100 480 --initCoordKen = Coord 1330 480 -- placement le plus a droite --initCoordKen = Coord 1330 145 -- placement le plus a droite en haut
initCoordRyu :: Coord
initCoordRyu = Coord 200 480  --initCoordRyu = Coord 0 480 -- placement le plus a gauche    --initCoordRyu = Coord 0 145 -- placement le plus a gauche en haut

------------------------------------------------------------------------------------------------
-- # Invariants Hitbox et initHitbox------------------------------------------------------------
-- Une hitbox ne peut pas etre vide
prop_inv_hitbox1 :: Hitbox -> Bool
prop_inv_hitbox1 (Rect c largeur hauteur) | prop_inv_coord c = largeur > 0 && hauteur > 0 
prop_inv_hitbox1 (Composite (h :<| hs)) = (prop_inv_hitbox1 h) || (prop_inv_hitbox1 (Composite hs))

-- les hitbox ne peuvent pas etre plus larges ou hautes que la zone
prop_inv_hitbox2 :: Zone -> Hitbox -> Bool
prop_inv_hitbox2 (Zone l h) (Rect c largeur hauteur) = largeur <= l && hauteur <= h
prop_inv_hitbox2 z@(Zone l h) (Composite Empty) = True
prop_inv_hitbox2 z@(Zone l h) (Composite sh) = (prop_inv_hitbox2 z (Seq.index sh 0)) && (prop_inv_hitbox2 z (Composite (Seq.drop 1 sh)))

-- verifie que la hitbox h n'est pas vide et qu'elle ne depasse pas la zone z
prop_inv_hitbox ::Hitbox -> Zone ->  Bool
prop_inv_hitbox h z = (prop_inv_hitbox1 h) && (prop_inv_hitbox2 z h)
-- verifie que la fonction initHitbox cree correctement la hitbox en fonction de la zone z (utilise les prop invariants definis precedemment)
prop_post_init_hitbox :: Int -> Zone -> Bool 
prop_post_init_hitbox id z = (pre_id id && (prop_inv_hitbox hitbox z)) where hitbox = initHitbox id

initHitbox :: Int -> Hitbox
initHitbox 1 = initHitboxRyu
initHitbox 2 = initHitboxKen

initHitboxRyu :: Hitbox
initHitboxRyu = Rect (Coord 240 510) 120 300 -- hitbox de l'image au repos

initHitboxKen :: Hitbox
initHitboxKen = Rect (Coord 1140 510) 120 300 -- hit box de l'image au repos

safe_initHitbox :: Int -> Zone -> Maybe Hitbox
safe_initHitbox id z | pre_id id && prop_inv_Zone z = Just (initHitbox id)
                     | otherwise = Nothing

prop_post_inv_initHitbox :: Int -> Zone -> Bool
prop_post_inv_initHitbox id z | prop_inv_Zone z && pre_id id = 
  check (safe_initHitbox id z)
  where check Nothing = True
        check (Just h) = prop_inv_hitbox1 h && prop_inv_hitbox2 z h

-- # Invariants EtatCombattant et Bonus --------------------------------------------------------
prop_inv_etat :: EtatCombattant -> Bool
prop_inv_etat (Ok vie) = vie > 0 && vie <= 100
prop_inv_etat Ko = True

prop_inv_jauge :: Jauge -> Bool
prop_inv_jauge (Bonus b laps) = b >= 0 && b < 100 && laps >= 0 && laps <= 100
prop_inv_jauge Max = True

-- # Invariants Combattant et initPerso
prop_inv_comb :: Combattant -> Bool
prop_inv_comb c@(Comb {positionc = coord, hitboxc = hitb, facec = f, etatx = st, bonus = b, gravite = g}) = 
  g >= 0 && (prop_inv_coord coord) && (prop_inv_hitbox1 hitb) && (prop_inv_etat st) && (prop_inv_jauge b)

-- verifie que le combattant c est positionne dans la zone z et que sa hitbox est aussi dans la zone
prop_inv_comb_in_zone :: Combattant -> Zone -> Bool
prop_inv_comb_in_zone c@(Comb {positionc = coord, hitboxc = hitb, facec = f, etatx = st, bonus = b, gravite = g}) z =
  (prop_inv_coord_in_zone coord z) && (prop_inv_comb c) && (prop_inv_hitbox2 z hitb)

-- verifie que la fonction initPerso initialise correctemment le personnage en question
prop_init_perso :: Integer -> Zone -> Bool
prop_init_perso id z = id <= 2 && id >= 1 && (prop_inv_comb comb) && (prop_inv_hitbox2 z hitbox) && (prop_inv_coord_in_zone pos z) where
  comb@(Comb {positionc = pos, hitboxc = hitbox}) = initPerso id

initPerso :: Integer -> Combattant
initPerso 1 = initRyu
initPerso 2 = initKen 

initKen :: Combattant
initKen = Comb initCoordKen initHitboxKen G (Ok 100) (Bonus 0 0) 0 
initRyu :: Combattant
initRyu = Comb initCoordRyu initHitboxRyu D (Ok 100) (Bonus 0 0) 0 

-- # Invariant Zone ---------------------------------------------------------------------------
prop_inv_Zone :: Zone -> Bool
prop_inv_Zone (Zone x y) = x >= 0 && y >= 0

initZone :: Zone 
initZone = Zone 325 1330

pre_init_zone :: Integer -> Integer -> Bool
pre_init_zone x y = x >= 0 && y >= 0

initZoneXY :: Integer -> Integer -> Zone
initZoneXY x y = Zone x y 

safeInitZone :: Integer -> Integer -> Maybe Zone
safeInitZone x y | pre_init_zone x y = Just (initZoneXY x y)
                 | otherwise = Nothing

post_inv_initZoneXY :: Integer -> Integer -> Bool
post_inv_initZoneXY x y = 
  check (safeInitZone x y)
  where check Nothing = True
        check (Just z) = prop_inv_Zone z

-- # Invariants Jeu ---------------------------------------------------------------------------
prop_inv_jeu :: Jeu -> Bool
prop_inv_jeu jeu@(GameOver id) = id == 1 || id == 2
prop_inv_jeu jeu@(EnCours {joueur1 = ryu, joueur2 = ken, zoneJeu = z}) =
  prop_inv_Zone z && (prop_inv_comb_in_zone ryu z) && (prop_inv_comb_in_zone ken z)   

prop_post_initJeu :: Bool
prop_post_initJeu = prop_inv_jeu jeu where jeu = initJeu

initJeu :: Jeu 
initJeu = EnCours initRyu initKen initZone

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------                                                                              Getters                                                                               ------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pre_id :: Int -> Bool
pre_id 1 = True
pre_id 2 = True
pre_id _ = False

pre_get :: Jeu -> Bool
pre_get (EnCours _ _ _) = True
pre_get _ = False

get :: (Int -> Jeu -> Maybe a)-> Int -> Jeu -> Maybe a
get getter id jeu | pre_get jeu && pre_id id = getter id jeu

px :: Int -> Jeu -> Maybe Integer
px 1 j@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y)})}) = Just x
px 2 j@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y)})}) = Just x
px _ _ = Nothing

py :: Int -> Jeu -> Maybe Integer
py 1 j@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y)})}) = Just y
py 2 j@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y)})}) = Just y
py _ _ = Nothing

getDirection :: Int -> Jeu -> Maybe Direction
getDirection 1 jeu@(EnCours {joueur1 = ryu@(Comb {facec = d})}) = Just d
getDirection 2 jeu@(EnCours {joueur2 = ken@(Comb {facec = d})}) = Just d
getDirection _ _ = Nothing

getLife :: Int -> Jeu -> Maybe Integer
getLife 1 jeu@(EnCours {joueur1 = ryu@(Comb {etatx = (Ok sante)})}) = Just sante 
getLife 2 jeu@(EnCours {joueur2 = ken@(Comb {etatx = (Ok sante)})}) = Just sante 
getLife _ _ = Nothing

getBonus :: Int -> Jeu -> Maybe Jauge
getBonus 1 jeu@(EnCours {joueur1 = ryu@(Comb {bonus = b})}) = Just b
getBonus 2 jeu@(EnCours {joueur2 = ken@(Comb {bonus = b})}) = Just b
getBonus _ _ = Nothing

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------                                                                              Hitbox                                                                               ------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Fonctions d'initialisation des hitboxs de personnages en fonction de leur position et de leur figure (normal/coup de pied/coup de poind)
pre_hitBoxRepos_Pied :: Zone -> Coord -> Bool
pre_hitBoxRepos_Pied (Zone l h) c@(Coord x y) |prop_inv_coord c = (x+40) <= l && (y+20) <= h

hitboxRepos_Pied :: Coord -> Hitbox
hitboxRepos_Pied (Coord x y) = Rect (Coord (x+40) (y+20)) 120 300

post_HitboxRepos_Pied :: Zone -> Coord -> Bool 
post_HitboxRepos_Pied z c@(Coord x y)
 | pre_hitBoxRepos_Pied z c = check (hitboxRepos_Pied c) 
  where check h@(Rect (Coord nx ny) _ _) = nx == (x+40) && ny == (y+20) && prop_inv_hitbox h z
post_HitboxRepos_Pied _ _ = True -- pre condition pas valide, donc pas de changements

pre_hitBoxPoing :: Zone -> Coord -> Bool
pre_hitBoxPoing (Zone l h) c@(Coord x y) |prop_inv_coord c = (x+15) <= l && (y+20) <= h

hitboxPoing :: Coord -> Hitbox
hitboxPoing (Coord x y) = Rect (Coord (x+15) (y+20)) 170 300

post_HitboxPoing :: Zone -> Coord -> Bool 
post_HitboxPoing z c@(Coord x y) | pre_hitBoxPoing z c = check (hitboxPoing c)
  where check h@(Rect (Coord nx ny) _ _) = nx == (x+15) && ny == (y+20) && prop_inv_hitbox h z
post_HitboxPoing _ _ = True -- pre condition pas valide, donc pas de changements

hitboxRightPied :: Coord -> Hitbox
hitboxRightPied c@(Coord x y) = Composite ( (hitboxRepos_Pied c) <| (Rect (Coord (x+134) (y+20)) 66 40) <| Empty)
hitboxLeftPied :: Coord -> Hitbox
hitboxLeftPied c@(Coord x y) = Composite ( (hitboxRepos_Pied c) <| (Rect (Coord x (y+20)) 66 40) <| Empty)

post_HitboxRightPied :: Zone -> Coord -> Bool 
post_HitboxRightPied z c@(Coord x y) | pre_hitBoxRepos_Pied z c = check (hitboxRightPied c) where
  check h@(Composite ((Rect (Coord nx ny) _ _):<| h2@(Rect (Coord nx2 ny2) _ _):<| empty)) =
     nx == (x+40) && ny == (y+20) && nx2 == (x+134) && ny2 == (y+20) && prop_inv_hitbox h2 z
post_HitboxRightPied _ _ = True -- pre condition pas valide, donc pas de changements

post_HitboxLeftPied :: Zone -> Coord -> Bool 
post_HitboxLeftPied z c@(Coord x y) | pre_hitBoxRepos_Pied z c = check (hitboxLeftPied c) where
  check h@(Composite ((Rect (Coord nx ny) _ _):<| h2@(Rect (Coord nx2 ny2) _ _):<| empty)) =
     nx == (x+40) && ny == (y+20) && nx2 == x && ny2 == (y+20) && prop_inv_hitbox h2 z
post_HitboxLeftPied _ _ = True -- pre condition pas valide, donc pas de changements

hitboxRightPoing :: Coord -> Hitbox
hitboxRightPoing c@(Coord x y) = Composite ( (hitboxPoing c) <| (Rect (Coord (x+145) (y+20)) 50 110) <| Empty)
hitboxLeftPoing :: Coord -> Hitbox
hitboxLeftPoing c@(Coord x y) = Composite ( (hitboxPoing c) <| (Rect (Coord x (y+30)) 50 110) <| Empty)

post_HitboxRightPoing :: Zone -> Coord -> Bool 
post_HitboxRightPoing z c@(Coord x y) | pre_hitBoxPoing z c = check (hitboxRightPoing c) 
  where check h@(Composite ((Rect (Coord nx ny) _ _):<| h2@(Rect (Coord nx2 ny2) _ _):<| empty)) = nx == (x+15) && ny == (y+20) && nx2 == (x+145) && ny2 == (y+20) && prop_inv_hitbox h2 z
post_HitboxRightPoing _ _ = True -- pre condition pas valide, donc pas de changements

post_HitboxLeftPoing :: Zone -> Coord -> Bool 
post_HitboxLeftPoing z c@(Coord x y) | pre_hitBoxPoing z c = check (hitboxLeftPoing c) where
  check h@(Composite ((Rect (Coord nx ny) _ _):<| h2@(Rect (Coord nx2 ny2) _ _):<| empty)) =
     nx == (x+15) && ny == (y+20) && nx2 == x && ny2 == (y+30) && prop_inv_hitbox h2 z
post_HitboxLeftPoing _ _ = True -- pre condition pas valide, donc pas de changements

-- set des HItboxs
setXSeqHitbox :: Seq Hitbox -> Integer -> Seq Hitbox
setXSeqHitbox (h:<|hs) i = (setXHitbox h i) :<| (setXSeqHitbox hs i)
setXSeqHitbox Empty i = Empty

setXHitbox :: Hitbox -> Integer -> Hitbox 
setXHitbox hitbox i =
  case hitbox of
    Rect (Coord xh yh) l h -> Rect (Coord (xh + i) yh) l h
    Composite sh -> Composite (setXSeqHitbox sh i)

post_setXHitbox :: Hitbox -> Integer -> Bool
post_setXHitbox hitbox@(Rect (Coord xh yh) l h) i = xh + i == xn where (Rect (Coord xn yn) l h) = setXHitbox hitbox i
post_setXHitbox (Composite Empty) i = True
post_setXHitbox hitbox@(Composite ((Rect (Coord xh _) _ _) :<| hs)) i = xh + i == xn || (post_setXHitbox (Composite hs) i) where (Composite ((Rect (Coord xn _) _ _) :<| hsn)) = setXHitbox hitbox i

prop_set_unsetX :: Hitbox -> Integer -> Bool
prop_set_unsetX h i = setXHitbox (setXHitbox h (-i)) i == h 

setYSeqHitbox :: Seq Hitbox -> Integer -> Seq Hitbox
setYSeqHitbox (h:<|hs) i = (setYHitbox h i) :<| (setYSeqHitbox hs i)
setYSeqHitbox Empty i = Empty

setYHitbox :: Hitbox -> Integer -> Hitbox 
setYHitbox hitbox i =
  case hitbox of
    Rect (Coord xh yh) l h -> Rect (Coord xh (yh + i)) l h
    Composite sh -> Composite (setYSeqHitbox sh i)

post_setYHitbox :: Hitbox -> Integer -> Bool
post_setYHitbox hitbox@(Rect (Coord xh yh) l h) i = yh + i == yn where (Rect (Coord xn yn) l h) = setYHitbox hitbox i
post_setYHitbox hitbox@(Composite ((Rect (Coord _ yh) _ _) :<| hs)) i = yh + i == yn || (post_setYHitbox (Composite hs) i) where (Composite ((Rect (Coord _ yn) _ _) :<| hsn)) = setYHitbox hitbox i

prop_set_unsetY :: Hitbox -> Integer -> Bool
prop_set_unsetY h i = setYHitbox (setYHitbox h (-i)) i == h 

-- determine la liste des coordonnees d'une hitbox
frontiereHitboxs :: Seq Hitbox -> [Coord] -> [Coord]
frontiereHitboxs hitbox acc =
  case hitbox of
    hb@(Rect c@(Coord x y) l h) :<| hs -> frontiereHitboxs hs ((frontiereHitbox hb l h []) ++ acc)
    Empty -> acc

frontiereHitbox :: Hitbox -> Integer -> Integer -> [Coord] -> [Coord]
frontiereHitbox (Rect c@(Coord x y) l h) largeur_entiere hauteur_entiere acc 
 | l == 0 && h == 0 = c : acc
 | l == 0 = (frontiereHitbox (Rect c l (h-1)) largeur_entiere hauteur_entiere ([(Coord x (y + h))] ++ [(Coord (x + largeur_entiere) (y + h))] ++ acc))
 | h == 0 = (frontiereHitbox (Rect c (l-1) h) largeur_entiere hauteur_entiere ([(Coord (x + l) (y + hauteur_entiere))] ++ [(Coord (x + l) y)] ++ acc))
 | otherwise = (frontiereHitbox (Rect c (l-1) (h-1)) largeur_entiere hauteur_entiere ([(Coord x (y + h))] ++ [(Coord (x + largeur_entiere) (y + h))] ++ [(Coord (x + l) (y + hauteur_entiere))] ++ [(Coord (x + l) y)] ++ acc))

-- # FONCTION AUX de one_is_inHitbox
coord_is_in_Hitbox :: Coord -> Hitbox -> Bool
coord_is_in_Hitbox (Coord x y) hb@(Rect (Coord px py) largeur hauteur) 
 | x == px && y == py = True 
 | x >= px && x < (px + largeur) && y >= py && y < (py + hauteur) = True
 | otherwise = False
coord_is_in_Hitbox c@(Coord x y) (Composite hits) =
  case hits of
    hb :<| hs -> coord_is_in_Hitbox c hb
    Empty -> False

-- # FONCTION AUX de la fonction collision
-- Teste si au moins une des Coordonnees de la liste est contenue dans la hitbox donnee en parametre 
-- si oui, la fonction retourne True
-- False sinon
one_is_in_Hitbox :: Hitbox -> [Coord] -> Bool
one_is_in_Hitbox h liste = 
  case liste of
    [] -> False
    coord:ls -> (coord_is_in_Hitbox coord h) || (one_is_in_Hitbox h ls)

-- # FONCTION AUX de la fonction collision_move
collision :: Hitbox -> Hitbox -> Bool
collision h1@(Rect (Coord x1 y1) largeur1 hauteur1) h2@(Rect (Coord x2 y2) largeur2 hauteur2) =  
  one_is_in_Hitbox h1 (frontiereHitbox h2 largeur2 hauteur2 []) 
collision h1@(Rect (Coord x1 y1) largeur1 hauteur1) (Composite sh) = one_is_in_Hitbox h1 (frontiereHitboxs sh []) 
collision (Composite sh) h2@(Rect (Coord x2 y2) largeur2 hauteur2) = one_is_in_Hitbox h2 (frontiereHitboxs sh []) 
collision s@(Composite sh1) (Composite sh2) = one_is_in_Hitbox s (frontiereHitboxs sh2 []) 

-- # PRE CONDITION des fonctions de mouvements
-- si le corps des personnages se rencontrent alors ils ne peuvent plus avancer, le personnage en question doit contourner l'autre en sautant par dessus par exemple
-- le premier rectangle des hitbox represente le corps du personnage
-- Retourne True s'il y a une collision dans le jeu
collision_move :: Jeu -> Bool
collision_move jeu@(EnCours {joueur1 = ryu@(Comb {hitboxc = (Composite sh1)}), joueur2 = ken@(Comb {hitboxc = (Composite sh2)})}) = 
  collision (Composite (Seq.take 1 sh1)) (Composite (Seq.take 1 sh2)) 
collision_move jeu@(EnCours {joueur1 = ryu@(Comb {hitboxc = h1}), joueur2 = ken@(Comb {hitboxc = (Composite sh2)})}) = collision h1 (Composite (Seq.take 1 sh2)) 
collision_move jeu@(EnCours {joueur1 = ryu@(Comb {hitboxc = (Composite sh1)}), joueur2 = ken@(Comb {hitboxc = h2})}) = collision (Composite (Seq.take 1 sh1)) h2 
collision_move jeu@(EnCours {joueur1 = ryu@(Comb {hitboxc = h1}), joueur2 = ken@(Comb {hitboxc = h2})}) = collision h1 h2


-- # FONCTION AUX des fonctions PRE CONDITION de coup (ken_frappe_ryu et ryu_frappe_ken)
-- regarde si la hitbox qui frappe touche l'adversaire
-- (precision : la personne normal (au repos) n'a qu'un rectangle hitbox, elle n'a pas de hitbox frappante, on ne considere pas que la personne a touch'e avec un coup)
-- (            si la personne a une hitbox sous la forme d'un Composite (Seq Hitbox), alors
--                  - la hitbox d'index 0 correspond a son corps et non pas à son coup frappant, celle ci n'est pas comptabilis'e dans le coup 
--                    et donc si le corps touche l'adversaire alors on renvoie quand meme false car on ne frappe pas avec le corps
--                  - les hitbox d'index suivant sont les parties frappantes du personnage, si l'une des ses hitbox touchent l'adversaire alors on retourne True
--                     car le personnage touche l'adversaire avec son pied ou son poing en fonction de sa technique
-- )
-- le premier parametre correspond à celui qui frappe
toucheCoup :: Hitbox -> Hitbox -> Bool
toucheCoup h1@(Rect (Coord x1 y1) largeur1 hauteur1) _ =  False
toucheCoup h1@(Composite sh) h2 = collision (Composite (Seq.drop 1 sh)) h2

-- # FONCTIONS PRE CONDITIONS de frappeTouche
-- teste si ryu touche ken avec son pied ou son poing en fonction de sa technique courante
ryu_frappe_ken :: Jeu -> Bool
ryu_frappe_ken jeu@(EnCours {joueur1 = ryu@(Comb {hitboxc = (Rect _ _ _)})}) = False
ryu_frappe_ken jeu@(EnCours {joueur1 = ryu@(Comb {hitboxc = h1}), joueur2 = ken@(Comb {hitboxc = h2})}) = toucheCoup h1 h2 
ryu_frappe_ken _ = False
-- teste si ryu touche ken avec son pied ou son poing en fonction de sa technique courante
ken_frappe_ryu :: Jeu -> Bool
ken_frappe_ryu jeu@(EnCours {joueur2 = ken@(Comb {hitboxc = (Rect _ _ _)})}) = False
ken_frappe_ryu jeu@(EnCours {joueur1 = ryu@(Comb {hitboxc = h1}), joueur2 = ken@(Comb {hitboxc = h2})}) = toucheCoup h2 h1
ken_frappe_ryu _ = False

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------                                                                              Jauges                                                                               ------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pre_augmente_bonus :: Jeu -> Bool
pre_augmente_bonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = br}), joueur2 = ken@(Comb {bonus = bk})}) = 
  prop_inv_jauge br && prop_inv_jauge bk
pre_augmente_bonus _ = False

-- augmenteBonus augment les bonus des joueurs, il sera appeler à chaque boucle du jeu 
-- les bonus n'augmentent pas s'ils sont deja au Max
augmenteBonus :: Jeu -> Jeu
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = Max}), joueur2 = ken@(Comb {bonus = Max})}) = jeu
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = Max}), joueur2 = ken@(Comb {bonus = (Bonus b 100)})})
 | b + 10 >= 100 = jeu{joueur2 = ken{bonus = Max}}
 | b + 10 < 100 = jeu{joueur2 = ken{bonus = (Bonus (b + 1) 0)}}
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = (Bonus b 100)}), joueur2 = ken@(Comb {bonus = Max})})
 | b + 10 >= 100 = jeu{joueur1 = ryu{bonus = Max}}
 | b + 10 < 100 = jeu{joueur1 = ryu{bonus = (Bonus (b + 1) 0)}}
-- ken et ryu remplissent leur jauge au max
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = (Bonus bryu 100)}), joueur2 = ken@(Comb {bonus = (Bonus bken 100)})})
 | (bryu + 10) >= 100 && (bken + 10) >= 100 = jeu{joueur1 = ryu{bonus = Max}, joueur2 = ken{bonus = Max}}
 | (bryu + 10) >= 100 = jeu{joueur1 = ryu{bonus = Max}, joueur2 = ken{bonus = (Bonus (bken + 1) 0)}}
 | (bken + 10) >= 100 = jeu{joueur1 = ryu{bonus = (Bonus (bryu + 1) 0)}, joueur2 = ken{bonus = Max}}
 | otherwise = jeu{joueur1 = ryu{bonus = (Bonus (bryu + 1) 0)}, joueur2 = ken{bonus = (Bonus (bken + 1) 0)}}
-- si les deux jauges sont décalées
-- ryu en avance
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = (Bonus bryu 100)}), joueur2 = ken@(Comb {bonus = _})})
 | (bryu + 10) >= 100  = jeu{joueur1 = ryu{bonus = Max}}
 | otherwise = jeu{joueur1 = ryu{bonus = (Bonus (bryu + 1) 0)}}
-- ken en avance
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = _}), joueur2 = ken@(Comb {bonus = (Bonus bken 100)})})
 | (bken + 10) >= 100 = jeu{joueur2 = ken{bonus = Max}}
 | otherwise = jeu{joueur2 = ken{bonus = (Bonus (bken + 1) 0)}}
-- si le laps de tour n'est pas encore atteint, on augmente la jauge des deux personnages
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = (Bonus bryu lryu)}), joueur2 = ken@(Comb {bonus = (Bonus bken lken)})})
  | lken + 10 > 100 && lryu + 10 > 100 = jeu{joueur1 = ryu{bonus = (Bonus bryu 100)}, joueur2 = ken{bonus = (Bonus bken 100 )}}
  | lken + 10 > 100 = jeu{joueur1 = ryu{bonus = (Bonus bryu (lryu + 10))}, joueur2 = ken{bonus = (Bonus bken 100 )}}
  | lryu + 10 > 100 = jeu{joueur1 = ryu{bonus = (Bonus bryu 100)}, joueur2 = ken{bonus = (Bonus bken (lken + 10) )}}
  | otherwise = jeu{joueur1 = ryu{bonus = (Bonus bryu (lryu + 10))}, joueur2 = ken{bonus = (Bonus bken (lken + 10) )}}
-- la jauge de ryu est deja au max 
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = Max}), joueur2 = ken@(Comb {bonus = (Bonus b l)})})
  | l + 10 > 100 = jeu{joueur2 = ken{bonus = (Bonus b 100)}}
  | l < 100 = jeu{joueur2 = ken{bonus = (Bonus b (l+10))}}
-- la jauge de ken est deja au max 
augmenteBonus jeu@(EnCours {joueur1 = ryu@(Comb {bonus = (Bonus b l)}), joueur2 = ken@(Comb {bonus = Max})})
  | l + 10 > 100 = jeu{joueur1 = ryu{bonus = (Bonus b 100)}}
  | l < 100 = jeu{joueur1 = ryu{bonus = (Bonus b (l+10))}}
-- sinon
augmenteBonus jeu = jeu

safe_augmenteBonus :: Jeu -> Jeu 
safe_augmenteBonus jeu | pre_augmente_bonus jeu = augmenteBonus jeu
                       | otherwise = jeu

post_augmenteBonus :: Jeu -> Bool
post_augmenteBonus jeu | pre_augmente_bonus jeu = check (augmenteBonus jeu) where
  check jeu@(EnCours {joueur1 = ryu@(Comb {bonus = br}), joueur2 = ken@(Comb {bonus = bk})}) = 
     prop_inv_jauge br && prop_inv_jauge bk -- && prop_inv_jeu jeu --prop_inv_Zone z && (prop_inv_comb_in_zone ryu z) && (prop_inv_comb_in_zone ken z)   
  check (GameOver _) = True

-- # PRE CONDITION de l'utilisation du coup special 
is_jauge_max :: Int -> Jeu -> Bool 
is_jauge_max 1 jeu@(EnCours {joueur1 = ryu@(Comb {bonus = Max})}) = True
is_jauge_max 2 jeu@(EnCours {joueur2 = ken@(Comb {bonus = Max})}) = True
is_jauge_max _ _ = False

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------                                                                             Mouvement                                                                             ------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- pre condition des mouvements, si on detecte une collision des hitboxs (corps (premier rectangle de la hitbox)) alors on renvoie false
safeMouv :: (Jeu -> Jeu) -> Jeu -> Bool
safeMouv move jeu | collision_move (move jeu) = False
                  | otherwise = True

------------------------------------                                                 Left 
-- fonction interne
moveLefti :: Int -> Integer -> Jeu -> Jeu
moveLefti 1 mouv j@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y), hitboxc = h})}) 
 | (x - mouv) < 0 = j{joueur1 = ryu{positionc = (Coord 0 y), hitboxc = (hitboxRepos_Pied (Coord 0 y)) , facec = G}}
 | otherwise = j{joueur1 = ryu{positionc = (Coord (x - mouv) y), hitboxc = (hitboxRepos_Pied (Coord (x - mouv) y)), facec = G}}
moveLefti 2 mouv j@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y), hitboxc = h})}) 
 | (x - mouv) < 0 = j{joueur2 = ken{positionc = (Coord 0 y), hitboxc = (hitboxRepos_Pied (Coord 0 y)), facec = G}}
 | otherwise = j{joueur2 = ken{positionc = (Coord (x - mouv) y), hitboxc = (hitboxRepos_Pied (Coord (x - mouv) y)), facec = G}}

-- fonction externe permettant de se deplacer vers la gauche  (utilise la precondition definie avec safeMouv)
safeMoveLeft :: Int -> Integer -> Jeu -> Jeu
safeMoveLeft 1 v jeu
 | safeMouv (moveLefti 1 v) jeu = moveLefti 1 v jeu 
 | v > 1 = safeMoveLeft 1 (v - 1) jeu
 | otherwise = jeu
safeMoveLeft 2 v jeu
 | safeMouv (moveLefti 2 v) jeu = moveLefti 2 v jeu 
 | v > 1 = safeMoveLeft 2 (v - 1) jeu
 | otherwise = jeu

postMoveLeft :: Int -> Integer -> Jeu -> Bool
postMoveLeft 1 v jeu@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y)})}) 
 | safeMouv (moveLefti 1 v) jeu && (x - v) < 0 = (nx == 0 && y == ny)
 | (safeMouv (moveLefti 1 v) jeu) = (nx == (x-v) && y == ny) 
 | otherwise = True 
 where j@(EnCours {joueur1 = (Comb {positionc = (Coord nx ny)})}) = (safeMoveLeft 1 v jeu)
postMoveLeft 2 v jeu@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y)})}) 
 | safeMouv (moveLefti 2 v) jeu && x - v < 0 = (nx == 0 && y == ny)
 | (safeMouv (moveLefti 2 v) jeu) = nx == (x-v) && y == ny
 | otherwise = True
  where j@(EnCours {joueur2 = (Comb {positionc = (Coord nx ny)})}) = safeMoveLeft 2 v jeu

same_pos :: Int -> Jeu -> Jeu -> Bool
same_pos 1 jeu1@(EnCours {joueur1 = (Comb {positionc = (Coord x1 y1)})}) jeu2@(EnCours {joueur1 = (Comb {positionc = (Coord x2 y2)})}) = x1 == x2 && y1 == y2 
same_pos 2 jeu1@(EnCours {joueur2 = (Comb {positionc = (Coord x1 y1)})}) jeu2@(EnCours {joueur2 = (Comb {positionc = (Coord x2 y2)})}) = x1 == x2 && y1 == y2 

prop_moveleft_unmove_right :: Int -> Integer -> Jeu -> Bool
prop_moveleft_unmove_right 1 i jeu@(EnCours {joueur1 = (Comb{positionc = Coord x y})}) | x < (1330 - i) = same_pos 1 (moveLefti 1 i (moveRighti 1 i jeu)) jeu 
                                    | otherwise = True
prop_moveleft_unmove_right 2 i jeu@(EnCours {joueur2 = (Comb{positionc = Coord x y})}) | x < (1330 - i) = same_pos 2 (moveLefti 2 i (moveRighti 2 i jeu)) jeu 
                                    | otherwise = True
prop_moveleft_unmove_right _ i _ = True

------------------------------------                                                 Right 
-- fonction interne
moveRighti :: Int -> Integer -> Jeu -> Jeu
moveRighti 1 mouv j@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y), hitboxc = h})}) 
 | (x + mouv) > 1330 = j{joueur1 = ryu{positionc = (Coord 1330 y), hitboxc = (hitboxRepos_Pied (Coord 1330 y)), facec = D}}
 | otherwise = j{joueur1 = ryu{positionc = (Coord (x + mouv) y), hitboxc = (hitboxRepos_Pied (Coord (x + mouv) y)), facec = D}}
moveRighti 2 mouv j@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y), hitboxc = h})}) 
 | (x + mouv) > 1330 = j{joueur2 = ken{positionc = (Coord 1330 y), hitboxc = (hitboxRepos_Pied (Coord 1330 y)), facec = D}}
 | otherwise = j{joueur2 = ken{positionc = (Coord (x + mouv) y), hitboxc = (hitboxRepos_Pied (Coord (x + mouv) y)), facec = D}}

--fonction externe permettant de bouger vers la droite sans passer a travers l'autre personnage et sans sortir de la zone
safeMoveRight :: Int -> Integer -> Jeu -> Jeu
safeMoveRight 1 v jeu
 | safeMouv (moveRighti 1 v) jeu = moveRighti 1 v jeu 
 | v > 1 = safeMoveRight 1 (v - 1) jeu
 | otherwise = jeu
safeMoveRight 2 v jeu
 | safeMouv (moveRighti 2 v) jeu = moveRighti 2 v jeu 
 | v > 1 = safeMoveRight 2 (v - 1) jeu
 | otherwise = jeu

postMoveRight :: Int -> Integer -> Jeu -> Bool
postMoveRight 1 v jeu@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y)})}) 
 | safeMouv (moveRighti 1 v) jeu && (x + v) > 1330 = (nx == 1330 && y == ny)
 | (safeMouv (moveRighti 1 v) jeu) = (nx == (x+v) && y == ny) 
 | otherwise = True 
 where j@(EnCours {joueur1 = (Comb {positionc = (Coord nx ny)})}) = (safeMoveRight 1 v jeu)
postMoveRight 2 v jeu@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y)})}) 
 | safeMouv (moveRighti 2 v) jeu && x + v > 1330 = (nx == 1330 && y == ny)
 | (safeMouv (moveRighti 2 v) jeu) = nx == (x+v) && y == ny
 | otherwise = True
  where j@(EnCours {joueur2 = (Comb {positionc = (Coord nx ny)})}) = safeMoveRight 2 v jeu
------------------------------------                                                 UP 
-- fonction interne
moveUpi :: Int -> Integer -> Jeu -> Jeu
moveUpi 1 mouv j@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y), hitboxc = h})}) 
 | (y - mouv) < 145 = j{joueur1 = ryu{positionc = (Coord x 145), hitboxc = (setYHitbox h (145 - y)), facec = H, gravite = 0}}
 | otherwise = j{joueur1 = ryu{positionc = (Coord x (y - mouv)), hitboxc = (setYHitbox h (-mouv)), facec = H}}
moveUpi 2 mouv j@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y), hitboxc = h})}) 
 | (y - mouv) < 145 = j{joueur2 = ken{positionc = (Coord x 145), hitboxc = (setYHitbox h (145 - y)), facec = H, gravite = 0}}
 | otherwise = j{joueur2 = ken{positionc = (Coord x (y - mouv)), hitboxc = (setYHitbox h (-mouv)) , facec = H}}
-- fonction externe permettant de sauter sans aller trop haut
safeMoveUp :: Int -> Integer -> Jeu -> Jeu
safeMoveUp 1 v jeu
 | safeMouv (moveUpi 1 v) jeu = moveUpi 1 v jeu 
 | v > 1 = safeMoveUp 1 (v - 1) jeu
 | otherwise = jeu
safeMoveUp 2 v jeu
 | safeMouv (moveUpi 2 v) jeu = moveUpi 2 v jeu 
 | v > 1 = safeMoveUp 2 (v - 1) jeu
 | otherwise = jeu

------------------------------------                                                 Chute 
-- fonctions internes
chuteRyu :: Jeu -> Jeu
chuteRyu j@(EnCours {joueur1 = ryu@(Comb {positionc = (Coord x y), hitboxc = h, gravite = sr})})
 | y < 145 = j{joueur1 = ryu{positionc = (Coord x 145), hitboxc = (setYHitbox h (145 - y)), gravite = 0}}
 | y == 145 = j{joueur1 = ryu{positionc = (Coord x (y + sr)), hitboxc = (setYHitbox h sr), gravite = ((480 - (480 - y))`div` 15)}}
 | y > 145 && y < 480 = j{joueur1 = ryu{positionc = (Coord x (y + sr)), hitboxc = (setYHitbox h sr), gravite = ((480 - (480 - y))`div` 15)}}
 | y >= 480 = j{joueur1 = ryu{positionc = (Coord x 480), hitboxc = (setYHitbox h (-(y-480))), gravite = 0}}
 | (y + sr) < 145 = j{joueur1 = ryu{positionc = (Coord x 480), hitboxc = (setYHitbox h (-(y-480))), gravite = 0}}

chuteKen :: Jeu -> Jeu
chuteKen j@(EnCours {joueur2 = ken@(Comb {positionc = (Coord x y), hitboxc = h, gravite = sk})})
 | y < 145 = j{joueur2 = ken{positionc = (Coord x 145), hitboxc = (setYHitbox h (145 - y)), gravite = 0}}
 | y == 145 = j{joueur2 = ken{positionc = (Coord x (y + sk)), hitboxc = (setYHitbox h sk), gravite = ((480 - (480 - y))`div` 15)}}
 | y > 145 && y < 480 = j{joueur2 = ken{positionc = (Coord x (y + sk)), hitboxc = (setYHitbox h sk), gravite = ((480 - (480 - y))`div` 15)}}
 | y >= 480 = j{joueur2 = ken{positionc = (Coord x 480), hitboxc = (setYHitbox h (-(y-480))), gravite = 0}}
 | (y + sk) < 145 = j{joueur2 = ken{positionc = (Coord x 480), hitboxc = (setYHitbox h (-(y-480))), gravite = 0}}

-- fonctions externes
safeChute :: Int -> Jeu -> Jeu 
safeChute 1 jeu | safeMouv chuteRyu jeu = chuteRyu jeu
safeChute 2 jeu | safeMouv chuteKen jeu = chuteKen jeu
safeChute _ jeu = jeu

post_inv_chute :: Int -> Jeu -> Bool
post_inv_chute 1 jeu | prop_inv_jeu jeu = prop_inv_jeu (safeChute 1 jeu) 
post_inv_chute 2 jeu | prop_inv_jeu jeu = prop_inv_jeu (safeChute 2 jeu) 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------                                                                           Barres de Vie                                                                           ------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pre_setVie :: Int -> Integer -> Jeu -> Bool
pre_setVie 1 modif jeu@(EnCours {joueur1 = (Comb {etatx = e@(Ok v)})}) = prop_inv_etat e
pre_setVie 2 modif jeu@(EnCours {joueur2 = (Comb {etatx = e})}) = prop_inv_etat e
pre_setVie _ _ _ = False

setVie :: Int -> Integer -> Jeu -> Jeu
setVie 1 modif jeu@(EnCours {joueur1 = ryu@(Comb {etatx = (Ok etat_ryu)})}) 
 | etat_ryu + modif > 100 = jeu{joueur1 = ryu{etatx = (Ok 100)}}
 | etat_ryu + modif > 0 = jeu{joueur1 = ryu{etatx = (Ok (etat_ryu + modif))}}
 | otherwise = GameOver 2
setVie 2 modif jeu@(EnCours {joueur2 = ken@(Comb {etatx = (Ok etat_ken)})})
 | etat_ken + modif > 100 = jeu{joueur2 = ken{etatx = (Ok 100)}}
 | etat_ken + modif > 0 = jeu{joueur2 = ken{etatx = (Ok (etat_ken + modif))}}
 | otherwise = GameOver 1
setVie _ _ jeu = jeu

prop_post_inv_setVie :: Int -> Integer -> Jeu -> Bool
prop_post_inv_setVie 1 modif jeu
 | pre_setVie 1 modif jeu = prop_inv_etat nwe 
 | otherwise = True
 where (EnCours {joueur1 = (Comb _ _ _ nwe _ _ )}) = setVie 1 modif jeu
prop_post_inv_setVie 2 modif jeu
 | pre_setVie 2 modif jeu = prop_inv_etat nwe 
 | otherwise = True
 where (EnCours {joueur1 = (Comb _ _ _ nwe _ _ )}) = setVie 1 modif jeu
prop_post_inv_setVie _ _ _ = True

post_setVie :: Int -> Integer -> Jeu -> Bool
post_setVie 1 modif jeu@(EnCours {joueur1 = (Comb {etatx = (Ok etat_ryu)})}) = 
  check (setVie 1 modif jeu) etat_ryu modif
  where check (GameOver _) e m = (e-m) <= 0
        check (EnCours (Comb _ _ _ (Ok nv_etat) _ _ ) _ _ ) e m = (e + m > 100 && nv_etat == 100) || (e + m > 0 && nv_etat == e + m)
post_setVie 2 modif jeu@(EnCours {joueur2 = (Comb {etatx = (Ok etat_ken)})}) =
  check (setVie 2 modif jeu) etat_ken modif
  where check (GameOver _) e m = (e-m) <= 0
        check (EnCours _ (Comb _ _ _ (Ok nv_etat) _ _ ) _ ) e m = (e + m > 100 && nv_etat == 100) || (e + m > 0 && nv_etat == e + m)
post_setVie _ modif jeu = True

---- Extensions Regain d'energie ---- 
pre_majTour :: Integer -> Maybe Integer -> Integer -> Bool
pre_majTour nwvie (Just oldvie) cpt = nwvie > 0 && nwvie <= 100 && oldvie > 0 && oldvie <= 100 && cpt >= 0

majTour :: Integer -> Maybe Integer -> Integer -> Integer
majTour nwvie j@(Just oldvie) cpt | (pre_majTour nwvie j cpt) && nwvie == oldvie = cpt + 1
                                  | otherwise = 0
majTour nwvie Nothing cpt = cpt

prop_postMajTour :: Integer -> Maybe Integer -> Integer -> Bool
prop_postMajTour nwvie ov@(Just oldvie) cpt
 |( pre_majTour nwvie ov) cpt && nwvie == oldvie = res == cpt + 1 
 | otherwise = res == 0
  where res = (majTour nwvie ov cpt) 
prop_postMajTour nwvie Nothing cpt = True --pas de changements
-------------
pre_cond_gain_vie :: Int -> Integer -> Integer -> Integer -> Jeu -> Bool
pre_cond_gain_vie id condition nb_tour gain jeu = pre_id id && condition > 0 && nb_tour >= 0 && gain > 0 

condition_gain_vie :: Int -> Integer -> Integer -> Integer -> Jeu -> Jeu
condition_gain_vie id condition nb_tour gain jeu
 | (pre_cond_gain_vie id condition nb_tour gain jeu) && nb_tour /= 0 && condition == (nb_tour `mod` condition) + condition = setVie id gain jeu
 | otherwise = jeu

prop_post_gain_vie :: Int -> Integer -> Integer -> Integer -> Jeu -> Bool
prop_post_gain_vie 1 condmintour nb_tour gain jeu@(EnCours {joueur1 = (Comb {etatx = (Ok etat_ryu)})}) = 
  check (condition_gain_vie 1 condmintour nb_tour gain jeu)
  where check (EnCours (Comb _ _ _ (Ok nv_etat) _ _ ) _ _ ) = ((pre_cond_gain_vie 1 condmintour nb_tour gain jeu) && nb_tour /= 0 && condmintour == (nb_tour `mod` condmintour) + condmintour && nv_etat == etat_ryu + gain)
                                                              || (nv_etat == etat_ryu)
        check _ = True
prop_post_gain_vie 2 condmintour nb_tour gain jeu@(EnCours {joueur1 = (Comb {etatx = (Ok etat_ken)})}) = 
  check (condition_gain_vie 2 condmintour nb_tour gain jeu)
  where check (EnCours _ (Comb _ _ _ (Ok nv_etat) _ _ ) _ ) = ((pre_cond_gain_vie 2 condmintour nb_tour gain jeu) && nb_tour /= 0 && condmintour == (nb_tour `mod` condmintour) + condmintour && nv_etat == etat_ken + gain)
                                                              || (nv_etat == etat_ken)
        check _ = True
prop_post_gain_vie _ _ _ _ _  = True 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------                                                                              Frappes                                                                              ------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pre_perte :: Integer -> Bool
pre_perte perte = 
  perte < 100 && perte > 0  
pre_frappe :: Direction -> Bool
pre_frappe dir = dir == D || dir == G

frappeTouche :: Integer -> Jeu -> Jeu
frappeTouche perte jeu 
  | (pre_perte perte) && ryu_frappe_ken jeu && ken_frappe_ryu jeu = setVie 1 (-perte) (setVie 2 (-perte) jeu)
  | (pre_perte perte) && ryu_frappe_ken jeu = setVie 2 (-perte) jeu
  | (pre_perte perte) && ken_frappe_ryu jeu = setVie 1 (-perte) jeu
  | otherwise = jeu

post_frappeTouche :: Integer -> Jeu -> Bool
post_frappeTouche perte jeu@(EnCours {joueur1 = ryu@(Comb {etatx = (Ok etat_ryu)}), joueur2 = ken@(Comb {etatx = (Ok etat_ken)})}) = 
  check (frappeTouche perte jeu) jeu etat_ryu etat_ken perte
  where check (GameOver id) jeu etat_ryu etat_ken perte = (ryu_frappe_ken jeu && etat_ken <= perte && id == 1) || (ken_frappe_ryu jeu && etat_ryu <= perte && id == 2)
        check (EnCours (Comb _ _ _ (Ok nv_etat_r) _ _) (Comb _ _ _ (Ok nv_etat_k) _ _) (Zone _ _)) jeu etat_ryu etat_ken perte 
          | (pre_perte perte) && ryu_frappe_ken jeu && ken_frappe_ryu jeu && etat_ryu > perte && etat_ken > perte = nv_etat_r == (etat_ryu - perte) && nv_etat_k == (etat_ken - perte) 
          | (pre_perte perte) && ryu_frappe_ken jeu && etat_ken > perte = nv_etat_k == (etat_ken - perte) 
          | (pre_perte perte) && ken_frappe_ryu jeu && etat_ryu > perte = nv_etat_r == (etat_ryu - perte)
          | otherwise = True -- il n'y pas de changement

prop_post_inv_frappeTouche :: Integer -> Jeu -> Bool
prop_post_inv_frappeTouche perte jeu = post_frappeTouche perte jeu && prop_inv_jeu (frappeTouche perte jeu)

---------------------------------------
-- assure que le joueur est en position repos avant de frapper
pre_frappePerso :: Int -> Jeu -> Bool
pre_frappePerso 1 jeu@(EnCours {joueur1 = (Comb {positionc = c, hitboxc = h})}) = h == (hitboxRepos_Pied c)
pre_frappePerso 2 jeu@(EnCours {joueur2 = (Comb {positionc = c, hitboxc = h})}) = h == (hitboxRepos_Pied c)
pre_frappePerso _ _ = True

frappeRyu :: Bool -> Jeu -> Jeu
frappeRyu True jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 1 jeu && dir == D  && (is_jauge_max 1 jeu) = frappeTouche 5 (jeu{joueur1 = ryu{facec = BD, hitboxc = (hitboxRightPoing c)}})
frappeRyu True jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 1 jeu && dir == G  && (is_jauge_max 1 jeu) = frappeTouche 5 (jeu{joueur1 = ryu{facec = BG, hitboxc = (hitboxLeftPoing c)}})
frappeRyu False jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 1 jeu && dir == D = frappeTouche 2 (jeu{joueur1 = ryu{facec = HD, hitboxc = (hitboxRightPied c)}})
frappeRyu False jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 1 jeu && dir == G = frappeTouche 2 (jeu{joueur1 = ryu{facec = HG, hitboxc = (hitboxLeftPied c)}})
frappeRyu _ j = j

frappeKen :: Bool-> Jeu ->  Jeu
frappeKen True jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 2 jeu && dir == D  && (is_jauge_max 2 jeu) = frappeTouche 5 (jeu{joueur2 = ken{facec = BD, hitboxc = (hitboxRightPoing c)}})
frappeKen True jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 2 jeu && dir == G  && (is_jauge_max 2 jeu) = frappeTouche 5 (jeu{joueur2 = ken{facec = BG, hitboxc = (hitboxLeftPoing c)}})
frappeKen False jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 2 jeu && dir == D = frappeTouche 1 (jeu{joueur2 = ken{facec = HD, hitboxc = (hitboxRightPied c)}})
frappeKen False jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | pre_frappePerso 2 jeu && dir == G = frappeTouche 1 (jeu{joueur2 = ken{facec = HG, hitboxc = (hitboxLeftPied c)}})
frappeKen _ j = j

---- Frappe special et jauges
prop_post_coup_special_ou_non :: Int -> Bool -> Jeu -> Bool
prop_post_coup_special_ou_non 1 iscoupspecial jeu@(EnCours {joueur1 = (Comb {facec = dir}), joueur2 = (Comb {etatx = (Ok ek)})}) | pre_frappe dir && pre_frappePerso 1 jeu =
  check (frappeRyu iscoupspecial jeu)
  where check (EnCours _ (Comb _ _ _ (Ok nvek) _ _) _) | iscoupspecial && is_jauge_max 1 jeu && ryu_frappe_ken jeu = nvek == ek - 5
                                                       | iscoupspecial && not (is_jauge_max 1 jeu) && ryu_frappe_ken jeu = nvek == ek
                                                       | not iscoupspecial && ryu_frappe_ken jeu = nvek == ek - 2
                                                       | otherwise = True -- pas de changement
        check (GameOver id) = id == 1 && ((iscoupspecial && ek < 5) || (not iscoupspecial && ek < 2))
prop_post_coup_special_ou_non 2 iscoupspecial jeu@(EnCours {joueur1 = (Comb {etatx = (Ok er)}),joueur2 = (Comb {facec = dir})}) | pre_frappe dir && pre_frappePerso 2 jeu = 
  check (frappeKen iscoupspecial jeu)
  where check  (EnCours (Comb _ _ _ (Ok nver) _ _) _ _) | iscoupspecial && is_jauge_max 2 jeu && ken_frappe_ryu jeu = nver == er - 5
                                                        | iscoupspecial && not (is_jauge_max 2 jeu) && ken_frappe_ryu jeu = nver == er
                                                        | not iscoupspecial && ken_frappe_ryu jeu = nver == er - 1
                                                        | otherwise = True -- pas de changement
        check (GameOver id) = id == 2 && ((iscoupspecial && er < 5) || (not iscoupspecial && er < 1))
prop_post_coup_special_ou_non _ _ _ = True

-- verifie le principe de la jauge qui doit etre remis a zero quand elle est utilisee
prop_post_reset_jauge :: Int -> Bool -> Jeu -> Bool
prop_post_reset_jauge 1 True jeu@(EnCours {joueur1 = (Comb {bonus = (Bonus 100 _)})}) = check (frappeRyu True jeu)
  where check (EnCours (Comb _ _ _ _ (Bonus x l) _) _ _) = x == 0 && l == 0
        check (GameOver id) = id == 1
prop_post_reset_jauge 2 True jeu@(EnCours {joueur2 = (Comb {bonus = (Bonus 100 _)})}) = check (frappeKen True jeu)
  where check (EnCours _ (Comb _ _ _ _ (Bonus x l) _) _) = x == 0 && l == 0
        check (GameOver id) = id == 2 
prop_post_reset_jauge _ _ _ = True 

all_prop_jauge :: Int -> Bool -> Jeu -> Bool
all_prop_jauge id iscoupspecial jeu = prop_post_coup_special_ou_non id iscoupspecial jeu && prop_post_reset_jauge id iscoupspecial jeu


---- retour en position precedente ----
retourRyu :: Jeu -> Jeu
retourRyu jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | dir == BD = jeu{joueur1 = ryu{facec = D, bonus = (Bonus 0 0), hitboxc = (hitboxRepos_Pied c)}} -- la jauge de ryu est remise a zero 
retourRyu jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | dir == BG = jeu{joueur1 = ryu{facec = G, bonus = (Bonus 0 0), hitboxc = (hitboxRepos_Pied c)}} -- la jauge de ryu est remise a zero 
retourRyu jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | dir == HD = jeu{joueur1 = ryu{facec = D, hitboxc = (hitboxRepos_Pied c)}}
retourRyu jeu@(EnCours {joueur1 = ryu@(Comb {facec = dir, positionc = c})}) | dir == HG = jeu{joueur1 = ryu{facec = G, hitboxc = (hitboxRepos_Pied c)}}
retourRyu j = j

-- verifie que l'on revient à bonus 0 0 apres la frappe special

retourKen :: Jeu -> Jeu
retourKen jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | dir == BD = jeu{joueur2 = ken{facec = D, bonus = (Bonus 0 0), hitboxc = (hitboxRepos_Pied c)}} -- la jauge de ken est remise a zero 
retourKen jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | dir == BG = jeu{joueur2 = ken{facec = G, bonus = (Bonus 0 0), hitboxc = (hitboxRepos_Pied c)}} -- la jauge de ken est remise a zero 
retourKen jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | dir == HD = jeu{joueur2 = ken{facec = D, hitboxc = (hitboxRepos_Pied c)}}
retourKen jeu@(EnCours {joueur2 = ken@(Comb {facec = dir, positionc = c})}) | dir == HG = jeu{joueur2 = ken{facec = G, hitboxc = (hitboxRepos_Pied c)}}
retourKen j = j

prop_post_retourNormal :: Int -> Jeu -> Bool
prop_post_retourNormal 1 jeu@(EnCours {joueur1 = (Comb {facec = old, positionc = c})}) = check old (retourRyu jeu)
  where check old (EnCours {joueur1 = (Comb {facec = dir, hitboxc = h})}) | old == BD  && old == HD = dir == D && h == (hitboxRepos_Pied c)
                                                                          | old == BG  && old == HG = dir == G && h == (hitboxRepos_Pied c)
                                                                          | otherwise = True --pas de changements
prop_post_retourNormal 2 jeu@(EnCours {joueur2 = (Comb {facec = old, positionc = c})}) = check old (retourKen jeu)
  where check old (EnCours {joueur2 = (Comb {facec = dir, hitboxc = h})}) | old == BD  && old == HD = dir == D && h == (hitboxRepos_Pied c)
                                                                          | old == BG  && old == HG = dir == G && h == (hitboxRepos_Pied c)
                                                                          | otherwise = True --pas de changements
prop_post_retourNormal _ _ = True

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gameStep :: RealFrac a => Jeu -> Keyboard -> a -> Jeu
gameStep jeu kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then (safeMoveLeft 2 10) else id)
              .
              (if K.keypressed KeycodeRight kbd
               then (safeMoveRight 2 10) else id)
              .
              (if K.keypressed KeycodeUp kbd
               then (safeMoveUp 2 30) else id)
              . --- Mouvement du deuxieme personnage
              (if K.keypressed KeycodeQ kbd
               then (safeMoveLeft 1 10) else id)
              .
              (if K.keypressed KeycodeD kbd
               then (safeMoveRight 1 10) else id)
              .
              (if K.keypressed KeycodeZ kbd
               then (safeMoveUp 1 30) else id)
              . --- La Chute
              (if (K.keyreleased KeycodeUp kbd)
              then (safeChute 2) else id)
              .
              (if (K.keyreleased KeycodeZ kbd)
              then (safeChute 1) else id)
  in modif jeu

gameStepCoup :: RealFrac a => Jeu -> Keyboard -> a -> Jeu
gameStepCoup jeu kbd deltaTime =
  let modif = (if (K.keypressed KeycodeSpace kbd)
               then (frappeRyu True) else id)
              .
              (if K.keypressed KeycodeS kbd 
               then (frappeRyu False) else id)
              .
              (if (K.keyreleased KeycodeS kbd) && (K.keyreleased KeycodeSpace kbd)
               then retourRyu else id)
              .
              (if K.keypressed KeycodeDown kbd
               then (frappeKen False) else id)
              .
              (if (K.keyreleased KeycodeDown kbd) && (K.keyreleased KeycodeReturn kbd)
               then retourKen else id)
              .
              (if (K.keypressed KeycodeReturn kbd) 
               then (frappeKen True) else id)
              .
              (if not (K.keypressed KeycodeReturn kbd)
               then retourKen else id)
  in modif jeu