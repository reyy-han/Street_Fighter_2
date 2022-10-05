import Test.Hspec

import TypesSpec as TS
import HitboxSpec as HS
import MouvSpec as MS
import FrappeSpec as FS
import BarreVieSpec as BS

main :: IO ()
main = hspec $ do
    -- types
    TS.zoneSpecInit
    TS.zoneSpecGenOk
    TS.coordSpecInit
    TS.hitboxSpecGenOk
    TS.hitboxSpecInit
    TS.combSpecGenOk
    TS.combSpecInit
    TS.jeuEnCoursSpecGenOk
    -- Hitbox
    HS.hitboxReposPiedSpecInit
    HS.hitboxReposPoingSpecInit
    HS.hitboxReposLeftPoingSpecInit
    HS.hitboxReposRightPoingSpecInit
    HS.hitboxReposLeftPiedSpecInit
    HS.hitboxReposRightPiedSpecInit
    HS.hitboxSetXSpec
    HS.hitboxSetXPropSpec1
    HS.hitboxSetXPropSpec2
    HS.hitboxSetYSpec
    HS.hitboxSetYPropSpec1
    HS.hitboxSetYPropSpec2
    -- Mouvements
    MS.moveLeftSpec1
    MS.moveLeftSpec2
    MS.moveRightSpec1
    MS.moveRightSpec2
    MS.movePropSpec1
    MS.moveUpSpec1
    MS.moveUpSpec2
    MS.postMoveLeftAndRightSpec
    MS.chuteSpec
    -- Frappes & Jauges
    FS.prop_inv_JaugeSpec                   -- extension (1) - jauge avec coup special
    FS.prop_post_augmenteJaugeSpec          -- extension (1) - jauge avec coup special
    FS.prop_pre_jauge_max                   -- extension (1) - jauge avec coup special
    FS.prop_pre_ken_frappe_ryuSpec
    FS.post_frappeTouchSpec
    FS.prop_inv_post_frappeTouchSpec
    FS.prop_post_coup_special_ou_nonSpec    -- extension (1) - jauge avec coup special
    FS.prop_post_frappe_specialSpec         -- extension (1) - jauge avec coup special
    FS.all_prop_post_JaugeSpec              -- extension (1) - jauge avec coup special
    FS.prop_post_retourNormalSpec
    -- Barre de vie
    BS.setVieSPec
    BS.post_setVieSpec
    BS.prop_postMajTourSpec     -- extension (2) - regain d'energie
    BS.prop_post_gain_vieSpec   -- extension (2) - regain d'energie
