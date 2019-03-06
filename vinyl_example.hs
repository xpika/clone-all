{-#LANGUAGE DataKinds#-}
{-#LANGUAGE PolyKinds#-}
{-#LANGUAGE TypeApplications#-}
{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE TypeFamilies#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE NoMonomorphismRestriction#-}
{-#LANGUAGE GADTs#-}
{-#LANGUAGE TypeSynonymInstances#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE StandaloneDeriving#-}

import Data.Vinyl
import Data.Vinyl.Functor
import Control.Applicative
import Control.Lens hiding (Identity)
import Control.Lens.TH
import Data.Char
import Test.DocTest
import Data.Singletons.TH (genSingletons)
import Data.Maybe

data Fields = Name | Age | Sleeping | Master deriving Show

type LifeForm = [Name, Age, Sleeping]

type family ElF (f :: Fields) :: * where
  ElF Name = String
  ElF Age = Int
  ElF Sleeping = Bool
  ElF Master = Rec Attr LifeForm
newtype Attr f = Attr { _unAttr :: ElF f }
makeLenses ''Attr
genSingletons [ ''Fields ]
instance Show (Attr Name) where show (Attr x) = "name: " ++ show x
instance Show (Attr Age) where show (Attr x) = "age: " ++ show x
instance Show (Attr Sleeping) where show (Attr x) = "sleeping: " ++ show x
instance Show (Attr Master) where show (Attr x) = "master: " ++ show x


(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

jon = (SName =:: "jon")
       :& (SAge =:: 23)
       :& (SSleeping =:: True)
       :& RNil


wakeUp :: (Sleeping ∈ fields) => Rec Attr fields -> Rec Attr fields
wakeUp = rput $ SSleeping =:: False


main = do
       print jon
       let jon' = wakeUp jon
       print jon'
