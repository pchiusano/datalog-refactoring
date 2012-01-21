{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module PrettyPrint where

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (($$),(<>),(<+>))

import Control.Applicative

import Backend

class Pretty p where
    doc :: p -> PP.Doc

instance Pretty Con where
    doc = PP.text . conName 

instance Pretty Var where
    doc = PP.text . varName

instance Pretty Term where
    doc = eitherTerm doc doc 

instance Pretty a => Pretty (Atom a) where
    doc (Atom p b) = doc p <> PP.parens (PP.hsep $ PP.punctuate PP.comma (doc <$> b))

instance Pretty Pat where
    doc (Pat p) = doc p 
    doc (Not p) = PP.text "\\+" <+> doc p

instance Pretty Rule where
    doc (Rule h b) = 
        doc h <+> PP.text ":-" <+> (PP.hsep $ PP.punctuate PP.comma (doc <$> b))
        
instance Pretty [Rule] where
    doc [] = PP.empty
    doc (a:as) = doc a <> PP.text "." $$ doc as

instance Pretty [Fact] where
    doc [] = PP.empty
    doc (a:as) = doc a <> PP.text "." $$ doc as

instance Pretty ([Fact],[Rule]) where
    doc (x,y) = doc x $$ doc y

-- instance Pretty Subst
instance Pretty [(Var,Con)] where
    doc vs = PP.vcat $ map (\(v,n) -> doc v <+> PP.text "=" <+> doc n) vs  

