; 2009 Daniel Schüssler, License: BSD3

(require 'yasnippet)


; parameterise the mode out because we want both haskell-mode and literate-haskell-mode
(defun hsnips (the-mode)
  
(yas/define-snippets the-mode
 '(

   ; LANGUAGE pragmas
   
   ; scheme: "-x", then one letter for every uppercase letter of the extension name

   ("-xbp"   "{-# LANGUAGE BangPatterns #-}")
   ("-xcpp"  "{-# LANGUAGE CPP #-}")
   ("-xddt"  "{-# LANGUAGE DeriveDataTypeable #-}")
   ("-xedd"  "{-# LANGUAGE EmptyDataDecls #-}")
   ("-xeq"   "{-# LANGUAGE ExistentialQuantification #-}")
   ("-xfc"   "{-# LANGUAGE FlexibleContexts #-}")
   ("-xfd"   "{-# LANGUAGE FunctionalDependencies #-}")
   ("-xfi"   "{-# LANGUAGE FlexibleInstances #-}")
   ("-xgadt" "{-# LANGUAGE GADTs #-}")
   ("-xgnd"  "{-# LANGUAGE GeneralizedNewtypeDeriving #-}")
   ("-xii"   "{-# LANGUAGE IncoherentInstances #-}")
   ("-xit"   "{-# LANGUAGE ImpredicativeTypes #-}")
   ("-xks"   "{-# LANGUAGE KindSignatures #-}")
   ("-xlts"  "{-# LANGUAGE LiberalTypeSynonyms #-}")
   ("-xmh"   "{-# LANGUAGE MagicHash #-}")
   ("-xmptc" "{-# LANGUAGE MultiParamTypeClasses #-}")
   ("-xnfp"  "{-# LANGUAGE NamedFieldPuns #-}")
   ("-xnip"  "{-# LANGUAGE NoImplicitPrelude #-}")
   ("-xnmr"  "{-# LANGUAGE NoMonomorphismRestriction #-}")
   ("-xoi"   "{-# LANGUAGE OverlappingInstances #-}")
   ("-xos"   "{-# LANGUAGE OverloadedStrings #-}")
   ("-xpg"   "{-# LANGUAGE PatternGuards #-}")
   ("-xpi"   "{-# LANGUAGE PackageImports #-}")
   ("-xr2t"  "{-# LANGUAGE Rank2Types #-}")
   ("-xrnt"  "{-# LANGUAGE RankNTypes #-}")
   ("-xrwc"  "{-# LANGUAGE RecordWildCards #-}")
   ("-xsd"   "{-# LANGUAGE StandaloneDeriving #-}")
   ("-xstv"  "{-# LANGUAGE ScopedTypeVariables #-}")
   ("-xtf"   "{-# LANGUAGE TypeFamilies #-}")
   ("-xth"   "{-# LANGUAGE TemplateHaskell #-}")
   ("-xto"   "{-# LANGUAGE TypeOperators #-}")
   ("-xtsi"  "{-# LANGUAGE TypeSynonymInstances #-}")
   ("-xui"   "{-# LANGUAGE UndecidableInstances #-}")
   ("-xvp"   "{-# LANGUAGE ViewPatterns #-}")
   
   ; OPTIONS pragmas
   ("opt"   "{-# OPTIONS $1 #-}")
   ("-fge"  "{-# OPTIONS -fglasgow-exts #-}" t)
   ("-dds"  "{-# OPTIONS -ddump-splices #-}" t)
   ("-dd"   "{-# OPTIONS -ddump-$1 #-}" t)
   ("-fwms" "{-# OPTIONS -fwarn-missing-signatures #-}" t)
   ("-fw"   "{-# OPTIONS -fwarn-$1 #-}" t)
   ("-fnw"  "{-# OPTIONS -fno-warn-$1 #-}" t)
   ("-wall" "{-# OPTIONS -Wall #-}" t)

   ; other pragmas
   ("inline" "{-# INLINE $1 #-}")
   ("scc"    "{-# SCC \"$1\" #-}")
   

   ; imports
   
   ; scheme: "imp", then one letter for the initial letter of each dot-separated part
   
   ; currently uses the ya/snippets disambiguation popup for ambiguities (should it?); this is specified by a dot in the macro string (you only need to type "impdm", not "impdm.Map" -- the part after the dot is the name of the menu item)

   ("impc"         "import Control.$1")
   ("impca"        "import Control.Applicative")
   ("impce"        "import Control.Exception")
   ("impcm"        "import Control.Monad")
   ("impcmr"       "import Control.Monad.Reader")
   ("impcms"       "import Control.Monad.State")
   ("impcmt"       "import Control.Monad.Trans")
   ("impcmw"       "import Control.Monad.Writer")
   ("impd"         "import Data.$1")
   ("impdb"        "import Data.ByteString as B")
   ("impdbl"       "import Data.ByteString.Lazy as B")
   ("impdf"        "import Data.Foldable as Fold")
   ("impdm.Map"    "import Data.Map as Map")
   ("impdm.Maybe"  "import Data.Maybe")
   ("impdm.Monoid" "import Data.Monoid")
   ("impds"        "import Data.Set as Set")
   ("impdt"        "import Data.Traversable as Trav")
   ("impl"         "import Language.$1")
   ("implh"        "import Language.Haskell.$1")
   ("implht"       "import Language.Haskell.TH")
   ("impn"         "import Network.$1")
   ("impp"         "import Prelude")
   ("imps"         "import System.$1")
   ("impsd"        "import System.Directory")
   ("impse"        "import System.Environment")
   ("impsi"        "import System.IO")
   ("impsp"        "import System.Process")
   ("impt"         "import Text.$1")
   ("imptp"        "import Text.Printf")
   ("imptpal"      "import Text.PrettyPrint.ANSI.Leijen")
   ("imptpt"       "import Text.Printf.TH")
   
   ; misc
   ("deriving" "deriving (Eq,Ord,Show,Read,Data,Typeable)
$0")
   
   ("par"  "($1) $0")
   ("par2" "($1) ($2) $0")
   ("par3" "($1) ($2) ($3) $0")
   ("par4" "($1) ($2) ($3) ($4) $0")
   
   ("if" "if $1
  then $2
  else $3")
   
   ("bot"  "⊥")
   
   ))



)

(hsnips 'haskell-mode)
(hsnips 'literate-haskell-mode)

