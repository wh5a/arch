{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Adapted from:  XMonad.Layout.Maximize
-- Copyright   :  (c) 2007 James Webb
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  xmonad#jwebb,sygneca,com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Temporarily yanks the focused window out of the layout to mostly fill
-- the screen.
--
-----------------------------------------------------------------------------

module Maximize (
        -- * Usage
        -- $usage
        maximize,
        maximizeRestore
    ) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier
-- Requires hacking this module to export this function  
import XMonad.Layout.NoBorders (setBorders)  
import Data.List ( partition )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Maximize
--
-- Then edit your @layoutHook@ by adding the Maximize layout modifier:
--
-- > myLayout = maximize (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key-bindings, do something like:
--
-- >        , ((modm, xK_backslash), withFocused (sendMessage . maximizeRestore))
-- >        ...
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data Maximize a = Maximize (Maybe Window) deriving ( Read, Show )
maximize :: LayoutClass l Window => l Window -> ModifiedLayout Maximize l Window
maximize = ModifiedLayout $ Maximize Nothing

data MaximizeRestore = MaximizeRestore Window deriving ( Typeable, Eq )
instance Message MaximizeRestore
maximizeRestore :: Window -> MaximizeRestore
maximizeRestore = MaximizeRestore

instance LayoutModifier Maximize Window where
    modifierDescription (Maximize _) = "Maximize"

    pureModifier (Maximize (Just target)) rect (Just (S.Stack focused _ _)) wrs =
            if focused == target
                then (maxed ++ rest, Nothing)
                else (rest ++ maxed, Nothing)
        where
            (toMax, rest) = partition (\(w, _) -> w == target) wrs
            maxed = map (\(w, _) -> (w, maxRect)) toMax
            maxRect = Rectangle (rect_x rect) (rect_y rect + 25)
                (rect_width rect) (rect_height rect - 25)
    pureModifier _ _ _ wrs = (wrs, Nothing)

    handleMess mm@(Maximize mw) m = do
      case fromMessage m of
        Just (MaximizeRestore w) -> case mw of
            Just w' -> if (w == w')
                        then do
                         asks (borderWidth . config) >>= setBorders [w]
                         return $ Just $ Maximize Nothing   -- restore window
                        else do
                         asks (borderWidth . config) >>= setBorders [w']
                         setBorders [w] 0
                         return $ Just $ Maximize $ Just w  -- maximize different window
            Nothing -> setBorders [w] 0 >>
                       (return $ Just $ Maximize $ Just w)        -- maximize window
        _ -> return Nothing
