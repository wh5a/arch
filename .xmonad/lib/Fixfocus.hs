{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
-- A hack from XMonad mailing list "tabbed, floating & focus"
module Fixfocus where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified Data.Map as M  
import qualified XMonad.StackSet as W
import Data.Maybe

data FixFocus a = FixFocus (Maybe a)
                deriving (Read, Show)

instance LayoutModifier FixFocus Window where
    modifyLayout (FixFocus mlf) ws@(W.Workspace id lay Nothing) r = runLayout ws r
    modifyLayout (FixFocus Nothing) ws r = runLayout ws r
    modifyLayout (FixFocus (Just lf)) (W.Workspace id lay (Just st)) r = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        is_rf_floating <- maybe (return False) (\rf -> withWindowSet $ return . M.member rf . W.floating) mreal_f -- real focused window is floating?
        let new_stack_f = if is_rf_floating then lf else stack_f --if yes: replace stack's focus with our last saved focus
        let new_st' = until (\s -> new_stack_f == W.focus s) W.focusUp' st -- new stack with focused new_stack_f
        let new_st = if (new_stack_f `elem` (W.integrate st)) then new_st' else st -- use it only when it's possible to
        runLayout (W.Workspace id lay (Just new_st)) r

    redoLayout (FixFocus mlf) r Nothing wrs = return (wrs, Just $ FixFocus mlf)
    redoLayout (FixFocus mlf) r (Just st) wrs = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        let crf_in_stack = maybe False ((flip elem) (W.integrate st)) mreal_f -- current real focus belongs to stack?
        let new_saved_f = if crf_in_stack then fromJust mreal_f else stack_f -- if yes: replace saved focus
        return (wrs, Just $ FixFocus $ Just new_saved_f)

fixFocus = ModifiedLayout $ FixFocus Nothing
