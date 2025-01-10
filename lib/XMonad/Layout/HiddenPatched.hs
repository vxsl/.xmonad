{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.HiddenPatched
-- Description :  Hide windows from layouts.
-- Copyright   :  (c) Peter Jones 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  pjones@devalot.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Similar to "XMonad.Layout.Minimize" but completely removes windows
-- from the window set so "XMonad.Layout.BoringWindows" isn't
-- necessary.  Perfect companion to
-- "XMonad.Layout.BinarySpacePartition" since it can be used to move
-- windows to another part of the BSP tree.
--
-----------------------------------------------------------------------------
module XMonad.Layout.HiddenPatched
       ( -- * Usage
         -- $usage
         HiddenWindows
       , HiddenMsg (..)
       , hiddenWindows
       , hideWindow
       , popOldestHiddenWindow
       , popNewestHiddenWindow
       , popHiddenWindow
       , hideAllWindowsWithClassPrefix
       , popAllHiddenWindows
       , toggleAllWindowsWithClassPrefix
       , popWindowWithClass
       , hideWindowWithClass
       ) where

--------------------------------------------------------------------------------
import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W
import Data.List (isPrefixOf)
import Control.Monad

--------------------------------------------------------------------------------
-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Layout.Hidden
--
-- Then edit your @layoutHook@ by adding the @HiddenWindows@ layout modifier:
--
-- > myLayout = hiddenWindows (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial> and
-- "XMonad.Doc.Extending#Editing_the_layout_hook".
--
-- In the key bindings, do something like:
--
-- >        , ((modMask, xK_backslash), withFocused hideWindow)
-- >        , ((modMask .|. shiftMask, xK_backslash), popOldestHiddenWindow)
-- >        ...
--
-- For detailed instruction on editing the key bindings see:
--
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial>.

--------------------------------------------------------------------------------
newtype HiddenWindows a = HiddenWindows [Window] deriving (Show, Read)

--------------------------------------------------------------------------------
-- | Messages for the @HiddenWindows@ layout modifier.
data HiddenMsg = HideWindow Window                      -- ^ Hide a window.
               | PopNewestHiddenWindow                  -- ^ Restore window (FILO).
               | PopOldestHiddenWindow                  -- ^ Restore window (FIFO).
               | PopSpecificHiddenWindow Window         -- ^ Restore specific window.
               | HideAllWindowsWithClassPrefix String   -- ^ Hide all windows with a particular WM_CLASS prefix.
               | PopAllHiddenWindows                    -- ^ Restore all hidden windows.
               | ToggleAllWindowsWithClassPrefix String -- ^ Toggle all windows with a particular WM_CLASS prefix.
               | PopByClass String                      -- ^ Restore a window by WM_CLASS attribute.
               | HideByClass String                     -- ^ Hide a window by WM_CLASS attribute.
               deriving (Eq)

instance Message HiddenMsg

--------------------------------------------------------------------------------
instance LayoutModifier HiddenWindows Window where
  handleMess h@(HiddenWindows hidden) mess
    | Just (HideWindow win)                           <- fromMessage mess = hideWindowMsg h win
    | Just PopNewestHiddenWindow                      <- fromMessage mess = popNewestMsg h
    | Just PopOldestHiddenWindow                      <- fromMessage mess = popOldestMsg h
    | Just (PopSpecificHiddenWindow win)              <- fromMessage mess = popSpecificMsg win h
    | Just (HideAllWindowsWithClassPrefix clsPrefix)  <- fromMessage mess = hideAllMsg clsPrefix h
    | Just PopAllHiddenWindows                        <- fromMessage mess = popAllMsg h
    | Just (ToggleAllWindowsWithClassPrefix clsPrefix)<- fromMessage mess = toggleAllMsg clsPrefix h
    | Just (PopByClass cls)                           <- fromMessage mess = popByClassMsg cls h
    | Just (HideByClass cls)                          <- fromMessage mess = hideByClassMsg cls h
    | Just ReleaseResources                           <- fromMessage mess = doUnhook
    | otherwise                                                           = return Nothing
    where doUnhook = do mapM_ restoreWindow hidden
                        return Nothing

  modifierDescription _ = "Hidden"

--------------------------------------------------------------------------------
-- | Apply the @HiddenWindows@ layout modifier.
hiddenWindows :: LayoutClass l Window => l Window -> ModifiedLayout HiddenWindows l Window
hiddenWindows = ModifiedLayout $ HiddenWindows []

--------------------------------------------------------------------------------
-- | Remove the given window from the current layout.  It is placed in
-- list of hidden windows so it can be restored later.
hideWindow :: Window -> X ()
hideWindow = sendMessage . HideWindow

--------------------------------------------------------------------------------
-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FIFO queue.  That is, the
-- first window hidden will be restored.
popOldestHiddenWindow :: X ()
popOldestHiddenWindow = sendMessage PopOldestHiddenWindow

--------------------------------------------------------------------------------
-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FILO queue.  That is, the
-- most recently hidden window will be restored.
popNewestHiddenWindow :: X ()
popNewestHiddenWindow = sendMessage PopNewestHiddenWindow

popHiddenWindow :: Window -> X ()
popHiddenWindow = sendMessage . PopSpecificHiddenWindow

--------------------------------------------------------------------------------
popAllHiddenWindows :: X ()
popAllHiddenWindows = sendMessage PopAllHiddenWindows

hideAllWindowsWithClassPrefix :: String ->  X ()
hideAllWindowsWithClassPrefix clsPrefix = sendMessage $ HideAllWindowsWithClassPrefix clsPrefix

toggleAllWindowsWithClassPrefix :: String -> X ()
toggleAllWindowsWithClassPrefix clsPrefix = sendMessage $ ToggleAllWindowsWithClassPrefix clsPrefix

popWindowWithClass :: String -> X ()
popWindowWithClass cls = sendMessage $ PopByClass cls

hideWindowWithClass :: String -> X ()
hideWindowWithClass cls = sendMessage $ HideByClass cls

--------------------------------------------------------------------------------
hideWindowMsg :: HiddenWindows a -> Window -> X (Maybe (HiddenWindows a))
hideWindowMsg (HiddenWindows hidden) win = do
  modifyWindowSet $ W.delete' win
  return . Just . HiddenWindows $ hidden ++ [win]

--------------------------------------------------------------------------------
hideAllMsg :: String -> HiddenWindows a -> X (Maybe (HiddenWindows a))
hideAllMsg clsPrefix (HiddenWindows hidden) = do
    ws <- gets windowset
    hideables <- filterM (runQuery className >=> (return . (clsPrefix `isPrefixOf`))) (W.allWindows ws ++ hidden)
    if null hideables
        then return Nothing
        else do
            mapM_ (modifyWindowSet . W.delete') hideables
            return . Just . HiddenWindows $ hideables

--------------------------------------------------------------------------------
popAllMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popAllMsg (HiddenWindows hidden) = do
  mapM_ restoreWindow hidden
  return . Just . HiddenWindows $ []

toggleAllMsg :: String -> HiddenWindows a -> X (Maybe (HiddenWindows a))
toggleAllMsg clsPrefix (HiddenWindows hidden) = do
    ws <- gets windowset
    hideables <- filterM (runQuery className >=> (return . (clsPrefix `isPrefixOf`))) (W.allWindows ws ++ hidden)
    if null hidden
        then do
            -- If no windows are currently hidden, hide all hideables
            mapM_ (modifyWindowSet . W.delete') hideables
            return . Just . HiddenWindows $ hideables
        else do
            -- If some windows are hidden, restore all hidden windows
            mapM_ restoreWindow hidden
            return . Just . HiddenWindows $ []

--------------------------------------------------------------------------------
popNewestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popNewestMsg (HiddenWindows [])     = return Nothing
popNewestMsg (HiddenWindows hidden) = do
  let (win, rest) = (last hidden, init hidden)
  restoreWindow win
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popOldestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popOldestMsg (HiddenWindows [])         = return Nothing
popOldestMsg (HiddenWindows (win:rest)) = do
  restoreWindow win
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popSpecificMsg :: Window -> HiddenWindows a -> X (Maybe (HiddenWindows a))
popSpecificMsg _   (HiddenWindows []) = return Nothing
popSpecificMsg win (HiddenWindows hiddenWins) = if win `elem` hiddenWins
  then do
    restoreWindow win
    return . Just . HiddenWindows $ filter (/= win) hiddenWins
  else
    return . Just . HiddenWindows $ hiddenWins

--------------------------------------------------------------------------------
popByClassMsg :: String -> HiddenWindows a -> X (Maybe (HiddenWindows a))
popByClassMsg cls (HiddenWindows hiddenWins) = do
  matchingWins <- filterM (runQuery (className =? cls)) hiddenWins
  case matchingWins of
    [] -> return . Just . HiddenWindows $ hiddenWins
    (win:_) -> do
      restoreWindow win
      return . Just . HiddenWindows $ filter (/= win) hiddenWins

--------------------------------------------------------------------------------
hideByClassMsg :: String -> HiddenWindows a -> X (Maybe (HiddenWindows a))
hideByClassMsg cls (HiddenWindows hiddenWins) = do
  ws <- gets windowset
  matchingWins <- filterM (runQuery (className =? cls)) (W.allWindows ws ++ hiddenWins)
  case matchingWins of
    [] -> return . Just . HiddenWindows $ hiddenWins
    (win:_) -> do
      modifyWindowSet $ W.delete' win
      return . Just . HiddenWindows $ hiddenWins ++ [win]


--------------------------------------------------------------------------------
restoreWindow :: Window -> X ()
restoreWindow = windows . W.insertUp
