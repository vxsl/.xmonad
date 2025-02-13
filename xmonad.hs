{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Monad
import Data.List
import Data.Set qualified as Set
import Data.Map qualified as M
import Data.Data (Typeable, cast)
import Data.Maybe
import Data.Either
import Graphics.X11
import Graphics.X11.ExtraTypes
import System.Exit
import Text.Read (readMaybe)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GroupNavigationPatched qualified as GNP
import XMonad.Actions.OnScreen
import XMonad.Actions.SinkAll
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook, isUnfocused)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.RefocusLast
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.IndependentScreens (onCurrentScreen)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.ShowWNamePatched qualified as SWNP
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Window
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpadPatched
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers
import Data.Monoid (All(..))
import GHC.Utils.Monad (whenM)
import Text.Printf (printf)
import System.Posix (touchFile)
import System.Directory (removeFile, renameFile, doesFileExist)
import Numeric
import Data.Ratio
import GHC.Prelude (Fractional(fromRational))
import XMonad.Hooks.ManageHelpers (doFocus)
import Control.Concurrent (threadDelay, forkIO)
import XMonad (getClassHint)
import XMonad.Actions.CopyWindow (copyToAll)

winMask, altMask :: KeyMask
winMask = mod4Mask
altMask = mod1Mask

debug=False
-- debug=True

------------------------------------------------------------------------
-- workspaces:
numScreens :: ScreenId
numScreens = 1

numWorkspacesPerScreen :: Int
numWorkspacesPerScreen = 9

myWorkspaces :: [String]
myWorkspaces = withScreens numScreens ([show n | n <- [1 .. numWorkspacesPerScreen]])

------------------------------------------------------------------------
-- logging:
debugLogFile="/home/kyle/.xmonad-debug-log"
persistentLogFile="/home/kyle/.xmonad-persistent-log"

logToFile :: String -> (Show a, Typeable a) => a -> X ()
logToFile path str =
  let output = case cast str of
                 Just s -> s            -- If `str` is a `String`, use it directly
                 Nothing -> show str    -- Otherwise, use `show`
  in io $ appendFile path (output ++ "\n")

debugLog msg = when debug $ logToFile debugLogFile msg

data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq)

persistentLog lvl msg = do
  logToFile persistentLogFile $ "[" ++ show lvl ++ "]\t" ++ msg
  logToFile debugLogFile msg

------------------------------------------------------------------------
-- misc. helpers:

compareNumbers :: String -> String -> Ordering
compareNumbers a b =
  case (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int) of
    -- if they're both valid numbers then compare them
    (Just x, Just y) -> compare x y
    -- push numbers to the front of strings
    (Nothing, Just _) -> GT
    (Just _, Nothing) -> LT
    -- strings get normal string comparison
    (Nothing, Nothing) -> compare a b

doOnScreen :: ScreenId -> X () -> X ()
doOnScreen s x = do
  windows $ focusScreen s
  x

killAllWindowsByClass :: Query Bool -> X ()
killAllWindowsByClass q = do
  win <- GNP.getNextMatch q GNP.Forward
  case win of
    Just w -> killWindow w >> killAllWindowsByClass q
    Nothing -> return ()

toggleOrViewNoSP = toggleOrDoSkip ["NSP"] W.greedyView

centerRect s = center s s
  where
    center w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h


data Corner = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Show, Eq)

cornerRect :: Int -> Rational -> Rational -> Corner -> W.RationalRect
cornerRect i w h BottomRight = W.RationalRect (1 - w) (1 - h - (fromIntegral i * h)) w h
cornerRect i w h BottomLeft = W.RationalRect 0 (1 - h - (fromIntegral i * h)) w h
cornerRect i w h TopRight = W.RationalRect (1 - w) (fromIntegral i * h) w h
cornerRect i w h TopLeft = W.RationalRect 0 (fromIntegral i * h) w h


maintainFocus :: X () -> X ()
maintainFocus action = do
  ws <- gets windowset
  let originallyFocused' = W.focus <$> W.stack (W.workspace (W.current ws))
  if isJust originallyFocused'
    then do
      let originallyFocused = fromJust originallyFocused'
      let ogWorkspace = W.findTag originallyFocused ws
      action
      ws <- gets windowset
      let newWorkspace = W.findTag originallyFocused ws
      when (ogWorkspace == newWorkspace) $ focus originallyFocused
    else action

bringToFront win = withFocused $ \focused -> do
  -- focusCls <- runQuery className focused
  -- let focusedIsNSP = "NSP_" `isPrefixOf` focusCls
  -- when focusedIsNSP $ windows $ W.shiftWin "NSP" focused 

  ws <- gets windowset
  let stack' = W.stack $ W.workspace $ W.current ws
  let stack = fromJust stack'

  pnps' <- getPNPsOnActiveWorkspace
  let pnps = Set.fromList pnps'

  let (pnpsInUp, restOfUp) = partition (`Set.member` pnps) (W.up stack)
  let newUp = filter (/= win) restOfUp

  focusCls <- runQuery className focused
  let newDown'' = W.focus stack : W.down stack
  -- let focusedIsPNP = "PNP_" `isPrefixOf` focusCls
  -- let newDown'' = if focusedIsPNP
  --     then stack.down
  --     else stack.focus : stack.down
  let newDown' = pnpsInUp ++ newDown''
  let newDown = filter (/= win) newDown'

  let stack'' = W.Stack { W.focus = win, W.up = newUp, W.down = newDown }
  debugLog $ "bringToFront new stack: " ++ show stack''
  windows $ const ws {
          W.current = (W.current ws) {
            W.workspace = (W.workspace $ W.current ws) {
              W.stack = Just stack''
            }
          }
        }
  -- when focusedIsNSP $ windows $ W.shiftWin "NSP" focused 

findWinOnActiveWorkspace :: Query Bool -> X (Maybe Window)
findWinOnActiveWorkspace q = do
  ws <- gets windowset
  let W.Workspace _ _ stack = W.workspace $ W.current ws
  let curWorkspaceWindowList = W.integrate' stack
  listToMaybe <$> filterM (runQuery q) curWorkspaceWindowList

findWinsOnActiveWorkspace :: Query Bool -> X [Window]
findWinsOnActiveWorkspace q = do
  ws <- gets windowset
  let W.Workspace _ _ stack = W.workspace $ W.current ws
  let curWorkspaceWindowList = W.integrate' stack
  filterM (runQuery q) curWorkspaceWindowList

findWinOnAnyWorkspace :: Query Bool -> X (Maybe Window)
findWinOnAnyWorkspace q = GNP.getNextMatch q GNP.Forward

findWinsOnAnyWorkspace :: Query Bool -> X [Window]
findWinsOnAnyWorkspace q = do
  ws <- gets windowset
  let allWindows = W.allWindows ws
  filterM (runQuery q) allWindows

printWorkspaceState :: X String
printWorkspaceState = do
  ws <- gets windowset

  let windowIdToName w = do
        name <- runQuery className w
        return (show w, name)

  let W.Workspace _ _ stack' = W.workspace $ W.current ws
  if isJust stack' then do
    let stack = fromJust stack'

    let curWorkspaceWindowList = W.integrate stack  -- Extract window list
    windowMap <- mapM windowIdToName curWorkspaceWindowList
    let nameMap = M.fromList windowMap

    let curWorkspaceWindowList = W.integrate stack
    let printName w = fromMaybe (show w) (M.lookup (show w) nameMap)
    return $ "\n\n========================\n" ++ show stack ++ "\n" ++ show nameMap ++ "\n" ++
      "focus: " ++ printName (W.focus stack) ++
      "\nup: " ++ concatMap (\w -> show w ++ " " ++ printName w ++ ", ") (W.up stack) ++
      "\ndown: " ++ concatMap (\w -> show w ++ " " ++ printName w ++ ", ") (W.down stack) ++
      "\nintegrate: " ++ concatMap (\w -> printName w ++ ", ") (W.integrate stack)

  else return "empty"


getCurrentWorkspaceID :: X String
getCurrentWorkspaceID = do
  ws <- gets windowset
  return $ W.tag $ W.workspace $ W.current ws

rationalAsDecimal :: Rational -> String
rationalAsDecimal v = show $ fromRational v

------------------------------------------------------------------------
-- prompt:
myPromptConfig :: XPConfig
myPromptConfig =
  def
    { bgColor = "#2600ff",
      fgColor = "#ffffff",
      position = CenteredAt 0.5 0.5,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 100,
      font = "xft:Liberation Mono-18:normal",
      searchPredicate = isInfixOf
    }

confirmCmd :: String -> X ()
confirmCmd cmd = confirmPrompt myPromptConfig cmd $ spawn cmd

confirm :: String -> X () -> X ()
confirm = confirmPrompt myPromptConfig

------------------------------------------------------------------------
-- layout:
myLayout =
  ifWider
    2561
    (ThreeColMid 1 (3 / 100) (1 / 2) ||| Full)
    (tiled ||| Mirror tiled ||| Full ||| Grid)
  where
    tiled = Tall nmaster delta ratio -- default partitions the screen into two panes
    nmaster = 1 -- The default number of windows in the master pane
    ratio = 3 / 5 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

------------------------------------------------------------------------
-- mouse bindings:
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

------------------------------------------------------------------------
-- startup hook:
logXmonadState :: X ()
logXmonadState = do
  ws <- gets windowset
  let currentScreen = W.current ws
      currentLayout = description . W.layout $ W.workspace currentScreen
  let focused = W.focus <$> W.stack (W.workspace (W.current ws))
  let result :: String = printf "Screen '%s' is active on workspace '%s'\nLayout: '%s'\nFocused window: '%s'"
                          (show $ W.screen currentScreen)
                          (W.tag $ W.workspace currentScreen)
                          currentLayout
                          (show $ fromJust focused)
  debugLog result

myStartupHook :: X ()
myStartupHook = do
  spawn "picom"
  sendMessage (SetStruts [] [U,L])
  spawnOnce "nitrogen --restore &"
  mapM_ cycleAllWorkspacesOnScreen [0 .. (numScreens - 1)]
  windows $ focusScreen 0
  setFade defaultFadeOpacity

  let pnpDef = getPNPDefByClassName "PNP_log"
  let ((cls, cmd, _, _, _),_,_,_,_) = pnpDef
  when debug $ spawn $ "touch $HOME/.xmonad-init-flag \
    \&& tmux kill-session -t tmuxa-pnp-log\
    \; tmux detach-client -t $(cat /tmp/tmux-pane-view-client)\
    \; "
    ++ cmd
    ++ "; sleep 1; rm -f $HOME/.xmonad-init-flag"

  oldCopyExists <- io $ doesFileExist (debugLogFile ++ ".1")
  when oldCopyExists $ io $ removeFile (debugLogFile ++ ".1")
  curExists <- io $ doesFileExist debugLogFile
  when curExists $ io $ renameFile debugLogFile (debugLogFile ++ ".1")



  spawn $ "\
    \echo ==================================================================== >> " ++ debugLogFile ++ "\
    \ && date >> " ++ debugLogFile ++ "\
    \ && echo ==================================================================== >> " ++ persistentLogFile ++ "\
    \ && date >> " ++ persistentLogFile ++ "\
    \"


  when debug $ do
    debugLog "=====================================================================\n"
    debugLog "*********************************************************"
    logXmonadState
    debugLog "*********************************************************\n"

    debugLog "*********************************************************"
    workspaceState <- printWorkspaceState
    debugLog workspaceState
    debugLog "*********************************************************\n"

    spawn $ "x-summary >> " ++ debugLogFile
  -- ensureNoPNPFocus


  where
    cycleAllWorkspacesOnScreen i = do
      mapM_
        ( \n -> do
            let id = show i ++ "_" ++ show n
            windows $ viewOnScreen i id
        )
        ([1 .. numWorkspacesPerScreen] ++ [1])
      windows $ viewOnScreen i $ coerceScreenID i ++ "_1"
      where
        coerceScreenID :: ScreenId -> String
        coerceScreenID (S i) = show i

------------------------------------------------------------------------
-- app rules:
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "zenity" --> doFloat,
      className =? "Arandr" --> doFloat,
      className =? "Org.gnome.Nautilus" --> doFloat,
      className =? "Xmessage" --> doFloat,
      title =? "Picture-in-Picture" --> doFloat,
      -- title =? "Profile error occurred" --> doIgnore >> doFocus >> doFloatAt (-10000) (-10000),
      title =? "Profile error occurred" --> doHideIgnore,
      appName =? "code-insiders-url-handler (remote-debug-profile)" --> doShift "2_1",
      className =? "Chromium-browser" --> doShift "2_1",
      className =? "tmux-pane-view" --> doShift "1_1",
      className =? "Google-chrome" --> doShift "project",
      className =? "partmin-ui" --> doShift "project",
      fmap (`Set.member` pnpNamesThatStartMinimized) className --> doF W.focusDown
    ]

------------------------------------------------------------------------
-- event hook:
newtype WorkspaceState = WorkspaceState String
  deriving (Typeable, Read, Show)
instance ExtensionClass WorkspaceState where
  initialValue = WorkspaceState ""


resizeHook :: Event -> X All
resizeHook (ConfigureEvent { ev_window = w }) = do
    className <- runQuery className w
    when (className == "PNP_whiteboard") $ spawn "lock-tablet-area PNP_whiteboard \"UGTABLET M908 Pen stylus\" \"UGTABLET M908 Pen eraser\""
    return (All True)
resizeHook _ = return (All True)

summarizeWorkspaceStateEventHook _ = do
  WorkspaceState prev <- XS.get
  cur <- printWorkspaceState
  when (prev /= cur) $ do
    XS.put $ WorkspaceState cur
    debugLog cur
  return (All True)

myHandleEventHook =
  (if debug then summarizeWorkspaceStateEventHook else mempty) <>
  (resizeHook <> refocusLastWhen (refocusingIsActive <||> isFloat))


------------------------------------------------------------------------
-- log hook:

defaultFadeOpacity = 0.2

newtype FadeOpacity = FadeOpacity { getFadeOpacity :: Rational }
  deriving (Typeable)
instance ExtensionClass FadeOpacity where
  initialValue = FadeOpacity defaultFadeOpacity
setFade newVal = do
  XS.modify $ const $ FadeOpacity newVal
  refresh

rotaryAdjustFade f = do
  val <- getFadeOpacity <$> XS.get
  let newVal = max 0 (min 1 (f val))
  -- debugLog $ "rotaryAdjustFade: " ++ rationalAsDecimal newVal ++ " (was " ++ rationalAsDecimal val ++ ")"
  if newVal > val then do
    visiblePNPs <- getPNPsOnActiveWorkspace
    when (null visiblePNPs) unhidePNPs
    setFade $ max defaultFadeOpacity newVal
  else do
    setFade newVal
    when (newVal <= 0) hidePNPs

newtype LastFocusedWindow = LastFocusedWindow (Maybe Window)
    deriving (Typeable, Read, Show)
instance ExtensionClass LastFocusedWindow where
    initialValue = LastFocusedWindow Nothing
myLogHook xmproc = do
  FadeOpacity fade <- XS.get
  refocusLastLogHook
  fadeOutLogHook
    (fadeIf
      ((&&) <$> isUnfocused <*> (className =? "Com.github.xournalpp.xournalpp" <||> (("PNP_" `isPrefixOf`) <$> className)))
      fade
    )
  nsHideOnFocusLoss
    (mapNSPDefToNSP $ filter (\(_, _, _, _, hideOnFocusLoss) -> hideOnFocusLoss) nspDefs)
  dynamicLogWithPP
    def
      { ppOutput = hPutStrLn xmproc,
        ppSort = mkWsSort $ return compareNumbers,
        ppTitle = mempty,
        ppCurrent = xmobarColor "#00ff1e" "#000000",
        ppHidden = mempty,
        ppVisible = xmobarColor "#ff0000" "#000000",
        ppSep = "  ",
        ppWsSep = "  ",
        ppLayout = const ""
      }
  pnpTrackFocusChange

pnpTrackFocusChange :: X ()
pnpTrackFocusChange = do
  ws <- gets windowset
  let focusedWindow = W.focus <$> W.stack (W.workspace (W.current ws))
  LastFocusedWindow lastFocused <- XS.get
  when (focusedWindow /= lastFocused) $ do
    XS.put $ LastFocusedWindow focusedWindow
    case lastFocused of
      Just lw -> do
        cls <- runQuery className lw
        when ("PNP_" `isPrefixOf` cls) $
          let pnpDef = getPNPDefByClassName cls
           in pnpMinimize lw pnpDef
      Nothing -> return ()

------------------------------------------------------------------------
-- seeWin: a function to get the next occurrence of a window matching a query
data SeeWinParams = SeeWinParams
  { queryStr :: String,
    notFoundAction :: X (),
    greedy :: Bool,
    searchBackwards :: Bool,
    exact :: Bool,
    useClassName :: Bool,
    extraAction :: X () -- extra action to perform when the window is found or created
  }

defaultSeeWinParams :: SeeWinParams -- default SeeWinParams
defaultSeeWinParams =
  SeeWinParams
    { useClassName = False,
      exact = False,
      greedy = False,
      searchBackwards = False,
      extraAction = mempty
    }

winQuery :: Bool -> Bool -> String -> Query Bool
winQuery useClassName exact str =
  fmap fn prop
  where
    fn = if exact then (str ==) else isInfixOf str
    prop = if useClassName then className else appName

seeWin :: SeeWinParams -> X ()
seeWin SeeWinParams {queryStr, notFoundAction, greedy, searchBackwards, exact, useClassName, extraAction} =
  do
    hideAllNSPs
    win <-
      GNP.getNextMatch
        (winQuery useClassName exact queryStr)
        (if searchBackwards then GNP.Backward else GNP.Forward)
    if isJust win
      then do
        let w = fromJust win
        if greedy then windows $ bringWindow w else mempty
        windows $ W.focusWindow w
      else notFoundAction
    do extraAction

------------------------------------------------------------------------
-- nspBinds: function to get the keybindings for cycling through windows via seeWin
data NspBindsParams = NspBindsParams
  { keySym :: KeySym,
    nspName :: String
  }

type AbbreviatedNspBindsParams = (KeySym, String)

nspBinds :: NspBindsParams -> [((KeyMask, KeySym), X ())]
nspBinds NspBindsParams {keySym, nspName} =
  [
    ((altMask, keySym), openNSPOnScreen nspName 0
    -- do
    --   openNSPOrPNPOnScreen nspName 0
    --   win <- findWinOnActiveWorkspace $ className =? nspName
    --   debugLog "hee"
    --   debugLog win
    --   if isJust win then windows $ bringToFront (fromJust win)
    --   else ensureNoPNPFocus
    ),
    ( (altMask + shiftMask, keySym),
      do
        hideNSP nspName
        openNSPOrPNPOnScreen nspName 0
        -- win <- findWinOnActiveWorkspace $ className =? nspName
        -- debugLog win
        -- when (isJust win) $ windows $ bringToFront (fromJust win)
        -- resetFocusedNSP
    )
  ]

ezNspBinds :: [AbbreviatedNspBindsParams] -> [((KeyMask, KeySym), X ())]
ezNspBinds =
  concatMap
    ( \(keySym, nspName) -> do
        nspBinds $ NspBindsParams {keySym, nspName}
    )

------------------------------------------------------------------------
-- winBinds: function to get the keybindings for cycling through windows via seeWin
data WinBindsParams = WinBindsParams
  { keySym :: KeySym,
    queryStr :: String,
    notFoundAction :: X (),
    _exact :: Bool,
    _useClassName :: Bool,
    _extraAction :: X () -- extra action to perform when the window is found or created
  }

defaultWinBindsParams :: WinBindsParams -- default WinBindsParams
defaultWinBindsParams =
  WinBindsParams
    { _exact = False,
      _useClassName = False,
      _extraAction = mempty
    }

winBinds :: WinBindsParams -> [((KeyMask, KeySym), X ())]
winBinds WinBindsParams {keySym, queryStr, notFoundAction, _exact, _useClassName, _extraAction} =
  [ ((altMask, keySym), seeWin params),
    ((altMask + shiftMask, keySym), seeWin params {greedy = True})
    -- ((altMask + controlMask, keySym), seeWin params {searchBackwards = True})
  ]
  where
    params :: SeeWinParams
    params =
      defaultSeeWinParams
        { queryStr = queryStr,
          notFoundAction = notFoundAction,
          exact = _exact,
          useClassName = _useClassName,
          extraAction = _extraAction
        }

winBindsIDE :: [KeySym] -> [((KeyMask, KeySym), X ())]
winBindsIDE keycodes =
  concat $ zipWith binds [1 ..] keycodes
  where
    binds :: Int -> KeySym -> [((KeyMask, KeySym), X ())]
    binds i keySym =
      winBinds
        defaultWinBindsParams
          { keySym = keySym,
            queryStr = name,
            notFoundAction =
              spawn $
                "not-dotfiles spawn-with-name " ++ name ++ " Code \"code --disable-gpu -n " ++ dir ++ "\" 2"
          }
      where
        name = "customvsc_" ++ show i
        dir = "$(sed -n " ++ show i ++ "p $HOME/.xmonad/code_workspaces | sed 's/ .*//')"

winBindsTmuxaStableView :: b -> Int -> [((KeyMask, b), X ())]
winBindsTmuxaStableView keySym num =
  [ ((altMask, keySym), seeWin params),
    ((altMask + shiftMask, keySym), seeWin params {greedy = True})
  ]
  where
    name = "tmuxa-" ++ show num
    notFoundAction =
      spawn $
        "unique-term "
          ++ name
          ++ " \"tmuxa "
          ++ name
          ++ " --dir=$HOME\""
          ++ if num >= 2
            then " $HOME/.config/alacritty/alacritty" ++ show num ++ ".yml"
            else ""
    params :: SeeWinParams
    params = defaultSeeWinParams {queryStr = name, notFoundAction = notFoundAction, exact = True}

type AbbreviatedWinBindsParams = (KeySym, String, X (), Maybe WinBindsParams)

ezWinBinds :: [AbbreviatedWinBindsParams] -> [((KeyMask, KeySym), X ())]
ezWinBinds =
  concatMap
    ( \(keySym, queryStr, notFoundAction, ops') -> do
        winBinds $
          if isJust ops'
            then do
              let ops = fromJust ops'
              defaultWinBindsParams
                { keySym = keySym,
                  queryStr = queryStr,
                  notFoundAction = notFoundAction,
                  _exact = _exact ops,
                  _useClassName = _useClassName ops,
                  _extraAction = _extraAction ops
                }
            else
              defaultWinBindsParams
                { keySym = keySym,
                  queryStr = queryStr,
                  notFoundAction = notFoundAction
                }
    )

------------------------------------------------------------------------
-- docs search

newtype TabIndex = TabIndex Int deriving (Read, Show)

instance ExtensionClass TabIndex where
  initialValue = TabIndex 0

searchDocs :: String -> X ()
searchDocs prefix = do
  hideNSP "NSP_docs"
  openNSPOrPNPOnScreen "NSP_docs" 0
  let promptConfig = myPromptConfig
  inputPrompt promptConfig (prefix ++ " docs") ?+ \query -> do
    TabIndex currentIndex <- XS.get
    let nextIndex = if currentIndex < 1 || currentIndex >= 9 then 1 else currentIndex + 1
    XS.put $ TabIndex nextIndex
    let cmd = unwords
          [ "xdotool key ctrl+" ++ show nextIndex
          , "key ctrl+l key ctrl+a key BackSpace key BackSpace &&  "
          , "xdotool type \"! \" && "
          , "xdotool type \" " ++ prefix ++ " " ++ query ++ "\" && "
          , "xdotool key KP_Enter"
          ]
    spawn cmd

------------------------------------------------------------------------
-- named scratchpads ("NSPs"):
type NSPDef =
  ( String, -- scratchpad name
    String, -- command used to run application
    Query Bool, -- query to find already running application
    Either W.RationalRect ManageHook,
    Bool -- hide on focus loss
  )

type PNPDef = (
    NSPDef,
    -- Either W.RationalRect ManageHook, -- minimized state
    Corner,
    Rational, -- width
    Rational, -- height
    Bool -- start minimized
  )

getPNPs :: X [Window]
getPNPs = findWinsOnAnyWorkspace (("PNP_" `isPrefixOf`) <$> className)

getPNPsOnActiveWorkspace :: X [Window]
getPNPsOnActiveWorkspace = findWinsOnActiveWorkspace (("PNP_" `isPrefixOf`) <$> className)

getNSPs :: X [Window]
getNSPs = findWinsOnActiveWorkspace (("NSP_" `isPrefixOf`) <$> className)

nspDefs' :: [NSPDef]
nspDefs' =
  [ ( "NSP_assistant",
      "firefox -P clone1 --class NSP_assistant --new-window \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/",
      className =? "NSP_assistant",
      Left $ centerRect 0.8,
      True
    ),
    ( "NSP_docs",
      "chromium-browser --user-data-dir=/home/kyle/.config/chromium/DefaultClone1 --class=NSP_docs --new-window \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com \
      \--new-tab google.com",
      className =? "NSP_docs",
      Left $ centerRect 0.8,
      False
    ),
    ( "NSP_browse",
      "firefox -P clone3 --class NSP_browse",
      className =? "NSP_browse",
      -- "chromium-browser",
      -- className =? "Chromium-browser",
      Right nonFloating,
      False
    ),
    ( "NSP_files",
      "nautilus",
      className =? "org.gnome.Nautilus",
      Right nonFloating,
      False
    ),
    ( "NSP_discord",
      "discord",
      className =? "discord",
      Right nonFloating,
      False
    ),
    ( "NSP_obsidian",
      "spawn-with-name NSP_obsidian obsidian 'obsidian \"obsidian://open?vault=main\"' 2",
      className =? "NSP_obsidian",
      Left $ centerRect 0.8,
      False
    ),
    ( "NSP_homelab",
      "firefox -P clone4 --class NSP_homelab --new-window \
      \-new-tab -url http://pve.local:8006/ \
      \-new-tab -url https://10.0.0.3:9443/ \
      \-new-tab -url http://npm-gui.kylegrimsrudma.nz/ \
      \-new-tab -url http://umami.kylegrimsrudma.nz/ \
      \-new-tab -url https://dash.cloudflare.com/05ec19eeeec296ddd6cfd2bda7df1384/kylegrimsrudma.nz/dns/records",
      className
        =? "NSP_homelab",
      Right $ nonFloating,
      True
    ),
    ( "NSP_tmuxa-1",
      "unique-term NSP_tmuxa-1 \"tmuxa tmuxa-1 --dir=$HOME\"",
      className =? "NSP_tmuxa-1",
      Left $ centerRect 0.9,
      False
    ),
    ( "NSP_tmuxa-2",
      "unique-term NSP_tmuxa-2 \"tmuxa tmuxa-2 --dir=$HOME\" " ++ " /home/kyle/.config/alacritty/alacritty2.toml",
      className =? "NSP_tmuxa-2",
      Left $ centerRect 0.9,
      False
    ),
    ( "NSP_tmuxa-3",
      "unique-term NSP_tmuxa-3 \"zsh\" " ++ " /home/kyle/.config/alacritty/alacritty3.toml",
      className =? "NSP_tmuxa-3",
      Left $ centerRect 0.4,
      False
    ),
    ( "NSP_testing",
      "",
      className =? "Cypress",
      Left $ centerRect 0.7,
      False
    ),
    ( "NSP_audio",
      "firefox -P clone2 --class NSP_audio --new-window \
      \-new-tab -url https://open.spotify.com/ \
      \-new-tab -url https://noises.online/",
      className =? "NSP_audio",
      Left $ centerRect 0.7,
      False
    ),
    ( "NSP_spotify",
      "spotify",
      className =? "Spotify",
      Left $ centerRect 0.7,
      False
    ),
    -- ( "NSP_project",
    --   -- mempty,
    --   "google-chrome --new-window",
    --   -- className =? "Code-insiders-url-handler",
    --   -- className =? "Chromium-browser",
    --   -- className =? "Google-chrome",
    --   -- className =? "partmin-ui",
    --   className =? "Google-chrome",
    --   Right $ nonFloating,
    --   True
    -- ),
    ( "NSP_hubstaff",
      "/home/kyle/Hubstaff/HubstaffClient.bin.x86_64",
      className =? "Netsoft-com.netsoft.hubstaff",
      Left $ centerRect 0.3,
      -- nonFloating,
      True
    ),
    nspRemonadDef
  ]

nspRemonadCmd = singleCommandTermCmd "NSP_remonad" "tmuxa-remonad" "remonad --interactive --restart && xdotool search NSP_remonad windowkill"
nspRemonadDef :: NSPDef
nspRemonadDef = (
    "NSP_remonad",
    nspRemonadCmd,
    className =? "NSP_remonad",
    Left $ centerRect 0.8,
    True
  )

confirmRemonad :: X ()
confirmRemonad = confirm "restart xmonad?" $ spawn $ "tmux kill-session -t tmuxa-remonad; xdotool search NSP_remonad windowkill; " ++ nspRemonadCmd

singleCommandTermCmd :: String -> String -> String -> String
singleCommandTermCmd cls tmuxSessionID initCmd =
  "tmux kill-session -t " ++ tmuxSessionID ++ "; "
  ++ "unique-term " ++ cls ++ " "
    ++ "\"tmuxa " ++ tmuxSessionID ++ " --dir=$HOME --no-layout "
      ++"--cmd=\\\""
        ++ "trap 'tmux kill-session -t " ++ tmuxSessionID ++ "' INT; "
        ++ initCmd
      ++ "\\\"\""

pnpDefs' :: [PNPDef] = [
    (
      (
        "PNP_log",
        if debug then singleCommandTermCmd "PNP_log" "tmuxa-pnp-log" "xmdebug --from-beginning"
              ++ "; [ ! -f $HOME/.xmonad-init-flag ] && tmux-pane-view --class=PNP_log"
          else "tmux-pane-view --class=PNP_log",
        className =? "PNP_log",
        Left $ centerRect 0.7,
        False
      ),
      BottomRight,
      0.2,
      0.2,
      True
    ),
    (
      (
        "PNP_alt_log",
        "tmux-pane-view --class=PNP_alt_log --alt-client",
        className =? "PNP_alt_log",
        Left $ centerRect 0.7,
        False
      ),
      BottomRight,
      0.2,
      0.2,
      True
    ),
    (
      ( "PNP_whiteboard",
        -- "firefox -P clone5 --class PNP_whiteboard --new-window https://whimsical.com",
        "firefox -P clone5 --class PNP_whiteboard --new-window https://excalidraw.com -kiosk",
        className =? "PNP_whiteboard",
        Left $ centerRect 0.7,
        False
      ),
      BottomRight,
      0.2,
      0.2,
      False
    ),
    (
      ( "PNP_whiteboard*nokiosk",
        "firefox -P clone6 --class PNP_whiteboard*nokiosk --new-window https://excalidraw.com",
        className =? "PNP_whiteboard*nokiosk",
        Left $ centerRect 0.7,
        False
      ),
      BottomRight,
      0.2,
      0.2,
      False
    ),
    (
      ( "PNP_meeting",
        "chromium-browser --user-data-dir=/home/kyle/.config/chromium/DefaultClone2 --class=PNP_meeting --new-window --new-tab 'https://us04web.zoom.us/myhome' --start-fullscreen",
        className =? "PNP_meeting",
        Left $ centerRect 0.7,
        False
      ),
      BottomRight,
      0.2,
      0.2,
      False
    ),
    (
      ( "PNP_timer",
        "",
        className =? "PNP_timer",
        Left $ centerRect 0.7,
        False
      ),
      BottomLeft,
      0.07,
      0.07,
      True
    ),
    (
      ( "PNP_proj",
        "multi-instance-chromium-browser --class=PNP_proj --new-window --app='http://localhost:5173' --start-fullscreen --remote-debugging-port=9222",
        className =? "PNP_proj",
        Left $ centerRect 0.7,
        False
      ),
      BottomRight,
      0.2,
      0.2,
      True
    )
  ]

pnpNamesThatStartMinimized :: Set.Set String
pnpNamesThatStartMinimized = Set.fromList $ map (\((n,_,_,_,_),_,_,_,_) -> n) $ filter (\(_,_,_,_,startMinimized) -> startMinimized) pnpDefs'

isEqualPNP :: PNPDef -> PNPDef -> Bool
isEqualPNP ((name1, _, _, _, _), _, _, _, _) ((name2, _, _, _, _), _, _, _, _) =
  name1 == name2

pnpElemIndex :: PNPDef -> [PNPDef] -> Maybe Int
pnpElemIndex pnpDef = go 0
  where
    go _ [] = Nothing
    go i (x:xs)
      | isEqualPNP pnpDef x = Just i
      | otherwise           = go (i + 1) xs


pnpMinimizationRectsMap :: M.Map String W.RationalRect
pnpMinimizationRectsMap = M.fromList $ map (\pnpDef -> do
      let ((name,_,_,_,_), corner, width, height, _) = pnpDef
      -- let ogDef = if "*" `isInfixOf` name
      let ogDef = if "*" `isInfixOf` name
          then fromJust $ find (\((n,_,_,_,_),_,_,_,_) -> n == takeWhile (/= '*') name) pnpDefs'
          else pnpDef
      -- let filtered = filter (\(n, c, _, _, _) -> ((c == corner) && (not $ "*" `isInfixOf` n))) pnpDefs'
      -- let filtered = filter (\(n, c, _, _, _) -> ((c == corner) && (not $ "*" `isInfixOf` n))) pnpDefs'
      let filtered = filter (\((n,_,_,_,_), c, _, _, _) -> c == corner && not ("*" `isInfixOf` n)) pnpDefs'
      let idx = fromJust $ pnpElemIndex ogDef filtered
      (name, cornerRect idx width height corner)
  ) pnpDefs'


pnpDefsAsNSPDefs :: [NSPDef]
pnpDefsAsNSPDefs = map (\(pnpDef, _, _, _, _) -> pnpDef) pnpDefs'

nspDefs :: [NSPDef] = nspDefs' ++ pnpDefsAsNSPDefs

mapNSPDefToNSP :: [NSPDef] -> [NamedScratchpad]
mapNSPDefToNSP = map (\(cls, cmd, query, manage, _) ->
  NS cls cmd query (
    case manage of
      Left rect -> customFloating rect
      Right manageHook -> manageHook
    )
  )

mapPNPDefToNSP :: [PNPDef] -> [NamedScratchpad]
mapPNPDefToNSP = map (\((cls, cmd, query, maximizeManage, _),_,_,_,startMinimized) ->
  NS cls cmd query (
    if startMinimized then customFloating (fromJust $ M.lookup cls pnpMinimizationRectsMap)
    else case maximizeManage of
      Left rect -> customFloating rect
      Right manageHook -> manageHook
    )
  )

nsps :: [NamedScratchpad]
nsps = mapNSPDefToNSP nspDefs' ++ mapPNPDefToNSP pnpDefs'

openNSPOnScreen :: String -> ScreenId -> X ()
openNSPOnScreen name scn = do
  ensureNoPNPFocus
  openNSPOrPNPOnScreen name scn
  win <- findWinOnActiveWorkspace $ className =? name
  maybe ensureNoPNPFocus bringToFront win
  -- forM_ win bringToFront

openNSPOrPNPOnScreen :: String -> ScreenId -> X ()
openNSPOrPNPOnScreen name scn = doOnScreen scn (namedScratchpadAction nsps name)

hideNSP :: String -> X ()
hideNSP nsp = do
  let (_, _, q, _, _) = head $ filter (\(n, _, _, _, _) -> n == nsp) nspDefs
  win <- GNP.getNextMatch q GNP.Forward
  when (isJust win) $ do
    windows $ W.shiftWin "NSP" $ fromJust win

hideAllNSPs :: X ()
hideAllNSPs = mapM_ (\(n, _, _, _, _) -> hideNSP n) nspDefs'

hideAllNSPsAndPNPs :: X ()
hideAllNSPsAndPNPs = hidePNPs >> hideAllNSPs

------------------------------------------------------------------------
-- picture-in-picture binds:

-- cornerIndexes is a dict of all corner types and a list of the indexes of the PNPDefs that use that corner:


-- getPNPRectangleFromCornerUsingIndex :: PNPDef -> W.RationalRect
-- getPNPRectangleFromCornerUsingIndex pnpDef = do mempty


getPNPDefByClassName :: String -> PNPDef
getPNPDefByClassName cls = fromJust $ find (\((n, _, _, _, _),_,_,_,_) -> n == cls) pnpDefs'

data PnpBindParams = PnpBindParams
  {
    name :: String,
    toggleMaximizationBind :: (KeyMask, KeySym),
    toggleVisibilityBind :: (KeyMask, KeySym)
  }

ezPnpBinds = concatMap createBinds
  where
    getWin cls = GNP.getNextMatch (className =? cls) GNP.Forward
    createBinds PnpBindParams {name, toggleMaximizationBind, toggleVisibilityBind} = [
          (toggleMaximizationBind, pnpToggleMaximization pnpDef),
            (toggleVisibilityBind, maintainFocus $ pnpToggleVisibility pnpDef)
        ]
      where
        pnpDef = getPNPDefByClassName name

------------------------------------------------------------------------
-- picture-in-picture utils:

newtype HiddenPNPWindows = HiddenPNPWindows [Window]
  deriving (Typeable, Read, Show)

instance ExtensionClass HiddenPNPWindows where
  initialValue = HiddenPNPWindows []
  extensionType = PersistentExtension

hidePNPs :: X ()
hidePNPs = do
  winsToHide <- getPNPs
  unless (null winsToHide) $ do
    nspNames <- mapM (runQuery className) winsToHide
    mapM_ hideNSP nspNames
    XS.put $ HiddenPNPWindows winsToHide

unhidePNPs :: X ()
unhidePNPs = do
  HiddenPNPWindows hiddenWins <- XS.get
  nspNames <- mapM (runQuery className) hiddenWins
  mapM_ (`openNSPOrPNPOnScreen` 0) nspNames
  XS.put $ HiddenPNPWindows []
  fade <- getFadeOpacity <$> XS.get
  when (fade < defaultFadeOpacity) $ setFade defaultFadeOpacity

togglePNPs = maintainFocus $
  whenM ((/= "NSP") <$> getCurrentWorkspaceID) $ do
    HiddenPNPWindows hiddenWins <- XS.get
    if null hiddenWins then do
        visibleWins <- getPNPs
        if null visibleWins then greedyUnhidePNPs else hidePNPs
    else unhidePNPs

greedyUnhidePNPs :: X ()
greedyUnhidePNPs = do
  HiddenPNPWindows hiddenWins <- XS.get
  nspNames <- mapM (runQuery className) hiddenWins
  mapM_ (`openNSPOrPNPOnScreen` 0) (if null nspNames then map (\((n, _, _, _, _),_,_,_,_) -> n) pnpDefs' else nspNames)
  XS.put $ HiddenPNPWindows []
  fade <- getFadeOpacity <$> XS.get
  when (fade < defaultFadeOpacity) $ setFade defaultFadeOpacity

pnpMaximize :: PNPDef -> X ()
pnpMaximize pnpDef = do
  unhidePNPs
  win' <- findWinOnAnyWorkspace (className =? cls)
  when (isJust win') $ do
    let win = fromJust win'
    windows $ either
        (W.float win)
        (const $ W.sink win) -- TODO figure out how to use manageHook on just the targeted window here
        manage
      -- . bringToFront win
    -- withFocused $ \focused -> windows $ bringToFront win focused
    bringToFront win
    -- windows $ W.focusWindow win
    -- focus win
  where
    ((cls, cmd, _, manage, _),_,_,_,_) = pnpDef

ensureNoPNPFocus = withFocused $ \focused -> do
  cls <- runQuery className focused
  when ("PNP_" `isPrefixOf` cls) $ do
    ws <- gets windowset
    let stack' = W.stack $ W.workspace $ W.current ws
    when (isJust stack') $ do
      let stack = fromJust stack'
      let down = W.down stack
      toFocus <- filterM ( \w -> do
                                cls <- runQuery className w
                                return $ not $ "PNP_" `isPrefixOf` cls
                          ) down
      focus $ head toFocus

pnpMinimize :: Window -> PNPDef -> X ()
pnpMinimize win pnpDef = do
  let rect = fromJust $ M.lookup name pnpMinimizationRectsMap
  windows $ \ws -> W.float win rect ws
  where
    ((name, _, _, _, _),_,_,_,_) = pnpDef

pnpMinimizeAndReturnFocus :: Window -> Window -> PNPDef -> X ()
pnpMinimizeAndReturnFocus win originallyFocused pnpDef = do
  pnpMinimize win pnpDef
  if originallyFocused /= win then focus originallyFocused
  else ensureNoPNPFocus

pnpToggleMaximization :: PNPDef -> X ()
pnpToggleMaximization pnpDef = withFocused $ \originallyFocused -> do
    debugLog "TOGGLING MAXIMIZATION"
    win' <- findWinOnActiveWorkspace (className =? cls)
    if isNothing win' then do
      hideNSP cls
      openNSPOrPNPOnScreen cls 0
      win <- findWinOnActiveWorkspace (className =? cls)
      pnpMaximize pnpDef
    else do
      let win = fromJust win'
      ws <- gets windowset
      let isFloating = M.member win (W.floating ws)
      if isFloating then do
        case M.lookup win (W.floating ws) of
          Just (W.RationalRect x y w h) -> do
            if w < 0.4 then pnpMaximize pnpDef
            else pnpMinimizeAndReturnFocus win originallyFocused pnpDef
      else pnpMinimizeAndReturnFocus win originallyFocused pnpDef
  where
    ((cls, _, _, _, _),_,_,_,_) = pnpDef

pnpToggleVisibility :: PNPDef -> X ()
pnpToggleVisibility pnpDef = withFocused $ \originallyFocused -> do
    openNSPOrPNPOnScreen name 0
    win' <- findWinOnActiveWorkspace (className =? name)
    when (isJust win') $ focus originallyFocused
  where
    ((name, _, _, _, _),_,_,_,_) = pnpDef

doTimer :: X ()
doTimer = do
  let promptConfig = myPromptConfig
  inputPrompt promptConfig "minutes" ?+ \query -> do
    spawn $ "guitimer " ++ query

------------------------------------------------------------------------
-- keybindings:
getKeybindings conf =
  ---------------------------------------------------------------
  -- switch workspaces on the currently-focused screen:
  [ ((m .|. winMask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9],
      (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]
    ---------------------------------------------------------------
    -- switch screens:
    ++ [ ((m, key), do screenWorkspace sc >>= flip whenJust (windows . f))
         | (key, sc) <- zip [xK_comma, xK_period, xK_slash, xK_Shift_R, xK_Up] [0 ..],
           (f, m) <- [(W.view, winMask)]
       ]
    ---------------------------------------------------------------
    -- winBinds:
    ++ winBindsTmuxaStableView xK_semicolon 1
    ++ winBindsTmuxaStableView xK_comma 2
    ++ winBindsIDE [xK_b, xK_s]
    ++ [
      ((altMask, xK_o), toggleOrViewNoSP "project"),
      ((altMask+controlMask, xK_o), do
        -- windows $  W.view "project"
        seeWin defaultSeeWinParams
          { queryStr = "Google-chrome",
            notFoundAction = spawn "google-chrome --new-window --remote-debugging-port=9222 http://localhost:5173",
            exact = True,
            useClassName = True,
            extraAction = mempty
          })
    ]
    ++ ezPnpBinds [
      PnpBindParams
        { name = "PNP_whiteboard",
          toggleMaximizationBind = (altMask, xK_i),
          toggleVisibilityBind = (altMask+shiftMask, xK_i)
        },
      PnpBindParams
        { name = "PNP_proj",
          toggleMaximizationBind = (altMask+controlMask, xK_o),
          toggleVisibilityBind = (altMask+shiftMask, xK_o)
        },
      PnpBindParams
        { name = "PNP_log",
          toggleMaximizationBind = (altMask, xK_bracketright),
          toggleVisibilityBind = (altMask+shiftMask, xK_bracketright)
        },
      PnpBindParams
        { name = "PNP_alt_log",
          toggleMaximizationBind = (altMask, xK_bracketleft),
          toggleVisibilityBind = (altMask+shiftMask, xK_bracketleft)
        },
      PnpBindParams
        { name = "PNP_meeting",
          toggleMaximizationBind = (altMask, xK_z),
          toggleVisibilityBind = (altMask+shiftMask, xK_z)
        },
      PnpBindParams
        { name = "PNP_timer",
          toggleMaximizationBind = (altMask+controlMask+shiftMask, xK_backslash),
          toggleVisibilityBind = (altMask+controlMask, xK_a)
        }
    ]
    ++ ezWinBinds
      [
        ( xK_x,
          "code",
          spawn "not-dotfiles code -n",
          Just $ defaultWinBindsParams {_exact = True}
        ),
        -- ( xK_e,
        --   "tmux-pane-view",
        --   do
        --     windows $ focusScreen 0
        --     spawn "tmux-pane-view",
        --   Nothing
        -- ),
        ( xK_p,
          "firefox",
          spawn "firefox --new-window",
          Just $ defaultWinBindsParams {_useClassName = True}
        )
        -- ( xK_period,
        --   -- "partmin-ui",
        --   -- mempty,
        --   "netsoft-com.netsoft.hubstaff",
        --   spawn "/home/kyle/Hubstaff/HubstaffClient.bin.x86_64",
        --   Nothing
        -- ),
      ]
    ---------------------------------------------------------------
    -- NSPs:
    ++ ezNspBinds
      [
        (xK_u, "NSP_obsidian"),
        (xK_e, "NSP_spotify"),
        (xK_r, "NSP_files"),
        (xK_d, "NSP_testing"),
        (xK_q, "NSP_assistant"),
        (xK_t, "NSP_homelab"),
        (xK_w, "NSP_audio"),
        (xK_period, "NSP_hubstaff"),
        (xK_f, "NSP_discord")
      ]
    ---------------------------------------------------------------
    --
    ++ [
         ---------------------------------------------------------------
         -- window management:
         --  ((altMask, xK_h), toggleFocus),
         --  ((altMask + shiftMask, xK_h), swapWithLast),
        --  ((altMask + shiftMask + controlMask, xK_h), mempty),
         ((winMask, xK_Escape), kill),
         ((altMask, xK_Tab), windows W.focusDown),
         ((winMask, xK_j), windows W.focusDown),
         ((altMask + shiftMask, xK_Tab), windows W.focusUp),
         ((winMask, xK_k), windows W.focusUp),
         ((winMask .|. shiftMask, xK_j), windows W.swapDown),
         ((winMask .|. shiftMask, xK_k), windows W.swapUp),
         ((winMask + controlMask, xK_s), withFocused $ windows . W.sink),
         ((winMask + controlMask, xK_x), shiftNextScreen >> nextScreen),
         ((winMask + controlMask, xK_a), shiftPrevScreen >> prevScreen),
         ((winMask, xK_f), sinkAll),
         ((winMask + shiftMask, xK_g), windowPrompt myPromptConfig Goto allWindows),
         ((winMask, xK_g), windowPrompt myPromptConfig Goto wsWindows),
         ---------------------------------------------------------------
         -- layout management:
         ((winMask + shiftMask, xK_space), sendMessage NextLayout),
         ((winMask, xK_h), sendMessage Shrink),
         ((winMask, xK_l), sendMessage Expand),
         ---------------------------------------------------------------
         -- NSPs:
         ((winMask, xK_minus), toggleOrView "NSP"),
         ((altMask, xK_grave),
            whenM ((/= "NSP") <$> getCurrentWorkspaceID) $ do
              nspsToHide <- getNSPs
              if not $ null nspsToHide then hideAllNSPs
              else do
                pnpsToHide <- getPNPsOnActiveWorkspace
                -- toHide <- findWinsOnActiveWorkspace $ fmap (\cls -> any (`isPrefixOf` cls) ["PNP_", "NSP_"]) className
                -- debugLog toHide
                if null pnpsToHide then togglePNPs
                else hideAllNSPsAndPNPs
          ),
         ((altMask+controlMask, xK_grave), togglePNPs),
         ((altMask, xK_Escape),
           do
             ws <- gets windowset
             let focused' = runQuery className . W.focus <$> W.stack (W.workspace (W.current ws))
             if isJust focused' then do
              focused <- fromJust focused'
              if focused == "NSP_tmuxa-2" then hideNSP "NSP_tmuxa-2"
              else if focused == "NSP_remonad" then hideNSP "NSP_remonad"
              else openNSPOnScreen "NSP_tmuxa-1" 0
             else do
              openNSPOnScreen "NSP_tmuxa-1" 0
         ),
         ( (altMask + controlMask, xK_Escape),
           do
             hideNSP "NSP_tmuxa-1"
             openNSPOnScreen "NSP_tmuxa-2" 0
         ),
         ------------------------------------------------------------
         -- window fade:
         ((altMask+shiftMask+controlMask, xF86XK_AudioLowerVolume), maintainFocus $ rotaryAdjustFade (subtract 0.1)),
         ((altMask+controlMask, xF86XK_AudioLowerVolume), maintainFocus $ rotaryAdjustFade (subtract 10)),
         ((altMask+shiftMask+controlMask, xF86XK_AudioRaiseVolume), maintainFocus $ rotaryAdjustFade (+0.1)),
         ((altMask+controlMask, xF86XK_AudioRaiseVolume), maintainFocus $ rotaryAdjustFade (+10)),
         ------------------------------------------------------------
         -- volume:
         ---------- up/down
         ((0, xK_Insert), spawn "volctrl -d"),
         ((0, xK_Print), spawn "volctrl -u"),
         ((0, xF86XK_AudioLowerVolume), spawn "volctrl -d"),
         ((0, xF86XK_AudioRaiseVolume), spawn "volctrl -u"),
         ---------- up/down (coarse)
         ((altMask, xK_Insert), spawn "volctrl -dc"),
         ((altMask, xK_Print), spawn "volctrl -uc"),
         ((altMask, xF86XK_AudioLowerVolume), spawn "volctrl -dc"),
         ((altMask, xF86XK_AudioRaiseVolume), spawn "volctrl -uc"),
         ---------- up/down (fine)
         ((altMask + shiftMask, xK_Insert), spawn "volctrl -df"),
         ((altMask + shiftMask, xK_Print), spawn "volctrl -uf"),
         ((altMask + shiftMask, xF86XK_AudioLowerVolume), spawn "volctrl -df"),
         ((altMask + shiftMask, xF86XK_AudioRaiseVolume), spawn "volctrl -uf"),
         ---------- mute
         ((altMask + shiftMask + controlMask, xK_Print), spawn "volmute"),
        --  ((altMask + shiftMask + controlMask, xF86XK_AudioLowerVolume), spawn "volmute"),
         ((0, xF86XK_AudioMute), spawn "volmute"),
         ---------------------------------------------------------------
         -- media control:
         ((winMask, 0xffff), spawn "playerctl play-pause"),
         ((winMask + shiftMask + controlMask, 0xffff), spawn "playerctl pause"),
         ------------------------------------------------------------
         -- redshift:
         ---------- temperature
         ((winMask, xF86XK_AudioLowerVolume), spawn "setredshift -td"),
         ((winMask, xF86XK_AudioRaiseVolume), spawn "setredshift -ti"),
         ---------- brightness
         ((winMask + shiftMask, xF86XK_AudioLowerVolume), spawn "setredshift -bd"),
         ((winMask + shiftMask, xF86XK_AudioRaiseVolume), spawn "setredshift -bi"),
         ---------- reset
         ((winMask + controlMask + shiftMask, xF86XK_AudioLowerVolume), spawn "setredshift --reset"),
         ---------------------------------------------------------------
         -- apps
         ((altMask+shiftMask+controlMask, xK_i), do
            -- let ((_,cmd,_,_,_),_,_,_,_) = getPNPDefByClassName "PNP_whiteboard"
            -- spawn $ cmd ++ " -kiosk"
            -- spawn "firefox -P clone5 --class PNP_whiteboard --new-window https://excalidraw.com"
            pnpToggleMaximization $ getPNPDefByClassName "PNP_whiteboard*nokiosk"
          ),
         ((altMask+controlMask+shiftMask, xK_a), doTimer),
         ((altMask+controlMask, xK_apostrophe), spawn "toggle-alacritty-transparent"),
         ((altMask+controlMask+shiftMask, xK_apostrophe), spawn "toggle-picom-blur"),
         ((winMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
         ((winMask, xK_space), spawn "dmenu-custom"),
         ((altMask+shiftMask, xK_a), searchDocs "haskell"),
         ((altMask, xK_a), openNSPOrPNPOnScreen "NSP_docs" 0),
         ((altMask+controlMask, xK_u), spawn "spawn-with-name obsidian-alt obsidian \"obsidian \'obsidian://open?vault=mainmirror\'\" 2"),
         ((winMask + shiftMask, xK_s), spawn "flameshot gui &"),
        --  ( (altMask, xK_z),
        --    seeWin
        --      SeeWinParams
        --        { queryStr = "zoom",
        --          notFoundAction = mempty,
        --          greedy = False,
        --          searchBackwards = False,
        --          exact = True,
        --          useClassName = True
        --        }
        --  ),
         ( (winMask + shiftMask, xK_e),
           seeWin
             SeeWinParams
               { queryStr = "org.gnome.Nautilus",
                 notFoundAction = spawn "nautilus",
                 greedy = False,
                 searchBackwards = False,
                 exact = True,
                 useClassName = False
               }
         ),
         ( (winMask, xK_e),
           seeWin
             SeeWinParams
               { queryStr = "ranger",
                 notFoundAction = spawn "unique-term ranger ranger",
                 greedy = False,
                 searchBackwards = False,
                 exact = True,
                 useClassName = False
               }
          ),
          ---------------------------------------------------------------
          -- system
          ((winMask , xK_m), sendMessage ToggleStruts),
          ((winMask, xK_q), do
              win <- findWinOnAnyWorkspace $ className =? "NSP_remonad"
              if isJust win then openNSPOnScreen "NSP_remonad" 0
              else confirmRemonad
          ),
         ((winMask + altMask, xK_q), confirmRemonad),
         ((altMask + controlMask + shiftMask, xK_F10), spawn "xlock -mode random"),
         ((altMask + controlMask + shiftMask, xK_F11), spawn "systemctl suspend"),
         ((winMask + altMask + controlMask + shiftMask, xK_F11), confirmCmd "sudo reboot now"),
         ((winMask + altMask + controlMask + shiftMask, xK_F12), confirmCmd "shutdown now"),
         ((winMask + shiftMask, xK_q), confirm "logout" $ io exitSuccess),
         ((winMask + shiftMask + controlMask, xK_q), confirmCmd "configure-multihead"),
         ((altMask + controlMask + shiftMask, xK_q), confirm "rescreen" rescreen),
         ---------------------------------------------------------------
         -- scripts
         ((altMask + shiftMask, xK_Delete), spawn "vpnctrl --up"),
         ((altMask + shiftMask + controlMask, xK_Delete), spawn "vpnctrl --down"),
         ((altMask+shiftMask, xK_slash), spawn "toggle-kp"),
         ((altMask, xK_slash), spawn "toggle-screen-blanking")
         ---------------------------------------------------------------
         -- ephemeral
        --  ( (altMask, xK_1),
        --    do
        --      killAllWindowsByClass $ className =? "Chromium-browser"
        --      spawn "run-on-tmux-pane-view \"yarn start | tee main.log\""
        --      windows $ focusScreen 0
        --  ),
        --  ( (altMask, xK_2),
        --    do
        --      killAllWindowsByClass $ className =? "Chromium-browser"
        --      spawn "run-on-tmux-pane-view"
        --  )
       ]

------------------------------------------------------------------------
-- main conf:
getConf xmproc =
  def
    { terminal = "alacritty",
      focusFollowsMouse = True,
      clickJustFocuses = False,
      borderWidth = 0,
      modMask = winMask,
      startupHook = myStartupHook,
      workspaces = myWorkspaces ++ ["NSP", "project"],
      normalBorderColor = "#000000",
      focusedBorderColor = "#ffffff",
      mouseBindings = myMouseBindings,
      layoutHook =
        refocusLastLayoutHook
          $ ( SWNP.showWName' $
                def
                  { SWNP.swn_font = "xft:Monospace:pixelsize=20:regular:hinting=true",
                    SWNP.swn_fade = 0.5,
                    SWNP.swn_bgcolor = "blue",
                    SWNP.swn_color = "red"
                  }
            )
          $ avoidStruts myLayout,
      manageHook = namedScratchpadManageHook nsps <+> myManageHook <+> manageSpawn,
      handleEventHook = myHandleEventHook,
      logHook = myLogHook xmproc
    }

------------------------------------------------------------------------
-- main:
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar -x 2 $HOME/.xmonad/xmobarrc; toggle-screen-blanking --init; toggle-kp --init"
  let conf = getConf xmproc
  xmonad $ ewmh $ docks $ conf `additionalKeys` getKeybindings conf
