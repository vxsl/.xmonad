{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad
import Data.List
import Data.Map qualified as M
import Data.Maybe
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
import XMonad.Layout.HiddenPatched
import XMonad.Hooks.ManageHelpers

winMask, altMask :: KeyMask
winMask = mod4Mask
altMask = mod1Mask

------------------------------------------------------------------------
-- workspaces:
numScreens :: ScreenId
numScreens = 1

numWorkspacesPerScreen :: Int
numWorkspacesPerScreen = 9

myWorkspaces :: [String]
myWorkspaces = withScreens numScreens ([show n | n <- [1 .. numWorkspacesPerScreen]])

------------------------------------------------------------------------
-- misc. helpers:
debugLog :: Show a => a -> X ()
debugLog str =
  io $ appendFile "/home/kyle/.xmonad-debug-log" $ show str ++ "\n"

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

cornerRect :: Rational -> Int -> W.RationalRect
cornerRect s idx = W.RationalRect x y w h
  where
    w = s
    h = s
    x = 1 - w
    y = 1 - h - (fromIntegral idx * h)

maintainFocus :: X () -> X ()
maintainFocus action = do
  ws <- gets windowset
  let focused = W.focus <$> W.stack (W.workspace (W.current ws))
  action
  focus $ fromJust focused

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
  hiddenWindows $
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
myStartupHook :: X ()
myStartupHook = do
  sendMessage (SetStruts [] [U,L])
  spawnOnce "nitrogen --restore &"
  mapM_ cycleAllWorkspacesOnScreen [0 .. (numScreens - 1)]
  hideAllWindowsWithClassPrefix "pnp_"
  windows $ focusScreen 0
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
      title =? "Profile error occurred" --> doFloat,
      appName =? "code-insiders-url-handler (remote-debug-profile)" --> doShift "2_1",
      className =? "Chromium-browser" --> doShift "2_1",
      className =? "tmux-pane-view" --> doShift "1_1",
      className =? "Google-chrome" --> doShift "project",
      className =? "partmin-ui" --> doShift "project"
    ]

------------------------------------------------------------------------
-- log hook:
newtype LastFocusedWindow = LastFocusedWindow (Maybe Window)
    deriving (Typeable, Read, Show)
instance ExtensionClass LastFocusedWindow where
    initialValue = LastFocusedWindow Nothing
myLogHook xmproc = do
  refocusLastLogHook
  fadeOutLogHook
    (fadeIf
      ((&&) <$> isUnfocused <*> (className =? "Com.github.xournalpp.xournalpp" <||> (("pnp_" `isPrefixOf`) <$> className)))
      0.2
    )
  nsHideOnFocusLoss
    (mapToNSP $ filter (\(_, _, _, _, hideOnFocusLoss) -> hideOnFocusLoss) nspDefs)
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
        when ("pnp_" `isPrefixOf` cls) $
          let pnpDef = getPnpDefByClassName cls
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
  [ ((altMask, keySym), openNSPOnScreen nspName 0),
    ( (altMask + shiftMask, keySym),
      do
        hideNSP nspName
        openNSPOnScreen nspName 0
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
    exact :: Bool,
    useClassName :: Bool,
    extraAction :: X () -- extra action to perform when the window is found or created
  }

defaultWinBindsParams :: WinBindsParams -- default WinBindsParams
defaultWinBindsParams =
  WinBindsParams
    { exact = False,
      useClassName = False,
      extraAction = mempty
    }

winBinds :: WinBindsParams -> [((KeyMask, KeySym), X ())]
winBinds WinBindsParams {keySym, queryStr, notFoundAction, exact, useClassName, extraAction} =
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
          exact = exact,
          useClassName = useClassName,
          extraAction = extraAction
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
          ++ " $HOME\""
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
                  exact = ops.exact,
                  useClassName = ops.useClassName,
                  extraAction = ops.extraAction
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
  openNSPOnScreen "NSP_docs" 0
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
    ManageHook, -- manage hook called for application window, use it to define the placement.
    Bool -- hide on focus loss
  )

nspDefs :: [NSPDef]
nspDefs =
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
      customFloating $ centerRect 0.8,
      True
    ),
    ( "NSP_docs",
      "google-chrome-unstable --class=NSP_docs --new-window \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page \
      \--new-tab chrome://new-tab-page",
      className =? "NSP_docs",
      customFloating $ centerRect 0.8,
      True
    ),
    ( "NSP_browse",
      "firefox -P clone3 --class NSP_browse",
      className =? "NSP_browse",
      -- "chromium-browser",
      -- className =? "Chromium-browser",
      nonFloating,
      False
    ),
    ( "NSP_files",
      "nautilus",
      className =? "org.gnome.Nautilus",
      nonFloating,
      False
    ),
    ( "NSP_discord",
      "discord",
      className =? "discord",
      nonFloating,
      False
    ),
    ( "NSP_obsidian",
      "spawn-with-name NSP_obsidian obsidian 'obsidian \"obsidian://open?vault=main\"' 2",
      className =? "NSP_obsidian",
      customFloating $ centerRect 0.95,
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
      nonFloating,
      True
    ),
    ( "NSP_tmuxa-1",
      "unique-term NSP_tmuxa-1 \"tmuxa tmuxa-1 $HOME\"",
      className =? "NSP_tmuxa-1",
      customFloating $ centerRect 0.9,
      False
    ),
    ( "NSP_tmuxa-2",
      "unique-term NSP_tmuxa-2 \"tmuxa tmuxa-2 $HOME\" " ++ " /home/kyle/.config/alacritty/alacritty2.toml",
      className =? "NSP_tmuxa-2",
      customFloating $ centerRect 0.9,
      False
    ),
    ( "NSP_tmuxa-3",
      "unique-term NSP_tmuxa-3 \"zsh\" " ++ " /home/kyle/.config/alacritty/alacritty3.toml",
      className =? "NSP_tmuxa-3",
      customFloating $ centerRect 0.4,
      False
    ),
    ( "NSP_testing",
      "",
      className =? "Cypress",
      customFloating $ centerRect 0.7,
      False
    ),
    ( "NSP_meeting",
      "chromium-browser --user-data-dir=/home/kyle/.config/chromium/DefaultClone2 --class=NSP_meeting --new-window --app='https://us04web.zoom.us/myhome' --start-fullscreen",
      className =? "NSP_meeting",
      customFloating $ centerRect 0.7,
      False
    ),
    ( "NSP_audio",
      "firefox -P clone2 --class NSP_audio --new-window \
      \-new-tab -url https://open.spotify.com/ \
      \-new-tab -url https://noises.online/",
      className =? "NSP_audio",
      customFloating $ centerRect 0.7,
      False
    ),
    ( "NSP_spotify",
      "spotify",
      className =? "Spotify",
      customFloating $ centerRect 0.7,
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
    --   nonFloating,
    --   True
    -- ),
    ( "NSP_hubstaff",
      "/home/kyle/Hubstaff/HubstaffClient.bin.x86_64",
      className =? "Netsoft-com.netsoft.hubstaff",
      customFloating $ centerRect 0.3,
      -- nonFloating,
      True
    )
  ]

mapToNSP :: [NSPDef] -> [NamedScratchpad]
mapToNSP = map (\(n, tn, cn, cf, _) -> NS n tn cn cf)

nsps :: [NamedScratchpad]
nsps = mapToNSP nspDefs

openNSP :: String -> X ()
openNSP = namedScratchpadAction nsps

openNSPOnScreen :: String -> ScreenId -> X ()
openNSPOnScreen name scn = doOnScreen scn $ namedScratchpadAction nsps name

hideNSP :: String -> X ()
hideNSP nsp = do
  let (_, _, q, _, _) = head $ filter (\(n, _, _, _, _) -> n == nsp) nspDefs
  win <- GNP.getNextMatch q GNP.Forward
  when (isJust win) $ do
    windows $ W.shiftWin "NSP" $ fromJust win

hideAllNSPs :: X ()
hideAllNSPs =
  mapM_
    ( \(_, _, q, _, _) -> do
        win <- GNP.getNextMatch q GNP.Forward
        when (isJust win) $ do
          windows $ W.shiftWin "NSP" $ fromJust win
    )
    nspDefs

------------------------------------------------------------------------
-- picture-in-picture binds:
type PnpDef' = (String, String)

pnpDefs' :: [PnpDef']
pnpDefs' = [
    (
      "pnp_whiteboard",
      "firefox -P clone5 --class pnp_whiteboard --new-window https://whimsical.com"
    ),
    (
      "pnp_proj",
      "multi-instance-chromium-browser --class=pnp_proj --new-window --app='http://localhost:5173' --start-fullscreen --remote-debugging-port=9222"
    )
  ]

type PnpDef = (Int, String, String)
pnpDefs = zipWith (\i (n, c) -> (i, n, c)) [0..] pnpDefs'

getPnpDefByClassName :: String -> PnpDef
getPnpDefByClassName cls = fromJust $ find (\(_, n, _) -> n == cls) pnpDefs

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
          (toggleMaximizationBind, do
              win <- getWin name
              if isNothing win then pnpSpawnMaximized pnpDef else pnpToggleMaximization pnpDef
          ),
          (toggleVisibilityBind, do
              win <- getWin name
              if isNothing win then pnpSpawnMinimized pnpDef else hideAllWindowsWithClassPrefix "pnp_"
          )
        ]
      where 
        pnpDef = getPnpDefByClassName name
        (index, _, cmd) = pnpDef

------------------------------------------------------------------------
-- picture-in-picture utils:

pnpMaximizeAndFocus :: Window -> X ()
pnpMaximizeAndFocus win = do
  windows $ \ws -> W.focusWindow win $ W.float win (centerRect 0.7) $ W.insertUp win $ W.delete win ws

pnpMinimize :: Window -> PnpDef -> X ()
pnpMinimize win (i,_,_) = do
  windows $ \ws -> W.float win (cornerRect 0.2 i) ws

pnpMinimizeAndReturnFocus :: Window -> Window -> PnpDef -> X ()
pnpMinimizeAndReturnFocus win originallyFocused pnpDef = do
  pnpMinimize win pnpDef
  if originallyFocused /= win then do
    focus originallyFocused
  else windows W.focusDown

pnpToggleMaximization :: PnpDef -> X ()
pnpToggleMaximization pnpDef = withFocused $ \originallyFocused -> do
    win' <- GNP.getNextMatch (className =? cls) GNP.Forward
    when (isNothing win') $ return mempty
    let win = fromJust win'
    ws <- gets windowset
    let isFloating = M.member win (W.floating ws)
    if isFloating then do
      case M.lookup win (W.floating ws) of
        Just (W.RationalRect x y w h) -> do
          if w < 0.4 then pnpMaximizeAndFocus win
          else pnpMinimizeAndReturnFocus win originallyFocused pnpDef
    else pnpMinimizeAndReturnFocus win originallyFocused pnpDef
  where 
    (i,cls,_) = pnpDef

pnpSpawnMaximized :: PnpDef -> X ()
pnpSpawnMaximized pnpDef = do
  popWindowWithClass cls
  win <- GNP.getNextMatch (className =? cls) GNP.Forward
  maybe (spawn cmd) pnpMaximizeAndFocus win
  where
    (i,cls,cmd) = pnpDef

pnpSpawnMinimized :: PnpDef -> X ()
pnpSpawnMinimized pnpDef = maintainFocus $ do
  popWindowWithClass cls
  win <- GNP.getNextMatch (className =? cls) GNP.Forward
  if isNothing win then spawn cmd else pnpMinimize (fromJust win) pnpDef
  where 
    (i,cls,cmd) = pnpDef

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
        { name = "pnp_whiteboard",
          toggleMaximizationBind = (altMask, xK_i),
          toggleVisibilityBind = (altMask+shiftMask, xK_i)
        },
      PnpBindParams
        { name = "pnp_proj",
          toggleMaximizationBind = (altMask+controlMask, xK_o),
          toggleVisibilityBind = (altMask+shiftMask, xK_o)
        }
    ]
    ++ ezWinBinds
      [
        ( xK_x,
          "code",
          spawn "not-dotfiles code -n",
          Just $ defaultWinBindsParams {exact = True}
        ),
        ( xK_e,
          "tmux-pane-view",
          do
            windows $ focusScreen 0
            spawn "tmux-pane-view",
          Nothing
        ),
        ( xK_p,
          "firefox",
          spawn "firefox --new-window",
          Just $ defaultWinBindsParams {useClassName = True}
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
        (xK_z, "NSP_meeting"),
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
         ((altMask, xK_grave), do
            hideAllNSPs
            hideAllWindowsWithClassPrefix "pnp_"
         ),
         ((altMask+controlMask, xK_grave), maintainFocus $ do
            toggleAllWindowsWithClassPrefix "pnp_"
          ),
         ( (altMask, xK_Escape),
           do
             ws <- gets windowset
             tmuxa2 <- GNP.getNextMatch (winQuery False False "NSP_tmuxa-2") GNP.Forward
             let focused = W.focus <$> W.stack (W.workspace (W.current ws))
             if focused == tmuxa2
               then hideNSP "NSP_tmuxa-2"
               else openNSPOnScreen "NSP_tmuxa-1" 0
         ),
         ( (altMask + controlMask, xK_Escape),
           do
             hideNSP "NSP_tmuxa-1"
             openNSPOnScreen "NSP_tmuxa-2" 0
         ),
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
         ((altMask + shiftMask + controlMask, xF86XK_AudioLowerVolume), spawn "volmute"),
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
         ((winMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
         ((winMask, xK_space), spawn "dmenu-custom"),
         ((altMask, xK_a), searchDocs "tailwindcss"),
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
         ((winMask + altMask, xK_q), confirmCmd "remonad --restart"),
         ((altMask + controlMask + shiftMask, xK_F10), spawn "xlock -mode random"),
         ((altMask + controlMask + shiftMask, xK_F11), spawn "toggle-kp --off; systemctl suspend"),
         ((winMask + altMask + controlMask + shiftMask, xK_F11), confirmCmd "sudo reboot now"),
         ((winMask + altMask + controlMask + shiftMask, xK_F12), confirmCmd "shutdown now"),
         ((winMask + shiftMask, xK_q), confirm "logout" $ io exitSuccess),
         ((winMask + shiftMask + controlMask, xK_q), confirmCmd "configure-multihead"),
         ((altMask + controlMask + shiftMask, xK_q), confirm "rescreen" rescreen),
         ---------------------------------------------------------------
         -- scripts
         ((altMask + shiftMask, xK_Delete), spawn "vpnctrl --up"),
         ((altMask + shiftMask + controlMask, xK_Delete), spawn "vpnctrl --down"),
         ((altMask, xK_slash), spawn "toggle-kp"),
         ---------------------------------------------------------------
         -- ephemeral
         ( (altMask, xK_1),
           do
             killAllWindowsByClass $ className =? "Chromium-browser"
             spawn "run-on-tmux-pane-view \"yarn start | tee main.log\""
             windows $ focusScreen 0
         ),
         ( (altMask, xK_2),
           do
             killAllWindowsByClass $ className =? "Chromium-browser"
             spawn "run-on-tmux-pane-view"
         )
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
      handleEventHook = refocusLastWhen $ refocusingIsActive <||> isFloat,
      logHook = myLogHook xmproc
    }

------------------------------------------------------------------------
-- main:
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar -x 2 $HOME/.xmonad/xmobarrc"
  spawn "get-kp-status"
  let conf = getConf xmproc
  xmonad $ ewmh $ docks $ conf `additionalKeys` getKeybindings conf
