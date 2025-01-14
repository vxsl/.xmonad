{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Control.Monad
import Data.List
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
import System.Posix (sleep, touchFile)
import System.Directory (removeFile, renameFile, doesFileExist)

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
-- logging:
debugLogFile="/home/kyle/.xmonad-debug-log"
persistentLogFile="/home/kyle/.xmonad-persistent-log"

logToFile :: String -> (Show a, Typeable a) => a -> X ()
logToFile path str =
  let output = case cast str of
                 Just s -> s            -- If `str` is a `String`, use it directly
                 Nothing -> show str    -- Otherwise, use `show`
  in io $ appendFile path (output ++ "\n")

debugLog msg = logToFile debugLogFile msg

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

bottomRightCornerRect :: Rational -> Int -> W.RationalRect
bottomRightCornerRect s idx = W.RationalRect x y w h
  where
    w = s
    h = s
    x = 1 - w
    y = 1 - h - (fromIntegral idx * h)

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

bringToFront win ws =  W.focusWindow win $ W.insertUp win $ W.delete win ws

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
      "focus: " ++ printName stack.focus ++
      "\nup: " ++ concatMap (\w -> show w ++ " " ++ printName w ++ ", ") (stack.up) ++
      "\ndown: " ++ concatMap (\w -> show w ++ " " ++ printName w ++ ", ") (stack.down)
  else return "empty"


getCurrentWorkspaceID :: X String
getCurrentWorkspaceID = do
  ws <- gets windowset
  return $ W.tag $ W.workspace $ W.current ws

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
  sendMessage (SetStruts [] [U,L])
  spawnOnce "nitrogen --restore &"
  mapM_ cycleAllWorkspacesOnScreen [0 .. (numScreens - 1)]
  windows $ focusScreen 0

  let pnpDef = getPnpDefByClassName "PNP_xmonad-log"
  let (_, (cls, cmd, _, _, _)) = pnpDef

  spawn $ "xdotool search --classname " ++ cls ++ " windowclose"
  liftIO $ sleep 1

  oldCopyExists <- io $ doesFileExist (debugLogFile ++ ".1")
  when oldCopyExists $ io $ removeFile (debugLogFile ++ ".1")
  curExists <- io $ doesFileExist debugLogFile
  when curExists $ io $ renameFile debugLogFile (debugLogFile ++ ".1")

  spawn cmd
  liftIO $ sleep 1 -- TODO find a less hacky solution to wait for previous commands to finish

  spawn $ "\
     \echo ==================================================================== >> " ++ debugLogFile ++ "\
     \ && date >> " ++ debugLogFile ++ "\
     \ && echo ==================================================================== >> " ++ persistentLogFile ++ "\
     \ && date >> " ++ persistentLogFile ++ "\
     \"
  liftIO $ sleep 1


  debugLog "=====================================================================\n"
  debugLog "*********************************************************"
  logXmonadState
  debugLog "*********************************************************\n"

  debugLog "*********************************************************"
  workspaceState <- printWorkspaceState
  debugLog workspaceState
  debugLog "*********************************************************\n"

  spawn $ "x-summary >> " ++ debugLogFile

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
  -- summarizeWorkspaceStateEventHook .
  resizeHook <> refocusLastWhen (refocusingIsActive <||> isFloat)


------------------------------------------------------------------------
-- log hook:

newtype FadeOpacity = FadeOpacity { getFadeOpacity :: Rational }
  deriving (Typeable)
instance ExtensionClass FadeOpacity where
  initialValue = FadeOpacity 0.2
setFade newVal = do
  XS.modify $ const $ FadeOpacity newVal
  refresh

rotaryAdjustFade f = do
  val <- getFadeOpacity <$> XS.get
  let newVal = max 0 (min 1 (f val))
  if val <= 0 && newVal > val then do
    greedyUnhidePNPs
    setFade $ max 0.2 newVal
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
        when ("PNP_" `isPrefixOf` cls) $
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
    Either W.RationalRect ManageHook,
    Bool -- hide on focus loss
  )

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
      Left $ centerRect 0.95,
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
    ( "NSP_meeting",
      "chromium-browser --user-data-dir=/home/kyle/.config/chromium/DefaultClone2 --class=NSP_meeting --new-window --app='https://us04web.zoom.us/myhome' --start-fullscreen",
      className =? "NSP_meeting",
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
    )
  ]

pnpDefs' :: [NSPDef] = [
    ( "PNP_xmonad-log",
        "tmux kill-session -t tmuxa-pnp-log; unique-term PNP_xmonad-log "
        ++ "\"zsh -i -c 'tmuxa tmuxa-pnp-log --dir=$HOME --no-layout --cmd=\\\"tail -f " ++ debugLogFile ++ "\\\"'\" "
        ++ "$HOME/.config/alacritty/transparent.toml",
        className =? "PNP_xmonad-log",
        Left $ centerRect 0.7,
        False
    ),
    ( "PNP_whiteboard",
      "firefox -P clone5 --class PNP_whiteboard --new-window https://whimsical.com",
      className =? "PNP_whiteboard",
      Left $ centerRect 0.7,
      False
    ),
    ( "PNP_proj",
      "multi-instance-chromium-browser --class=PNP_proj --new-window --app='http://localhost:5173' --start-fullscreen --remote-debugging-port=9222",
      className =? "PNP_proj",
      Left $ centerRect 0.7,
      False
    )
  ]

nspDefs :: [NSPDef] = nspDefs' ++ pnpDefs'

mapToNSP :: [NSPDef] -> [NamedScratchpad]
mapToNSP = map (\(cls, cmd, query, manage, _) -> 
  NS cls cmd query (
    -- TODO this is a bandaid. Will have to refactor, because NSPDefs assume the initial window state is equivalent to the maximized state
    if cls == "PNP_xmonad-log" then customFloating (bottomRightCornerRect 0.2 0) <+> doF W.focusDown

    else case manage of
      Left rect -> customFloating rect
      Right manageHook -> manageHook
    )
  )

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
hideAllNSPs = do
  hidePNPs
  mapM_ (\(n, _, _, _, _) -> hideNSP n) nspDefs'

------------------------------------------------------------------------
-- picture-in-picture binds:
type PnpDef = (Int, NSPDef)
pnpDefs = zip [0..] pnpDefs'

getPnpDefByClassName :: String -> PnpDef
getPnpDefByClassName cls = fromJust $ find (\(_, (n, _, _, _, _)) -> n == cls) pnpDefs

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
            (toggleVisibilityBind, pnpToggleVisibility pnpDef)
        ]
      where
        pnpDef = getPnpDefByClassName name

------------------------------------------------------------------------
-- picture-in-picture utils:

newtype HiddenPNPWindows = HiddenPNPWindows [Window]
  deriving (Typeable, Read, Show)

instance ExtensionClass HiddenPNPWindows where
  initialValue = HiddenPNPWindows []
  extensionType = PersistentExtension

hidePNPs :: X ()
hidePNPs = do
  winsToHide <- findWinsOnActiveWorkspace (("PNP_" `isPrefixOf`) <$> className)
  nspNames <- mapM (runQuery className) winsToHide
  mapM_ hideNSP nspNames
  XS.put $ HiddenPNPWindows winsToHide

unhidePNPs :: X ()
unhidePNPs = do
  HiddenPNPWindows hiddenWins <- XS.get
  nspNames <- mapM (runQuery className) hiddenWins
  mapM_ (`openNSPOnScreen` 0) nspNames
  XS.put $ HiddenPNPWindows []
  fade <- getFadeOpacity <$> XS.get
  when (fade <= 0.2) $ setFade 0.2

greedyUnhidePNPs :: X ()
greedyUnhidePNPs = do
  HiddenPNPWindows hiddenWins <- XS.get
  nspNames <- mapM (runQuery className) hiddenWins
  mapM_ (`openNSPOnScreen` 0) (if null nspNames then map (\(_, (n, _, _, _, _)) -> n) pnpDefs else nspNames)
  XS.put $ HiddenPNPWindows []
  fade <- getFadeOpacity <$> XS.get
  when (fade <= 0.2) $ setFade 0.2

pnpMaximize :: PnpDef -> X ()
pnpMaximize pnpDef = do
  unhidePNPs
  win' <- findWinOnAnyWorkspace (className =? cls)
  if isJust win' then do
    let win = fromJust win'
    windows $ either
        (W.float win)
        (const $ W.sink win) -- TODO figure out how to use manageHook on just the targeted window here
        manage
      . bringToFront win
  else persistentLog Error ("pnpMaximize: window not found: " ++ cls)
  where
    (i, (cls, cmd, _, manage, _)) = pnpDef

ensureNoPNPFocus = do
  ws <- gets windowset
  let stack' = W.stack $ W.workspace $ W.current ws
  when (isJust stack') $ do
    let stack = fromJust stack'
    let down = stack.down
    toFocus <- filterM ( \w -> do
                              cls <- runQuery className w
                              return $ not $ "PNP_" `isPrefixOf` cls
                        ) down
    focus $ head toFocus


pnpMinimize :: Window -> PnpDef -> X ()
pnpMinimize win (i,_) = do
  windows $ \ws -> W.float win (bottomRightCornerRect 0.2 i) ws


pnpMinimizeAndReturnFocus :: Window -> Window -> PnpDef -> X ()
pnpMinimizeAndReturnFocus win originallyFocused pnpDef = do
  pnpMinimize win pnpDef
  if originallyFocused /= win then focus originallyFocused
  else ensureNoPNPFocus

pnpToggleMaximization :: PnpDef -> X ()
pnpToggleMaximization pnpDef = withFocused $ \originallyFocused -> do
    win' <- findWinOnActiveWorkspace (className =? cls)
    if isNothing win' then do
      hideNSP cls
      openNSPOnScreen cls 0
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
    (i, (cls, _, _, _, _)) = pnpDef

pnpToggleVisibility :: PnpDef -> X ()
pnpToggleVisibility pnpDef = withFocused $ \originallyFocused -> do
    openNSPOnScreen name 0
    win' <- findWinOnActiveWorkspace (className =? name)
    when (isJust win') $ focus originallyFocused
  where
    (i, (name, _, _, _, _)) = pnpDef


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
        { name = "PNP_xmonad-log",
          toggleMaximizationBind = (altMask, xK_bracketright),
          toggleVisibilityBind = (altMask+shiftMask, xK_bracketright)
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
         ((altMask, xK_grave), hideAllNSPs),
         ((altMask+controlMask, xK_grave), maintainFocus $
            whenM ((/= "NSP") <$> getCurrentWorkspaceID) $ do
              HiddenPNPWindows hiddenWins <- XS.get
              if null hiddenWins then do
                  visibleWins <- findWinsOnActiveWorkspace (("PNP_" `isPrefixOf`) <$> className)
                  if null visibleWins then greedyUnhidePNPs else hidePNPs
              else unhidePNPs
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
         -- window fade:
         ((altMask+controlMask, xF86XK_AudioLowerVolume), maintainFocus $ rotaryAdjustFade (subtract 0.1)),
         ((altMask+shiftMask+controlMask, xF86XK_AudioLowerVolume), maintainFocus $ rotaryAdjustFade (subtract 10)),
         ((altMask+controlMask, xF86XK_AudioRaiseVolume), maintainFocus $ rotaryAdjustFade (+0.1)),
         ((altMask+shiftMask+controlMask, xF86XK_AudioRaiseVolume), maintainFocus $ rotaryAdjustFade (+10)),
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
         ((altMask+controlMask, xK_bracketright), spawn "toggle-alacritty-transparent"),
         ((winMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
         ((winMask, xK_space), spawn "dmenu-custom"),
         ((altMask+shiftMask, xK_a), searchDocs "haskell"),
         ((altMask, xK_a), openNSPOnScreen "NSP_docs" 0),
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
      handleEventHook = myHandleEventHook,
      logHook = myLogHook xmproc
    }

------------------------------------------------------------------------
-- main:
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar -x 2 $HOME/.xmonad/xmobarrc"
  spawn "get-kp-status"
  let conf = getConf xmproc
  xmonad $ ewmh $ docks $ conf `additionalKeys` getKeybindings conf
