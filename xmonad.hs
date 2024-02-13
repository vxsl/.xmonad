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
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.RefocusLast
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.ShowWName
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Window
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

winMask, altMask :: KeyMask
winMask = mod4Mask
altMask = mod1Mask

------------------------------------------------------------------------
-- workspaces:
numScreens :: ScreenId
numScreens = 3

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
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  mapM_ cycleAllWorkspacesOnScreen [0 .. (numScreens - 1)]
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
      appName =? "code-insiders-url-handler (remote-debug-profile)" --> doShift "2_1",
      className =? "Chromium-browser" --> doShift "2_1",
      className =? "tmux-pane-view" --> doShift "1_1"
    ]

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
        windows $ focusWindow' w
      else notFoundAction
    do extraAction

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
                "not-dotfiles spawn-with-name " ++ name ++ " Code \"code-insiders --disable-gpu -n " ++ dir ++ "\" 2"
          }
      where
        name = "customvsc_" ++ show i
        dir = "$(sed -n " ++ show i ++ "p $HOME/.xmonad/code_workspaces | sed 's/ .*//')"

winBindsTmuxaStableView :: b -> String -> [((KeyMask, b), X ())]
winBindsTmuxaStableView keySym name =
  [ ((altMask, keySym), seeWin params),
    ((altMask + shiftMask, keySym), seeWin params {greedy = True})
  ]
  where
    notFoundAction = spawn $ "unique-term " ++ name ++ " \"tmuxa " ++ name ++ "\""
    params :: SeeWinParams
    params = defaultSeeWinParams {queryStr = name, notFoundAction = notFoundAction}

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
      \-new-tab -url https://chat.openai.com/?model=gpt-4",
      className =? "NSP_assistant",
      customFloating $ nspRect 0.5,
      True
    ),
    ( "NSP_browse",
      "firefox -P clone3 --class NSP_browse",
      className =? "NSP_browse",
      nonFloating,
      True
    ),
    ( "NSP_vikunja",
      "firefox --kiosk -P clone5 --class NSP_vikunja --new-window \
      \-new-tab -url http://web.vikunja.internal/",
      className =? "NSP_vikunja",
      customFloating $ nspRect 0.5,
      False
    ),
    ( "NSP_obsidian",
      "obsidian",
      className =? "obsidian",
      customFloating $ nspRect 0.95,
      False
    ),
    ( "NSP_homelab",
      "firefox -P clone4 --class NSP_homelab --new-window \
      \-new-tab -url http://web.pve.internal/ \
      \-new-tab -url http://pi.hole/ \
      \-new-tab -url http://web.homebridge.internal/ \
      \-new-tab -url http://localhost:5600/#/activity/$HOSTNAME \
      \-new-tab -url http://web.nginx.internal",
      className =? "NSP_homelab",
      nonFloating,
      True
    ),
    ( "NSP_tmuxa-1",
      "unique-term NSP_tmuxa-1 \"tmuxa tmuxa-1 $HOME\"",
      className =? "NSP_tmuxa-1",
      customFloating $ nspRect 0.97,
      True
    ),
    ( "NSP_tmuxa-2",
      "unique-term NSP_tmuxa-2 \"tmuxa tmuxa-2 $HOME\"",
      className =? "NSP_tmuxa-2",
      customFloating $ nspRect 0.97,
      False
    ),
    ( "NSP_audio",
      "firefox -P clone2 --class NSP_audio --new-window \
      \-new-tab -url https://open.spotify.com/ \
      \-new-tab -url https://noises.online/",
      className =? "NSP_audio",
      customFloating $ nspRect 0.7,
      False
    )
  ]
  where
    nspRect s = center s s
      where
        center w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

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
    ++ winBindsTmuxaStableView xK_semicolon "tmuxa-1"
    ++ winBindsTmuxaStableView xK_comma "tmuxa-2"
    ++ winBindsIDE [xK_b, xK_s]
    ++ ezWinBinds
      [ ( xK_x,
          "code - insiders",
          spawn "not-dotfiles code-insiders --disable-gpu -n",
          Just $ defaultWinBindsParams {exact = True}
        ),
        ( xK_d,
          "customvsc_dof",
          spawn "dotfiles spawn-with-name customvsc_dof Code \"code-insiders --disable-gpu -n $HOME\" 2",
          Nothing
        ),
        ( xK_e,
          "tmux-pane-view",
          do
            windows $ focusScreen 1
            spawn "tmux-pane-view",
          Nothing
        ),
        ( xK_i,
          "Scrivano",
          spawn "scrivano",
          Just $ defaultWinBindsParams {exact = True, extraAction = spawn "$HOME/bin/personal/confwacom"}
        ),
        ( xK_p,
          "firefox",
          spawn "firefox --new-window",
          Just $ defaultWinBindsParams {useClassName = True}
        ),
        ( xK_period,
          "chromium-browser",
          mempty,
          Nothing
        ),
        ( xK_f,
          "discord",
          spawn "discord",
          Nothing
        )
      ]
    ++ [
         ---------------------------------------------------------------
         -- window management:
         ((altMask, xK_h), toggleFocus),
         ((altMask + shiftMask, xK_h), swapWithLast),
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
         ( (altMask, xK_Escape),
           do
             hideNSP "NSP_tmuxa-2"
             openNSPOnScreen "NSP_tmuxa-1" 0
         ),
         ( (altMask + controlMask, xK_Escape),
           do
             hideNSP "NSP_tmuxa-1"
             openNSPOnScreen "NSP_tmuxa-2" 0
         ),
         ((altMask, xK_q), openNSPOnScreen "NSP_assistant" 0),
         ((altMask, xK_o), openNSPOnScreen "NSP_browse" 0),
         ((altMask, xK_y), openNSPOnScreen "NSP_vikunja" 0),
         ((altMask, xK_u), openNSPOnScreen "NSP_obsidian" 0),
         ((altMask, xK_t), openNSP "NSP_homelab"),
         ((altMask, xK_w), openNSPOnScreen "NSP_audio" 0),
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
         ((winMask + shiftMask, xK_s), spawn "flameshot gui &"),
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
         ((winMask + altMask, xK_q), confirmCmd "remonad --restart"),
         ((altMask + controlMask + shiftMask, xK_F10), spawn "xlock -mode random"),
         ((altMask + controlMask + shiftMask, xK_F11), spawn "systemctl suspend"),
         ((winMask + altMask + controlMask + shiftMask, xK_F11), confirmCmd "sudo reboot now"),
         ((winMask + shiftMask, xK_q), confirm "logout" $ io exitSuccess),
         ((winMask + shiftMask + controlMask, xK_q), confirmCmd "configure-multihead"),
         ((altMask + controlMask + shiftMask, xK_q), confirm "rescreen" rescreen),
         ---------------------------------------------------------------
         -- scripts
         ((altMask + shiftMask, xK_Delete), spawn "vpnctrl --up"),
         ((altMask + shiftMask + controlMask, xK_Delete), spawn "vpnctrl --down")
       ]

------------------------------------------------------------------------
-- main conf:
getConf xmproc =
  def
    { terminal = "alacritty",
      focusFollowsMouse = True,
      clickJustFocuses = False,
      borderWidth = 1,
      modMask = winMask,
      startupHook = myStartupHook,
      workspaces = myWorkspaces,
      normalBorderColor = "#000000",
      focusedBorderColor = "#ffffff",
      mouseBindings = myMouseBindings,
      layoutHook =
        refocusLastLayoutHook
          $ ( showWName' $
                def
                  { swn_font = "xft:Monospace:pixelsize=30:regular:hinting=true",
                    swn_fade = 0.5,
                    swn_bgcolor = "blue",
                    swn_color = "red"
                  }
            )
          $ avoidStruts myLayout,
      manageHook = namedScratchpadManageHook nsps <+> myManageHook <+> manageSpawn,
      handleEventHook = refocusLastWhen $ refocusingIsActive <||> isFloat,
      logHook =
        refocusLastLogHook
          >> nsHideOnFocusLoss
            (mapToNSP $ filter (\(_, _, _, _, hideOnFocusLoss) -> hideOnFocusLoss) nspDefs)
            <+> dynamicLogWithPP
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
              <> refocusLastLogHook
    }

------------------------------------------------------------------------
-- main:
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar -x 2 $HOME/.xmonad/xmobarrc"
  let conf = getConf xmproc
  xmonad $ ewmh $ docks $ conf `additionalKeys` getKeybindings conf
