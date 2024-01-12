import Control.Monad
import Control.Monad.Cont (liftM)
import Data.Bool
import Data.Functor
import Data.List
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Tree
import Foreign.C.String
import Graphics.X11 (controlMask, mod1Mask, xK_semicolon)
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import System.IO.Error (catchIOError)
import System.Process (readProcess)
import Text.Read (Lexeme (Number), readMaybe)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWorkspaceByScreen
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigationPatched
import XMonad.Actions.KeyRemap
import XMonad.Actions.OnScreen
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.TreeSelect qualified as TS
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Focus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.FixedColumn
import XMonad.Layout.Fullscreen (fullscreenSupport, fullscreenSupportBorder)
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutScreens
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Font
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName, unName)
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Timer
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare
import XMonad.Util.XUtils

myModMask = mod4Mask

altMask = mod1Mask

myNormalBorderColor = "#000000"

myFocusedBorderColor = "#ffffff"

myWorkspaces = withScreens 3 ([show n | n <- [1 .. 9]])

debugLog str =
  io $ appendFile "/home/kyle/.xmonad-debug-log" $ show str ++ "\n"

confirm = confirmPrompt myPromptConfig

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {} =
  M.fromList $
    [ ((myModMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      ((myModMask, xK_space), spawn "dmenu-custom"),
      ((myModMask, xK_Escape), kill),
      ((myModMask + shiftMask, xK_space), sendMessage NextLayout),
      ((altMask, xK_Tab), windows W.focusDown),
      ((myModMask, xK_j), windows W.focusDown),
      ((altMask + shiftMask, xK_Tab), windows W.focusUp),
      ((myModMask, xK_k), windows W.focusUp),
      ((myModMask .|. shiftMask, xK_j), windows W.swapDown),
      ((myModMask .|. shiftMask, xK_k), windows W.swapUp),
      ((myModMask, xK_h), sendMessage Shrink),
      ((myModMask, xK_l), sendMessage Expand),
      ((myModMask + controlMask, xK_s), withFocused $ windows . W.sink),
      ((myModMask + shiftMask, xK_q), confirm "logout" $ io exitSuccess),
      ( (myModMask + altMask, xK_q),
        confirm "remonad" $ spawn "remonad --restart"
      ),
      ( (myModMask + shiftMask + controlMask, xK_q),
        spawn "configure-multihead"
      )
    ]
      ++ [ ((m .|. myModMask, k), windows $ onCurrentScreen f i)
           | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9],
             (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]
      ++ [ ((m, key), do screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_comma, xK_period, xK_slash, xK_Shift_R, xK_Up] [0 ..],
             (f, m) <- [(W.view, myModMask)]
         ]

------------------------------------------------------------------------

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

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

coerceScreenID :: ScreenId -> String
coerceScreenID (S i) = show i

viewAllWorkspaces i =
  mapM_
    ( \n -> do
        let id = coerceScreenID i ++ "_" ++ show n
        -- debugLog $ "viewAllWorkspaces - " ++ id
        windows $ viewOnScreen i id
    )
    ([1 .. 9] ++ [1])

-- there seems to be some kind of race condition that sometimes prevents the trailing [1]  from working in viewAllWorkspaces:
ensureFirstWorkspace i = do
  let id = coerceScreenID i ++ "_1"
  -- debugLog $ "ensure - " ++ id
  windows $ viewOnScreen i id

myStartupHook = do
  spawnOnce "nitrogen --restore &"
  mapM_ viewAllWorkspaces [0 .. 2]
  mapM_ ensureFirstWorkspace [0 .. 2]

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

------------------------------------------------------------------------
--
myManageHook =
  composeAll
    [ className =? "zenity" --> doFloat,
      className =? "Arandr" --> doFloat,
      className =? "Org.gnome.Nautilus" --> doFloat,
      className =? "Xmessage" --> doFloat,
      title =? "Picture-in-Picture" --> doFloat,
      className =? "Google-chrome-unstable" --> doShift "2_1"
    ]

------------------------------------------------------------------------
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

showWorkspaceIDWhenSwitching =
  showWName' $
    def
      { swn_font = "xft:Monospace:pixelsize=30:regular:hinting=true",
        swn_fade = 0.5,
        swn_bgcolor = "blue",
        swn_color = "red"
      }

fuzzyQuery str = fmap (isInfixOf str) appName

eqQuery str = fmap (str ==) appName

fuzzyQueryClassName str = fmap (isInfixOf str) className

eqQueryClassName str = fmap (str ==) className

eqQueryAppName str = fmap (str ==) appName

getWinByClassName :: String -> Direction -> X (Maybe Window)
getWinByClassName name direction =
  getNextMatch (eqQueryClassName name) direction

getWinByAppName :: String -> Direction -> X (Maybe Window)
getWinByAppName name direction =
  getNextMatch (eqQueryAppName name) direction

seeWin name or mod reverse exact queryClassName =
  do
    win <-
      getNextMatch
        ( ( if queryClassName
              then (if exact then eqQueryClassName else fuzzyQueryClassName)
              else (if exact then eqQuery else fuzzyQuery)
          )
            name
        )
        (if reverse then Backward else Forward)
    if isJust win
      then do
        let w = fromJust win
        if mod then windows $ bringWindow w else mempty
        windows $ focusWindow' w
      else or

-- winBinds :: b -> String -> X -> [((KeyMask, b), X ())]
winBindsForward' key name or exact queryClassName =
  [ ((altMask, key), seeWin name or False False exact queryClassName),
    ( (altMask + shiftMask, key),
      seeWin name mempty True False exact queryClassName
    )
  ]

winBinds' key name or exact queryClassName =
  winBindsForward' key name or exact queryClassName
    ++ [ ( (altMask + controlMask, key),
           seeWin name mempty False True exact queryClassName
         )
         --  ((altMask + shiftMask + controlMask, key), seeWin name mempty True True exact queryClassName)
       ]

winBindsExactClassName key name or = winBinds' key name or True True

winBindsExact key name or = winBinds' key name or True False

winBinds key name or = winBinds' key name or False False

winBindsClassName key name or = winBinds' key name or False True

winBindsPlusAction key name or = winBinds' key name or False False

tmuxaBinds :: b -> String -> String -> [((KeyMask, b), X ())]
tmuxaBinds key name dir =
  [ ((altMask, key), seeWin tmuxaName or False False False False),
    ((altMask + shiftMask, key), seeWin tmuxaName or True False False False),
    ((altMask + controlMask, key), seeWin tmuxaName or False True False False),
    ( (altMask + shiftMask + controlMask, key),
      seeWin tmuxaName or True True False False
    )
  ]
  where
    tmuxaName = "tmuxa-" ++ name
    or =
      spawn $
        "alacritty --class "
          ++ tmuxaName
          ++ " -e zsh -c \"tmuxa "
          ++ tmuxaName
          ++ " "
          ++ dir
          ++ "\""

doubleWinBinds key name name2 or =
  [ ( (altMask, key),
      do
        seeWin name or False False False False
        seeWin name2 mempty False False False False
    ),
    ( (altMask + shiftMask, key),
      do
        seeWin name or True False False False
        seeWin name2 mempty True False False False
    ),
    ( (altMask + controlMask, key),
      do
        seeWin name or False True False False
        seeWin name2 mempty False True False False
    ),
    ( (altMask + shiftMask + controlMask, key),
      do
        seeWin name or True True False False
        seeWin name2 mempty True True False False
    )
  ]

spSize = 9.7

scratchpadRect = W.RationalRect ((10 - spSize) / 20) ((10 - spSize) / 20) (spSize / 10) (spSize / 10)

sizedScratchpadRect s = center s s
  where
    center w h = W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

type NSPDef = (String, String, Query Bool, ManageHook)

shyNSPDefs :: [NSPDef]
shyNSPDefs =
  [ ( "floatllm",
      "firefox -P clone1 --class floatllm --new-window \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/ \
      \-new-tab -url https://chat.openai.com/?model=gpt-4",
      className =? "floatllm",
      customFloating $ sizedScratchpadRect 0.5
    ),
    ( "floatbrowse",
      "firefox -P clone3 --class floatbrowse",
      className =? "floatbrowse",
      mempty
    )
  ]

confidentNSPDefs :: [NSPDef]
confidentNSPDefs =
  [ ( "floattmuxa-1",
      tmuxaName,
      className =? "floattmuxa-1",
      customFloating scratchpadRect
    ),
    ( "pane-clone-floattmuxa-1",
      tmuxaPaneCloneName,
      className =? "pane-clone-floattmuxa-1",
      customFloating scratchpadRect
    ),
    ( "floattmuxa-2",
      tmuxaName2,
      className =? "floattmuxa-2",
      customFloating scratchpadRect
    ),
    ( "float-web-search-prompt",
      "alacritty --class float-web-search-prompt -e zsh",
      className =? "float-web-search-prompt",
      customFloating $ sizedScratchpadRect 0.3
    ),
    ( "floataudio",
      "firefox -P clone2 --class floataudio --new-window \
      \-new-tab -url https://open.spotify.com/ \
      \-new-tab -url https://noises.online/",
      className =? "floataudio",
      customFloating $ sizedScratchpadRect 0.7
    )
  ]
  where
    tmuxaName = "alacritty --class " ++ "floattmuxa-1" ++ " -e zsh -c \" tmuxa tmuxa-scm $HOME\""
    tmuxaPaneCloneName = "alacritty --class " ++ "pane-clone-floattmuxa-1" ++ " -e sh -c \"tmux attach-session -t clone\""
    tmuxaName2 = "alacritty --class " ++ "floattmuxa-2" ++ " -e zsh -c \" tmuxa  tmuxa-work $HOME\""

mapToNS :: [NSPDef] -> [NamedScratchpad]
mapToNS = map (\(n, tn, cn, cf) -> NS n tn cn cf)

confidentScratchpads :: [NamedScratchpad]
confidentScratchpads = mapToNS confidentNSPDefs

shyScratchpads :: [NamedScratchpad]
shyScratchpads = mapToNS shyNSPDefs

scratchpads :: [NamedScratchpad]
scratchpads = confidentScratchpads <+> shyScratchpads

getWinBindsForCodeInstance :: Int -> KeySym -> [((KeyMask, KeySym), X ())]
getWinBindsForCodeInstance i keycode =
  winBinds
    keycode
    name
    (spawn $ "not-dotfiles spawn-with-name " ++ name ++ " Code \"code-insiders --disable-gpu -n " ++ dir ++ "\" 2")
  where
    name = "customvsc_" ++ show i
    dir = "$(sed -n " ++ show i ++ "p $HOME/.xmonad/code_workspaces.sh | sed 's/ .*//')"

getWinBindsForCodeInstances keycodes =
  concat $
    zipWith
      getWinBindsForCodeInstance
      [1 ..]
      keycodes

hideAllNSPs :: X ()
hideAllNSPs = do
  mapM_
    ( \className -> do
        win <- getWinByClassName className Forward
        when (isJust win) $ do
          let w = fromJust win
          windows $ W.shiftWin "NSP" w
    )
    classNames
  where
    classNames = map (\(className, _, _, _) -> className) (confidentNSPDefs <+> shyNSPDefs)

doOnScreen :: ScreenId -> X () -> X ()
doOnScreen s x = do
  windows $ focusScreen s
  x

-- main
------------------------------------------------------------------------
main =
  spawnPipe "/usr/bin/xmobar -x 2 $HOME/.xmonad/xmobarrc" >>= \xmproc ->
    xmonad $
      ewmh $
        docks $
          def
            { terminal = "alacritty",
              focusFollowsMouse = True,
              clickJustFocuses = False,
              borderWidth = 1,
              modMask = myModMask,
              startupHook = myStartupHook,
              workspaces = myWorkspaces,
              normalBorderColor = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor,
              keys = myKeys,
              mouseBindings = myMouseBindings,
              layoutHook = refocusLastLayoutHook $ showWorkspaceIDWhenSwitching $ avoidStruts myLayout,
              manageHook = namedScratchpadManageHook scratchpads <+> myManageHook <+> manageSpawn,
              handleEventHook = refocusLastWhen $ refocusingIsActive <||> isFloat,
              logHook =
                refocusLastLogHook
                  >> nsHideOnFocusLoss
                    shyScratchpads
                    <+> dynamicLogWithPP
                      def
                        { ppOutput = hPutStrLn xmproc,
                          ppSort = mkWsSort $ return compareNumbers,
                          ppTitle = mempty,
                          ppCurrent = xmobarColor "#00ff1e" "#000000",
                          ppHidden = mempty,
                          ppVisible = xmobarColor "#ff0000" "#000000",
                          ppSep = "  ", -- between WSs and title
                          ppWsSep = "  ",
                          ppLayout = const ""
                        }
                      <> refocusLastLogHook
            }
            `additionalKeys` ( [ ((myModMask, 0xffff), spawn "playerctl play-pause"),
                                 ((myModMask + shiftMask + controlMask, 0xffff), spawn "playerctl pause"),
                                 ((altMask, xK_h), toggleFocus),
                                 ((altMask + shiftMask, xK_h), swapWithLast),
                                 ((altMask, xK_z), hideAllNSPs),
                                 ( (altMask, xK_Escape),
                                   doOnScreen 0 $
                                     namedScratchpadAction scratchpads "floattmuxa-1"
                                 ),
                                 ( (altMask + controlMask, xK_Escape),
                                   doOnScreen 0 $
                                     namedScratchpadAction scratchpads "floattmuxa-2"
                                 ),
                                 ( (altMask, xK_q),
                                   doOnScreen 0 $
                                     namedScratchpadAction scratchpads "floatllm"
                                 ),
                                 ( (altMask, xK_o),
                                   doOnScreen 0 $
                                     namedScratchpadAction scratchpads "floatbrowse"
                                 ),
                                 ( (altMask, xK_w),
                                   doOnScreen 0 $
                                     namedScratchpadAction scratchpads "floataudio"
                                 )
                               ]
                                 ++ tmuxaBinds
                                   xK_semicolon
                                   "scm"
                                   "$HOME/work/gui/site"
                                 ++ tmuxaBinds
                                   xK_comma
                                   "work"
                                   "$HOME/work/gui/site"
                                 ++ tmuxaBinds
                                   xK_v
                                   "personal"
                                   "$HOME"
                                 ++ getWinBindsForCodeInstances [xK_b, xK_s]
                                 ++ winBindsExact
                                   xK_x
                                   "code - insiders"
                                   (spawn "not-dotfiles code-insiders --disable-gpu -n")
                                 ++ winBinds
                                   xK_d
                                   "customvsc_dof"
                                   (spawn "dotfiles spawn-with-name customvsc_dof Code \"code-insiders --disable-gpu -n $HOME\" 2")
                                 ++ winBinds
                                   xK_e
                                   "tmux-pane-clone"
                                   ( do
                                       windows $ focusScreen 1
                                       spawn "tmux-pane-clone --view"
                                   )
                                 ++ [ ( (altMask, xK_i),
                                        do
                                          spawn "$HOME/bin/personal/confwacom"
                                          seeWin
                                            "Scrivano"
                                            (spawn "$HOME/dev/Scrivano_0.17.7/scrivano")
                                            False
                                            False
                                            True
                                            False
                                      )
                                    ]
                                 ++ winBindsClassName
                                   xK_p
                                   "firefox"
                                   (spawn "firefox --new-window")
                                 ++ winBinds
                                   xK_period
                                   "google-chrome-unstable"
                                   mempty
                             )
            `additionalKeysP` [
                                ------------------------------------------------------------
                                -- move windows among physical screens
                                ("M-C-x", shiftNextScreen >> nextScreen),
                                ("M-C-a", shiftPrevScreen >> prevScreen),
                                ------------------------------------------------------------
                                -- volume
                                ("<Insert>", spawn "volctrl -d"),
                                ("<Print>", spawn "volctrl -u"),
                                ("M1-<Insert>", spawn "volctrl -dc"),
                                ("M1-<Print>", spawn "volctrl -uc"),
                                ("M1-S-<Insert>", spawn "volctrl -df"),
                                ("M1-S-<Print>", spawn "volctrl -uf"),
                                ("M1-S-C-<Insert>", spawn "volmute"),
                                ("M1-S-C-<Print>", spawn "volmute"),
                                ("<XF86AudioLowerVolume>", spawn "volctrl -d"),
                                ("<XF86AudioRaiseVolume>", spawn "volctrl -u"),
                                ("M1-<XF86AudioLowerVolume>", spawn "volctrl -dc"),
                                ("M1-<XF86AudioRaiseVolume>", spawn "volctrl -uc"),
                                ("M1-S-<XF86AudioLowerVolume>", spawn "volctrl -df"),
                                ("M1-S-<XF86AudioRaiseVolume>", spawn "volctrl -uf"),
                                ("<XF86AudioMute>", spawn "volmute"),
                                ------------------------------------------------------------
                                -- color temp
                                ("M-<XF86AudioRaiseVolume>", spawn "setredshift -ti"),
                                ("M-<XF86AudioLowerVolume>", spawn "setredshift -td"),
                                -- brightness
                                ("M-S-<XF86AudioRaiseVolume>", spawn "setredshift -bi"),
                                ("M-S-<XF86AudioLowerVolume>", spawn "setredshift -bd"),
                                -- reset color temp and brightness
                                ("M-C-S-<XF86AudioLowerVolume>", spawn "setredshift --reset"),
                                ------------------------------------------------------------
                                -- apps
                                ("M-p", spawn ""),
                                ("M-o", spawn "firefox --new-window &"),
                                ("M-S-o", spawn "chromium-browser &"),
                                ("M-c", spawn "code &"),
                                ("M-S-s", spawn "flameshot gui &"),
                                ("M-S-e", spawn "nautilus &"),
                                ("M-e", spawn "alacritty -e ranger"),
                                ------------------------------------------------------------
                                -- system
                                ("M1-C-S-<F10>", spawn "xlock -mode random"),
                                ("M1-C-S-<F11>", spawn "systemctl suspend"),
                                ("M-M1-C-S-<F11>", spawn "sudo reboot now"),
                                ------------------------------------------------------------
                                -- scripts
                                ("M1-S-<Delete>", spawn "vpnctrl --up"),
                                ("M1-C-S-<Delete>", spawn "vpnctrl --down"),
                                ------------------------------------------------------------
                                -- xmonad
                                ("M-f", sinkAll),
                                ("M-S-g", windowPrompt myPromptConfig Goto allWindows),
                                ("M-g", windowPrompt myPromptConfig Goto wsWindows),
                                ("M1-C-S-q", rescreen)
                                -- ("M-Tab", cycleWorkspaceOnCurrentScreen [] xK_j xK_k), -- TODO figure this out
                              ]
