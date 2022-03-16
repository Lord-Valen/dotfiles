-- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
--import XMonad.Hooks.StatusBar

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap, checkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Inconsolata Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super key

myRunPrompt :: String
myRunPrompt = "rofi -show run"

myTerminal :: String
myTerminal = "kitty"    -- Sets default terminal

myBrowser :: String
myBrowser = "brave "  -- Sets qutebrowser as browser

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"   -- Border color of focused windows

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
{-Uncomment after Nix switch
mySB = statusBarProp "xmobar ~/.config/xmobar/xmobarrc"

barSpawner :: screenId -> IO StatusBarConfig
barSpawner = pure $ mySB
-}

myStartupHook :: X ()
myStartupHook = do
  return () >> checkKeymap myConfig myKeymap
  spawnOnce "lxsession &"
  spawnOnce "picom &"
  spawnOnce "nm-applet &"
  spawnOnce "volumeicon &"
  spawnOnce "conky -c $HOME/.config/conky/xmonad/doom-one-01.conkyrc"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true       --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
  spawnOnce "feh --no-fehbg --bg-fill -z ~/.config/wallpaper/*"  -- feh set random wallpaper

myScratchPads :: [NamedScratchpad]
myScratchPads = [
  NS "terminal" spawnTerm findTerm manageTerm,
  NS "calculator" spawnCalc findCalc manageCalc
  ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad";
      findTerm = title =? "scratchpad";
    manageTerm = customFloating $ W.RationalRect l t w h
      where l = 0.95 -w; t = 0.95 -h; w = 0.9; h = 0.9
    spawnCalc = "qalculate-gtk";
      findCalc = className =? "Qalculate-gtk";
    manageCalc = customFloating $ W.RationalRect l t w h
      where l = 0.7 -w; t = 0.75 -h; w = 0.4; h = 0.5;

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing' 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
                -- I cannot add spacing to this layout because it will add spacing between window and tabs which looks bad.
       $ tabbed shrinkText myTabTheme
tallAccordion = renamed [Replace "tallAccordion"]
                Accordion
wideAccordion = renamed [Replace "wideAccordion"]
                $ Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def {
  fontName = myFont,
  activeColor         = "#46d9ff",
  inactiveColor       = "#313846",
  activeBorderColor   = "#46d9ff",
  inactiveBorderColor = "#282c34",
  activeTextColor     = "#282c34",
  inactiveTextColor   = "#d0d0d0"
  }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def {
  swn_font    = "xft:Ubuntu:bold:size=60",
  swn_fade    = 1.0,
  swn_bgcolor = "#1c1f24",
  swn_color   = "#ffffff"
  }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                      ||| magnify
                      ||| noBorders monocle
                      ||| noBorders tabs
                      ||| grid
                      ||| threeCol
                      ||| threeRow
                      ||| tallAccordion
                      ||| wideAccordion
                      ||| floats
                      ||| spirals

myWorkspaces = [" sys ", " doc ", " www ", " dev ", " cht ", " vms ", " mus ", " vid ", " gfx "]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..]

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook = composeAll [
  className =? "confirm"        --> doFloat,
  className =? "file_progress"  --> doFloat,
  className =? "dialog"         --> doFloat,
  className =? "download"       --> doFloat,
  className =? "error"          --> doFloat,
  className =? "notification"   --> doFloat,
  className =? "pinentry-gtk-2" --> doFloat,
  className =? "splash"         --> doFloat,
  className =? "toolbar"        --> doFloat,
  className =? "Yad"            --> doCenterFloat,
  className =? "Zotero"         --> doShift ( myWorkspaces !! 1 ),
  className =? "Brave-browser"  --> doShift ( myWorkspaces !! 2 ),
  className =? "Ferdi"          --> doShift ( myWorkspaces !! 4 ),
  className =? "Element"        --> doShift ( myWorkspaces !! 4 ),
  className =? "Signal"         --> doShift ( myWorkspaces !! 4 ),
  className =? "zoom"           --> doShift ( myWorkspaces !! 4 ),
  className =? "Virt-manager"   --> doShift ( myWorkspaces !! 5 ),
  className =? "mpv"            --> doShift ( myWorkspaces !! 7 ),
  className =? "Steam"          --> doShift ( myWorkspaces !! 8 ),
  className =? "Lutris"         --> doShift ( myWorkspaces !! 8 ),
  className =? "itch"           --> doShift ( myWorkspaces !! 8 ),
  className =? "Gimp"           --> doShift ( myWorkspaces !! 8 ),
  className =? "Inkscape"       --> doShift ( myWorkspaces !! 8 ),
  isFullscreen                  --> doFullFloat
  ] <+> namedScratchpadManageHook myScratchPads

-- START_KEYS
myKeymap :: [(String, X ())]
myKeymap = [
  -- KB_GROUP Xmonad
  ("M-C-r", spawn "xmonad --recompile"),
  ("M-S-r", spawn "xmonad --restart"),
  ("M-S-x", io exitSuccess),

  -- KB_GROUP Run Prompt
  ("M-S-<Return>", spawn myRunPrompt),
  ("M-C-<Return>", spawn myRunPrompt), -- Workaround for when M-S-<Return> won't register <Return>

  -- KB_GROUP Commonly used programs
  ("M-<Return>", spawn myTerminal),
  ("M-b", spawn myBrowser),
  ("M-M1-h", spawn (myTerminal ++ " -e htop")),

  -- KB_GROUP Kill windows
  ("M-S-q", kill1),     -- Kill the currently focused client
  ("M-S-c", killAll),   -- Kill all windows on current workspace

  -- KB_GROUP Workspaces
  ("M-.", nextScreen),  -- Switch focus to next monitor
  ("M-,", prevScreen),  -- Switch focus to prev monitor
  ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP),       -- Shifts focused window to next ws
  ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP),  -- Shifts focused window to prev ws

  -- KB_GROUP Floating windows
  ("M-f", sendMessage (T.Toggle "floats")), -- Toggles my 'floats' layout
  ("M-t", withFocused $ windows . W.sink), -- Push floating window back to tile
  ("M-S-t", sinkAll),                       -- Push ALL floating windows to tile

  -- KB_GROUP Increase/decrease spacing (gaps)
  ("C-M1-m", decScreenSpacing 4),         -- Decrease screen spacing
  ("C-M1-n", decWindowSpacing 4),         -- Decrease window spacing
  ("C-M1-e", incWindowSpacing 4),         -- Increase window spacing
  ("C-M1-i", incScreenSpacing 4),         -- Increase screen spacing

  -- KB_GROUP Windows navigation
  ("M-m", windows W.focusMaster),  -- Move focus to the master window
  ("M-n", windows W.focusDown),    -- Move focus to the next window
  ("M-e", windows W.focusUp),      -- Move focus to the prev window
  ("M-i", windows W.swapMaster), -- Swap the focused window and the master window
  ("M-S-n", windows W.swapDown),   -- Swap focused window with next window
  ("M-S-e", windows W.swapUp),     -- Swap focused window with prev window
  ("M-<Backspace>", promote),      -- Moves focused window to master, others maintain order
  ("M-S-<Tab>", rotSlavesDown),    -- Rotate all windows except master and keep focus in place
  ("M-C-<Tab>", rotAllDown),       -- Rotate all the windows in the current stack

  -- KB_GROUP Layouts
  ("M-<Tab>", sendMessage NextLayout),           -- Switch to next layout
  ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full

  -- KB_GROUP Increase/decrease windows in the master pane or the stack
  ("M-S-<Up>", sendMessage (IncMasterN 1)),      -- Increase # of clients master pane
  ("M-S-<Down>", sendMessage (IncMasterN (-1))), -- Decrease # of clients master pane
  ("M-C-<Up>", increaseLimit),                   -- Increase # of windows
  ("M-C-<Down>", decreaseLimit),                 -- Decrease # of windows

  -- KB_GROUP Window resizing
  ("M-h", sendMessage Shrink),                   -- Shrink horiz window width
  ("M-l", sendMessage Expand),                   -- Expand horiz window width
  ("M-M1-n", sendMessage MirrorShrink),          -- Shrink vert window width
  ("M-M1-e", sendMessage MirrorExpand),          -- Expand vert window width

  -- KB_GROUP Sublayouts
  -- This is used to push windows to tabbed sublayouts, or pull them out of it.
  ("M-C-m", sendMessage $ pullGroup L),
  ("M-C-n", sendMessage $ pullGroup R),
  ("M-C-e", sendMessage $ pullGroup U),
  ("M-C-i", sendMessage $ pullGroup D),
  ("M-C-?", withFocused (sendMessage . MergeAll)),
  ("M-C-/", withFocused (sendMessage . UnMergeAll)),
  ("M-C-.", onGroup W.focusUp'),    -- Switch focus to next tab
  ("M-C-,", onGroup W.focusDown'),  -- Switch focus to prev tab

  -- KB_GROUP Scratchpads
  -- Toggle show/hide these programs.  They run on a hidden workspace.
  -- When you toggle them to show, it brings them to your current workspace.
  -- Toggle them to hide and it sends them back to hidden workspace (NSP).
  ("M-s t", namedScratchpadAction myScratchPads "terminal"),
  ("M-s c", namedScratchpadAction myScratchPads "calculator"),

  -- KB_GROUP Emacs
  ("M-a a", spawn myEmacs), -- emacs
  ("M-a e", spawn (myEmacs ++ ("--eval '(eshell)'"))), -- eshell
  ("M-a f", spawn (myEmacs ++ ("--eval '(elfeed)'"))), -- elfeed
  ("M-a w", spawn (myEmacs ++ ("--eval '(eww)'"))), -- emacs web wowser
  ("M-a i", spawn (myEmacs ++ ("--eval '(circe)'"))) -- emacs irc client
  {-
  ,
  ("M-a b", spawn (myEmacs ++ ("--eval '(ibuffer)'"))), -- list buffers
  ("M-a d", spawn (myEmacs ++ ("--eval '(dired nil)'"))), -- dired
  ("M-a m", spawn (myEmacs ++ ("--eval '(mastodon)'"))), -- mastodon.el
  ("M-a v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'"))), -- vterm
  ("M-a a", spawn (myEmacs ++ ("--eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/\")'")))
  -}
  ]
  -- The following lines are needed for named scratchpads.
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"));
    nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
-- END_KEYS

myConfig = ewmh def {
    manageHook = myManageHook <+> manageDocks,
    --keys = \c -> mkKeymap c myKeymap,
    handleEventHook = docksEventHook,
    modMask = myModMask,
    terminal = myTerminal,
    startupHook = myStartupHook,
    layoutHook = showWName' myShowWNameTheme myLayoutHook,
    workspaces = myWorkspaces,
    borderWidth = myBorderWidth,
    normalBorderColor = myNormColor,
    focusedBorderColor = myFocusColor,
    logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP xmobarPP
    --  $ dynamicEasySBs barSpawner
    } `additionalKeysP` myKeymap
main :: IO ()
main = do xmonad $ myConfig