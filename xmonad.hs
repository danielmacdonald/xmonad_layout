
-- http://chtau.be/xmonad
--
-- This configuration is in use on Debian Linux, Arch Linux and FreeBSD
--
-- required packages:
--   xmonad>=0.10, xmonad-contrib>=0.10
--   dmenu (suckless-tools package on Debian)
-- recommended packages:
--   xmobar
--   gmrun

import Data.List    -- isInfixOf
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Promote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer -- doesn't seem to bring windows?
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers   -- (-?>)
import XMonad.Hooks.SetWMName
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.MagicFocus
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts as TL
import XMonad.Prompt
import XMonad.Prompt.DirExec
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.Run (safeSpawn, unsafeSpawn, spawnPipe, hPutStrLn)
import qualified Data.Map as M
import qualified Data.Monoid (All(..))    -- for declaration of myHandleEventHook
import qualified XMonad.StackSet as W
import XMonad.Hooks.EwmhDesktops


-- The preferred terminal program, which is used in a binding
-- below and by certain contrib modules.

myTerminal :: String
myTerminal = "terminator"

-- Width of the window border in pixels.

myBorderWidth :: Dimension
myBorderWidth = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.

myModMask :: KeyMask
myModMask = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the
-- length of this list.

myWorkspaces :: [String]
myWorkspaces = ["1:web", "2:rem1", "3:rem2",
                "4:pdf", "5:loc1", "6:loc2",
                "7", "8:mus", "9:tmp",
                "0:mov", "A:email", "B"]

-- Border colors for unfocused and focused windows, respectively.

myNormalBorderColor :: String
myNormalBorderColor = "black"

myFocusedBorderColor :: String
myFocusedBorderColor = "red"

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- Note: If you change this function and let xmonad recompile its config
-- file, you won't see any changes until you close and reopen the
-- appropriate window!
--
-- myManageHook is based on
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/hgabreu%27s_xmonad.hs
-- Note: -?> is the "Maybe"-version of -->, as required by composeOne



myManageHook :: ManageHook
myManageHook = (composeAll . concat $           -- shifting Actions
    [ [ title     =? x --> doCenterFloatToAll   | x <- important ]
    , [ className =? x --> doShift "2:com"      | x <- cShiftCom ]
    , [ className =? x --> doShift "3:web"      | x <- cShiftWeb ]
    , [ className =? x --> doShift "4:pdf"      | x <- cShiftPdf ]
    , [ className =? x --> doShift "7:vm"       | x <- cShiftVm  ]
    , [ className =? x --> doShift "9:tmp"      | x <- cShiftTmp ]
    , [ className =? x --> doShift "0:mov"      | x <- cShiftFlo ]

    ]) <+> (composeOne . concat $               -- floating Actions
    [ [ className =? x -?> doCenterFloat'       | x <- cCenterF  ]
    , [ title     =? x -?> doCenterFloat'       | x <- tCenterF  ]
    , [ className ~? x -?> doCenterFloat'       | x <- pcFloat   ]
    , [ title     ~? x -?> doCenterFloat'       | x <- ptCenterF ]
    , [ className =? x -?> doFloat'             | x <- cFloat    ]
    , [ title     =? x -?> doFloat'             | x <- tFloat    ]
    , [ isFullscreen   -?> doFullFloat'   ]
    , [ isDialog       -?> doCenterFloat' ]
    , [ className =? "" <&&> title =? "" -?> doFullFloat' ] --slim preview
    -- enable this to prevent new windows from stealing focus:
--    , [ return True    -?> doF W.swapDown ]
    , [ return True    -?> doMaster ]
    ]) where
        important   = ["New Pounces", "XMPP Message Error","nmtui"]
        -- auto-shift to workspaces
        cShiftWeb   = ["firefox"]
        cShiftCom   = ["Kopete", "Claws-mail", "Choqok", "Pidgin"]
        cShiftVm    = ["Vmware"]
        cShiftFlo   = ["Kaffeine", "MPlayer", "xine", "Gimp","mpv","vlc"]
        cShiftTmp   = ["Envy24control", "Audacious"]
        cShiftPdf   = ["Zathura"]
     
        -- auto-float
        cCenterF    = ["Kaffeine", "xine", "Xmessage", "Xdialog"]
        tCenterF    = ["Plugins", "Add-ons"]
        ptCenterF   = ["About", "Open File", "Save As"]
        pcFloat     = ["Xfce4-"]
        cFloat      = ["Xfrun4", "Wine",
                        -- "Gimp", "Gimp-2.6", "Gimp-2.8",
                        "Nm-applet", "Envy24control", "Audacious",
                        "Orage", "X64"]
        tFloat      = ["glxgears"]
        doMaster    = doF W.shiftMaster -- append this to all floats so
                                        -- new windows go always on the top,
                                        -- regardless of the current focus
        doFloat' = doFloat -- <+> doMaster
        doCenterFloat' = doCenterFloat -- <+> doMaster
        doFullFloat' = doFullFloat -- <+> doMaster
        doCopyToAll = ask >>= doF . \w -> (\ws ->
                        foldr ($) ws (map (copyWindow w) myWorkspaces))
        doCenterFloatToAll = doCopyToAll <+> doCenterFloat'


myLogHook :: Handle -> X ()
myLogHook xmproc =
    dynamicLogWithPP xmobarPP
            { -- ppCurrent         = wrap "#e5f9ff"
            --, ppVisible         = wrap "^fg(#a00000)" "^fg()"
            --, ppHidden          = wrap "^fg(#ffffff)" "^fg()"
            --, ppHiddenNoWindows = wrap "^fg(#7eacb9)" "^fg()"
            --, ppOutput          = System.IO.UTF8.hPutStrLn xmproc
             ppOutput          = XMonad.Util.Run.hPutStrLn xmproc
            -- ppTitle           = xmobarColor "#7fe07f" "" . shorten 80
            }


myStartupHook :: X ()
myStartupHook =
    setWMName "LG3D"        -- workaround for Java GUI programs

myHandleEventHook :: Event -> X Data.Monoid.All
-- myHandleEventHook = fullscreenEventHook
myHandleEventHook = docksEventHook

myTabConfig :: Theme
myTabConfig = defaultTheme {
          activeBorderColor     = "red"
        , activeTextColor       = "red"
        , activeColor           = "#222222"
        , inactiveBorderColor   = "#7C7C7C"
        , inactiveTextColor     = "#7C7C7C"
        , decoHeight            = 14
        , fontName              =
                "-misc-fixed-medium-r-condensed-*-13-120-75-75-c-60-iso10646-1"
                -- "-misc-fixed-medium-r-semicondensed-*-13-120-75-75-c-60-iso10646-1"
        , inactiveColor         = "#000000" }

myLayout =
    onWorkspaces ["3:web", "4"] tabbedLayoutFirst $ -- tabbed layout for ws 3+4
    onWorkspaces ["0:mov"] floatingLayoutFirst $       -- floating layout for ws 8
    defaultLayoutOrder
    where
        defaultLayoutOrder = maximize $ boringWindows $ toggleLayouts Full (
                                tiled |||
                                tabbed shrinkText myTabConfig )
                                --Full ||| )
                                --simpleFloat )
        tabbedLayoutFirst =  maximize $ boringWindows $ toggleLayouts Full (
                                tabbed shrinkText myTabConfig |||
                                --Full |||
                                tiled )
        floatingLayoutFirst = maximize $ boringWindows (
                                tiled |||
                                tabbed shrinkText myTabConfig )
                              --  Full )

        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta (2/(1+(toRational(sqrt(5)::Double))))


        -- The default number of windows in the master pane
        nmaster = 1
        
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Magic Focus
        -- mfocus  = named "Magic Focus" (magicFocus (Mirror tiled))


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        -- quit and restart
        -- Quit xmonad:
        [ ((modm .|. shiftMask, xK_BackSpace), io (exitWith ExitSuccess))

        , ((modm,               xK_BackSpace), unsafeSpawn  -- %! Restart xmonad
                "if type xmonad; then xmonad --recompile && xmonad --restart; \
                \else xmessage xmonad not in \\$PATH: \"$PATH\"; fi" )
        {- -- %! Quit xmonad
        , ((modm .|. shiftMask, xK_q        ), io (exitWith ExitSuccess))
        , ((modm              , xK_q        ), unsafeSpawn
                "if type xmonad; then xmonad --recompile && xmonad --restart; \
                \else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
        -}

        -- launching and killing programs

        , ((modm .|. shiftMask, xK_t        ), spawn "xterm")
        , ((modm .|. shiftMask, xK_Return   ), spawn "terminator")
        , ((modm .|. shiftMask, xK_l        ), spawn "exe=`nmtui`")
        , ((modm .|. shiftMask, xK_f        ), spawn "firefox")
        , ((modm .|. shiftMask, xK_i        ), spawn "chromium")
        , ((modm .|. shiftMask, xK_x        ), unsafeSpawn "~/.xmonad/actions/run-filemanager" )
        , ((modm,               xK_p        ), spawn "exe=`dmenu_run -b -nb black -nf yellow -sf yellow` && eval \"exec $exe\"") -- spawn dmenu
        , ((modm .|. shiftMask, xK_p        ), spawn "gmrun") -- %! Launch gmrun
        , ((modm .|. shiftMask, xK_c        ), kill) -- %! Close the focused window
        , ((modm              , xK_space    ), sendMessage NextLayout)
        
        -- |Reset the layouts on the current workspace to default
        , ((modm .|. shiftMask, xK_space    ), setLayout $ XMonad.layoutHook conf) 

        -- move focus up or down the window stack
        , ((modm,               xK_j        ), focusDown)
        , ((modm,               xK_k        ), focusUp)
        , ((modm,               xK_Tab      ), focusDown)
        , ((modm .|. shiftMask, xK_Tab      ), focusUp)
        , ((modm,               xK_m        ), focusMaster)
        , ((modm,               xK_b        ), markBoring)
        , ((modm .|. controlMask, xK_b      ), clearBoring)
        
        -- modifying the window order
        , ((modm,               xK_Return   ), promote)
        
        -- |Swap the focused window with the next window
        , ((modm .|. shiftMask, xK_j        ), windows W.swapDown  )
        
        -- |Swap the focused window with the previous window
        , ((modm .|. shiftMask, xK_k        ), windows W.swapUp    )
        
        -- |Navigate through all windows, including boring ones
        , ((modm .|. controlMask, xK_j      ), windows W.focusDown)
        , ((modm .|. controlMask, xK_k      ), windows W.focusUp)
        
        -- temporarily maximize window
        , ((modm,               xK_backslash),
                withFocused (sendMessage . maximizeRestore))
        
        -- |Resizing the master/slave ratio
        -- |Shrink the master area
        , ((modm,               xK_h        ), sendMessage Shrink)
        
        -- |Expand the master area
        , ((modm,               xK_l        ), sendMessage Expand)
        
        -- |Floating layer support
        -- |Push window back into tiling
        , ((modm,               xK_t        ), withFocused $ windows . W.sink)
        
        -- |Toggle xmobar
        , ((modm, 		xK_b        ), sendMessage ToggleStruts)
        , ((modm,		xK_f	    ), sendMessage $ TL.Toggle "Full")
        
        -- increase or decrease number of windows in the master area
        -- |Increment the number of windows in the master area
        , ((modm,               xK_comma    ), sendMessage (IncMasterN 1))
        
        -- |Deincrement the number of windows in the master area
        , ((modm,               xK_period   ), sendMessage (IncMasterN (-1)))
        
        -- screenshot
        , ((controlMask,        xK_Print    ), spawn "sleep 0.2; scrot -s")
        
        -- |Dvorak
        , ((modm .|. shiftMask, xK_a), spawn "setxkbmap -layout dvorak")
        
        -- kill windows – EXPERIMENTAL!!
        , ((modm,               xK_Delete   ), safeSpawn "xkill" [])   -- xkill interactive
        , ((modm .|. shiftMask, xK_Delete   ), kill)
        , ((modm .|. controlMask, xK_Delete ), withFocused $
            \w -> spawn ("xkill -id " ++ show w))   -- xkill focused
        
        -- end of kill windows – EXPERIMENTAL!!
        -- prompt

--                windowPromptBring defaultXPConfig)
        , ((modm, xK_x  ), dirExecPromptNamed defaultXPConfig
                spawn "~/.xmonad/actions/" "Run Action: ")
        , ((modm, xK_F1 ), manPrompt    defaultXPConfig)
        , ((modm, xK_F2 ), runOrRaisePrompt    defaultXPConfig)
        , ((modm, xK_F3 ), shellPrompt  defaultXPConfig)
        , ((modm, xK_F12), xmonadPrompt defaultXPConfig)

        -- mark windows as boring and navigate through windows
        -- XMonad.Actions.PhysicalScreens
        -- this block requires xmonad-contrib >= 0.10
--        , ((modm, xK_a), onPrevNeighbour W.view)
--        , ((modm, xK_o), onNextNeighbour W.view)
--        , ((modm .|. shiftMask, xK_a), onPrevNeighbour W.shift)
--        , ((modm .|. shiftMask, xK_o), onNextNeighbour W.shift)
        -- mod-{w,e,r} switch to physical screens 1, 2 or 3
        -- mon-shift-{w,e,r} mode client to screen 1, 2 or 3
        ] ++ [((modm .|. mask, key), f sc)
                | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
        -- mod-[1..0,-,=] @@ Switch to workspace N
        -- mod-shift-[1..0,-,=] @@ Move client to workspace N
        -- mod-control-shift-[1..0,-,=] @@ Copy client to workspace N (from CopyWindow)
        ] ++ [((modm .|. mask, key), windows $ f sc)
                | (key, sc) <- zip
                        ([xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal])
                        (XMonad.workspaces conf)
                , (f, mask) <- [(W.greedyView, 0), (W.shift, shiftMask),
                                (copy, shiftMask .|. controlMask)]
        ]

-- newKeys is for merging the default keyboard layout with our own.
-- We don't use it in this file anymore. But it doesn't harm to
-- have the function here.
newKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
newKeys x = myKeys x `M.union` keys defaultConfig x

(~?) :: (Eq a, Functor f) => f [a] -> [a] -> f Bool
q ~? x = fmap (x `isInfixOf`) q



---------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    spawn "xsetroot -cursor_name left_ptr"
    xmonad $ myConfig xmproc

-- myConfig :: Handle -> XConfig
myConfig xmproc = defaultConfig {
        -- simple stuff
        terminal           = myTerminal,
--        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
--        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        -- key bindings
        --keys               = newKeys,
        keys               = myKeys,
--        mouseBindings      = myMouseBindings,
        -- hooks, layouts
        handleEventHook    = myHandleEventHook,
        layoutHook         = smartBorders $ avoidStruts myLayout,
        manageHook         = myManageHook <+> manageDocks,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook
   

    }
