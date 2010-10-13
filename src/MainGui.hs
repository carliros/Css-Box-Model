module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX

main :: IO()
main = start gui

gui :: IO()
gui = do
    f   <- frame [text:= "Box Css Model"]
    -- control panel
    pn1 <- panel f []
    bcp <- button pn1 [text := "Apply Properties"]
    cs1 <- entry pn1 [text := "1000 100"]
    cs2 <- entry pn1 [text := "12 12 12 12"]
    cs3 <- entry pn1 [text := "1  1  1  1 "]
    cs4 <- entry pn1 [text := "1  1  1  1 "]
    cs5 <- entry pn1 [text := "red red red red"]
    cs6 <- entry pn1 [text := "solid solid solid solid"]
    cs7 <- entry pn1 [text := "100 100"]
    set pn1 [layout := row 5 [hfill $ column 5 
                [ hfill $ row 5 [label "position"      , hfill $ widget cs7, label "size :"       , hfill $ widget cs1]
                , hfill $ row 5 [label "margin:"       , hfill $ widget cs2]
                , hfill $ row 5 [label "border-width:" , hfill $ widget cs3, label "border-color:", hfill $ widget cs5, label "border-style:", hfill $ widget cs6]
                , hfill $ row 5 [label "padding-width:", hfill $ widget cs4]], vfill $ widget bcp]]

    -- paint panel
    pn2 <- panel f [bgcolor := yellow]
    box <- window pn2 [on paint := onBoxPaint cs2 cs3 cs4 cs5 cs6] -- [size := sz 100 100, border := BorderStatic]

    set f [layout := column 5 [hfill $ widget pn1, fill $ widget pn2]]

    set bcp [on click := rePaint pn2 cs1 cs7 box]
    return ()

onBoxPaint cs2 cs3 cs4 cs5 cs6 dc rt@(Rect x y w h) = do
    -- margin points
    mpt <- get cs2 text
    let [mt,mr,mb,ml] = map toInt $ words mpt
    let (bx1,by1) = (x+ml+1  ,y+mt+1)
    let (bx2,by2) = (x+w-mr-1,y+mt+1)
    let (bx3,by3) = (x+w-mr-1,y+h-mb-1)
    let (bx4,by4) = (x+ml+1  ,y+h-mb-1)
    
    --border color
    wcl <- get cs5 text
    let toColor c = case c of
                        "red"      -> red
                        "yellow"   -> yellow
                        "darkgrey" -> darkgrey
                        "grey"     -> grey
                        "white"    -> white
                        "green"    -> green
                        "blue"     -> blue
                        "cyan"     -> cyan
                        "magenta"  -> magenta
                        _          -> black
    let [bct,bcr,bcb,bcl] = map toColor $ words wcl

    -- border style
    wst <- get cs6 text
    let toPenStyle s = case s of
                        "hidden" -> PenTransparent
                        "dotted" -> PenDash DashDot
                        "dashed" -> PenDash DashLong
                        _        -> PenSolid
    let [bst,bsr,bsb,bsl] = map toPenStyle $ words wst

    -- border widths
    bwd <- get cs3 text
    let [(bt,dt),(br,dr),(bb,db),(bl,dl)] = map (\s -> let n = toInt s in (n,n `div` 2)) $ words bwd
    when (bt /= 0) (line dc (pt (bx1+dt) (by1+dt)) (pt (bx2-dt) (by2+dt)) [penWidth := bt, penColor := bct, penKind := bst])
    when (br /= 0) (line dc (pt (bx2-dr) (by2+dr)) (pt (bx3-dr) (by3-dr)) [penWidth := br, penColor := bcr, penKind := bsr])
    when (bb /= 0) (line dc (pt (bx3-db) (by3-db)) (pt (bx4+db) (by4-db)) [penWidth := bb, penColor := bcb, penKind := bsb])
    when (bl /= 0) (line dc (pt (bx4+dl) (by4-dl)) (pt (bx1+dl) (by1+dl)) [penWidth := bl, penColor := bcl, penKind := bsl])
    
    -- padding widths
    pwd <- get cs4 text
    let [ppt,ppr,ppb,ppl] = map toInt $ words pwd
    let (cx1,cy1) = (x+ml+bl+ppl+1  ,y+mt+bt+ppt+1)
    let (cx2,cy2) = (x+w-mr-br-ppr  ,y+mt+bt+ppt+1)
    let (cx3,cy3) = (x+w-mr-br-ppr  ,y+h-mb-bb-ppb)
    let (cx4,cy4) = (x+ml+bl+ppl+1  ,y+h-mb-bb-ppb)
    drawRect dc (rect (pt cx1 cy1) (sz (cx2-cx1) (cy4-cy1))) []

rePaint pn2 cs1 cs7 box p = do
    tsz <- get cs1 text
    tpt <- get cs7 text
    let [w,h] = map toInt $ words tsz
    let [x,y] = map toInt $ words tpt
    set box [size := sz w h]
    windowMove box (pt x y)
    repaint pn2

toInt :: String -> Int
toInt = read
