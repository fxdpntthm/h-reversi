{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

import           Graphics.Blank

import           Control.Concurrent
import           Control.Concurrent.STM


main' :: IO ()
main' = do
        boardV <- newTVarIO Map.empty
        blankCanvas 3000 { events = ["mousedown"] } $ loop boardV

data XO = X | O
        deriving (Eq,Ord,Show)

swap :: XO -> XO
swap X = O
swap O = X

viewer :: TVar (Map (Int, Int) XO) -> DeviceContext -> IO ()
viewer boardV context = do
   board <- atomically $ readTVar boardV
   send context $ do
        let (w,h) = (width context, height context)
        clearRect (0,0,w,h)
        beginPath()

        let sz = min w h
        save()
        translate (w / 2, h / 2)
        sequence_ [ do bigLine (-sz * 0.45,n) (sz * 0.45,n)
                       bigLine (n,-sz * 0.45) (n,sz * 0.45)
                  | n <- [-sz * 0.15,sz * 0.15]
                  ]


        sequence_ [ do save()
                       translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                       case Map.lookup (x,y) board of
                          Just X  -> drawX (sz * 0.1)
                          Just O  -> drawO (sz * 0.1)
                          Nothing -> return ()
                       restore()
                  | x <- [-1,0,1]
                  , y <- [-1,0,1]
                  ]
        restore()
   print "got to reading board"
   atomically $ do
           board' <- readTVar boardV
           if board == board' then retry else return ()
   print ("got the board",board)
   viewer boardV context

loop :: TVar (Map (Int, Int) XO) -> DeviceContext -> IO ()
loop boardV context = do

        forkIO $ viewer boardV context


        loop2 boardV context X

loop2 :: TVar (Map (Int, Int) XO) -> DeviceContext -> XO -> IO ()
loop2 boardV context turn = do
        let (w,h) = (width context, height context)
        let sz = min w h

        let pointToSq :: (Double, Double) -> Maybe (Int,Int)
            pointToSq (x,y) = do
                    x' <- fd ((x - w / 2) / sz)
                    y' <- fd ((y - h / 2) / sz)
                    return (x',y')

            fd x =
--                    trace (show ("fx",x,r)) $
                    if r `elem` [-1..1] then Just (signum r) else Nothing
                where r = round (x * 3.3333)

        print "got to event"
        event <- wait context
        print event


        turn' <- atomically $ do
         board <- readTVar boardV
         case ePageXY event of
           -- if no mouse location, ignore, and redraw
           Nothing -> return turn
           Just (x',y') -> case pointToSq (x',y') of
                             Nothing -> return turn
                             Just pos -> case Map.lookup pos board of
                                           Nothing -> do
                                                writeTVar boardV (Map.insert pos turn board)
                                                return (swap turn)
                                                    -- already something here
                                           Just _ ->  return turn

        loop2 boardV context turn'


{-
loop :: DeviceContext -> Map (Int, Int) XO -> XO -> IO ()
loop context board turn = do
--        print board
--        print turn
        (w,h,sz) <- send context $ do
                let (w,h) = (width context, height context)
                clearRect (0,0,w,h)
                beginPath()

                let sz = min w h
                save()
                translate (w / 2, h / 2)
                sequence_ [ do bigLine (-sz * 0.45,n) (sz * 0.45,n)
                               bigLine (n,-sz * 0.45) (n,sz * 0.45)
                          | n <- [-sz * 0.15,sz * 0.15]
                          ]


                sequence_ [ do save()
                               translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                               case Map.lookup (x,y) board of
                                  Just X -> drawX (sz * 0.1)
                                  Just O -> drawO (sz * 0.1)
                                  Nothing -> return ()
                               restore()
                          | x <- [-1,0,1]
                          , y <- [-1,0,1]
                          ]
                restore()
                return (w,h,sz)

        let pointToSq :: (Double, Double) -> Maybe (Int,Int)
            pointToSq (x,y) = do
                    x' <- fd ((x - w / 2) / sz)
                    y' <- fd ((y - h / 2) / sz)
                    return (x',y')

            fd x =
--                    trace (show ("fx",x,r)) $
                    if r `elem` [-1..1] then Just (signum r) else Nothing
                where r = round (x * 3.3333)

        event <- wait context
--        print event
        case ePageXY event of
           -- if no mouse location, ignore, and redraw
           Nothing -> loop context board turn
           Just (x',y') -> case pointToSq (x',y') of
                             Nothing -> loop context board turn
                             Just pos -> case Map.lookup pos board of
                                           Nothing -> loop context
                                                            (Map.insert pos turn board)
                                                            (swap turn)
                                                    -- already something here
                                           Just _ -> loop context board turn
-}
xColor, oColor, boardColor :: Text
xColor = "#ff0000"
oColor = "#00a000"
boardColor = "#000080"

drawX :: Double -> Canvas ()
drawX size = do
        strokeStyle xColor
        lineCap "butt"
        beginPath()
        moveTo(-size,-size)
        lineTo(size,size)
        lineWidth 10
        stroke()
        beginPath()
        moveTo(-size,size)
        lineTo(size,-size)
        lineWidth 10
        stroke()

drawO :: Double -> Canvas ()
drawO radius = do
        beginPath()
        arc(0, 0, radius, 0, 2 * pi, False)
        lineWidth 10
        strokeStyle oColor
        stroke()

bigLine :: (Double, Double) -> (Double, Double) -> Canvas ()
bigLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 20
        strokeStyle boardColor
        lineCap "round"
        stroke()

