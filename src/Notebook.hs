import Graphics.UI.WX
import Graphics.UI.WXCore

main = start gui

gui :: IO ()
gui = do
    f  <- frame [ ]
    nb <- notebook f [on notebookEvent := (\evt -> case evt of
                                            PageSelected i -> print i
                                            _ -> print "unkown" )]
    a <- panel nb []
    ap <- panel a []

    b <- panel nb []
    bp <- panel b []

    set f [ layout := tabs nb
                        [ tab "a" $ container a $ fill $ widget ap
                        , tab "b" $ container b $ fill $ widget bp
                        ]
          ]

data EventNotebook = PageSelected !Int | Unknown deriving (Show)

notebookEvent :: Graphics.UI.WX.Event (Notebook a) (EventNotebook -> IO ())
notebookEvent
  = newEvent "notebookEvent" nbCtrlGetOnNotebookEvent nbCtrlOnNotebookEvent

nbCtrlOnNotebookEvent :: Notebook a -> (EventNotebook -> IO ()) -> IO ()
nbCtrlOnNotebookEvent nb eventHandler
  = windowOnEvent nb (map fst nbEvents) eventHandler nbHandler
  where
    nbHandler event
      = do eventList <- fromNbEvent (objectCast event)
           eventHandler eventList

nbCtrlGetOnNotebookEvent :: Notebook a -> IO (EventNotebook -> IO ())
nbCtrlGetOnNotebookEvent nb
  = unsafeWindowGetHandlerState nb wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED (\evt -> skipCurrentEvent)


nbEvents = [(wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED, withItem PageSelected)]
  where withItem make evt = do col <- commandEventGetSelection evt
                               return (make col)


fromNbEvent :: NotebookEvent a -> IO EventNotebook
fromNbEvent evt
  = do tp <- eventGetEventType evt
       print tp
       case lookup tp nbEvents of
         Just f  -> f evt
         Nothing -> return Unknown


