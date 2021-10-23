{-# LANGUAGE TemplateHaskell #-}

module Run (run) where

import Brick
import qualified Brick.AttrMap as A
import Brick.Focus (
  focusGetCurrent,
  focusRingCursor,
 )
import Brick.Forms (
  Form,
  allFieldsValid,
  checkboxField,
  editShowableField,
  editTextField,
  focusedFormInputAttr,
  formFocus,
  formState,
  handleFormEvent,
  invalidFields,
  invalidFormInputAttr,
  newForm,
  renderForm,
  setFieldValid,
  (@@=),
 )
import qualified Brick.Main as M
import Brick.Types ()
import qualified Brick.Types as T
import qualified Brick.Util as U
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, txt, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Import
import Lens.Micro.TH
import qualified RIO.Text as RT
import Data.Text.IO (hPutStrLn)

data Task = Task
  { title :: Text
  , priority :: Int
  , added_at :: UTCTime
  , completed_at :: Maybe UTCTime
  }
  deriving (Show)

makeLensesFor
  [ ("title", "_title")
  , ("priority", "_priority")
  , ("added_at", "_added_at")
  , ("completed_at", "_completed_at")
  ]
  ''Task

data ResName
  = CreateTask
  | EditTask
  | TitleField
  | PriorityField
  | ListTasks
  deriving (Eq, Ord, Show)

instance Display ResName where
  display = fromString . show

data Page e n
  = ListPage
  | CreatePage (Form Task e n)
  | EditPage Int (Form Task e n)

data AppState e n = AppState
  { activeTasks :: L.GenericList n Vector Task
  , page :: Page e n
  }

makeLensesFor
  [ ("activeTasks", "_activeTasks")
  , ("page", "_page")
  ]
  ''AppState

drawListPageUI :: L.List ResName Task -> [Widget ResName]
drawListPageUI tasks =
  [ vBox
      [ C.hCenter box
      , C.hCenter $ str "Press c to add a task."
      , C.hCenter $ str "Press x to mark/unmark, e to edit a task."
      , C.hCenter $ str "Press Esc to exit."
      ]
  ]
 where
  cur = case tasks ^. L.listSelectedL of
    Nothing -> str "-"
    Just i -> str (show (i + 1))
  total = str $ show $ Vec.length $ tasks ^. L.listElementsL
  label = str "Item " <+> cur <+> str " of " <+> total
  box =
    B.borderWithLabel label $
      L.renderList listDrawElement True tasks

createForm :: Task -> Form Task e ResName
createForm =
  let label s w =
        padBottom (Pad 1) $
          vLimit 1 $ hLimit 15 $ (str s <+> fill ' ') <+> w
   in newForm
        [ label "Task"
            @@= editTextField _title TitleField (Just 1)
        , label "Priority"
            @@= editShowableField _priority PriorityField
        ]

drawTaskFormUI :: Form Task e ResName -> [Widget ResName]
drawTaskFormUI f =
  [ C.vCenter $
      C.hCenter $
        hLimit 50 $
          vBox
            [ B.border $ renderForm f
            , txt "smf"
            ]
  ]

drawUI :: AppState e ResName -> [Widget ResName]
drawUI s =
  case s ^. _page of
    CreatePage form -> drawTaskFormUI form
    EditPage _ form -> drawTaskFormUI form
    ListPage -> drawListPageUI $ s ^. _activeTasks

listDrawElement :: Bool -> Task -> Widget ResName
listDrawElement sel task = padRight Max listElem
 where
  completedCheck =
    if isJust $ task ^. _completed_at
      then "[x]"
      else
        "[ ]" ::
          Text
  title = completedCheck <> " " <> task ^. _title
  listElem =
    if sel
      then withAttr customAttr $ txt title
      else txt title

blankTask :: Int -> UTCTime -> Task
blankTask priority added_at =
  Task
    { title = ""
    , priority = priority
    , added_at = added_at
    , completed_at = Nothing
    }

appEvent :: AppState e ResName -> T.BrickEvent ResName e -> T.EventM ResName (T.Next (AppState e ResName))
appEvent s ev@(T.VtyEvent e) = do
  case s ^. _page of
    ListPage -> case e of
      V.EvKey x [] | x `elem` [V.KEsc, V.KChar 'q'] -> M.halt s
      V.EvKey (V.KChar 'c') [] -> do
        added_at <- liftIO getCurrentTime
        M.continue $ over _page (const $ CreatePage $ createForm $ blankTask 0 added_at) s
      V.EvKey (V.KChar 'e') [] ->
        case L.listSelectedElement $ s ^. _activeTasks of
          Just (idx, task) -> do
            M.continue $ over _page (const $ EditPage idx $ createForm task) s
          Nothing ->
            M.continue s
      V.EvKey (V.KChar x) [] | x `elem` ("x " :: String) -> do
        timestamp <- liftIO getCurrentTime
        M.continue $
          over
            _activeTasks
            ( L.listModify $
                over _completed_at $ maybe (Just timestamp) (const Nothing)
            )
            s
      V.EvKey V.KUp [] -> M.continue $ over _activeTasks L.listMoveUp s
      V.EvKey (V.KChar 'j') [] -> M.continue $ over _activeTasks L.listMoveUp s
      V.EvKey V.KDown [] -> M.continue $ over _activeTasks L.listMoveDown s
      V.EvKey (V.KChar 'k') [] -> M.continue $ over _activeTasks L.listMoveUp s
      V.EvKey V.KHome [] -> M.continue $ over _activeTasks L.listMoveToBeginning s
      V.EvKey V.KEnd [] -> M.continue $ over _activeTasks L.listMoveToEnd s
      _ -> M.continue s
    EditPage idx form -> case e of
      V.EvKey V.KEsc [] -> M.continue $ over _page (const ListPage) s
      V.EvKey V.KEnter [] -> do
        if allFieldsValid form
          then
            M.continue $
              s
                { activeTasks = L.listModify (const $ formState form) $ s ^. _activeTasks
                , page = ListPage
                }
          else M.continue s
      _ -> do
        form' <- handleFormEvent ev form
        let task = formState form'
        let rules =
              [ (TitleField, [RT.length (task ^. _title) >= 3, RT.length (task ^. _title) <= 100])
              , (PriorityField, [task ^. _priority >= 0, task ^. _priority <= 0])
              ]
        --let fml = displayShow rules
        --_ <- unRIO $ logInfo fml
        --_ <- liftRIO $ logInfo fml
        --_ <- unliftIO $ logInfo fml
        --_ <- liftIO $ logInfo fml
        M.continue $
          over
            _page
            ( const $
                EditPage idx $
                  foldl'
                    ( \form'' (field, rules') ->
                        foldl' (\form''' rule -> setFieldValid rule field form''') form'' rules'
                    )
                    form'
                    rules
            )
            s
    CreatePage form -> case e of
      V.EvKey V.KEsc [] -> M.continue $ over _page (const ListPage) s
      V.EvKey V.KEnter [] -> do
        let task = formState form
        if (3 >) $ RT.length $ RT.strip $ task ^. _title
          then M.continue s
          else do
            let nextPos = Vec.length $ s ^. _activeTasks . L.listElementsL
            M.continue $
              s
                { activeTasks = L.listInsert nextPos task $ s ^. _activeTasks
                , page = ListPage
                }
      _ -> do
        form' <- handleFormEvent ev form
        M.continue $ over _page (const $ CreatePage form') s
appEvent s _ = M.continue s

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `U.on` V.white)
    , (customAttr, U.fg V.black)
    ]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theApp :: M.App (AppState e ResName) e ResName
theApp =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theMap
    }

initialState :: AppState e ResName
initialState =
  AppState
    { activeTasks = L.list ListTasks (Vec.fromList []) 0
    , page = ListPage
    }

run :: RIO RioApp ()
run = liftIO $ void $ M.defaultMain theApp initialState

--johnId <- insert $ Person "John Doe" $ Just 35
--janeId <- insert $ Person "Jane Doe" Nothing

--insert $ BlogPost "My fr1st p0st" johnId
--insert $ BlogPost "One more for good measure" johnId

--oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
--liftIO $ print (oneJohnPost :: [Entity BlogPost])

--john <- get johnId
--liftIO $ print (john :: Maybe Person)

--delete janed
--deleteWhere [BlogPostAuthorId ==. johnId]
