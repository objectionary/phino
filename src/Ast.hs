module Ast where

newtype Program = Program Expression      -- Q -> expr
  deriving (Eq, Show)

data Expression 
  = ExFormation [Binding]                 -- [bindings]
  | ExThis                                -- $
  | ExGlobal                              -- Q
  | ExTermination                         -- T
  | ExMeta String                         -- !e
  | ExApplication Expression [TauBinding] -- expr(attr -> expr)
  | ExDispatch Expression Attribute       -- expr.attr
  | ExMetaTail Expression String          -- expr * !t
  deriving (Eq, Show)

data Binding
  = BiTau TauBinding                      -- attr -> expr
  | BiMeta String                         -- !B
  | BiDelta String                        -- D> 1F-2A
  | BiMetaDelta String                    -- D> !b
  | BiVoid Attribute                      -- attr -> ?
  | BiLambda String                       -- L> Function
  | BiMetaLambda String                   -- L> !F
  deriving (Eq, Show)

data TauBinding = TauBinding Attribute Expression
  deriving (Eq, Show)

data Attribute
  = AtLabel String                        -- attr
  | AtAlpha Integer                       -- ~1
  | AtPhi                                 -- @
  | AtRho                                 -- ^
  | AtMeta String                         -- !a
  deriving (Eq, Show)
