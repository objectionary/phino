{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The catalogue of λ functions — atoms — phino implements. A λ binding naming
-- a function absent from here cannot fire: dataization stops with
-- "Atom 'L_foo' does not exist", unless '--partial' parks on it. Both answers
-- are legitimate states for a caller to depend on, so the catalogue is printed
-- by the 'atoms' command, letting a build check its own table of names against
-- the binary instead of probing one name at a time.
--
-- Only the names are tied to the engine automatically: they are exactly the
-- ones 'Dataize.implementedAtoms' fires, and 'AtomsSpec' fails when the two
-- lists drift apart. Every other field — the labels, ρ, the forma and the
-- semantics — is a promise checked by hand against 'Dataize.implementations',
-- so an atom whose behaviour changes has to have its entry moved with it, in
-- the same commit. 'AtomsSpec' pins the entries most likely to drift by
-- dataizing the atom they describe.
module Atoms (Atom (..), atoms, printAtoms, printAtomsInJSON) where

import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- One λ function of the catalogue
data Atom = Atom
  { _name :: Text
  -- ^ The name of the function, as it stands in a λ binding
  , _labels :: [Text]
  -- ^ The labels the function reads off the formation it fires against
  , _rho :: Bool
  -- ^ Whether the function reads ρ of that formation
  , _forma :: Text
  -- ^ The forma of every object the function can answer, ⊥ aside
  , _semantics :: Text
  -- ^ One-line statement of what the function computes
  }
  deriving (Eq, Show)

-- Every λ function phino implements, in alphabetical order. An operand the
-- atom cannot read leaves it with ⊥ (termination), which the 'semantics' line
-- of each entry spells out. An atom that answers off more than one path names
-- every forma it can answer, not just the one its happy path takes.
atoms :: [Atom]
atoms =
  [ Atom
      { _name = "L_bytes_and"
      , _labels = ["b"]
      , _rho = True
      , _forma = "Φ.bytes"
      , _semantics = "Bitwise AND of ρ and b; ⊥ when the two byte arrays differ in size."
      }
  , Atom
      { _name = "L_bytes_concat"
      , _labels = ["b"]
      , _rho = True
      , _forma = "Φ.bytes"
      , _semantics = "The bytes of ρ followed by the bytes of b."
      }
  , Atom
      { _name = "L_bytes_eq"
      , _labels = ["b"]
      , _rho = True
      , _forma = "Φ.true or Φ.false"
      , _semantics = "Φ.true when ρ and b are the same byte array octet by octet, Φ.false otherwise."
      }
  , Atom
      { _name = "L_bytes_not"
      , _labels = []
      , _rho = True
      , _forma = "Φ.bytes"
      , _semantics = "Bitwise NOT of every byte of ρ."
      }
  , Atom
      { _name = "L_bytes_or"
      , _labels = ["b"]
      , _rho = True
      , _forma = "Φ.bytes"
      , _semantics = "Bitwise OR of ρ and b; ⊥ when the two byte arrays differ in size."
      }
  , Atom
      { _name = "L_bytes_right"
      , _labels = ["x"]
      , _rho = True
      , _forma = "Φ.bytes"
      , _semantics = "ρ shifted right by x bit positions, or left when x is negative, keeping its size; ⊥ unless x is a whole number within the 32-bit range."
      }
  , Atom
      { _name = "L_bytes_size"
      , _labels = []
      , _rho = True
      , _forma = "Φ.number"
      , _semantics = "The number of bytes in ρ."
      }
  , Atom
      { _name = "L_bytes_slice"
      , _labels = ["start", "len", "cant-slice"]
      , _rho = True
      , _forma = "Φ.bytes, or the forma of cant-slice"
      , _semantics = "The len bytes of ρ starting at offset start; a window reaching past the end of ρ applies cant-slice to a complaint string instead; ⊥ unless start and len are non-negative whole numbers within the 32-bit range."
      }
  , Atom
      { _name = "L_number_div"
      , _labels = ["x"]
      , _rho = True
      , _forma = "Φ.number"
      , _semantics = "ρ divided by x; ⊥ unless both operands are 8-byte numbers."
      }
  , Atom
      { _name = "L_number_eq"
      , _labels = ["x", "y"]
      , _rho = True
      , _forma = "Φ.number, or the forma of y"
      , _semantics = "ρ itself when it equals x, otherwise the y of the formation; ⊥ unless both operands are 8-byte numbers."
      }
  , Atom
      { _name = "L_number_gt"
      , _labels = ["x"]
      , _rho = True
      , _forma = "Φ.true or Φ.false"
      , _semantics = "Φ.true when ρ is greater than x, Φ.false otherwise; ⊥ unless both operands are 8-byte numbers."
      }
  , Atom
      { _name = "L_number_plus"
      , _labels = ["x"]
      , _rho = True
      , _forma = "Φ.number"
      , _semantics = "The sum of ρ and x; ⊥ unless both operands are 8-byte numbers."
      }
  , Atom
      { _name = "L_number_times"
      , _labels = ["x"]
      , _rho = True
      , _forma = "Φ.number"
      , _semantics = "The product of ρ and x; ⊥ unless both operands are 8-byte numbers."
      }
  ]

-- Render the catalogue as one name per line, which is all a human asking
-- "does this name exist?" needs
printAtoms :: String
printAtoms = T.unpack (T.intercalate "\n" (map _name atoms))

-- Render the catalogue as a JSON array of objects, one per λ function. The
-- keys of every object are written in a fixed order, so that two runs of the
-- same binary print byte-identical output and a build may diff it.
printAtomsInJSON :: String
printAtomsInJSON = T.unpack (T.concat ["[\n", T.intercalate ",\n" (map entry atoms), "\n]"])
  where
    entry :: Atom -> Text
    entry atom =
      T.intercalate
        "\n"
        [ "  {"
        , field "name" (quoted (_name atom)) <> ","
        , field "labels" (array (map quoted (_labels atom))) <> ","
        , field "rho" (flag (_rho atom)) <> ","
        , field "forma" (quoted (_forma atom)) <> ","
        , field "semantics" (quoted (_semantics atom))
        , "  }"
        ]
    field :: Text -> Text -> Text
    field key value = T.concat ["    ", quoted key, ": ", value]
    array :: [Text] -> Text
    array values = T.concat ["[", T.intercalate ", " values, "]"]
    flag :: Bool -> Text
    flag True = "true"
    flag False = "false"
    -- Escaping is aeson's job, since a semantics line may hold a quote or a
    -- backslash one day. Non-ASCII characters are left as they are, so that ρ
    -- and Φ stay readable in the output
    quoted :: Text -> Text
    quoted = TL.toStrict . encodeToLazyText
