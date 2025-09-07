module CLIOptions
  ( optRule,
    optNormalize,
    optTarget,
  )
where

import Options.Applicative

-- | Parser for rule file paths (used in rewrite and explain commands)
optRule :: Parser [FilePath]
optRule = many (strOption (long "rule" <> metavar "FILE" <> help "Path to custom rule"))

-- | Parser for normalize flag (used in rewrite and explain commands)
optNormalize :: Parser Bool
optNormalize = switch (long "normalize" <> help "Use built-in normalization rules")

-- | Parser for target file output (used in rewrite and explain commands)
optTarget :: Parser (Maybe FilePath)
optTarget = optional (strOption (long "target" <> short 't' <> metavar "FILE" <> help "File to save output to"))