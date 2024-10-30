module Format where

class Format a where
  format :: Int -> a -> String

indent :: Int -> String
indent level = "\n" ++ replicate (2 * level) ' '

formatOne :: (Format a) => Int -> a -> String
formatOne level target = indent (level + 1) ++ format (level + 1) target

formatAll :: (Format a) => Int -> [a] -> String
formatAll level = concatMap (formatOne level)

formatAllBracket :: (Format a) => Int -> [a] -> String
formatAllBracket level targets =
  concat
    [ indent (level + 1),
      "[",
      formatAll (level + 1) targets,
      "]"
    ]
