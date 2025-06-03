module Test.Utils
  ( goldenDiffCustom,
  )
where

import Test.Tasty (TestName, TestTree)
import Test.Tasty.Golden (goldenVsFileDiff)

-- | We use a custom diff as this will print the actual diff to stdout,
-- whereas ordinary goldenVsFile will merely print something like
-- 'files are different'. The former is especially useful when we do not have
-- easy access to the diff files e.g. CI.
goldenDiffCustom :: TestName -> FilePath -> FilePath -> IO () -> TestTree
goldenDiffCustom x = goldenVsFileDiff x diffArgs
  where
    -- Apparently, the 'diff' program exists for windows and unix on CI. Thus
    -- the arguments ["diff", "-u" "--color=always", ref, new] also seem fine.
    -- Nevertheless, we use git as it is possibly more portable.
    diffArgs ref new =
      [ "git",
        "diff",
        "--exit-code",
        "--color=always",
        "--no-index",
        ref,
        new
      ]
