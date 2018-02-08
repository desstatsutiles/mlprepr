#
# # Most tricks come from : http://r-pkgs.had.co.nz/r.html#r
# # This file should not be included in the package
# # I'm not sure if they'll end-up in the github yet
#
# # Devtools tricks  ------------------------------------------------------------
# install.packages("devtools")
# library(devtools)
# install_github("hadley/devtools")
# devtools::build_github_devtools()
# find_rtools()
# has_devel()
# build_github_devtools()
#
# # Strings trick ---------------------------------------------------------------
# # No string should include non-ascii chars
# stringi::stri_escape_unicode()
#
# # Documentation ---------------------------------------------------------------
# devtools::document()
# # @inheritParams avoids work !
#
# # Tests -----------------------------------------------------------------------
# devtools::use_testthat()
# expect_equal_to_reference() # helps deal with complex tables and so on
# # - Whenever you are tempted to type something into a print statement or a
# # debugger expression, write it as a test instead. — Martin Fowler
# # - Bonus : Always write a test when you discover a bug.
# # - Tests that take more than 1mn should use skip_on_cran()
