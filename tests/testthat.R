
# Source this file to run all tests in testthat directory

# Notes on unit testing
# 
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)

testthat::test_dir( here::here( "tests/testthat/" ) ) # reporter="minimal" )


if ( FALSE ) {
  testthat::test_file( here::here( "tests/testthat/test-08_perform_simulation.R" ) )
}