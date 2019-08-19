
context('calcDIs')

test_that('calcDIs works',{
  files <- list.files(system.file('phenotypeDataCollectionSheets',package = 'pdi'),full.names = TRUE)
  DIs <- calcDIs(files)
  
  expect_true(is.list(DIs))
})