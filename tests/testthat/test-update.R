library(testthat)

test_that("test EBXInsert", {
  data_before <- EBXRead(
      branch='Fishery',
      instance='Fishery',
      folder='Metadata',
      table='CSVConcept')

  data_insert <- data.frame(
    Identifier = 999,
    Acronym = 'TEST_CSV',
    EBX_Codelist = 200
  )

  EBXInsert(
    branch='Fishery',
    instance='Fishery',
    folder='Metadata',
    table='CSVConcept ',
    data=data_insert)

  data_after <- EBXRead(
    branch='Fishery',
    instance='Fishery',
    folder='Metadata',
    table='CSVConcept')

  expect_true(nrow(data_before)+1==nrow(data_after), "did we add one row?")
})

test_that("test EBXUpdate", {
  data_update <- data.frame(
    Identifier = 999,
    Acronym = 'TEST_CSV_CONTINENT',
    EBX_Codelist = 201
  )
  EBXUpdate(
    branch='Fishery',
    instance='Fishery',
    folder='Metadata',
    table='CSVConcept ',
    data=data_update)

  data_check <- EBXRead(
    branch='Fishery',
    instance='Fishery',
    folder='Metadata',
    table='CSVConcept')

  print(data_check)

  expect_true(data_check$Acronym[data_check$Identifier==999] == "TEST_CSV_CONTINENT", "did we update the Acronym?")
  expect_true(data_check$EBX_Codelist[data_check$Identifier==999] == 201, "did we update the Identifier?")
})

test_that("test EBXRemove", {

  data_before <- EBXRead(
    branch='Fishery',
    instance='Fishery',
    folder='Metadata',
    table='CSVConcept')
  print(paste('before delete rows=',nrow(data_before)))

  ## -- delete Identifier = 999
  data_remove <- data.frame(
    Identifier = 999
  )
  EBXRemove(
    branch='Fishery',
    instance='Fishery',
    table='CSVConcept ',
    data=data_remove)

  data_check <- EBXRead(
    branch='Fishery',
    instance='Fishery',
    folder='Metadata',
    table='CSVConcept')
  print(paste('after delete rows=',nrow(data_check)))

  expect_true(nrow(data_before)-1==nrow(data_check), "did we delete one row?")})

