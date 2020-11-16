library(testthat)
context("faoebx5")

test_that("test SetupEBXConnection", {
  # his test is used to perist the connection details into the yaml file
  skip_if(TRUE, message = "skipped, so we do not delete the yaml file")
  filename <- "ebx5_connection.yaml"
  unlink(filename)
  expect_false(filename %in% dir())

  SetupEBXConnection(
    meta_branch='Fishery',
    meta_instance='Fishery',
    ebx_soap_url='http://hqldvebx1.hq.un.fao.org:8080/ebx-dataservices/connector',
    username='Fishery-SOAP',
    password='xxx2')

  expect_true(file.exists(file.path(getwd(),filename)))
})

test_that("test GetEBXConnection", {
  # this test reads the connection details from the yaml file
  connection <- GetEBXConnection()

  expect_true(length(connection) == 5)
  expect_true(exists('username', connection))
  expect_true(exists('password', connection))
  expect_true(exists('meta_branch', connection))
  expect_true(exists('meta_instance', connection))
  expect_true(exists('ebx_soap_url', connection))

  expect_false(is.null(connection$username))
  expect_false(is.null(connection$password))
  expect_false(is.null(connection$meta_branch))
  expect_false(is.null(connection$meta_instance))
  expect_false(is.null(connection$ebx_soap_url))
})

test_that("test EBXRead", {

  # this test read the definitions of code-lists (Metadata/EBXCodelist)
  metadata_cls <- EBXRead(branch='Fishery',
                  instance='Fishery',
                  folder='Metadata',
                  table='EBXCodelist')

  expect_true(ncol(metadata_cls) > 5)
  expect_true(nrow(metadata_cls) > 10)

  expect_true(exists('Identifier', metadata_cls))
  expect_true(exists('Acronym', metadata_cls))
  expect_true(exists('Folder', metadata_cls))
  expect_true(exists('Name', metadata_cls))
  expect_true(exists('Branch', metadata_cls))
  expect_true(exists('Instance', metadata_cls))
})

test_that("test read Country", {
  # this test is reading the country code-list
  country <- EBXRead(branch='Fishery',
                     instance='Fishery',
                     folder='Country',
                     table='CountryItem')

  expect_true(ncol(country) > 20)
  expect_true(nrow(country) > 270)

  expect_true(exists('Identifier', country))
  expect_true(exists('UN_Code', country))
  expect_true(exists('ISO3_Code', country))
})

