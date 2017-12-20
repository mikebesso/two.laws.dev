library(two.laws.dev)


SetTestContext("initialization")

CreateTestCase(
  "create new",
  {
    PackageData <- PackageDataClass$new()
  }
)