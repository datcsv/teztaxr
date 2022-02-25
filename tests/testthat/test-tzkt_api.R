test_that("tzkt_operations", {
  # Test wallet by Claus Wilke, transacted 15 times total
  # see: https://clauswilke.com/art/post/tezos-nfts
  address <- "tz1cnfEeha6cibgKVs6DTGKyrP9V4bz5F2N6"
  x <- tzkt_operations(address)
  expect_equal(nrow(x), 15)
  expect_equal(x$level, c(2062489, 2062487, 2062483, 2053945, 2053945, 2053945, 2053945,
 2053945, 2053792, 2053792, 2053792, 2053792, 2053792, 2053736, 2053729))
  expect_equal(x$quote$usd, c(2.937317, 2.936970, 2.936970, 3.013968, 3.013968, 3.013968, 3.013968, 3.013968, 2.935688, 2.935688, 2.935688, 2.935688, 2.935688, 2.903569, 2.917219), tolerance = 0.0001)

  # transactions starting at level 2053945 only
  y <- tzkt_operations(address, level = 2053945)
  expect_equal(nrow(y), 7)
  expect_equal(y$id, x$id[9:15])
  expect_equal(y$level, x$level[9:15])
  expect_equal(y$hash, x$hash[9:15])
  expect_equal(y$timestamp, x$timestamp[9:15])

  # fetch three transactions, starting at level 2053945
  y <- tzkt_operations(address, level = 2053945, limit = 3)
  expect_equal(nrow(y), 3)
  expect_equal(y$id, x$id[9:11])
  expect_equal(y$level, x$level[9:11])
  expect_equal(y$hash, x$hash[9:11])
  expect_equal(y$timestamp, x$timestamp[9:11])

  # fetch transactions within a certain time span
  span <- x$timestamp[c(14, 4)] # x contains transactions in order from most to least recent
  y <- tzkt_operations(address, span = span)
  expect_equal(nrow(y), 11)
  expect_equal(y$id, x$id[4:14])
  expect_equal(y$level, x$level[4:14])
  expect_equal(y$hash, x$hash[4:14])
  expect_equal(y$timestamp, x$timestamp[4:14])
})

test_that("tzkt_operations_hash", {
  hash <- "onujxsRLwcYZuKVTKG1JLwFEe9EWBSPdiB6pRCg5WcH6NpXdRxA"
  x <- tzkt_operations_hash(hash)
  expect_equal(nrow(x), 5) # this operation consists of 5 separate transactions
  expect_equal(x$hash, rep(hash, 5))
  expect_equal(x$type, c("reveal", "transaction", "transaction", "transaction", "transaction"))
  expect_equal(x$level, rep(2053792, 5))
  expect_equal(x$id, 160258334:160258338)
  expect_equal(x$timestamp, rep("2022-01-22T23:34:00Z", 5))
  expect_equal(x$block, rep("BM6shrhs263XP36gQe7m63zXF6F5BMqXDUxVanBLsqgHc628YDV", 5))
  expect_equal(x$quote$usd, rep(2.935688, 5), tolerance = 0.0001)
})

test_that("tzkt_bigmap", {
  # Look up info for fxhash project #5577, in_vitro by Xeronimo
  # https://www.fxhash.xyz/generative/slug/in_vitro
  x <- tzkt_bigmap(70072, 5577)
  expect_equal(x$value$author, "tz1PfQDUD9UWGJ2hqHRttJU5wVtxrnPwBRa1")
  expect_equal(x$value$balance, "0")
  expect_equal(x$value$timestamp_minted, "2022-01-05T12:53:30Z")

  # Can also look up by hash
  y <- tzkt_bigmap(70072, x$hash)
  expect_identical(y, x)
})



test_that("tzkt_quote", {
  x <- tzkt_quote(level = 2148000)
  expect_equal(x$level, 2148000)
  expect_equal(x$timestamp, "2022-02-25T12:07:14Z")
  expect_equal(x$usd, 3.088042, tolerance = 0.0001)
})
