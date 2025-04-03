
#i had all these tests, but dropbox_downloader takes a pretty long time to load
#so i'm keeping the tests here for posterity but just keeping the last one uncommented
#that last one should encompass the results of the other three anyways
# test_that("there are no errors", {
#   expect_no_error(dropbox_downloader("https://www.dropbox.com/scl/fo/36jnhue6abszkq2jegjfv/AGRQvghYn6TpCx8uY2KCjZ8?rlkey=4l4xa8nofqtjgojf26xg18p76&dl=0"))
# })
#
# test_that("there are no warnings", {
#   expect_no_warning(dropbox_downloader("https://www.dropbox.com/scl/fo/36jnhue6abszkq2jegjfv/AGRQvghYn6TpCx8uY2KCjZ8?rlkey=4l4xa8nofqtjgojf26xg18p76&dl=0"))
# })
#
# test_that("function returns a string", {
#   expect_type(dropbox_downloader("https://www.dropbox.com/scl/fo/36jnhue6abszkq2jegjfv/AGRQvghYn6TpCx8uY2KCjZ8?rlkey=4l4xa8nofqtjgojf26xg18p76&dl=1"),
#               "character")
# })

test_that("function returns a string to a local directory that exists", {
  expect(dir.exists(dropbox_downloader("https://www.dropbox.com/scl/fo/36jnhue6abszkq2jegjfv/AGRQvghYn6TpCx8uY2KCjZ8?rlkey=4l4xa8nofqtjgojf26xg18p76&dl=0")),
         "your function returned a directory that does not exist")
})
