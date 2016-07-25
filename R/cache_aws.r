#' new_cache
#'
#' Use R to cache items.
#'
#' @export

cache_aws <- function(cache_name = "rcache") {

  if (!(aws.s3::bucket_exists(cache_name))) {
    aws.s3::put_bucket(cache_name)
    if (!(aws.s3::bucket_exists(cache_name))) {
      stop("Cache name must use unique bucket name")
    }
  }

  cache <- NULL
  cache_reset <- function() {
    aws.s3::delete_bucket(cache_name)
    aws.s3::put_bucket(cache_name)
  }

  cache_set <- function(key, value) {
    tfile = tempfile()
    save(value, file = tfile)
    aws.s3::put_object(tfile, object = key, bucket = cache_name)
  }

  cache_get <- function(key) {
    suppressWarnings(aws.s3::s3load(object = key, bucket = cache_name))
    base::get(ls()[ls() != "key"][[1]])
  }

  cache_has_key <- function(key) {
    aws.s3::head_object(object = key, bucket = cache_name)
  }

  cache_keys <- function() {
    items <- lapply(aws.s3::get_bucket(bucket = cache_name), function(x) {
      if ("Key" %in% names(x)) {
        return(x$Key)
      } else {
        return(NULL)
      }
    })
    unlist(Filter(Negate(is.null), items))
  }

  list(
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    keys = cache_keys
  )
}
