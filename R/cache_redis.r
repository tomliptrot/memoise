#' @name cache_redis
#' @title Redis cache
#' @description Initiate an redis cache
#'
#' @examples
#'
#' \dontrun{
#'
#' r1 <- cache_redis()
#' mem_runif <- memoise(runif, cache = r1)
#' }
#'
#'
#' @param ... Arguments passed to \code{rredis::redisConnect()}
#' @param index The nonnegative integer value of the redis database to select (default = 0).
#' @export
cache_redis <- function(cache_id = uuid::UUIDgenerate(), ..., index = 0) {

  rredis::redisConnect(...)
  if(index !=0) rredis::redisSelect(index)

  cache_reset <- function() {
    rredis::redisFlushDB()
  }

  cache_set <- function(key, value) {
    id_plus_key = paste0(cache_id, key)
    new_key = digest::digest(id_plus_key, algo = 'sha1')

    rredis::redisSet(new_key, value)
  }

  cache_get <- function(key) {
    id_plus_key = paste0(cache_id, key)
    new_key = digest::digest(id_plus_key, algo = 'sha1')

    rredis::redisGet(new_key )
  }

  cache_has_key <- function(key) {
    id_plus_key = paste0(cache_id, key)
    new_key = digest::digest(id_plus_key, algo = 'sha1')
    rredis::redisExists(new_key )
  }

  cache_keys <- function() {
    rredis::redisKeys()

  }

  list(
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    keys = cache_keys
  )
}
