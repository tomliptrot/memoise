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

  combine_id_and_key = function(cache_id, key){
    paste0(c(cache_id, key), collapse = ':')
  }

  cache_reset <- function() {
      rredis::redisDelete(rredis::redisKeys(pattern =   combine_id_and_key (cache_id, '*')))
  }

  cache_set <- function(key, value) {
    id_plus_key = combine_id_and_key (cache_id, key)
    rredis::redisSet(id_plus_key, value)
  }

  cache_get <- function(key) {
    id_plus_key = combine_id_and_key (cache_id, key)

    rredis::redisGet(id_plus_key )
  }

  cache_has_key <- function(key) {
    id_plus_key = combine_id_and_key (cache_id, key)
    rredis::redisExists(id_plus_key)
  }

  cache_keys <- function() {
    rredis::redisKeys(pattern =   combine_id_and_key (cache_id, '*'))
  }

  list(
    reset = cache_reset,
    set = cache_set,
    get = cache_get,
    has_key = cache_has_key,
    keys = cache_keys
  )
}
