#' @title Exchanger
#' @description Exchanger Class
#' @export
#' @examples \dontrun{
#' x <- Exchanger$new("gmail.com")
#' x
#' x$cached("gmail.com")
#' x
#' x$mxers_
#' x$mxers()
#' }
Exchanger <- R6::R6Class(
  "Exchanger",
  public = list(
    host = NULL,
    config = list(),
    dns_disabled = NULL,
    host_cache = NULL,
    cache_size = NULL,
    mxers_ = NULL,
    provider_ = NULL,

    cached = function(host, config = list()) {
      if (is.null(self$host_cache)) self$host_cache <- list()
      if (is.null(self$cache_size)) self$cache_size <- as.numeric(Sys.getenv("EMAIL_ADDRESS_CACHE_SIZE")) %||% 100
      if (host %in% names(self$host_cache)) {
        o <- self$host_cache[[host]]
        self$host_cache[[host]] <- NULL
        self$host_cache[[host]] <- o # LRU cache, move to end
      } else if (length(self$host_cache) >= self$cache_size) {
        self$host_cache[[names(self$host_cache)[1]]] <- NULL
        self$host_cache[[host]] <- self$initialize(host, config)
      } else {
        self$host_cache[[host]] <- self$initialize(host, config)
      }
      return(self)
    },

    initialize = function(host, config = list()) {
      self$host <- host
      self$config <- if (is.list(config)) Config$new(config) else config
      self$dns_disabled <- self$config$config[["host_validation"]] == "syntax" || self$config$config[["dns_lookup"]] == "off"
      self$mxers()
      return(self)
    },

    # Returns the provider name based on the MX-er host names, or nil if not matched
    provider = function() {
      if (!is.null(self$provider_)) return(self$provider_)
      Map(function(provider, config) {
        if (!is.null(config[["exchanger_match"]]) && asb(self$matches(config[["exchanger_match"]]))) {
          return(self$provider_ <- provider)
        }
      }, names(Config$new()$providers), Config$new()$providers)
      self$provider_ <- "default"
      return(self$provider_)
    },

    # Returns: [["mta7.am0.yahoodns.net", "66.94.237.139", 1], ["mta5.am0.yahoodns.net", "67.195.168.230", 1], ["mta6.am0.yahoodns.net", "98.139.54.60", 1]]
    # If not found, returns []
    # Returns a dummy record when dns_lookup is turned off since it may exists, though
    # may not find provider by MX name or IP. I'm not sure about the "0.0.0.0" ip, it should
    # be good in this context, but in "listen" context it means "all bound IP's"
    mxers = function() {
      if (self$dns_disabled) return(self$mxers_ <- list(c("example.com", "0.0.0.0", 1)))
      res <- tryCatch(pingr::nsl(self$host, type = 15L), error = function(e) e)
      if (inherits(res, "error")) return(self$mxers_ <- list(c("example.com", "0.0.0.0", 1)))
      z <- vapply(res$answer$data, function(w) {
        tmp <- tryCatch(pingr::nsl(w), error = function(e) e)
        if (inherits(tmp, "error")) return("")
        return(tmp$answer$data[[1]])
      }, "")
      self$mxers_ <- Map(function(x, y) c(x, y, 1), res$answer$data, z)
      # return(c(self$host, res, 1))
    },

    # Returns Array of domain names for the MX'ers, used to determine the Provider
    # def domains
    #   @_domains ||= mxers.map { |m| EmailAddress::Host.new(m.first).domain_name }.sort.uniq
    # end

    # Returns an array of MX IP address (String) for the given email domain
    mx_ips = function() {
      if (self$dns_disabled) return("0.0.0.0")
      # self$mxers.map { |m| m[1] }
      unlist(lapply(self$mxers_, "[[", 2))
    },

    # Simple matcher, takes an array of CIDR addresses (ip/bits) and strings.
    # Returns true if any MX IP matches the CIDR or host name ends in string.
    # Ex: match?(%w(127.0.0.1/32 0:0:1/64 .yahoodns.net))
    # Note: Your networking stack may return IPv6 addresses instead of IPv4
    # when both are available. If matching on IP, be sure to include both
    # IPv4 and IPv6 forms for matching for hosts running on IPv6 (like gmail).
    matches = function(rules) {
      for (i in seq_along(rules)) {
        rule <- rules[i]
        if (grepl("/", rule)) {
          if (self$in_cidr(rule)) return(rule)
        } else {
          lapply(self$mxers_, function(z) {
            if (grepl(paste0(rule, "$"), z[1])) return(rule)
          })
        }
      }
      return(FALSE)
    },

    # Given a cidr (ip/bits) and ip address, returns true on match. Caches cidr object.
    # cidr="192.168.2.0/28"
    in_cidr = function(cidr) {
      if (grepl(":", cidr)) {
        return(FALSE)
        # FIXME: not sure how to handle ipv6 yet
        # c = NetAddr::IPv6Net.parse(cidr)
        # return true if mx_ips.find do |ip|
        #   next unless ip.include?(":")
        #   rel = c.rel NetAddr::IPv6Net.parse(ip)
        #   !rel.nil? && rel >= 0
        # end
      } else if (grepl("\\.", cidr)) {
        # c = NetAddr::IPv4Net.parse(cidr)
        z <- self$mx_ips()
        comps <- vapply(z, function(w) {
          is_within(ip_address(w), ip_network(cidr))
        }, logical(1))
        if (any(comps)) return(TRUE)
        # return true if mx_ips.find do |ip|
        #   next if ip.include?(":")
        #   rel = c.rel NetAddr::IPv4Net.parse(ip)
        #   !rel.nil? && rel >= 0
        # end
      }
      return(FALSE)
    }
  )
)
