#' @title Address
#' @description Address Class
#' @export
#' @examples \dontrun{
#' x <- Address$new("User+tag@example.com")
#' x
#' x$local
#' x$local$valid()
#' x$host
#' x$host$valid()
#' x$valid()
#' x$fail()
#' x$munge()
#' x$normal()
#' 
#' x <- Address$new("User+tag@example.com",
#'   config = list(munge_string = "*^*^*^*"))
#' x$munge()
#' }
Address <- R6::R6Class(
  "Address",
  public = list(
    #' @field method (character) one or more URLs
    config = list(),
    #' @field url (character) one or more URLs
    original = NULL,
    host = NULL,
    local = NULL,
    error = NULL,
    reason = NULL,
    error_message = NULL,

    #' @description Create a new Address object
    #' @param email_address (character) an email address
    #' @param config (list) list of config options
    initialize = function(email_address, config = list()) {
      self$config <- config
      self$original <- email_address
      email_address <- trim(email_address %||% "")
      # email_address <- parse_rewritten(email_address) unless config[:skip_rewrite]
      z <- self$split_local_host(email_address)
      local <- z[1]
      host <- z[2]
      self$host <- Host$new(host, self$config)
      self$local <- Local$new(local, self$config, self$host)
      self$error <- self$error_message <- NULL
    },

    #' @description split local host
    #' @param email (character) email address
    #' @return character string
    split_local_host = function(email) {
      lh <- mtch(email, "(.+)@(.+)")
      if (length(lh[[1]]) > 0) {
        lh[[1]][2:3]
      } else {
        c(email, "")
      }
    },

    ############################################################################
    # Address methods
    ############################################################################

    # Returns the string representation of the normalized email address.
    normal = function() {
      if (!is.null(self$original)) {
        self$original
      } else if (nchar(self$local) == 0) {
        ""
      } else if (nchar(self$host) == 0) {
        self$local
      } else {
        sprintf("%s@%s", self$local, self$host)
      }
    },
    to_s = function() self$normal(),

    # Returns the munged form of the address, by default "mailbox@domain.tld"
    # returns "ma*****@do*****".
    munge = function() {
      paste(self$local$munge(), self$host$munge(), sep = "@")
    },

    canonical = function() {
      cc <- self$local$canonical()
      if (self$host$canonical() > " ") cc <- paste0(cc, "@", self$host$canonical())
      return(cc)
    },

    base = function() {
      paste0(self$mailbox, "@", self$host$host_name)
    },

    reference = function() digest::digest(self$base(), serialize = FALSE),

    #---------------------------------------------------------------------------
    # Validation
    #---------------------------------------------------------------------------
    # Returns true if this address is considered valid according to the format
    # configured for its provider, It test the normalized form.
    valid = function(options = list()) {
      self$error <- NULL
      if (!self$local$valid()) return(private$set_error(self$local$error))
      if (!self$host$valid()) return(private$set_error(self$host$error_message))

      if (
        !is.null(self$config$config[["address_size"]]) &&
        !nchar(self$normal()) %in% self$config$config[["address_size"]]
      ) {
        return(private$set_error("exceeds_size"))
      }
      
      if (inherits(self$config$config[["address_validation"]], "function")) {
        unless(eval(self$config$config[["address_validation"]])(self$normal()), {
          return(private$set_error("not_allowed"))
        })
      } else {
        if (!self$local$valid()) return(FALSE)
        if (!self$host$valid()) return(FALSE)
      }
      return(TRUE)
    },

    fail = function() {
      if (self$valid()) NULL else self$error_message
    }
  ),

  active = list(
    ## local accessor methods
    # Everything to the left of the @ in the address, called the local part.
    left = function() self$local$to_s(),

    # Returns the mailbox portion of the local port, with no tags. Usually, this
    # can be considered the user account or role account names. Some systems
    # employ dynamic email addresses which don't have the same meaning.
    mailbox = function() self$local$mailbox,

    # Returns the tag part of the local address, or nil if not given.
    tag = function() self$local$tag,

    # Retuns any comments parsed from the local part of the email address.
    # This is retained for inspection after construction, even if it is
    # removed from the normalized email address.
    comment = function() self$local$comment
  ),

  private = list(
    set_error = function(err, reason = NULL) {
      self$error <- err
      self$reason <- reason
      self$error_message <- Config$new()$error_message(err)
      return(FALSE)
    }
  )
)
