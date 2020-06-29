#' @title Local
#' @description Local Class
#' @export
#' @examples \dontrun{
#' host <- Host$new("gmail.com")
#' x <- Local$new("very.common", list(), host)
#' x
#' x$valid()
#' x$local
#' x$format()
#' x$munge()
#' x$error
#' x$error_message
#' x$config$config
#' }
Local <- R6::R6Class(
  "Local",
  public = list(
    original = NULL,
    config = NULL,
    local = NULL,
    host = NULL,
    error = NULL,
    error_message = NULL,
    mailbox = NULL,
    tag = NULL,
    comment = NULL,
    syntax = NULL,

    # RFC-2142: MAILBOX NAMES FOR COMMON SERVICES, ROLES AND FUNCTIONS
    BUSINESS_MAILBOXES = c("info", "marketing", "sales", "support"),
    NETWORK_MAILBOXES  = c("abuse", "noc", "security"),
    SERVICE_MAILBOXES  = c("postmaster", "hostmaster", "usenet", "news", "webmaster",
        "www", "uucp", "ftp"),
    SYSTEM_MAILBOXES   = c("help", "mailer-daemon", "root"), # Not from RFC-2142
    ROLE_MAILBOXES     = c("staff", "office", "orders", "billing", "careers", "jobs"), # Not from RFC-2142
    SPECIAL_MAILBOXES  = NULL,
    STANDARD_MAX_SIZE  = 64,

    # Conventional : word([.-+'_]word)*
    CONVENTIONAL_MAILBOX_REGEX  = '/\\A [[A-Za-z][0-9]_]+ (?: [\\.\\-\\+\'_] [[A-Za-z][0-9]_]+ )* \\z/x',
    CONVENTIONAL_MAILBOX_WITHIN = '/[[A-Za-z][0-9]_]+ (?: [\\.\\-\\+\'_] [[A-Za-z][0-9]_]+ )*/x',

    # Relaxed: same characters, relaxed order
    RELAXED_MAILBOX_WITHIN = '/[[A-Za-z][0-9]_]+ (?: [\\.\\-\\+\'_]+ [[A-Za-z][0-9]_]+ )*/x',
    RELAXED_MAILBOX_REGEX = '/\\A [[A-Za-z][0-9]_]+ (?: [\\.\\-\\+\'_]+ [[A-Za-z][0-9]_]+ )* \\z/x',

    # RFC5322 Token: token."token".token (dot-separated tokens)
    #   Quoted Token can also have: SPACE \" \\ ( ) , : ; < > @ [ \ ] .
    # FIXME: still need to translate this to R regex
    # https://stackoverflow.com/questions/201323/how-to-validate-an-email-address-using-a-regular-expression
    # https://github.com/cran/assertive.data/blob/8ae9bfa510f1a26c867f4babba83b6fa9f0689f4/R/is-data.R#L213
    STANDARD_LOCAL_WITHIN = "/
         (?: [[A-Za-z][0-9]\\!\\#\\$\\%\\&\\'\\*\\+\\-\\/\\=\\?\\^\\_\\`\\{\\|\\}\\~\\(\\)]+ | \" (?: \\[\" \\] | [\x20-\x21\x23-\x2F\x3A-\x40\x5B\x5D-\x60\x7B-\x7E[A-Za-z][0-9]] )+ \" )
(?: \\.  (?: [[A-Za-z][0-9]\\!\\#\\$\\%\\&\\'\\*\\+\\-\\/\\=\\?\\^\\_\\`\\{\\|\\}\\~\\(\\)]+ | \" (?: \\[\" \\] | [\x20-\x21\x23-\x2F\x3A-\x40\x5B\x5D-\x60\x7B-\x7E[A-Za-z][0-9]] )+ \" ) )* /x",
    # STANDARD_LOCAL_WITHIN = "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")",
    REDACTED_REGEX = '/\\A \\{ [0-9a-f]{40} \\} \\z/x', # {sha1}

    STANDARD_LOCAL_REGEX = NULL,

    # AZaz09_!'+-/=
    CONVENTIONAL_TAG_REGEX  = "^([[a-zA-Z0-9_]\\!\\'\\+\\-\\/\\=]+)$/i",
    # AZaz09_!#$%&'*+-/=?^`{|}~
    RELAXED_TAG_REGEX  = "^([[a-zA-Z0-9_]\\!\\#\\$\\%\\&\\'\\*\\+\\-\\/\\=\\?\\^\\`\\{\\|\\}\\~]+)$/i",

    initialize = function(local, config=list(), host=NULL) {
      self$config <- if (is.list(config)) Config$new(config) else config
      self$local <- local
      self$host <- host
      self$error <- self$error_message <- NULL
      self$SPECIAL_MAILBOXES <- c(self$BUSINESS_MAILBOXES, self$NETWORK_MAILBOXES,
        self$SERVICE_MAILBOXES, self$SYSTEM_MAILBOXES, self$ROLE_MAILBOXES)
      # FIXME: STANDARD_LOCAL_WITHIN not available yet
      self$STANDARD_LOCAL_REGEX <- self$STANDARD_LOCAL_WITHIN
        # sprintf("\\A %s \\z/x", self$STANDARD_LOCAL_WITHIN)
      self$set_local(local)
      return(self)
    },

    set_local = function(raw) {
      self$original <- raw
      if (self$config$config[["local_downcase"]]) {
        raw <- tolower(raw)
      }
      self$local <- raw

      if (inherits(self$config$config[["local_parse"]], "function")) {
        tmp <- eval(self$config$config[["local_parse"]])(raw)
        self$mailbox <- tmp[1]
        self$tag <- tmp[2]
        self$comment <- tmp[3]
      } else {
        tmp <- self$parse(raw)
        self$mailbox <- tmp[1]
        self$tag <- tmp[2]
        self$comment <- tmp[3]
      }

      self$format()
    },

    parse = function(raw) {
      # raw = "\"much.more unusual\""
      if (grepl('\\A\"(.*)\"\\z', raw, perl = TRUE)) { # Quoted
        z <- mtch(raw, '\\A\"(.*)\"\\z', perl = TRUE)[[1]]
        raw <- z[2]
        raw <- gsub("(.)", '\1', raw) # Unescape
      } else if (self$config$config[["local_fix"]] && self$config$config[["local_format"]] != "standard") {
        raw <- gsub(' ', '', raw)
        raw <- gsub(',', '.', raw)
      }
      z <- self$parse_comment(raw)
      raw <- z[1]
      comment <- z[2]
      w <- self$parse_tag(raw)
      mailbox <- w[1]
      tag <- w[2]
      if (is.null(mailbox)) mailbox <- ""
      c(mailbox, tag, comment)
    },

    # # "(comment)mailbox" or "mailbox(comment)", only one comment
    # # RFC Doesn't say what to do if 2 comments occur, so last wins
    parse_comment = function(raw) {
      cc <- NA_character_
      if (grepl('\\A\\((.+?)\\)(.+)\\z', raw, perl = TRUE)) {
        z <- mtch(raw, '\\A\\((.+?)\\)(.+)\\z', perl = TRUE)[[1]]
        raw <- z[3]
        cc <- z[2]
      }
      if (grepl('\\A(.+)\\((.+?)\\)\\z', raw, perl = TRUE)) {
        z <- mtch(raw, '\\A(.+)\\((.+?)\\)\\z', perl = TRUE)[[1]]
        raw <- z[2]
        cc <- z[3]
      }
      c(raw, cc)
    },

    parse_tag = function(raw) {
      separator <- self$config$config[["tag_separator"]] %||% '\\+'
      if (!grepl("[\\]", separator)) separator <- paste0("\\", separator)
      strsplit(raw, separator)[[1]]
    },

    # True if the the value contains only Latin characters (7-bit ASCII)
    is_ascii = function() !self$is_unicode(),

    # True if the the value contains non-Latin Unicde characters
    is_unicode = function() {
      grepl("[^\x20-\x7F]", self$local, perl = TRUE)
    },

    # Returns true if the value matches the Redacted format
    is_redacted = function() grepl(REDACTED_REGEX, self$local),

    # # Returns true if the value matches the Redacted format
    # def self.redacted?(local)
    #   local =~ REDACTED_REGEX ? true : false
    # end

    # # Is the address for a common system or business role account?
    # def special?
    #   SPECIAL_MAILBOXES.include?(mailbox)
    # end

    # Builds the local string according to configurations
    format = function(form = self$config$config[["local_format"]] %||% "conventional") {
      if (inherits(self$config$config[["local_format"]], "function")) {
        eval(self$config$config[["local_format"]])(self)
      } else if (form == "conventional") {
        self$conventional()
      } else if (form == "canonical") {
        self$canonical()
      } else if (form == "relaxed") {
        self$relax()
      } else if (form == "standard") {
        self$standard()
      }
    },
    to_s = function() self$format(),

    # Returns a conventional form of the address
    conventional = function() {
      if (!is.null(self$tag)) {
        paste(self$mailbox, self$tag, sep = self$config$config[["tag_separator"]])
      } else {
        self$mailbox
      }
    },

    # Returns a canonical form of the address
    canonical = function() {
      if (!is.null(self$config$config[["mailbox_canonical"]])) {
        eval(self$config$config[["mailbox_canonical"]])(self$mailbox)
      } else {
        tolower(self$mailbox)
      }
    },

    # Relaxed format: mailbox and tag, no comment, no extended character set
    relax = function() {
      form <- self$mailbox
      if (!is.null(self$tag)) form <- paste0(form, self$config$config[["tag_separator"]], self$tag)
      form <- gsub('[ \"\\(\\),:<>@\\[\\]\\]', '', form)
      return(form)
    },

    # Returns a normalized version of the standard address parts.
    standard = function() {
      form <- self$mailbox
      if (!is.null(self$tag)) form <- paste0(form, self$config$config[["tag_separator"]], self$tag)
      if (!is.null(self$comment)) form <- paste0(form, "(", self$comment, ")")
      form <- gsub('([\\\"])', '\\\1', form) # Escape \ and "
      if (grepl('[ \"\\(\\),:<>@\\[\\\\]]', form, perl = TRUE)) { # Space and "(),:;<>@[\]
        form <- sprintf("%s", form)
      }
      return(form)
    },

    # # Sets the part to be the conventional form
    # def conventional!
    #   self.local = self.conventional
    # end

    # # Sets the part to be the canonical form
    # def canonical!
    #   self.local = self.canonical
    # end

    # # Dropps unusual  parts of Standard form to form a relaxed version.
    # def relax!
    #   self.local = self.relax
    # end

    # Returns the munged form of the address, like "ma*****"
    munge = function() {
      # self.to_s.sub(/\A(.{1,2}).*/) { |m| $1 + self$config$config[["munge_string"]] }
      sub('\\A(.{1,2}).*', paste0("\\1", self$config$config[["munge_string"]]), self$format(), perl = TRUE)
    },

    # # Mailbox with trailing numbers removed
    # def root_name
    #   self.mailbox =~ /\A(.+?)\d+\z/ ? $1 : self.mailbox
    # end

    # ############################################################################
    # # Validations
    # ############################################################################

    # True if the part is valid according to the configurations
    valid = function(format = self$config$config[["local_format"]] %||% "conventional") {
      if (inherits(self$config$config[["mailbox_validator"]], "function")) {
        eval(self$config$config[["mailbox_validator"]])(self$mailbox, self$tag)
      } else if (inherits(format, "function")) {
        eval(format)(self)
      } else if (format == "conventional") {
        self$is_conventional()
      } else if (format == "relaxed") {
        self$is_relaxed()
      } else if (format == "redacted") {
        self$is_redacted()
      } else if (format == "standard") {
        self$is_standard()
      } else if (format == "none") {
        return(TRUE)
      } else {
        stop("Unknown format: ", format, call. = FALSE)
      }
    },

    # # Returns the format of the address
    # def format?
    #   # if :custom
    #   if self.conventional?
    #     :conventional
    #   elsif self.relaxed?
    #     :relax
    #   elsif self.redacted?
    #     :redacted
    #   elsif self.standard?
    #     :standard
    #   else
    #     :invalid
    #   end
    # end

    valid_size = function() {
      if (nchar(self$local) > self$STANDARD_MAX_SIZE) return(private$set_error("local_size_long"))
      if (!is.null(self$host) && self$host$hosted_service()) {
        if (
          self$config$config[["local_private_size"]] &&
          !self$valid_size_checks(self$config$config[["local_private_size"]])
        ) return(FALSE)
      } else {
        if (
          self$config$config[["local_size"]] &&
          !self$valid_size_checks(self$config$config[["local_size"]])
        ) return(FALSE)
      }
      if (
        self$config$config[["mailbox_size"]] &&
        !self$valid_size_checks(self$config$config[["mailbox_size"]])
      ) return(FALSE)
      return(TRUE)
    },

    valid_size_checks = function(range) {
      if (nchar(self$mailbox) < range[1]) return(private$set_error("local_size_short"))
      if (nchar(self$mailbox) > last(range)) return(private$set_error("local_size_long"))
      return(TRUE)
    },

    valid_encoding = function(enc = self$config$config[["local_encoding"]] %||% "ascii") {
      if (enc == "ascii" && self$is_unicode()) return(FALSE)
      return(TRUE)
    },

    # True if the part matches the conventional format
    is_conventional = function() {
      self$syntax <- "invalid"
      if (!is.null(self$tag)) {
        unless(
          grepl(self$CONVENTIONAL_MAILBOX_REGEX, self$mailbox, perl = TRUE) &&
          grepl(self$CONVENTIONAL_TAG_REGEX, self$tag, perl = TRUE), return(FALSE))
      } else {
        unless(grepl(self$CONVENTIONAL_MAILBOX_REGEX, self$local, perl = TRUE),
          return(FALSE))
      }
      if (!self$valid_size()) return(FALSE)
      if (!self$valid_encoding()) return(FALSE)
      self$syntax <- "conventional"
      return(TRUE)
    },

    # Relaxed conventional is not so strict about character order.
    is_relaxed = function() {
      self$syntax <- "invalid"
      if (!self$valid_size()) return(FALSE)
      if (!self$valid_encoding()) return(FALSE)
      if (!is.null(self$tag)) {
        unless(grepl(self$RELAXED_MAILBOX_REGEX, self$mailbox, perl = TRUE) &&
          grepl(self$RELAXED_TAG_REGEX, self$tag), return(FALSE))
      } else if (grepl(self$RELAXED_MAILBOX_REGEX, self$local, perl = TRUE)) {
        self$syntax <- "relaxed"
        return(TRUE)
      } else {
        return(FALSE)
      }
    },

    # True if the part matches the RFC standard format
    is_standard = function() {
      self$syntax <- "invalid"
      if (!self$valid_size()) return(FALSE)
      if (!self$valid_encoding()) return(FALSE)
      # FIXME: STANDARD_LOCAL_REGEX not available yet, see above
      # if (FALSE) {
      if (grepl(self$STANDARD_LOCAL_REGEX, self$local, perl = TRUE)) {
        self$syntax <- "standard"
        return(TRUE)
      } else {
        return(FALSE)
      }
    }

    # # Matches configured formated form against File glob strings given.
    # # Rules must end in @ to distinguish themselves from other email part matches.
    # def matches?(*rules)
    #   rules.flatten.each do |r|
    #     if r =~ /(.+)@\z/
    #       return r if File.fnmatch?($1, self.local)
    #     end
    #   end
    #   false
    # end
  ),

  private = list(
    set_error = function(err, reason = NULL) {
      self$error <- err
      self$reason <- reason
      self$error_message <- Config$new()$error_message(err)
      return(FALSE)
    }

    # def error_message
    #   @error_message
    # end

    # def error
    #   self.valid? ? nil : ( @error || :local_invalid)
    # end
  )
)
