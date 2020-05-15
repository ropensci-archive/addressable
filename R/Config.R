`%||%` <- function(x, y) if (is.null(x) || is.na(x) || length(x) == 0) y else x

#' @title Config
#' @description Config Class
#' @export
#' @examples \dontrun{
#' x <- Config$new()
#' x
#' x$config
#' x$config$dns_timeout
#' 
#' z <- Config$new(list(dns_timeout = 4))
#' z
#' z$config
#' z$config$dns_timeout
#' }
 Config <- R6::R6Class(
  "Config",
  public = list(
    config = list(
      dns_lookup = "mx", # "mx", :a, :off
      dns_timeout = NULL,
      sha1_secret = "",
      munge_string = "*****",

      local_downcase = TRUE,
      local_fix = FALSE,
      local_encoding = "ascii", # :ascii, :unicode,
      local_parse = NULL, # NULL, Proc
      local_format = "conventional", # :conventional, :relaxed, :redacted, :standard, Proc
      local_size = 1:64,
      tag_separator = "+", # NULL, character
      mailbox_size = 1:64, # without tag
      mailbox_canonical = NULL, # NULL,  Proc
      mailbox_validator = NULL, # NULL,  Proc

      host_encoding = "punycode" %||% "unicode",
      host_validation = "mx" %||% "a" %||% "connect" %||% "syntax",
      host_size = 1:253,
      host_allow_ip = FALSE,
      host_remove_spaces = FALSE,
      host_local = FALSE,

      address_validation = "parts", # :parts, :smtp, Proc
      address_size = 3:254,
      address_fqdn_domain = NULL # Fully Qualified Domain Name = [host].[domain.tld]
    ),
    
    all_settings = function(x) {
      config <- self$config
      config <- modifyList(config, x)
      return(config)
    },

    initialize = function(config = list()) {
      self$config <- self$all_settings(config)
      return(self)
    },

    # 2018-04: AOL and Yahoo now under "oath.com", owned by Verizon. Keeping separate for now
    providers = list(
      aol = list(
        host_match = c("aol.", "compuserve.", "netscape.", "aim.", "cs.")
      ),
      google = list(
        host_match = c("gmail.com", "googlemail.com"),
        exchanger_match = c("google.com", "googlemail.com"),
        local_size = 5:64,
        local_private_size = 1:64 # When hostname not in host_match (private label)
        # mailbox_canonical = ->(m) { m.delete(".") }
      ),
      msn = list(
        host_match = c("msn.", "hotmail.", "outlook.", "live.")
        # mailbox_validator = ->(m, t) { m =~ /\A\w[\-\w]*(?:\.[\-\w]+)*\z/i }
      ),
      yahoo = list(
        host_match = c("yahoo.", "ymail.", "rocketmail."),
        exchanger_match = c("yahoodns", "yahoo-inc")
      )
    ),

    # Loads messages
    # locale: en
    errors = list(en = list(
      email_address = list(
        address_unknown = "Unknown Email Address",
        domain_does_not_accept_email = "This domain is not configured to accept email",
        domain_invalid = "Invalid Domain Name",
        domain_no_localhost = "localhost is not allowed for your domain name",
        domain_unknown = "Domain name not registered",
        exceeds_size = "Address too long",
        incomplete_domain = "Domain name is incomplete",
        invalid_address = "Invalid Email Address",
        invalid_host = "Invalid Host/Domain Name",
        invalid_mailbox = "Invalid Mailbox",
        ip_address_forbidden = "IP Addresses are not allowed",
        ip_address_no_localhost = "Localhost IP addresses are not allowed",
        ipv4_address_invalid = "This is not a valid IPv4 address",
        ipv6_address_invalid = "This is not a valid IPv6 address",
        local_size_long = "Mailbox name too long",
        local_size_short = "Mailbox name too short",
        local_invalid = "Recipient is not valid",
        not_allowed = "Address is not allowed",
        server_not_available = "The remote email server is not available"
      )
    )),

    # Set multiple default configuration settings
    # def self.configure(config = {})
    #   @config.merge!(config)
    # end

    # def self.setting(name, *value)
    #   name = name.to_sym
    #   @config[name] = value.first if value.size > 0
    #   @config[name]
    # end

    # # Returns the hash of Provider rules
    # class << self
    #   attr_reader :providers
    # end

    # Configure or lookup a provider by name.
    provider = function(name, config = list()) {
      if (length(config) > 0) {
        self$providers[[name]] <- config
      }
      self$providers[[name]]
    },

    error_message = function(name, locale = "en") {
      if (is.null(name)) 
        stop("no `name` given in Config error_message fxn")
      self$errors[[locale]][["email_address"]][[name]] %||% name
    },

    # Customize your own error message text.
    error_messages = function(x = list(), locale = "en"){
      # x <- extra.first if extra.first.is_a? Hash
      unless(length(x) == 0, {
        self$errors[[locale]][["email_address"]] <- 
          modifyList(self$errors[[locale]][["email_address"]], x)
      })
      self$errors[[locale]][["email_address"]]
    },

    # def []=(setting, value)
    #   @config[setting.to_sym] = value
    # end

    # def [](setting)
    #   @config[setting.to_sym]
    # end

    configure = function(settings) {
      self$config <- modifyList(self$config, settings)
    }

    # def to_h
    #   @config
    # end
  )
)
