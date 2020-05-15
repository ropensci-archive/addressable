grepv <- function(str, pattern) grep(pattern, str, perl = TRUE, value = TRUE)

#' @title Host
#' @description Host Class
#' @export
#' @examples \dontrun{
#' x <- Host$new("example.com")
#' x
#' x$host_name
#' x$domain_name
#' x$registration_name
#' x$tld
#' x$valid()
#' # x$munge()
#' x$error
#' x$error_message
#' x$ip_address
#' x$config$config
#' 
#' x <- Host$new("gmail.com", config = list(dns_lookup = "off"))
#' x$config$config$dns_lookup
#' x$valid()
#' 
#' x <- Host$new("gmail.com")
#' x
#' x$valid()
#' x$fail()
#' 
#' x <- Host$new("gm ail.com")
#' x
#' x$valid()
#' x$fail()
#' 
#' x <- Host$new("user1")
#' x$domain_name
#' x$dns_name
#' x$registration_name
#' }
Host <- R6::R6Class(
  "Host",
  public = list(
    MAX_HOST_LENGTH = 255,
    original = NULL,
    config = NULL,
    error = NULL,
    reason = NULL,
    error_message = NULL,
    comment = NULL,
    host_name = NULL,
    registration_name = NULL,
    domain_name = NULL,
    dns_name = NULL,
    ip_address = NULL,
    subdomains = NULL,
    tld = NULL,
    tld2 = NULL,
    provider = NULL,
    dns_a_record_ = NULL,
    exchangers_ = NULL,

    # Sometimes, you just need a Regexp...
    DNS_HOST_REGEX = " [[A-Za-z][0-9]]+ (?: (?: \\-{1,2} | \\.) [[A-Za-z][0-9]]+ )*/x",

    # Matches conventional host name and punycode: domain.tld, x--punycode.tld
    # CANONICAL_HOST_REGEX = sprintf("\\A %s \\z/x",
      # "[[A-Za-z][0-9]]+ (?: (?: \\-{1,2} | \\.) [[A-Za-z][0-9]]+ )*/x"),
    # 2 below from https://stackoverflow.com/questions/10306690/what-is-a-regular-expression-which-will-match-a-valid-domain-name-without-a-subd
    CANONICAL_HOST_REGEX = '^(((?!-))(xn--|_{1,1})?[a-z0-9-]{0,61}[a-z0-9]{1,1}\\.)*(xn--)?([a-z0-9][a-z0-9\\-]{0,60}|[a-z0-9-]{1,30}\\.[a-z]{2,})$',
    # CANONICAL_HOST_REGEX = "^(([a-zA-Z]{1})|([a-zA-Z]{1}[a-zA-Z]{1})|([a-zA-Z]{1}[0-9]{1})|([0-9]{1}[a-zA-Z]{1})|([a-zA-Z0-9][a-zA-Z0-9-_]{1,61}[a-zA-Z0-9]))\\.([a-zA-Z]{2,6}|[a-zA-Z0-9-]{2,30}\\.[a-zA-Z]{2,3})$",

    # The IPv4 and IPv6 were lifted from Resolv::IPv?::Regex and tweaked to not
    # \A...\z anchor at the edges.
    # IPv6_HOST_REGEX = /\[IPv6:
    #   (?: (?:(?x-mi:
    #   (?:[0-9A-Fa-f]{1,4}:){7}
    #      [0-9A-Fa-f]{1,4}
    #   )) |
    #   (?:(?x-mi:
    #   (?: (?:[0-9A-Fa-f]{1,4}(?::[0-9A-Fa-f]{1,4})*)?) ::
    #   (?: (?:[0-9A-Fa-f]{1,4}(?::[0-9A-Fa-f]{1,4})*)?)
    #   )) |
    #   (?:(?x-mi:
    #   (?: (?:[0-9A-Fa-f]{1,4}:){6,6})
    #   (?: \d+)\.(?: \d+)\.(?: \d+)\.(?: \d+)
    #   )) |
    #   (?:(?x-mi:
    #   (?: (?:[0-9A-Fa-f]{1,4}(?::[0-9A-Fa-f]{1,4})*)?) ::
    #   (?: (?:[0-9A-Fa-f]{1,4}:)*)
    #   (?: \d+)\.(?: \d+)\.(?: \d+)\.(?: \d+)
    #   )))\]/ix

    # IPv4_HOST_REGEX = /\[((?x-mi:0
    #            |1(?:[0-9][0-9]?)?
    #            |2(?:[0-4][0-9]?|5[0-5]?|[6-9])?
    #            |[3-9][0-9]?))\.((?x-mi:0
    #            |1(?:[0-9][0-9]?)?
    #            |2(?:[0-4][0-9]?|5[0-5]?|[6-9])?
    #            |[3-9][0-9]?))\.((?x-mi:0
    #            |1(?:[0-9][0-9]?)?
    #            |2(?:[0-4][0-9]?|5[0-5]?|[6-9])?
    #            |[3-9][0-9]?))\.((?x-mi:0
    #            |1(?:[0-9][0-9]?)?
    #            |2(?:[0-4][0-9]?|5[0-5]?|[6-9])?
    #            |[3-9][0-9]?))\]/x


    # Matches Host forms: DNS name, IPv4, or IPv6 formats
    # STANDARD_HOST_REGEX = /\A (?: #{DNS_HOST_REGEX}
    #                           | #{IPv4_HOST_REGEX} | #{IPv6_HOST_REGEX}) \z/ix

    # host name -
    #   * host type - :email for an email host, :mx for exchanger host
    initialize = function(host_name, config = list()) {
      host_name <- host_name %||% ""
      self$original <- host_name
      # config$host_type <- config$host_type %||% "email" # AFAICT this isn't used
      self$config <- if (is.list(config)) Config$new(config) else config
      self$error <- self$error_message <- NULL
      self$parse(host_name)
    },

    # Returns the String representation of the host name (or IP)
    name = function() {
      if (self$ipv4()) {
        sprintf("[%s]", self$ip_address)
      } else if (self$ipv6()) {
        sprintf("[IPv6:%s]", self$ip_address)
      } else if (self$config$config[["host_encoding"]] == "unicode") {
        # ::SimpleIDN.to_unicode(host_name)
        utf8::as_utf8(self$host_name)
      } else {
        self$dns_name
      }
    },

    # The canonical host name is the simplified, DNS host name
    canonical = function() self$dns_name,

    # Returns the munged version of the name, replacing everything after the
    # initial two characters with "*****" or the configured "munge_string".
    munge = function() {
      sub('\\A(.{1,2}).*', paste0("\\1", self$config$config[["munge_string"]]), self$host_name, perl = TRUE)
      # sub('\\A(.{1,2}).*', "-", self$host_name, perl = TRUE)
      # { |m| $1 + self$config$config[["munge_string"]] }
    },

    ############################################################################
    # Parsing
    ############################################################################
    parse = function(host) {
      # host = "gmail.com"
      # host = "[127.0.0.1]"
      # # /\A\[(\d{1,3}(\.\d{1,3}){3})\]
      # grepl("\\A\\[([0-9]{1,3}(\\.[0-9]{1,3}){3})\\]", host, perl = TRUE)
      # host = "[IPv6:::1]"
      # host = "[ipv6:::1]"
      # grepl("\\A\\[IPv6:(.+)\\]", host, perl = TRUE, ignore.case = TRUE)
      pat_ipv6 <- "\\A\\[IPv6:(.+)\\]"
      pat_ipv4 <- "\\A\\[([0-9]{1,3}(\\.[0-9]{1,3}){3})\\]"
      host <- self$parse_comment(host)
      if (grepl(pat_ipv6, host, perl = TRUE, ignore.case = TRUE)) {# IPv6
        self$ip_address <- mtch(host, pat_ipv6, perl = TRUE, ignore.case = TRUE)[[1]][2]
      } else if (grepl(pat_ipv4, host, perl = TRUE)) {# IPv4
        self$ip_address <- mtch(host, pat_ipv4, perl = TRUE)[[1]][2]
      } else {
        self$make_host_name(host)
      }
    },

    parse_comment = function(host) {
      if (grepl("\\A\\((.+?)\\)(.+)", host, perl = TRUE)) {# (comment)domain.tld
        z <- mtch(host, "\\A\\((.+?)\\)(.+)", perl = TRUE)[[1]]
        self$comment <- z[2]
        host <- z[3]
      }
      if (grepl("\\A(.+)\\((.+?)\\)\\z", host, perl = TRUE)) {# domain.tld(comment)
        z <- mtch(host, "\\A(.+)\\((.+?)\\)\\z", perl = TRUE)[[1]]
        self$comment <- z[3]
        host <- z[2]
      }
      return(host)
    },

    # replaces ruby's host_name=(name) method
    make_host_name = function(name) {
      name <- self$fully_qualified_domain_name(tolower(name))
      host_name <- name
      if (!is.null(self$config$config[["host_remove_spaces"]])) {
        host_name <- trim(host_name)
      }
      self$host_name <- host_name
      self$dns_name <- if (grepl('[^[:ascii:]]', host_name, perl = TRUE)) {
        urltools::puny_encode(host_name)
      } else {
        host_name
      }

      pat1 <- '\\A(.+)\\.([0-9a-zA-Z_]{3,10})\\z'
      pat2 <- '\\A(.+)\\.([0-9a-zA-Z_]{1,3}\\.[0-9a-zA-Z_][0-9a-zA-Z_])\\z'
      pat3 <- '\\A(.+)\\.([0-9a-zA-Z_][0-9a-zA-Z_])\\z'

      # Subdomain only (root@localhost)
      if (!grepl("\\.", name)) {
        self$subdomains <- name
      } else if (
        # Split sub.domain from .tld: *.com, *.xx.cc, *.cc
        grepl(pat1, name, perl = TRUE) || 
        grepl(pat2, name, perl = TRUE) ||
        grepl(pat3, name, perl = TRUE)
      ) {
        z <- mtch(name, paste(pat1, pat2, pat3, sep = "|"), perl = TRUE)[[1]]
        z <- z[nzchar(z)]
        sub_and_domain <- z[2]
        self$tld2 <- z[3]
        self$tld <- sub('\\A.+\\.', "", self$tld2) # co.uk => uk
        if (grepl('\\A(.+)\\.(.+)\\z', sub_and_domain, perl = TRUE)) { # is subdomain? sub.example [.tld2]
          z <- mtch(sub_and_domain, '\\A(.+)\\.(.+)\\z', perl = TRUE)[[1]]
          self$subdomains <- z[2]
          self$registration_name <- z[3]
        } else {
          self$registration_name <- sub_and_domain
        }
        self$domain_name <- paste0(self$registration_name, ".", self$tld2)
        self$find_provider()
      } else { # Bad format
        self$subdomains <- self$tld <- self$tld2 = ""
        self$domain_name <- self$registration_name <- name
      }
    },

    fully_qualified_domain_name = function(host_part) {
      dn <- self$config$config[["address_fqdn_domain"]]
      if (is.null(dn)) {
        if ((is.null(host_part) || host_part <= " ") && self$config$config[["host_local"]]) {
          "localhost"
        } else {
          host_part
        }
      } else if (is.null(host_part) || host_part <= " ") {
        dn
      } else if (!grepl("\\.", host_part, perl = TRUE)) {
        paste0(host_part, ".", dn)
      } else {
        host_part
      }
    },

    # True if host is hosted at the provider, not a public provider host name
    hosted_service = function() {
      if (!is.null(self$registration_name)) return(FALSE)
      self$find_provider()
      if (is.null(self$config$config[["host_match"]])) return(FALSE)
      !self$matches(self$config$config[["host_match"]])
    },

    find_provider = function() {
      if (!is.null(self$provider)) return(self$provider)

      Map(function(provider, config) {
        if (!is.null(config[["host_match"]]) && asb(self$matches(config[["host_match"]]))) {
          return(self$set_provider(provider, config))
        }
      }, names(Config$new()$providers), Config$new()$providers)

      if (!self$dns_enabled()) return(self$set_provider("default"))

      # FIXME: exchangers not available yet
      if (is.null(self$exchangers_)) self$exchangers()
      provider <- self$exchangers_$provider()
      if (provider != "default") {
        self$set_provider(provider,
          Config$new()$provider(provider))
      }

      if (!is.null(self$provider)) self$provider <- self$set_provider("default")
    },

    set_provider = function(name, provider_config = list()) {
      self$config$configure(provider_config)
      self$provider <- name
    },

    # # Returns a hash of the parts of the host name after parsing.
    # def parts
    #   {host_name: host_name, dns_name: dns_name, subdomain: subdomains,
    #    registration_name: registration_name, domain_name: domain_name,
    #    tld2: tld2, tld: tld, ip_address: ip_address,}
    # end

    ############################################################################
    # Access and Queries
    ############################################################################

    # Is this a fully-qualified domain name?
    # def fqdn?
    #   tld ? true : false
    # end
    ip = function() !is.null(self$ip_address),
    ipv4 = function() self$ip() && grepl("\\.", self$ip_address),
    ipv6 = function() self$ip() && grepl(":", self$ip_address),

    ############################################################################
    # Matching
    ############################################################################

    # Takes a email address string, returns A STRING if it matches a rule
    # FALSE if no string match, so TRUE == A STRING for purposes of this function
    # Rules of the follow formats are evaluated:
    # * "example."  => registration name
    # * ".com"      => top-level domain name
    # * "google"    => email service provider designation
    # * "@goog*.com" => Glob match
    # * IPv4 or IPv6 or CIDR Address
    matches = function(rules) {
      # rules = Array(rules)
      if (length(rules) == 0) return(FALSE)
      for (i in seq_along(rules)) {
        rule <- rules[i]
        if (!is.null(self$domain_name)) {
          if (rule == self$domain_name) return(rule)
        }
        if (!is.null(self$dns_name)) {
          if (rule == self$dns_name) return(rule)
        }
        # if (rule == self$domain_name || rule == self$dns_name) return(rule)
        if (self$registration_name_matches(rule)) return(rule)
        if (self$tld_matches(rule)) return(rule)
        if (self$domain_matches(rule)) return(rule)
        if (!is.null(self$provider) && self$provider_matches(rule)) return(rule)
        if (self$ip_matches(rule)) return(rule)
      }
      return(FALSE)
    },

    # Does "example." match any tld?
    registration_name_matches = function(rule) {
      if (is.null(self$registration_name)) return(FALSE)
      rule == self$registration_name
    },

    # Does "sub.example.com" match ".com" and ".example.com" top level names?
    # Matches TLD (uk) or TLD2 (co.uk)
    # rule = ".com"
    tld_matches = function(rule) {
      if (is.null(self$tld) || is.null(self$tld2)) return(FALSE)
      pat <- '\\A\\.(.+)\\z'
      x <- grep(pat, rule, perl = TRUE, value = TRUE)
      grepl(pat, rule, perl = TRUE) && (x == self$tld || x == self$tld2)
    },

    provider_matches = function(rule) {
      grepl('\\A[[0-9a-zA-Z_]\\-]*\\z', rule, perl = TRUE) && self$provider == rule
    },

    # Does domain == rule or glob matches? (also tests the DNS (punycode) name)
    # Requires optionally starts with a "@".
    domain_matches = function(rule) {
      if (is.null(self$domain_name) || is.null(self$dns_name)) return(FALSE)
      if (grepl('\\A@(.+)', rule, perl = TRUE)) rule <- grepv(rule, '\\A@(.+)')
      if (!is.null(self$domain_name) && grepl(glob2rx(rule), self$domain_name)) return(rule)
      if (!is.null(self$dns_name) && grepl(glob2rx(rule), self$dns_name)) return(rule)
      return(FALSE)
    },

    # True if the host is an IP Address form, and that address matches
    # the passed CIDR string ("10.9.8.0/24" or "2001:..../64")
    ip_matches = function(cidr) {
      if (is.null(self$ip_address)) return(FALSE)
      if (!grepl("/", cidr) && cidr == self$ip_address) return(cidr)
      # FIXME: not sure if ip_in_range can handle ipv6 too, asked bob
      if (grepl(":", cidr) && grepl(":", self$ip_address)) {
        if (iptools::ip_in_range(self$ip_address, cidr)) return(cidr)
        # if (NetAddr::IPv6Net.parse(cidr).contains(NetAddr::IPv6.parse(self$ip_address))) return(cidr)
      } else if (grepl("\\.", cidr) && grepl("\\.", self$ip_address)) {
        if (iptools::ip_in_range(self$ip_address, cidr)) return(cidr)
        # if (NetAddr::IPv4Net.parse(cidr).contains(NetAddr::IPv4.parse(self$ip_address))) return(cidr)
      }
      return(FALSE)
    },

    ############################################################################
    # DNS
    ############################################################################

    # TRUE if the "dns_lookup" setting is enabled
    dns_enabled = function() {
      if (self$config$config[["dns_lookup"]] == "off") return(FALSE)
      if (self$config$config[["host_validation"]] == "syntax") return(FALSE)
      return(TRUE)
    },

    # # Returns: [official_hostname, alias_hostnames, address_family, *address_list]
    dns_a_record = function() {
      if (self$config$config[["dns_lookup"]] == "off") {
        self$dns_a_record_ < "0.0.0.0"
      } else {
        x <- tryCatch(pingr::nsl('example'), error = function(e) e)
        self$dns_a_record_ <- if (inherits(x, "error")) "" else x$answer$data[[1]]
      }
    #   @_dns_a_record = "0.0.0.0" if @config[:dns_lookup] == :off
    #   @_dns_a_record ||= Socket.gethostbyname(dns_name)
    # rescue SocketError # not found, but could also mean network not work
    #   @_dns_a_record ||= []
    },

    # Returns an array of EmailAddress::Exchanger hosts configured in DNS.
    # The array will be empty if none are configured.
    exchangers = function() {
      # EmailAddress::Exchanger.cached(dns_name, @config)
      # FIXME: haven't dealt with the Exchanger class yet
      # NULL
      self$exchangers_ <- Exchanger$new(self$dns_name, self$config$config)$cached(self$dns_name)
    },

    # # Returns a DNS TXT Record
    # def txt(alternate_host = nil)
    #   return nil unless dns_enabled?
    #   Resolv::DNS.open do |dns|
    #     dns.timeouts = @config[:dns_timeout] if @config[:dns_timeout]
    #     records = begin
    #       dns.getresources(alternate_host || dns_name,
    #         Resolv::DNS::Resource::IN::TXT)
    #               rescue Resolv::ResolvTimeout
    #                 []
    #     end

    #     records.empty? ? nil : records.map(&:data).join(" ")
    #   end
    # end

    # Parses TXT record pairs into a hash
    # def txt_hash(alternate_host = nil)
    #   fields = {}
    #   record = txt(alternate_host)
    #   return fields unless record

    #   record.split(/\s*;\s*/).each do |pair|
    #     (n, v) = pair.split(/\s*=\s*/)
    #     fields[n.to_sym] = v
    #   end
    #   fields
    # end

    # # Returns a hash of the domain's DMARC (https://en.wikipedia.org/wiki/DMARC)
    # # settings.
    # def dmarc
    #   dns_name ? txt_hash("_dmarc." + dns_name) : {}
    # end

    ############################################################################
    # Validation
    ############################################################################

    # Returns TRUE if the host name is valid according to the current configuration
    valid = function(rules = list()) {
      host_validation <- rules[["host_validation"]] %||% self$config$config[["host_validation"]] %||% "mx"
      dns_lookup <- rules[["dns_lookup"]] %||% host_validation
      self$error_message <- NULL
      if (!is.null(self$ip_address)) {
        self$valid_ip()
      } else if (!self$valid_format()) {
        FALSE
      } else if (dns_lookup == "connect") {
        self$valid_mx() && self$connect() # FIXME: connect not done yet
      } else if (dns_lookup == "mx") {
        self$valid_mx()
      } else if (dns_lookup == "a") {
        self$valid_dns()
      } else {
        TRUE
      }
    },

    # The inverse of valid? -- Returns NULL if valid, otherwise error message
    fail = function() {
      if (self$valid()) NULL else self$error_message
    },

    # True if the host name has a DNS A Record
    valid_dns = function() {
      if (!self$dns_enabled()) return(TRUE)
      self$dns_a_record()
      dnsalen <- nchar(self$dns_a_record_) > 0
      bool <- if (dnsalen) dnsalen else private$set_error("domain_unknown")
      if (self$localhost() && !self$config$config[["host_local"]]) {
        bool <- private$set_error("domain_no_localhost")
      }
      return(bool)
    },

    # # True if the host name has valid MX servers configured in DNS
    valid_mx = function() {
      self$exchangers()
      if (!self$dns_enabled()) return(TRUE)
      if (is.null(self$exchangers_)) {
        private$set_error("domain_unknown")
      } else if ((length(self$exchangers_$mx_ips()) %||% 0) > 0) {
        if (self$localhost() && !self$config$config[["host_local"]]) {
          private$set_error("domain_no_localhost")
        } else {
          TRUE
        }
      } else if (is.null(self$config$config[["dns_timeout"]]) && self$valid_dns()) {
        private$set_error("domain_does_not_accept_email")
      } else {
        private$set_error("domain_unknown")
      }
    },

    # TRUE if the host_name passes Regular Expression match and size limits.
    valid_format = function() {
      if (
        grepl(self$CANONICAL_HOST_REGEX, self$host_name, perl = TRUE) &&
        nchar(self$name()) <= self$MAX_HOST_LENGTH
      ) {
        if (self$localhost()) return(TRUE)
        if (grepl("\\.", self$host_name)) return(TRUE)
      }
      private$set_error("domain_invalid")
    },

    # Returns true if the IP address given in that form of the host name
    # is a potentially valid IP address. It does not check if the address
    # is reachable.
    valid_ip = function() {
      if (!self$config$config[["host_allow_ip"]]) {
        bool <- private$set_error("ip_address_forbidden")
      } else if (grepl(":", self$ip_address)) {
        # bool <- self$ip_address.match(Resolv::IPv6::Regex) ? TRUE : private$set_error("ipv6_address_invalid")
        bool <- if (iptools::is_ipv6(self$ip_address)) TRUE else private$set_error("ipv6_address_invalid")
      } else if (grepl("\\.", self$ip_address)) {
        # bool <- self$ip_address.match(Resolv::IPv4::Regex) ? TRUE : private$set_error("ipv4_address_invalid")
        bool <- if (iptools::is_ipv4(self$ip_address)) TRUE else private$set_error("ipv4_address_invalid")
      }

      if (bool && (self$localhost() && !self$config$config[["host_local"]])) {
        bool <- private$set_error("ip_address_no_localhost")
      }
      
      return(bool)
    },

    localhost = function(x) {
      if (!is.null(self$ip_address)) {
        grepl("::1|127\\.0\\.0\\.0", self$ip_address)
        # FIXME: not sure how to replicate this - above is a stopgap
        # rel =
        #   if (grepl(":", self$ip_address)) {
        #     NetAddr::IPv6Net.parse("" + "::1").rel(
        #       NetAddr::IPv6Net.parse(ip_address)
        #     )
        #   } else {
        #     NetAddr::IPv4Net.parse("" + "127.0.0.0/8").rel(
        #       NetAddr::IPv4Net.parse(ip_address)
        #     )
        #   }
        # !is.null(rel) && rel >= 0
      } else {
        self$host_name == "localhost"
      }
    }

    # # Connects to host to test it can receive email. This should NOT be performed
    # # as an email address check, but is provided to assist in problem resolution.
    # # If you abuse this, you *could* be blocked by the ESP.
    # def connect
    #   smtp = Net::SMTP.new(host_name || ip_address)
    #   smtp.start(@config[:helo_name] || "localhost")
    #   smtp.finish
    #   true
    # rescue Net::SMTPFatalError => e
    #   set_error(:server_not_available, e.to_s)
    # rescue SocketError => e
    #   set_error(:server_not_available, e.to_s)
    # ensure
    #   if smtp&.started?
    #     smtp.finish
    #   end
    # end
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
