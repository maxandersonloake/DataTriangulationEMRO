# ================================================================
# Shared AES-256-CBC encrypt/decrypt helpers.
# ----------------------------------------------------------------
# Used to keep Somalia's IDSR data out of the (public) GitHub repo in
# readable form, while still letting it live inside the repo as an
# ordinary committed file (Data/disease/somalia/SOM_IDSR_Data.enc) so Posit Connect Cloud's
# normal "deploy straight from the repo" flow needs no extra infrastructure
# and no runtime network call.
#
# The encryption key is never committed. It's a single passphrase, read
# from the SOMALIA_DATA_KEY environment variable at both ends:
#   - Locally, when you (re-)run scripts/somalia_process_workbook.R to refresh the
#     encrypted bundle -- set it in your own .Renviron.
#   - On Posit Connect Cloud, as a secret "environment variable" attached
#     to the deployed app (Publish settings -> Environment variables).
#     Use the EXACT SAME value in both places.
# If that variable is unset (or wrong) when app.R starts, decrypt_object_
# from_file() returns NULL rather than erroring -- app.R treats a NULL
# bundle as "Somalia data not configured yet" and shows a message instead
# of the Somalia tab's content, so a missing/wrong key never takes the
# whole dashboard down.
#
# Sourced by both app.R and scripts/somalia_process_workbook.R.
# ================================================================

# openssl is already a transitive dependency in renv.lock (via rsconnect/
# httr), so this adds no new package to install.
if (!requireNamespace("openssl", quietly = TRUE)) {
  stop("The 'openssl' package is required for encryption_utils.R but is not installed.")
}

# Stretches an arbitrary human-typable passphrase into a fixed 32-byte
# AES-256 key via SHA-256, so SOMALIA_DATA_KEY can be any string you like
# rather than needing to be a properly-formatted raw key.
.derive_aes_key <- function(passphrase) {
  openssl::sha256(charToRaw(passphrase))
}

# Serialises `obj` (any R object -- here, a named list of data frames),
# AES-256-CBC encrypts it under `passphrase`, and writes it to `path` as a
# single binary file: a 16-byte IV followed by the ciphertext.
encrypt_object_to_file <- function(obj, path, passphrase) {
  if (is.null(passphrase) || !nzchar(passphrase)) {
    stop("encrypt_object_to_file(): passphrase is empty -- set SOMALIA_DATA_KEY before running this.")
  }
  key <- .derive_aes_key(passphrase)
  raw_bytes <- serialize(obj, connection = NULL)
  enc <- openssl::aes_cbc_encrypt(raw_bytes, key = key)
  iv <- attr(enc, "iv")

  con <- file(path, "wb")
  on.exit(close(con))
  writeBin(iv, con)
  writeBin(as.raw(enc), con)
}

# Reads `path` back and returns the original R object -- or NULL (never an
# error) if the file doesn't exist, the passphrase is empty, or decryption/
# unserialisation fails for any reason (wrong key, corrupted file, etc.).
# Callers should treat NULL as "not available", not as a bug.
decrypt_object_from_file <- function(path, passphrase) {
  if (!file.exists(path) || is.null(passphrase) || !nzchar(passphrase)) return(NULL)

  tryCatch({
    key <- .derive_aes_key(passphrase)
    con <- file(path, "rb")
    on.exit(close(con))
    iv         <- readBin(con, "raw", n = 16)
    ciphertext <- readBin(con, "raw", n = file.size(path) - 16)
    dec <- openssl::aes_cbc_decrypt(ciphertext, key = key, iv = iv)
    unserialize(dec)
  }, error = function(e) NULL)
}


# ================================================================
# Page-password helpers (the Somalia tab's login gate in app.R).
# ----------------------------------------------------------------
# The repo is public, so the password itself is never written down anywhere:
# app.R only holds a random salt and the slow bcrypt-pbkdf hash of
# (password, salt). To change the password, run in R from the project root:
#
#   source("encryption_utils.R"); make_page_password_hash("new password here")
#
# and paste the two printed values over SOMALIA_PAGE_PW_SALT /
# SOMALIA_PAGE_PW_HASH near the top of app.R.
# ================================================================
.PAGE_PW_ROUNDS <- 64L   # ~0.25 s per check: negligible for a person, costly to brute-force

.hex_to_raw <- function(h) as.raw(strtoi(substring(h, seq(1, nchar(h), 2), seq(2, nchar(h), 2)), 16L))

.page_password_hash <- function(password, salt_hex) {
  paste(as.character(openssl::bcrypt_pbkdf(charToRaw(enc2utf8(password)),
                                           .hex_to_raw(salt_hex),
                                           rounds = .PAGE_PW_ROUNDS, size = 32L)),
        collapse = "")
}

make_page_password_hash <- function(password) {
  salt_hex <- paste(as.character(openssl::rand_bytes(16)), collapse = "")
  cat('SOMALIA_PAGE_PW_SALT <- "', salt_hex, '"\n', sep = "")
  cat('SOMALIA_PAGE_PW_HASH <- "', .page_password_hash(password, salt_hex), '"\n', sep = "")
  invisible(NULL)
}

check_page_password <- function(password, salt_hex, hash_hex) {
  is.character(password) && length(password) == 1 && nzchar(password) &&
    identical(.page_password_hash(password, salt_hex), tolower(hash_hex))
}
