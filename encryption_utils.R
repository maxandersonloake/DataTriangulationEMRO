# ================================================================
# Shared AES-256-CBC encrypt/decrypt helpers.
# ----------------------------------------------------------------
# Used to keep Somalia's IDSR data out of the (public) GitHub repo in
# readable form, while still letting it live inside the repo as an
# ordinary committed file (Data/SOM_IDSR_Data.enc) so Posit Connect Cloud's
# normal "deploy straight from the repo" flow needs no extra infrastructure
# and no runtime network call.
#
# The encryption key is never committed. It's a single passphrase, read
# from the SOMALIA_DATA_KEY environment variable at both ends:
#   - Locally, when you (re-)run 2_1_ProcessData_SOM.R to refresh the
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
# Sourced by both app.R and 2_1_ProcessData_SOM.R.
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
