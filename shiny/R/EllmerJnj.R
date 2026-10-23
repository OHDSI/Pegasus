ProviderAnthropic <- getFromNamespace(
  "ProviderAnthropic",
  "ellmer"
)

Chat <- getFromNamespace(
  "Chat",
  "ellmer"
)

Model <- getFromNamespace(
  "Model",
  "ellmer"
)

chat_path <- getFromNamespace(
  "chat_path",
  "ellmer"
)

chat_body <- getFromNamespace(
  "chat_body",
  "ellmer"
)

base_request <- getFromNamespace(
  "base_request",
  "ellmer"
)

chat_resp_stream <- getFromNamespace(
  "chat_resp_stream",
  "ellmer"
)

stream_parse <- getFromNamespace(
  "stream_parse",
  "ellmer"
)

stream_merge_chunks <- getFromNamespace(
  "stream_merge_chunks",
  "ellmer"
)

stream_content <- getFromNamespace(
  "stream_content",
  "ellmer"
)

stream_content_with_turns <- getFromNamespace(
  "stream_content_with_turns",
  "ellmer"
)

# Provider class ------------------------------------------
ProviderJnj <- S7::new_class(
  "ProviderJnj",
  package = "Helios",
  parent = ProviderAnthropic,
  properties = list(
    anthropic_version = S7::class_character
  )
)

# Store the parent methods before registering the subclass methods.
anthropic_chat_body <- S7::method(
  chat_body,
  ProviderAnthropic
)

anthropic_base_request <- S7::method(
  base_request,
  ProviderAnthropic
)

S7::method(chat_path, ProviderJnj) <- function(provider) {
  "invoke"
}

# Request headers ----------------------------------------------
S7::method(base_request, ProviderJnj) <- function(provider) {
  req <- anthropic_base_request(provider)

  # The inherited Anthropic method already adds x-api-key, but these
  # headers make the gateway requirements explicit.
  req |>
    httr2::req_headers(
      `content-type` = "application/json",
      accept = "application/json"
    )
}


# Request body -----------------------------
S7::method(chat_body, ProviderJnj) <- function(
    provider,
    model,
    stream = TRUE,
    turns = list(),
    tools = list(),
    type = NULL
) {
  # Let ellmer's Anthropic provider handle:
  #   * conversion of turns to Anthropic messages
  #   * system prompts
  #   * tool definitions
  #   * structured-output schemas
  #   * tool_choice
  #   * model parameters
  body <- anthropic_chat_body(
    provider = provider,
    model = model,
    stream = FALSE,
    turns = turns,
    tools = tools,
    type = type
  )

  # The J&J gateway gets the model identifier from the URL.
  body$model <- NULL

  # The example supplied by J&J puts this value in the JSON body.
  body$anthropic_version <- provider@anthropic_version

  # The provided /invoke endpoint returns a complete JSON response rather
  # than an Anthropic server-sent event stream.
  body$stream <- NULL
  return(body)
}


# Streaming ------------------------------------------------
# The /invoke endpoint always returns one complete JSON document, never an
# Anthropic server-sent event stream. When a caller streams (e.g. shinychat's
# chat_append_stream()), ellmer still drives its streaming code path, which
# would otherwise feed the plain JSON body to the inherited Anthropic SSE
# parser and fail with "Connection closed unexpectedly". These methods instead
# read the whole body and surface it as a single chunk; the inherited Anthropic
# value_turn() then builds the final turn exactly as in the non-streaming case.

S7::method(chat_resp_stream, ProviderJnj) <- function(provider, resp) {
  # Read the complete body here and return it as a single chunk. We block until
  # the body is complete rather than returning NULL between reads on purpose:
  # /invoke is not a real event stream, so there is nothing to emit
  # incrementally, and returning NULL would send ellmer's async stream generator
  # (used by shinychat) into a `later_fd()` wait that never resolves, because
  # the whole body is already buffered and the socket never signals "readable"
  # again. resp$cache persists across the generator's calls, so it also guards
  # against re-emitting the body on the generator's next poll.
  cache <- resp$cache
  if (isTRUE(cache$jnj_stream_returned)) {
    return(NULL)
  }
  cache$jnj_stream_returned <- TRUE

  buffer <- raw()
  repeat {
    chunk <- httr2::resp_stream_raw(resp, kb = 64)
    if (length(chunk) > 0) {
      buffer <- c(buffer, chunk)
    } else if (httr2::resp_stream_is_complete(resp)) {
      break
    } else {
      Sys.sleep(0.01)
    }
  }

  if (length(buffer) == 0) {
    return(NULL)
  }
  body <- rawToChar(buffer)
  Encoding(body) <- "UTF-8"
  jsonlite::fromJSON(body, simplifyVector = FALSE)
}

S7::method(stream_parse, ProviderJnj) <- function(provider, event) {
  # `event` is already the fully parsed /invoke response, or NULL at end.
  event
}

S7::method(stream_merge_chunks, ProviderJnj) <- function(
    provider,
    result,
    chunk
) {
  # There is only ever one chunk: the complete message.
  chunk
}

S7::method(stream_content, ProviderJnj) <- function(
    provider,
    event,
    completion = NULL
) {
  # Surface text for live display; the final turn is rebuilt by value_turn().
  if (is.null(event) || is.null(event$content)) {
    return(list())
  }
  text_blocks <- Filter(
    function(block) identical(block$type, "text"),
    event$content
  )
  lapply(text_blocks, function(block) ellmer::ContentText(block$text))
}

# ellmer's streaming loop calls stream_content_with_turns(), which
# ProviderAnthropic overrides with an SSE-delta parser that returns nothing for
# our single full-message chunk. Route it back to our stream_content().
S7::method(stream_content_with_turns, ProviderJnj) <- function(
    provider,
    event,
    completion = NULL,
    turns = list()
) {
  stream_content(provider, event, completion)
}

#' Chat with a model hosted on the J&J Bedrock
#'
#' @param model The deployment ID for the model. See available options in the details.
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param params Common model parameters, usually created by `ellmer::params()`.
#' @param api_key The API key.
#' @param base_url The base URL
#' @param anthropic_version The Anthropic API version.
#' @param api_args Named list of arbitrary extra arguments appended to the body of every chat API call. Combined with the body object generated by ellmer with `ellmer::modifyList()`.
#' @param api_headers Named character vector of arbitrary extra headers appended to every chat API call.
#' @param echo One of the following options:
#'   * `none`: don't emit any output (default when running in a function).
#'   * `output`: echo text and tool-calling output after the turn completes
#'     (default when running at the console).
#' @param echo One of the following options:
#'
#' @details
#' Available models:
#'
#' - anthropic.claude-3-haiku-20240307-v1:0
#' - us.anthropic.claude-3-7-sonnet-20250219-v1:0
#' - us.anthropic.claude-3-5-haiku-20241022-v1:0
#' - us.anthropic.claude-3-5-sonnet-20241022-v2:0
#' - anthropic.claude-3-5-sonnet-20240620-v1:0
#' - us.anthropic.claude-3-opus-20240229-v1:0
#' - anthropic.claude-3-sonnet-20240229-v1:0
#' - anthropic.claude-v2:1
#' - anthropic.claude-v2
#' - anthropic.claude-instant-v1
#' - us.meta.llama3-2-11b-instruct-v1:0
#' - us.meta.llama3-2-1b-instruct-v1:0
#' - us.meta.llama3-2-3b-instruct-v1:0
#' - us.meta.llama3-2-90b-instruct-v1:0
#' - amazon.titan-text-express-v1
#' - amazon.titan-text-lite-v1
#' - cohere.command-r-v1:0
#' - cohere.command-r-plus-v1:0
#' - amazon.titan-embed-text-v2:0
#' - amazon.titan-embed-image-v1
#' - amazon.titan-embed-text-v1
#' - cohere.embed-english-v3
#' - cohere.embed-multilingual-v3
#' - us.anthropic.claude-sonnet-4-20250514-v1:0
#' - global.anthropic.claude-sonnet-4-20250514-v1:0
#' - us.anthropic.claude-opus-4-20250514-v1:0
#' - us.anthropic.claude-opus-4-1-20250805-v1:0
#' - us.anthropic.claude-sonnet-4-5-20250929-v1:0
#' - global.anthropic.claude-sonnet-4-5-20250929-v1:0
#' - us.cohere.embed-v4:0
#' - global.cohere.embed-v4:0
#' - us.meta.llama3-3-70b-instruct-v1:0
#' - us.anthropic.claude-haiku-4-5-20251001-v1:0
#' - global.anthropic.claude-haiku-4-5-20251001-v1:0
#' - us.anthropic.claude-opus-4-5-20251101-v1:0
#' - global.anthropic.claude-opus-4-5-20251101-v1:0
#' - us.anthropic.claude-opus-4-6-v1
#' - global.anthropic.claude-opus-4-6-v1
#' - us.anthropic.claude-sonnet-4-6
#' - global.anthropic.claude-sonnet-4-6
#' - us.anthropic.claude-opus-4-7
#' - global.anthropic.claude-opus-4-7
#' - us.anthropic.claude-opus-4-8
#' - global.anthropic.claude-opus-4-8
#' - global.anthropic.claude-sonnet-5
#' - us.anthropic.claude-sonnet-5
#' - global.anthropic.claude-opus-5
#' - us.anthropic.claude-opus-5
#'
#'
#' @returns
#' A `ellmer::Chat` object.
#'
#' @examples
#' if (FALSE) { # To prevent example code from being executed
#'   client <- chat_jnj(model = "anthropic.claude-3-haiku-20240307-v1:0",
#'                      api_key = keyring::key_get("genai_bedrock_key"))
#'   client$chat("What is the capital of the Netherlands?")
#' }
#' @export
chat_jnj_bedrock <- function(
    model,
    system_prompt = NULL,
    params = NULL,
    api_key = NULL,
    base_url = NULL,
    anthropic_version = "bedrock-2023-05-31",
    api_args = list(),
    api_headers = character(),
    echo = c("none", "text", "all")
) {
  echo <- match.arg(echo)

  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    stop(
      "`model` must be one non-empty model identifier.",
      call. = FALSE
    )
  }

  if (!is.character(api_key) ||
      length(api_key) != 1L ||
      !nzchar(api_key)) {
    stop(
      paste(
        "No J&J GenAI API key was supplied."
      ),
      call. = FALSE
    )
  }

  if (!is.character(base_url) ||
      length(base_url) != 1L ||
      !nzchar(base_url)) {
    stop(
      "`base_url` must be one non-empty URL.",
      call. = FALSE
    )
  }

  if (!is.character(anthropic_version) ||
      length(anthropic_version) != 1L ||
      !nzchar(anthropic_version)) {
    stop(
      "`anthropic_version` must be one non-empty string.",
      call. = FALSE
    )
  }

  if (is.null(params)) {
    params <- ellmer::params()
  }

  # Remove a trailing slash to avoid URLs containing "//model".
  base_url <- sub("/+$", "", base_url)

  # Encode the model identifier because it becomes part of the URL path.
  encoded_model <- utils::URLencode(
    model,
    reserved = TRUE
  )

  model_base_url <- paste0(
    base_url,
    "/model/",
    encoded_model
  )

  provider <- ProviderJnj(
    name = "Anthropic",
    extra_headers = api_headers,
    base_url = model_base_url,
    beta_headers = character(),
    credentials = function() {
      c(`x-api-key` = api_key)
    },
    cache = "none",
    anthropic_version = anthropic_version
  )

  model_obj <- ellmer::Model(
    name       = model,
    params     = params,     # already defaulted to ellmer::params() above
    extra_args = api_args
  )

  Chat$new(
    provider = provider,
    model = model_obj,
    system_prompt = system_prompt,
    echo = echo
  )
}
