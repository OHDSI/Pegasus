library(R6)

# A stand-in for an ellmer `Chat` object. The `handler` receives the prompt, system prompt and (for structured calls)
# the requested output type, and returns what the LLM would have returned. Calls are recorded in `$calls`.
MockChat <- R6::R6Class(
  "Chat",
  public = list(
    handler = NULL,
    calls = list(),
    systemPrompt = NULL,
    tools = list(),
    costPerCall = 0.01,
    initialize = function(handler, costPerCall = 0.01) {
      self$handler <- handler
      self$costPerCall <- costPerCall
    },
    set_system_prompt = function(value) {
      self$systemPrompt <- value
      invisible(self)
    },
    set_turns = function(value) {
      invisible(self)
    },
    register_tool = function(tool) {
      self$tools[[length(self$tools) + 1]] <- tool
      invisible(self)
    },
    get_cost = function(include = "all") {
      self$costPerCall
    },
    chat = function(..., echo = "none") {
      private$respond(list(...)[[1]], type = NULL)
    },
    chat_structured = function(..., echo = "none", type = NULL) {
      private$respond(list(...)[[1]], type = type)
    }
  ),
  private = list(
    respond = function(prompt, type) {
      self$calls[[length(self$calls) + 1]] <- list(prompt = prompt, systemPrompt = self$systemPrompt, type = type)
      self$handler(prompt = prompt, systemPrompt = self$systemPrompt, type = type)
    }
  )
)

createMockLlmClient <- function(handler = function(prompt, systemPrompt, type) "OK", costPerCall = 0.01) {
  MockChat$new(handler, costPerCall)
}

# Handler that answers from a fixed list of responses, in order.
sequentialHandler <- function(...) {
  responses <- list(...)
  i <- 0
  function(prompt, systemPrompt, type) {
    i <<- i + 1
    if (i > length(responses)) {
      stop("The mock LLM ran out of responses")
    }
    responses[[i]]
  }
}
