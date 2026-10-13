library(Pegasus)

llmClient <- ellmer::chat_lmstudio(
  model = "qwen3.8-27b-splash"
)

refineDefinition("Acute liver injury", llmClient = llmClient)

