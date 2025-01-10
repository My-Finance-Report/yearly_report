# Big picture

make my yearly financial report using haskell

# Features

- (DONE) parse PDF bank statements and credit card statements (see src/Parsers.hs)
- (DONE) cleanup around in / out money -> summing deposits + withdrawls is weird
- (DONE) categorize expenses (see src/Categorizer.hs)
- (DONE) generate "report" (see src/HtmlGenerators.hs)
  - (DONE) per month drill down
  - (DONE) summary -> per transaction drill down
  - (DONE) allow for an interactive interface
- (DONE) try to serve the resulting report with haskell
  - minimally implimented with Scotty
- (DONE) allow uploading documents to generate "live" reports
  - (DONE) create a minimal "upload" configuration that is tied to a source and allows saving upload config
- (DONE) allow sankey nodes to be configurable
  -(DONE) remove internal notion of bank vs CC statement
  -(DONE) create a configurable object to make sankeys
  -(DONE) put it into the ui for processing
- (DONE) allow for manually update transactions from a ui
  - (TODO) need to persist the matchers to the DB so that in the future the same transactions get binned accordingly
- (TODO) deploy in some capacity so i can run that app and let users try it
  - (DONE) likely need some auth and user notion.
  - (DONE) probably should switch to postgres before we deploy

# Tech

- haskell for backend and webserver
- html for report
- sqlite to persist data
- LLM for PDF parsing (using structured outputs)
- LLM for categorization (using structured outputs)

# Disclaimer

this is mainly a project through which I will learn more about haskell, but I expect the code to be bad.

# Bugs

- Batch calls to open ai to make less likely to error out
- Some issue with case sensitivity in uploader matching
- Some issue with negativity on sankey diagram
