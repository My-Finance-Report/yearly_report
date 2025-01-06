# Big picture

make my yearly financial report using haskell

# Features

- (DONE) parse PDF bank statements and credit card statements (see src/Parsers.hs)
- (DONE) cleanup around in / out money -> summing deposits + withdrawls is weird
- (DONE) categorize expenses (see src/Categorizer.hs)
- (WORKING) generate "report" (see src/HtmlGenerators.hs)
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

# Tech

- haskell for "backend"
- html for report
- sqlite to persist data
- LLM for PDF parsing (using structured outputs)
- LLM for categorization (using structured outputs)

# Disclaimer

this is mainly a project through which I will learn more about haskell, but I expect the code to be bad.

# Bugs

- Batch calls to open ai to make less likely to error out
- ability to delete a file and its transactions
- ability to edit the "uploaders"
