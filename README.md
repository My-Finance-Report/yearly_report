# Big picture

make my yearly financial report using haskell

# Features

- parse PDF bank statements and credit card statements (see src/Parsers.hs)
- cleanup around in / out money -> summing deposits + withdrawls is weird
- categorize expenses (see src/Categorizer.hs)
- generate "report" (see src/HtmlGenerators.hs)
- per month drill down
- summary -> per transaction drill down
- allow for an interactive interface
- try to serve the resulting report with haskell
- minimally implimented with Scotty
- allow uploading documents to generate "live" reports
- create a minimal "upload" configuration that is tied to a source and allows saving upload config
- allow sankey nodes to be configurable
- remove internal notion of bank vs CC statement
- create a configurable object to make sankeys
- put it into the ui for processing
- allow for manually update transactions from a ui
- (TODO) need to persist the matchers to the DB so that in the future the same transactions get binned accordingly
- deploy in some capacity so i can run the app and let users try it
- (TODO) apply per user scope (RLS) to the db

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
- Build deploy is insanely slow
- the PDF reader code is not on the server
