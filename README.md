# Big picture

make my yearly financial report using haskell

# Features

- (DONE) parse PDF bank statements and credit card statements (see src/Parsers.hs)
 - (DONE) cleanup around in / out money -> summing deposits + withdrawls is weird
- (DONE) categorize expenses  (see src/Categorizer.hs)
- (WORKING) generate "report"  (see src/HtmlGenerators.hs)
  - (DONE) per month drill down 
  - (DONE) summary -> per transaction drill down
  - (DONE) allow for an interactive interface 
- (DONE) try to serve the resulting report with haskell 
  - minimally implimented with Scotty 
- (TODO) allow uploading documents to generate "live" reports
- (TODO) allow for manually update transactions from a ui 
  - this is probably a whole project in and of itself. basically a ui to memoize and "overrule" an llm's response

# Tech

- haskell for "backend" 
- html for report
- sqlite to persist data
- LLM for PDF parsing  (using structured outputs)
- LLM for categorization (using structured outputs)

# Disclaimer 

this is mainly a project through which I will learn more about haskell, but I expect the code to be bad.


# Bugs

- Ids are duplicated on backend for expansion of rows in subsequent sections
- Batch calls to open ai to make less likely to error out

