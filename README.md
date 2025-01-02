# Big picture

make my yearly financial report using haskell

# Features

- (DONE) parse PDF bank statements and credit card statements (see src/Parsers.hs)
 - (TODO) there is still some cleanup around in / out money -> summing deposits + withdrawls is weird
    - probably requires adding  inbound / outbound to the expense
- (DONE) categorize expenses  (see src/Categorizer.hs)
- (WORKING) generate "report"  (see src/HtmlGenerators.hs)
  - (DONE) per month drill down 
  - (DONE) summary -> per transaction drill down
- (TODO) try to serve the resulting report with haskell 
- (DONE) allow for an interactive interface 
- (TODO) allow uploading documents to generate "live" reports
- (TODO) allow for manually update transactions from a ui 

# Tech

- haskell for "backend" 
- html for report
- sqlite to persist data
- LLM for PDF parsing  (using structured outputs)
- LLM for categorization (using structured outputs)

# Disclaimer 

this is mainly a project through which I will learn more about haskell, but I expect the code to be bad.


# Scratch

- looks like duplicate keys get overwritten 
- Ids are duplicated on backend for expansion of rows

