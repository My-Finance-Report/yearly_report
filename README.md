# Big picture

make my yearly financial report using haskell

# Features

- (DONE) parse bank statements and credit card statements
 - (TODO) there is still some cleanup around in / out money -> summing deposits + withdrawls is weird
    - probably requires adding  inbound / outbound to the expense
- (DONE) categorize expenses 
- (WORKING) generate "report" 
  - (DONE) per month drill down 
  - (DONE) summary -> per transaction drill down
  - (DONE) styling
  - (DONE) fix decimals
- (TODO) try to serve the resulting report with haskell 
- (TODO) allow for an interactive interface 
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
- Ids are duplicated on backend

