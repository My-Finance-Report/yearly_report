# Big picture

make my yearly financial report using haskell

# Features

- (DONE) parse bank statements and credit card statements
- (DONE) categorize expenses 
- (WORKING) generate "report" 
  - (DONE) per month drill down 
  - (DONE) summary -> per transaction drill down
  - (DONE) styling
  - (DONE) fix decimals
- (TODO) try to serve the resulting report with haskell 
- (TODO) allow for an interactive interface 
- (TODO) allow uploading documents to generate "live" reports

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

