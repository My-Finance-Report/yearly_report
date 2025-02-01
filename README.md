# Big picture

this started as a project to generate my yearly financial report using haskell
and has evolved a bit to be a [simple webapp](https://myfinancereport.com/).

# Features

- parse PDF bank statements and credit card statements
- categorize expenses
- generate reports
  - per month drill down
  - summary -> per transaction drill down
  - configurable sankey nodes
- allow for manually update transactions from a ui

# Tech

- haskell for backend and webserver
- html for report
- postgres to persist data
- LLM for PDF parsing (using structured outputs)
- LLM for categorization (using structured outputs)

# Disclaimer

this is mainly a project through which I will learn more about haskell, but I expect the code to be bad.

# User feedback

- update the demo data to be more anon
- delete button in the table doesnt work
- manually add transactions
- budget and budget progress
- there still seems like some bug in the processing state, but may just be in local dev
- option to add a new source on the upload page after you drop in the file
- update schema to remove processed file fields
- update schema to put processed file into transactions table, not uploaded pdf
- it seems like the text on mobile just needs to be bigger
- there seems to be some general bug with the mobile presentation
- privacy policy
- install a linter for extraneous imports
- password dont match in wrong modal
