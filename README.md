# Big picture

this started as a project to generate my yearly financial report using haskell
and has evolved a bit to be a [simple webapp](https://myfinancereport.com/).

# Features

- parse PDF bank statements and credit card statements
- categorize expenses
- generate reports
  - per month drill down
  - summary -> per transaction drill down
  - configurable sankey and column diagrams
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



-> files processed table needs to be grouped by source and collapsolable
-> files dont reprocess when you change a source / category
-> changing a source name caused me to repopulate that source when i reran the upload
-> no way to merge when this happens
-> no way to delete transactions
-> sankey config doesnt filter off archived sources


- enable config of the barchart (categories to include + sources to include) (wip)

- shareable report
  - allow to select different charts

- manually add transactions

- privacy policy

- toasts are broken
- sort is broken for dates
