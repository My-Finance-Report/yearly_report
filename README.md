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

- remove linkage button doesnt work
- stacked column chart per cateogry
- enable config of the barchart (categories to include + sources to include) (wip)
- do we actually constrain a file from being uploaded twice without nuking the first results?
- uploads fail if the transactions source does not have any categories
  => constrain the db to prevent this
- shareable report
  - allow to select different charts
- dont show snakey if there is a cycle in the chart, and alert?
- need to preserve location in tab groups when navigating forms
- release an admin page
- why is it so slow when i upload n files in the first pass?
  - probably some async code we dont need anymore
- we should save some error logs to the db when a job fails
- update the demo data to be more anon
- manually add transactions
- budget and budget progress
- option to add a new source on the upload page after you drop in the file
- privacy policy
- install a linter for extraneous imports
