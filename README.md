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

# Blocking

delete button in the table doesnt work

-> designate cc, vs account vs investment at source level
sankey config
-> page to actually edit it
-> consider ability to regenerate sankey somewhere
ability to add accounts

# User feedback

- manually add transactions
- budget and budget progress

# Not Blocking

- update schema to remove nullability on processed file fields
- update schema to put processed file into transactions table, not uploaded pdf
- it seems like the text on mobile just needs to be bigger
- filter by year when you look at dashboard
- privacy policy
- refresh on the page keeps you in the same view
- there is a bug if you add / remove / add a source
- install a linter for extraneous imports

# Bad UI

- password dont match in wrong modal
- let people know something is happening async
