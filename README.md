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

# Launch blocking
- make the column charts make sense
- the configuration page is just wacky, unlink and work on removing
- Sankey currently only allows one intermediate node
- sankey config comes from openai, has configability somewhere
- privacy policy
- some CTA during the demo

- the llm seems bad at getting dates, may need to include more file context

# Not Blocking

- clicking on charts pops tables would be nice
- would be nice to be able to dump all the files at once
- consider poor mans analytics by writing visits to the db for now
- consider showing the source we associate with the file before we upload
- refactor css
- refactor Main.hs routes
- pretty print dates in the tables
- delete transactions

# Bad UI

- onboarding step 1 cards not consistent
- make example file loading async
- make sure all units are shown as dollars
- clicking add transactions with no file there
