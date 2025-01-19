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
- sort is wrong on the barchart x axis
- the configuration page is just wacky
- sankey config has configability somewhere

# Mom feedback

- make it clear when they start the onboarding that they need their bank / cc statements
- sankey needs to fix within its container
- First page need to know you need statements
  -> import to be able to name credit cards
  -> the first page spells out all the things you need to do
  -> harder to crash page on final step
  -> from the onboard page just upload those transactions
- manually add transactions
- budget and budget progress

# Not Blocking

- there is some redirect bug in the onboarding flow
- filter by year when you look at dashboard
- clicking on charts pops tables would be nice
- privacy policy
- ability to add accounts
- would be nice to be able to dump all the files at once
- consider poor mans analytics by writing visits to the db for now
- consider showing the source we associate with the file before we upload
- some bug with the job counter if a file has already been processed
- refactor css
- refactor Main.hs routes
- refresh on the page keeps you in the same view
- delete transactions

# Bad UI

- password dont match in wrong modal
- let people know something is happening async
- default to categories being added
