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

store file status and allow reprocessing
delete button in the table doesnt work

# User feedback

- 2x! make it clear when they start the onboarding that they need their bank / cc statements
- add the categories from the start
- the third page is just genreally confusing
- First page need to know you need statements
  -> import to be able to name credit cards
  -> the first page spells out all the things you need to do
  -> harder to crash page on final step
  -> from the onboard page just upload those transactions
- manually add transactions
- budget and budget progress

# Not Blocking

- need some sort of queue for jobs that fail to process
- it seems like the text on mobile just needs to be bigger
- sankey config has configability somewhere
- there is some redirect bug in the onboarding flow
- filter by year when you look at dashboard
- privacy policy
- refactor css
- refactor Main.hs routes
- refresh on the page keeps you in the same view
- delete transactions
- the data model should likely differentiate between cards and accounts and keep it at a type level
  - use this to inform what presets we show, and also which things we want in which charts

# Bad UI

- password dont match in wrong modal
- let people know something is happening async
- hit box is just the tiny thing on step 2 of onboard
