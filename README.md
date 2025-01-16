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

# Bugs

- Sankey currently only allows one intermediate node
- consider poor mans analytics by writing visits to the db for now

- the adjust page has no margins and is super confusing
  -> can we just remove this all together?

- the configuration page is just wacky
  -> maybe we can reuse elements from onboarding

- consider showing the source we associate with the file before we upload
- the llm seems bad at getting dates, may need to include more file context
- link to log in on the landing page
