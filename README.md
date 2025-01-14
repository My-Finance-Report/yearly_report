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

- Batch calls to open ai to make less likely to error out
- Sankey currently only allows one intermediate node
- need to build the example file endpoint, right now it doesnt actually work
- the adjust page has no margins and is super confusing
  -> can we just remove this all together?
- the configuration page is just wacky

- consider showing the source we associate with the file before we upload
- the llm seems bad at getting dates, may need to include more file context
- ability to add and remove categories and sources
  -- deletes just need to make sure that there are no transactions
- use llm to determine file regex and keywords
- link to log in on the landing page
