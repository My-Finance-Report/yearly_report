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
  - (TODO) need to persist the matchers to the DB so that in the future the same transactions get binned accordingly

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
- Update categorized transaction to have entity category for forms
- Some bug with pulling processed files

# notes from onboarding myself

- null state is kind of weird on landing page, should probably be a setup flow
- its hard to know you need to configure the sankey
- the adjust page has no margins and is super confusing
- the configuration page is just wacky
- i dont get a chance to add sources before
  -- maybe these should just be called accounts for clarity
- its weird that the first time thorugh the upload flow it doesnt actually run the upload
- consider showing the source we associate with the file before we upload
- the jobs running banner is vile
- no way to update any miscategorization (transaction page needs link)
- still some bug with the files that are in /transactions
- the llm seems bad at getting dates, may need to include more file context
- ability to add and remove categories and sources
  -- deletes just need to make sure that there are no transactions
