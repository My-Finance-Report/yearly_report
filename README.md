# Big picture

this started as a project to generate my yearly financial report using haskell
and has evolved a bit to be a [simple webapp](https://myfinancereport.com/).
I actually rewrote it in react + fastapi, so that is currently what you see on the site.

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

# User feedback / TODO


-> OAuth

-> might need a page that actually defines what the tool does


-> broke the mobile view
 --> filters

-> dashboard config and ability to save them (query params)
--> adding them but not able to save

-> manually add in an account + transactions to the account (CD, 401k etc)
-> general desire for the means to track assets (confirmed by rick)

-> you cant remove a category once it is used
--> doesnt rerun on category delete

-> manually override categories for transactions (memoize to llm) (confirmed by rick)
 --> this should mostly be working

-> restyle the budgets page
 --> actually just work on transactions page instead

-> redo filter group component (waiting on designs)

-> some bugs on the landing page version likely around existing filters

-> Expense should line up in the table

- privacy policy
- terms of service
- contact me page



