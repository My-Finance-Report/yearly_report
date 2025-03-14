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

-> monthly emails (resend)

-> stripe (when do people need to pay?)

-> try to prevent color overlaps

-> OAuth, nice but not required

-> csv support

-> database backups

-> remove an account from manage page if there is no transactions

-> dashboard config and ability to save them (query params)
--> parse the query params from the url

-> you cant remove a category once it is used
--> doesnt rerun on category delete

-> restyle the budgets page
 -> cleaner and remove the warning
 -> add some indication of budget progress on the landing page

-> redo filter group component (waiting on designs)
-> Expense should line up in the table

-> privacy policy
-> terms of service
-> contact me page (add to other pages or footer)



