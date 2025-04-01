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

big milestones before launch:

-> Stripe
  --> sort of exists but needs more work
-> Plaid
  --> done, but needs to be tested
  --> seems to be a bug on the sync frequency
-> Email

-> rethink navbar -> remove uploads, put in link with plaid, add account management
-> track uploads on the account level

-> would be nice to have some tests around user creation

-> there is a bug in how we write the settings to the db on user table -- possibly want new users to be power users?

-> add a last active at time on the user model

-> uploads page on mobile is rough

-> budget page should probably not use edit modal for create

-> shareable report

-> running grant to sequence after a new table: 
  GRANT USAGE, UPDATE ON SEQUENCE plaid_item_id_seq TO app_user;
  GRANT USAGE, UPDATE ON SEQUENCE plaid_account_id_seq TO app_user;
  GRANT USAGE, UPDATE ON SEQUENCE plaid_sync_log_id_seq TO app_user;

-> redirect everything to the main url

-> welcome email

-> merge account function
--> built backend but needs frontend

-> repro filter bug by getting a query that has no results

-> nice to have: search

-> try to prevent color overlaps

-> dashboard config and ability to save them (query params)
--> parse the query params from the url

-> you cant remove a category once it is used
--> doesnt rerun on category delete

-> Expense should line up in the table


