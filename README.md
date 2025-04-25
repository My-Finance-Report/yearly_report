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
  -> there must be some type of "update" payload that i need to handle from plaid
  -> prune the logs table of sync events
  -> webhooks setup
-> Email
  -> seems like the rate limiting is not working
  -> welcome email only configured to send to me

-> transaction delete button
-> manaual balance updates

-> parameters are getting confusing, how can i clean up the loop for each one?
  -> widgets could be fully self contained and fetch itself (done)
  --> page loads all of the widgets, and each has a fetch within it (done)
  -> each parameter has a set of dependent widgets. when we change a parameter we need to know.

  1. do i trigger a refetch?
  2. who do i trigger a refetch for?

  then when i am changed. if i dont trigger a refetch, i do nothing
  if i do trigger a refetch, i have to call the refetch of all my dependencies

  parameters:
  +/- global, (knows its dependent widgets)
  +/- triggers refetches

  widgets:



-> why are there no decimals at all on transactios..?
-> acccount balance over time would be cool

-> delete saved filter doesnt work

-> get user delete to work on app
--> make sure we remove plaid items so we dont get charged

-> redirect everything to the main url

-> merge account function
--> built backend but needs frontend

-> nice to have: search

-> you cant remove a category once it is used
--> doesnt rerun on category delete



