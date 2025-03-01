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

# User feedback

-> way to remove file
-> give a file a nick name when we process
-> general bug around uploading file twice
-> expect to have transactioons for january but don't
-> need to be able to edit
-> filter out from year start




-> unable to delete a file and transactions
- show error messages on login screens
-> no bounce after create account
- manually add transactions
- demo account -- exists but not useable yet
- edit transactions
- toasts are broken
- privacy policy
- terms of service
- contact me page


