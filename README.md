# Big picture

this started as a project to generate my yearly financial report using haskell
and has evolved a bit to be a [webapp](https://myfinancereport.com/).
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


# TODO

# Big Milestones Before Launch

* **Stripe**
  * Sort of exists but we need a way to actually charge people

* **Plaid**
  * Have a way to activate / deactivate accounts from the accounts page
   *(technically this is fixed, when you archive a transaction source it should archive the plaid_account)

* **Notifications**
  * Allow user to 'restore default notifications' 
    * The user can delete those, edit etc, and be able to reseed again if they want ( /notifications on app)
  * Emails only configured to a few people
  * condition editors are not working
  * Maybe build an in-app view of notifications (there is already an effect_type for this (see models/effect.py) along with emails)
    * probably low priority
  * Add some idempotency key to notifications so we dont send the same notification multiple times
  * Consider a method for batching notifications (lower priority though)

* **Accounts View**
  * Some mobile treatment
  * probably only show this if the user has plaid accounts synced
  * No code improvements: add / remove widgets and have the ability to reseed the page to defaults (see /seed/accounts_page.py)
    * There needs to be a way to actually add a widget -- right now the builder is not useable
  * group by selector default is wrong / out of line with the chart
  * date of transaction is not clean
  * Initial pageload is still pretty slow on prod
  * add the ability to manually add an account with no uploads / transactions

* **POS**
  * Add ability for a guest to order (WIP)
    * Need to remove RLS on tables for menu items
  * Bigger: add ability to charge for the item
  * Add an effect for the order to send a notification

* **Onboarding**
  * centrailize seeding when account is created (right now the api calls just handle it if not initialized but thats weird)
  * dont show account explorer until plaid accounts are added
  * make sure transaction explorer gracefully handles no accounts (perhaps show the demo in place of empty screen?)


* **Misc**
  * Why are there no decimals at all on transactions...?
  * Why are there so many database connections?
  * Delete saved filter doesn't work on /transactions
  * Get user delete to work on app (right now i just removed from the user page)
    * Make sure we remove plaid items so we don't get charged
  * Redirect everything to the main url
  * Merge account function
    * Built backend but needs frontend
  * You can't remove a category once it is used
    * Doesn't rerun on category delete


# Codestyle

* Prefer strong types and functional programming
  * In general make bad states unrepresentable and add types to enforce this (of course easier said than done)
  * Strong types > tests
  * wrapped primitives are a good way to catch bugs with the linter
  ```python
    MonthlyTarget = NewType("MonthlyTarget", Decimal)
    MonthlyTotal = NewType("MonthlyTotal", Decimal)
    MonthlyProgress = NewType("MonthlyProgress", Decimal)
    BudgetAmount = NewType("BudgetAmount", Decimal)
    YearlyTotal = NewType("YearlyTotal", Decimal)
    YearlyTarget = NewType("YearlyTarget", Decimal)
  ```


* For business logic lean toward composable "pipelines"
  * Pipelines can be easily traced and debugged 
  * Try to avoid conditional function calls within the pipeline, so that regardless of the inputs, the code path is the same

* RLS is a level of protection, but we don't treat it as unbreakable isolation
  * With RLS `select * from transactions` should be the same as `select * from transactions where user_id = current_user_id`
  * That said, we should prefer the redudency of not relying on RLS 

* Prefer explicit joins to ORM magic
  * I generally don't add `relationship` to the models, and instead explicitly join when required
  * I don't feel very strongly about this


# Practical Info

* bin/ has useful commands:

* bin/worker runs the worker
* bin/backend runs the server
* bin/frontend runs the frontend
* bin/db runs the db
* bin/check_for_deploy is linting / formatting
* bin/regen regenerates the frontend client from the API 

* To make a database migration you can often just update the model files and run:


```bash
cd fullstack/backend
PYTHONPATH=$(pwd) alembic revision --autogenerate -m "$@"
```

* You should check the generated migration and make sure it is correct, some things dont get auto updated like adding an enum value

* Then run with:
```bash
cd fullstack/backend
PYTHONPATH=$(pwd) alembic upgrade heads
```

* If you add a new table, you have to grant RLS access. see alembic/helpers.py
* When you add a new api route you have to register it in app/api/main.py
* Using the query client from tanstack is really nice. prefer to raw fetch calls





