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

big milestones before launch:

-> Stripe
  --> sort of exists but we need a way to actually charge people

-> Plaid
  -> prune the logs table of sync events (over 2mm events already, some sort of lifecycle)
  -> have a way to activate / deactivate accounts from the accounts page

-> Notifications
  -> allow user to 'restore default notifications' and delete the defaults
  -> emails only configured to send to me -- update to send to everyone
  -> not able to create a new notification on the ui right now
  -> maybe build an in-app view of notifications (there is already an effect_type for this (see models/effect.py) along with emails)
  -> add a budget based event


-> Accounts View
  -> no code improvements: add / remove widgets 
  -> weird initial loading state
  -> i think some of the queries are quite slow
    -> maybe be able to batch the inital load
  -> some mobile treatment

-> POS
  -> add ability for a guest to order (WIP)
  --> need to remove RLS on tables for menu items
  -> bigger: add ability to charge for the item
  -> add an effect for the order to send a notification

-> why are there no decimals at all on transactions..?

-> delete saved filter doesnt work on /transactions

-> get user delete to work on app (right now i just removed from the user page)
--> make sure we remove plaid items so we dont get charged

-> redirect everything to the main url
-> fix 2fa

-> merge account function
--> built backend but needs frontend

-> nice to have: search of transactions

-> you cant remove a category once it is used
--> doesnt rerun on category delete


# Codestyle

-> Prefer strong types and functional programming
--> in general make bad states unrepresentable and add types to enforce this (of course easier said than done)
--> strong types > tests

-> for business logic lean toward composable "pipelines"
--> pipelines can be easily traced and debugged 
--> try to avoid conditional function calls within the pipeline, so that regardless of the inputs, the code path is the same

-> RLS is a level of protection, but we don't treat it as unbreakable isolation
--> with RLS `select * from transactions` should be the same as `select * from transactions where user_id = current_user_id`
--> that said, we should prefer the redudency of not relying on RLS 

-> Prefer explicit joins to ORM magic. 
--> I generally don't add `relationship` to the models, and instead explicitly join when required. 
--> I dont feel very strongly about this



# Practical info
-> bin/ has some useful commands

-> bin/worker runs the worker
-> bin/backend runs the server
-> bin/frontend runs the frontend
-> bin/db runs the db
-> bin/check_for_deploy is linting / formatting
-> bin/regen regenerates the frontend client from the API 

-> to make a database migration you can often just update the model files and run:


```bash
cd fullstack/backend
PYTHONPATH=$(pwd) alembic revision --autogenerate -m "$@"
```

-> you should check the generated migration and make sure it is correct, some things dont get auto updated like adding an enum value

-> then run with:
```bash
cd fullstack/backend
PYTHONPATH=$(pwd) alembic upgrade heads
```

-> if you add a new table, you have to grant RLS access. see alembic/helpers.py
-> when you add a new api route you have to register it in app/api/main.py
-> using the query client from tanstack is really nice. prefer to raw fetch calls





