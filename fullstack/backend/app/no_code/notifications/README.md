# Notifications

This is a no-code notifications system that allows users to configure notifications for their accounts.

## Effects

Effects are actions that are configured for / by a users 

Effects have conditions that an event must meet in order for the effect to be propagated

## Events

Events are created when something happens in the app -- AKA when there is an event. This is the object
that "gets triggered"

Events have a type, which defines what info is included in the payload 

## Trigger

The trigger "fires the event at the effects" and manages their interactions.


## In Practice

1. A new transaction is added from Plaid for a user
2. This causes an Event to be triggered. 
3. The trigger code will fetch all the relevant Effects for this particular event. 
 a. it may filter off effects that have fired in recency 
 b. it may filter off effects because the event does not meet some bar
 c. it may aggregate effects that are duplicative 
4. The trigger then carrys out the effect using the information from the event
 - for example generating an email body based on the transactions and sending it to the user
5. The trigger will then record the fact that the effect was fired


