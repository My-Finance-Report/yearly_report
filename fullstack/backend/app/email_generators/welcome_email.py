from app.email.send import Email


HTML = """
<p>Thanks for making an account on My Financé! I'm really excited you're here!</p>
<br/>
<p>This is an automated email, but My Financé is built by a real human: me, Matt :waves:</p>
<br/>
<p>I try to be quick to fix bugs and receptive to feature requests! (I've built 100 percent of the requested features so far!). You can always reach me at mcarroll1220@gmail.com if you have any questions.</p>
<br/>
<p>Thanks again for joining!</p>
"""


def generate_welcome_email() -> Email:
    return Email(subject="Welcome to My Financé", html=HTML)
