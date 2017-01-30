# https://jedi.jedidjah.ch/en/latest/docs/usage.html
# http://sontek.net/blog/detail/tips-and-tricks-for-the-python-interpreter
from __future__ import print_function
import os
import sys

try:
    import readline
    import rlcompleter
    import atexit
except ImportError:
    print("You need readline, rlcompleter, and atexit")


try:
    from jedi.utils import setup_readline
    setup_readline()
except ImportError:
    # Fallback to the stdlib readline completer if it is installed.
    # Taken from http://docs.python.org/2/library/rlcompleter.html
    print("Jedi is not installed, falling back to readline")
    try:
        readline.parse_and_bind("tab: complete")
    except ImportError:
        print("Readline is not installed either.\
         No tab completion is enabled.")

class Completer(object):
    def __init__(self):
        # Enable a History
        self.HISTFILE = os.path.join(os.environ['HOME'], '.pythonhist')

        # Read the existing history if there is one
        if os.path.exists(self.HISTFILE):
            readline.read_history_file(self.HISTFILE)

        # Set maximum number of items that will be written to the history file
        readline.set_history_length(3000)
        atexit.register(self.savehist)

    def savehist(self):
        import readline
        readline.write_history_file(self.HISTFILE)


c = Completer()

WELCOME = ''

class TermColors(dict):
    """Gives easy access to ANSI color codes. Attempts to fall back to no color
    for certain TERM values. (Mostly stolen from IPython.)"""

    COLOR_TEMPLATES = (
        ("Black"       , "0;30"),
        ("Red"         , "0;31"),
        ("Green"       , "0;32"),
        ("Brown"       , "0;33"),
        ("Blue"        , "0;34"),
        ("Purple"      , "0;35"),
        ("Cyan"        , "0;36"),
        ("LightGray"   , "0;37"),
        ("DarkGray"    , "1;30"),
        ("LightRed"    , "1;31"),
        ("LightGreen"  , "1;32"),
        ("Yellow"      , "1;33"),
        ("LightBlue"   , "1;34"),
        ("LightPurple" , "1;35"),
        ("LightCyan"   , "1;36"),
        ("White"       , "1;37"),
        ("Normal"      , "0"),
    )

    NoColor = ''
    _base = '\001\033[%sm\002'

    def __init__(self):
        if os.environ.get('TERM') in ('xterm-color', 'xterm-256color', 'linux',
                                      'screen', 'screen-256color', 'screen-bce'):
            self.update(dict([(k, self._base % v) for k, v in self.COLOR_TEMPLATES]))
        else:
            self.update(dict([(k, self.NoColor) for k, v in self.COLOR_TEMPLATES]))
_c = TermColors()


# Enable Pretty Printing for stdout
def my_displayhook(value):
    if value is not None:
        try:
            import __builtin__
            __builtin__._ = value
        except ImportError:
            __builtins__._ = value

        import pprint
        pprint.pprint(value)
        del pprint

sys.displayhook = my_displayhook


# If we're working with a Django project, set up the environment
if 'DJANGO_SETTINGS_MODULE' in os.environ:
    from django.test.client import Client
    from django.test.utils import setup_test_environment, teardown_test_environment
    from django.conf import settings as S

    C = Client()

    WELCOME += """%(Green)s
    Django environment detected.
* Your project settings are available as `S`.
* The Django test client is available as `C`.
%(Normal)s""" % _c

    setup_test_environment()
    S.DEBUG_PROPAGATE_EXCEPTIONS = True

    WELCOME += """%(LightPurple)s
Warning: the Django test environment has been set up; to restore the
normal environment call `teardown_test_environment()`.

Warning: DEBUG_PROPAGATE_EXCEPTIONS has been set to True.
%(Normal)s""" % _c

print(WELCOME)
