# -*- encoding: utf-8 -*-
from django.template import Context
from django.template.engine import Engine


engine = Engine(autoescape=True)

# @idlesign though these settings will speed up
# rendering, but seems they are not play any role
builtin_tags = engine.template_builtins[0]
builtin_tags.tags = {'for': builtin_tags.tags['for']}
engine.template_builtins = [builtin_tags]
engine.template_context_processors = []
engine.template_libraries = {}
engine.builtins = []


with open('template.html') as f:
    template = engine.from_string(f.read())


# Move out Context reation out of index() to be just.
def index():
    ctx = {
        'user': {
            'username': 'art',
            'contacts': [
                {'type': 'email',
                 'value': 'sasha@example.com'},
            ]
        },
        'title': 'User Profile',
    }
    ctx = Context(ctx, autoescape=False)

    return template.render(ctx)


# In [1]: from templates_speed import index
# In [2]: %timeit index()
#
# ----------------
# $> cat /proc/cpuinfo | grep "model name" -i -m 1
#
# model name	: Intel(R) Core(TM) i3-4130 CPU @ 3.40GHz
# 10000 loops, best of 3: 53.2 µs per loop
#

# с включенным экранированием
# In [3]: %timeit index()
# 10000 loops, best of 3: 80.8 µs per loopA

# с выключенным
# 10000 loops, best of 3: 80.4 µs per loop

# с формированием контекста внутри функции
# 10000 loops, best of 3: 90.5 µs per loop
