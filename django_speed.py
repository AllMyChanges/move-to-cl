# coding: utf-8

from django.conf import settings
settings.configure(
    TEMPLATES=[{
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
    }]
)

import django
django.setup()

from django.template import Template, Context


with open('template.html') as f:
    template = Template(f.read())


def index():
    user = {
        'username': 'art',
        'contacts': [
            {'type': 'email',
             'value': 'sasha@example.com'},
        ]
    }
    title = 'User Profile'

    return template.render(Context({'user': user, 'title': title}))


# CPython
# In [1]: from django_speed import index

# In [2]: %timeit index()
# 10000 loops, best of 3: 133 µs per loop

# PyPy
# In [6]: %timeit index()
# The slowest run took 38.21 times longer than the fastest. This could mean that an intermediate result is being cached.
# 1000 loops, best of 3: 10.1 µs per loop
