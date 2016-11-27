# coding: utf-8

from jinja2 import Template

with open('template.html') as f:
    jinja_template = Template(f.read())


def jinja_index():
    user = {
        'username': 'art',
        'contacts': [
            {'type': 'email',
             'value': 'sasha@example.com'},
        ]
    }
    title = 'User Profile'

    return jinja_template.render(user=user, title=title)


# CPython
# In [10]: %timeit jinja_index()
# The slowest run took 5.19 times longer than the fastest. This could mean that an intermediate result is being cached.
# 100000 loops, best of 3: 19.5 µs per loops

# PyPy
# In [11]: %timeit jinja_index()
# The slowest run took 24.93 times longer than the fastest. This could mean that an intermediate result is being cached.
# 100000 loops, best of 3: 3.64 µs per loop


user = {
    'username': 'art',
    'contacts': [
        {'type': 'email',
         'value': 'sasha@example.com'},
    ]
}
title = 'User Profile'

def jinja_index2(user, title):
    return jinja_template.render(user=user, title=title)

# In [9]: %timeit jinja_index2(user, title)
# 100000 loops, best of 3: 17.5 µs per loop
