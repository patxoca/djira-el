.. -*- ispell-local-dictionary: "en" -*-

.. $Id:$


Introduction
============

``djira-el`` is an emacs client for `djira <https://github.com/patxoca/djira>`_
and a library of functions built on top of that.

``djira`` is a django app that makes easy writing simple REST APIs
intended as development helpers. ``djira-el`` consumes those APIs from
within emacs.

For an actual example take a look at
`django-el <https://github.com/patxoca/django-el>`_.


``djira-el`` provides three levels of abstraction:

- In the lowest level ``djira-call`` is the generic function used to
  perform calls to the endpoints. Returns the value as an Emacs Lisp
  data structure and can control the cache.

- The ``djira-api-xxx`` functions create a second layer of abstraction
  on top of ``djira-call``. There is a funcion for each endpoint. They
  define the format of the call, the interaction with the cache and
  may manipulate the format, not the content, of the value returned by
  the endpoint.

- The third level of abstraction is composed by the ``djira-info-xxx``
  functions. These functions return specific pieces of information,
  hidding the details of the data stracture returned by the endpoints.
