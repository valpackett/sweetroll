
Custom Data Types Used in Sweetroll
===================================

Sweetroll stores all the things as Microformats2 JSON objects.
As in, ALL the things.
So naturally, custom object types are used.

Feeds
-----

When Sweetroll renders a page, it looks up the request URL in the database and looks at the mf2 JSON object it has found:

- if it's of type ``h-entry``, it's just displayed as an entry
- if it's of type ``h-feed``, it's displayed as a feed -- but why would you ever store a static feed? -- so,
- if it's of type ``h-x-dynamic-feed``, the contents of the feed are built dynamically using filters!
- if it's of type ``h-x-reader-channel``, pretty much the same (more on that below: :ref:`reader`)

Here's a simple example of a dynamic feed:

.. literalinclude:: ../example-feeds/articles.json
   :language: json

The custom ``feed-settings`` property controls:

- whether the feed is shown in the list of categories a post belongs to
- whether the feed is shown in the list of the site's feeds (the navigation)
- its order in the navigation

The feed will include all items where

- the URL is on the current domain
- **any** of the conditions in the ``filter`` property match
- **none** of the conditions in the ``unfilter`` property match

The relevant part of the PostgreSQL query responsible for that is:

.. code-block:: sql

   WHERE coalesce(properties @> ANY(filter), True)
   AND NOT coalesce(properties @> ANY(unfilter), False)

Which means that it's using the OR of ``filter``/``unfilter`` *objects*, but each object is an AND of its properties.

So e.g. if you want to display posts from multiple categories, you need to include an object for each one:

.. code-block:: json

   [
     { "category": [ "_reposts" ] },
     { "category": [ "_replies" ] },
     { "category": [ "_likes" ] }
   ]

Tags
^^^^

Feeds can be parameterized with URL query keys by using ``{curly braces}`` in filters (and the feed name).
So feeds for arbitrary tags are dynamically constructed from one "template" feed configuration:

.. literalinclude:: ../example-feeds/tag.json
   :language: json

With the above configuration, the URL ``https://unrelenting.technology/tag?tag=memes`` will show posts that include ``memes`` in the ``category`` property.
And Sweetroll is smart enough to construct URLs like that in lists of tags.

Home Page
---------

The home (index) page is usually just a dynamic feed as seen above, but there are additional properties Sweetroll looks up specifically on the index page:

- ``author`` -- the ``h-card`` that's used to display the site owner's ``note`` and link to the ``photo``
- ``site-settings`` -- various settings related to the site as a whole
    - ``site-name`` -- the name for the ``<title>`` tag and the home link in the header
    - ``code-highlight-theme`` (default: ``github``) -- `highlight.js <https://github.com/isagalaev/highlight.js>`_ theme name for syntax highlighting for code blocks in posts
- ``site-css`` -- custom CSS stylesheet text that extends the default look
- ``site-web-push-subscriptions`` -- list of Web Push subscriptions (for reply notifications), automatically managed by Sweetroll

In the following example, you can see the above properties, as well as some filter tricks (such as an ``index-display`` property that allows explicitly forcing any post to show up or not to show up on the home feed).

.. literalinclude:: ../example-feeds/index.json
   :language: json

.. _reader:

Reader Channels
---------------

Sweetroll works as a `reader <https://indieweb.org/reader>`_: you can subscribe to feeds in channels and read them.
The `Microsub <https://indieweb.org/Microsub>`_ endpoint can manage channels, but you can also create them manually using Micropub using the following structure.

.. code-block:: json

   {
      "type": ["h-x-reader-channel"],
      "properties": {
         "url": ["https://unrelenting.technology/channels/indieweb"],
         "name": ["IndieWeb channel"],
         "subscriptions": [
            {
               "feed": "http://rhiaro.co.uk",
               "entries": ["http://rhiaro.co.uk/2018/07/croatia", "http://rhiaro.co.uk/2018/07/related"]
            }
         ],
         "blocked": [
            "https://asshole.example.com"
         ],
         "muted": [
            "https://annoying.example.com"
         ]
      }
   }

The ``entries`` field of each subscription is managed automatically by Sweetroll's feed fetcher.
The purpose of the field is:

- to store references to all the entries ever seen on that feed, not just the current state of the feed (so you can scroll far down into history)
- to allow removing entries from the channel (Microsub: ``action=timeline`` ``method=remove``)
