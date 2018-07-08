
How to Install Sweetroll
========================

Sweetroll consists of several services written in different programming languages, so it's not a copy-one-binary-and-run deal.
But there's nothing extraordinarily complicated either.

To run Sweetroll, you need:

- any modern UNIX-like operating system (like `FreeBSD <https://www.freebsd.org/>`_) that you know how to manage;
- a service manager like `Runit <http://smarden.org/runit/>`_;
- `PostgreSQL <https://www.postgresql.org/>`_ 10.x;
- `nginx <https://nginx.org/>`_;
- `Node.js <https://nodejs.org/en/>`_ 10.x (or 8.x maybe).

To compile the Sweetroll Backend, you need
- `Glasgow Haskell Compiler (GHC) <https://www.haskell.org/>`_ 8.4.x (or newer, or a bit older maybe, but not too old);
- `stack <https://docs.haskellstack.org/en/stable/README/>`_ (which can actually manage GHC installation for you).

Service run scripts in the examples below are for Runit, but should be easy to translate into any other service manager.
e.g. ``chpst`` changes various things like current user.

Database
--------

Create a database using ``createdb``, e.g.:

.. code-block:: shell

   sudo -u postgres createdb mywebsite

Make a regular user, e.g. ``sweetroll``, with access to that database.

Now open the database shell (``psql mywebsite``) and check if your database's default language for full text search matches your website's:

.. code-block:: sql

   SELECT current_setting('default_text_search_config');

If not, change that setting.

Now import the mf2sql schema using the `migrate <https://github.com/golang-migrate/migrate>`_ tool:

.. code-block:: shell

   cd mf2sql
   migrate -path=migrations -url=postgres://sweetroll@localhost/mywebsite\?sslmode=disable up

(Or just run the migrations manually with ``psql migrations/*.up.sql`` if you don't want to bother with that tool, but you'll have to manually track the migrations!)

Backend
-------

Build the backend using `stack <https://docs.haskellstack.org/en/stable/README/>`_:

.. code-block:: shell

   cd sweetroll-be
   stack install

``stack install`` should install it as ``~/.local/bin/sweetroll``.

Now configure your favorite service manager to run it like so:

.. code-block:: shell

   #!/bin/sh
   exec 2>&1
   export DATABASE_URL="postgres://sweetroll@localhost/mywebsite"
   export SWEETROLL_SECRET="YOUR OWN PSEUDORANDOM VALUE2MGy9ZkKgzex"
   rm /var/run/sweetroll/be.sock
   umask g+w
   exec chpst -u sweetroll:www /home/sweetroll/.local/bin/sweetroll \
           --protocol=unix --socket=/var/run/sweetroll/be.sock
     # or: --protocol=http --port=3030

Adjust the binary location to where you actually put it, of course.

Use something like ``head -c 1024 < /dev/random | openssl dgst -sha512`` to get the random value for the secret.
No, not dynamically in the script.
Copy and paste the generated value into the script, otherwise you'll be logged out on every restart.

Add ``--devlogging`` to get request logs.

Run on demand
^^^^^^^^^^^^^

Sweetroll Backend supports (systemd-compatible) socket activation with the ``--protocol=activate`` flag.
You can use `soad <https://github.com/myfreeweb/soad>`_ to run Sweetroll Backend on demand and stop when there's no activity.

.. code-block:: shell

   # ...
   exec chpst -u sweetroll:www \
      soad -s /var/run/sweetroll/be.sock \
      /home/sweetroll/.local/bin/sweetroll --protocol=activate

Frontend
--------

First, install npm dependencies and build client-side code:

.. code-block:: shell

   cd micro-panel
   npm i
   npm run build
   cd ../sweetroll-fe
   npm i
   npm run assets

Now, here's the service definition:

.. code-block:: shell

   #!/bin/sh
   exec 2>&1
   export IS_PROXIED=1 CACHE_TEMPLATES=1 DO_CACHE=1
   export DATABASE_URL="postgres://sweetroll@localhost/mywebsite"
   export SWEETROLL_SECRET="YOUR OWN PSEUDORANDOM VALUE2MGy9ZkKgzex"
   export ALLOWED_CDNS="https://YOURBUCKET.s3.dualstack.eu-west-1.amazonaws.com/"
   export VAPID_CONTACT="mailto:you@example.com"
   export VAPID_PUBLIC_KEY="AAA123YOUR OWN VAPID KEY"
   export VAPID_PRIVATE_KEY="AAA123YOUR OWN VAPID KEY"
   rm /var/run/sweetroll/fe.sock
   umask g+w
   cd /home/sweetroll/sweetroll/sweetroll-fe
   exec chpst -u sweetroll:www node index.js \
      --protocol=unix --socket=/var/run/sweetroll/fe.sock

(Obviously, the database URL and the secret must be the same across backend and frontend!)

The ``ALLOWED_CDNS`` variable is used to allow external domains for media in the `Content-Security-Policy <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy>`_.

The ``VAPID`` stuff is for Web Push, you can omit it if you don't want to be notified of incoming webmentions (replies/reposts/likes/etc.).
If you do want it though, generate a keypair using `web-push <https://www.npmjs.com/package/web-push>`_'s command line interface.

Sweetroll Frontend also supports (systemd-compatible) socket activation with the ``--protocol=activate`` flag.
But do not run it under soad, it is designed to run all the time, listening for modification notifications.

Media Endpoint
--------------

First, install npm dependencies:

.. code-block:: shell

   cd sweetroll-mu
   npm i

Now, here's the service definition:

.. code-block:: shell

   #!/bin/sh
   exec 2>&1
   export SWEETROLL_SECRET="YOUR OWN PSEUDORANDOM VALUE2MGy9ZkKgzex"
   # With S3 backend:
   export UPLOAD_BACKEND="S3"
   export S3_BUCKET="YOURBUCKET"
   export S3_URL="https://YOURBUCKET.s3.dualstack.eu-west-1.amazonaws.com/"
   export AWS_ACCESS_KEY_ID="ASDF123YOURKEY"
   export AWS_SECRET_ACCESS_KEY="ASDF123YOURSECRET"
   # or with filesystem backend:
   # export FS_ROOT="/var/www/sweetroll-media"
   # export FS_URL="https://unrelenting.technology/media/"
   rm /var/run/sweetroll/mu.sock
   umask g+w
   cd /home/sweetroll/sweetroll/sweetroll-mu
   exec chpst -u sweetroll:www node index.js \
      --protocol=unix --socket=/var/run/sweetroll/mu.sock

Reverse Proxy (nginx)
---------------------

Copy the ``sweetroll-site.nginx.conf`` to your nginx config directory (e.g. ``/usr/local/etc/nginx``) and write your main config like so:

.. literalinclude:: ../nginx.conf
   :language: nginx

Except if you use UNIX domain sockets as in the run scripts above, replace the upstreams with:

.. code-block:: nginx

   upstream sweetroll-be {
   	server unix:/var/run/sweetroll/be.sock;
   	keepalive 5;
   }
   
   upstream sweetroll-fe {
   	server unix:/var/run/sweetroll/fe.sock;
   	keepalive 5;
   }
   
   upstream sweetroll-mu {
   	server unix:/var/run/sweetroll/mu.sock;
   	keepalive 5;
   }

And figure out access permissions for these sockets.
In the scripts above, processes run as ``sweetroll:www``, and ``umask g+w`` makes the sockets they create group-writable.
If nginx is in the ``www`` group, it now has access to these sockets.
