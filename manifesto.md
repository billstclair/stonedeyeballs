# Manifesto

This document outlines my philosphy of webapps.

## My Philosophy of Webapps

My webapps are coded mostly in Elm. They compile to a single `elm.js`
file. Downloading that file, and associated `index.html` and images
and other JavaScript files is the ONLY interaction your browser has
with my server. In particular, your browser will never WRITE anything
to my server. I remember nothing about you, since I never know more
than your IP address, when you download the code, and I don't remember
that.

Of course, you can't take my word for that. You'll have to either
watch your network traffic, or rely on the opinion of somebody you
already trust.
