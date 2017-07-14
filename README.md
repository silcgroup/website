## How this gets released

Any changes made to the master branch of this repository will automatically be
deployed to [silc.ccs.neu.edu](http://silc.ccs.neu.edu). The recommended workflow for major changes is
to thus do work on a branch (or a fork) and then, via a pull request, get
feedback and merge to master. But, for small things changing directly on the
master branch is fine.

The deployment is orchestrated by the `.travis.yml` script and `deploy.sh`.
(Where the former invokes the latter). The `secret` block in the former is an
encrypted login token for an `silc-bot` user that has write access to the
artifact repository (`silcgroup.github.io`), and no other access. Note that this
is the same setup used for deployment of `prl.ccs.neu.edu`.

## How to edit

This is a statically generated site. The code that controls the generation is in
`site.hs`, and if you want to build it locally, install [stack](https://docs.haskellstack.org/en/stable/README/) and then run
`site.hs watch` as a script (i.e., on the command-line `./site.hs watch`) -- it
will download the haskell compiler, all needed libraries, and run it. The first
time, this may take a while! The `watch` command has it automatically serve the
website at `http://localhost:8000` and rebuild whenever you make changes. You
can also run `./site.hs build` and open the files in the `_site` directory
directly, and you can run `./site.hs clean` to get rid of all the built
artifacts (the most likely reason to do this is if you have changed the
generating code in `site.hs` but haven't changed any of the source files, so the
dependency management won't realize it should rebuild stuff).

The main pages are either markdown files you can edit directly (e.g.,
`index.markdown` or `projects.markdown`) or they are files that are generated
from a data file in YAML format (i.e., `publications.yaml`, `people.yaml`).
Changing any of these source files will cause the corresponding page to be
regenerated.

Every page uses templates from the `templates` directory. The `base.html`
template is used for everything, and the `publications.html` and `people.html`
templates are used to build those pages respectively, using the data from the
corresponding YAML file.

Any file in `static` or `css` will be copied over as-is, so you can add or
change things there directly.

## Changing the generator

Ideally, most changes will not require any changes to the site generator
(`site.hs`), but if they do, here is some advice. This site is built using
`Hakyll`, which is a static site generator written in Haskell. The `main` block
is what drives the generator, by matching particular source files and then
describing how they are built into output artifacts. 

Some of these are simple, like the files in `static`, where everything is just
copied verbatim. The markdown files are slightly more complicated, because we
want the urls to look like "/projects", which means we actually generate
"/projects/index.html", and we do that using `pandoc` (which can turn almost any
text format into almost any other one, in this case we are just dealing with
markdown but we could switch that if people prefer!).

The YAML files are more complicated, because we have to parse the files into
data types (which are defined at the bottom of the file), and then define
template `Context` fields for the template (in particular, we make a
`listField`, which is then used in a template as `$for(field_name)$`, and we
have to then give it the context to use for each item, which has the fields of
each item that we parsed).
