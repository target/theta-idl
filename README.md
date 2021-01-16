# Theta

Theta lets you define communication protocols between applications using algebraic data types. You can write your schema once, use it to generate user-friend Haskell, Rust and Python bindings and then share data between programs using [Avro][avro].

Already use Avro in your system? You can introduce Theta incrementally. All Theta types compile to Avro schemas so you can use Theta to define interfaces for your component, work with external project that understand Avro but not Theta and integrate with services like [Schema Registry][schema-registry].

See the [user guide](guide/index.md) for reference documentation.

Theta's original idea was inspired by [atd], a tool for specifying JSON schemas using OCaml types.

[schema-registry]: https://docs.confluent.io/current/schema-registry/index.html
[avro]: https://avro.apache.org/
[atd]: https://github.com/ahrefs/atd

## Example

Here's an example Theta schema split over two files (`ids.theta`) and (`music.theta`), available in the `guide/example` directory.

`ids.theta`:
```
language-version: 1.0.0
avro-version: 1.0.0
---

type AlbumId = {
  id : String
}
```

`music.theta`:
```
import ids

/** We keep track of people and bands separately because
  * people are usually sorted by part of their name (ie
  * last name) while bands are always sorted by their
  * whole name.
  */
data ArtistName = Band { name : String }
                | Person {
                    sorting_name   : String,
                    name_remainder : String?
                  }
type Track = {
  title     : String,
  length    : Int,
  artists   : [ArtistName]
}

data Album = {
  id        : ids.AlbumId,
  title     : String,
  published : Date,
  artists   : [ArtistName],
  tracks    : [Track]
}
```

Let's try using this schema from Python. Starting in this directory, set the `THETA_LOAD_PATH`:

```
export THETA_LOAD_PATH=guide/example
```

Then we can compile the `Album` record to an Avro schema file (`.avsc`):

```
theta avro type -t music.Album -o music.avsc
```

The documentation for the `ArtistName` type is preserved in the Avro schema's `doc` field in the `ArtistName` type definition.

Next, let's generate Python code for working with music metadata. This will generate two files (`ids.py` and `music.py`) so, to keep things organized, we'll group it into a Python module called `example`:

```
mkdir -p example
theta python -m music --prefix example -o example
```

This creates the following directory structure:

```
example
 ├── __init__.py
 ├── ids.py
 └── music.py
```

Now we can use the `Album` class from Python:

`albums.py`:
```python
import datetime

from example.music import Album, Person, Track
from example.ids import AlbumId

favorite_album = Album(
      AlbumId("1"),
      "Whenever You Need Somebody",
      datetime.date(1987, 7, 27),
      [Person("Astley", "Rick")],
      [Track("Never Gonna Give You Up", 215, [Person("Astley", "Rick")])]
    )
    
# Write "album.avro" file:
with open("album.avro", "wb") as out_stream:
    favorite_album.to_avro(out_stream)
    
# Read it back:
with open("album.avro", "rb") as in_stream:
    album = Album.from_avro(in_stream)
    print(album)
```

You can also read/write the [Avro container format][containers]:

```
with open("album_container.avro", "wb") as out_stream:
    Album.write_container([favorite_album], out_stream)
    
with open("album_container.avro", "rb") as in_stream:
    albums = Album.read_container(in_stream)
    print(albums[0])
```

To run this, you will need Theta's Python package from the `python` directory. You can install this with `pip`:

```
pip install -e python
```

Now let's run it with `python albums.py`, which gives us:

```
Album(id=AlbumId(id='1'), title='Whenever You Need Somebody', published=datetime.date(1987, 7, 27), artists=[Person(sorting_name='Astley', name_remainder='Rick')], tracks=[Track(title='Never Gonna Give You Up', length=215, artists=[Person(sorting_name='Astley', name_remainder='Rick')])])
```

[containers]: https://avro.apache.org/docs/1.10.0/spec.html#Object+Container+Files

## Emacs Integration

Theta comes with an Emacs mode. To install it, copy `emacs/theta-mode.el` into your [load path][load-path] somewhere, then add the following lines to your `.emacs` file:

```
(require 'theta-mode)
(add-to-list 'auto-mode-alist '("\\.theta" . theta-mode))
```

[load-path]: https://www.emacswiki.org/emacs/LoadPath

## Developing Theta

Theta uses [Nix] to manage dependencies and builds across several languages.

To get started, [install Nix][install-nix] if you don't already have it:

```
curl -L https://nixos.org/nix/install | sh
```

Once you have Nix set up, you can build Theta and check that it works:

```
nix-build
result/bin/theta --version
```

You can install and uninstall the `theta` executable for your user:

```
nix-env -i -f theta # install
nix-env -e theta    # uninstall
```

You can also run tests across all the languages Theta supports:

```
nix-build test
```

You can get the right version of development tools you need to work on Theta's components (core Haskell library, Python, Rust... etc) using Nix shells:

```
nix-shell theta  # cabal, ghc and Haskell packages
nix-shell rust   # cargo, rustc and Rust packages
nix-shell python # Python and Python packages
```

For example, if you want to run Theta's Haskell tests locally with `cabal`, you can run:

```
nix-shell theta
cd theta
cabal test
```

And to run tests on the Rust support library:

```
nix-shell rust
cd rust
cargo test
```

To manage development shells automatically, check out [lorri].

[Nix]: https://nixos.org/
[install-nix]: https://nixos.org/download.html#nix-quick-install
[lorri]: https://github.com/target/lorri

## Colophon

```
                COULSON
        What are we calling this anyway?
                KOENIG
        The Theta Protocol, sir.
                COUSLON
        Theta Protocol. I'll need you to write that down for me.
                KOENIG
        I gotta tell you, it's not easy keeping something as big as a Helicarrier a secret.
```

[*Theta Protocol*](http://marvelcinematicuniverse.wikia.com/wiki/Theta_Protocol), MCU Wiki
