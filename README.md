# Theta

Theta lets you define communication protocols between applications using algebraic data types. You can write your schema once, use it to generate user-friendly Haskell, Rust and Python bindings and then share data between programs using [Avro][avro].

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

// Comments starting with /** or /// are documentation comments attached to definitions

/** The recording artist for an album.
  *
  * We keep track of people and bands separately because
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
  length    : Int, // length in seconds
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

### Nix

Theta uses [Nix] to manage dependencies and builds across several languages.

To get started, [install Nix][install-nix] if you don't already have it:

``` shell
curl -L https://nixos.org/nix/install | sh
```

This project uses [Nix Flakes][flakes] which is a relatively new Nix feature that needs to be enabled in the your Nix config file. You can create a Nix config file and enable Flakes with the following command:

``` shell
mkdir -p ~/.config/nix
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
```

If this causes problems, take a look at [the Nix wiki][flakes-wiki] for possible solutions.

[flakes]: https://serokell.io/blog/practical-nix-flakes

[flakes-wiki]: https://nixos.wiki/wiki/flakes#Installing_flakes

### Building

Once you have Nix set up, you can build Theta and check that it works. Nix links the result of the build with a symlink named `result`:

``` shell
nix build
result/bin/theta --version
```

If you get an error about `build` being an experimental command that is not enabled, see [the Nix section](#nix) for how to enable it.

Which should result in something like:

```
Theta 1.0.0.0
```

The project is split into several components that are built separately:

  1. `theta`:  The core Haskell library and executable
  2. `rust`: The Rust support library
  3. `python`: The Python support library
  4. `test`: Tests for executable functionality and generated code (in Python, Rust and Kotlin)

Each of these can be built separately. For example, to run the cross-language tests, you would write:

``` shell
nix build .#test
```

`theta` is the default target, so `nix build` is the same as `nix build .#theta`.

### Development Tools

Nix manages development environments for each of the components—like a cross-language virtualenv. The environments have the dependencies you need to work on the respective component—`cabal` for Haskell development, `cargo` for Rust... etc.

```
nix develop .#theta  # cabal and Haskell packages
nix develop .#rust   # cargo, Rust packages, theta itself
nix develop .#python # Python, Python packages, theta itself
```

Give it a try:

``` shell
nix develop .#theta
cabal --version
```

This should give you an output something like this, even if you don't have `cabal` installed on your system:

```
cabal-install version 3.6.2.0
compiled using version 3.6.2.0 of the Cabal library
```

The first time you run `nix develop` for a target it might take some time to download and build the tools and dependencies it needs, but it should run instantly on subsequent times.

Once you've got everything set up, you can use [`direnv`][direnv] to automatically enter and exit development shells based on the directory you're in; while `direnv` has Nix integration built in, using [`nix-direnv`][nix-direnv] integration can provide better performance. While you don't strictly have to, it's convenient to install and manage both `direnv` and `nix-direnv` with Nix.

[Nix]: https://nixos.org/
[install-nix]: https://nixos.org/download.html#nix-quick-install
[direnv]: https://direnv.net/
[nix-direnv]: https://github.com/nix-community/nix-direnv


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
