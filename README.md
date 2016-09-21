# Arrowsmith

Arrowsmith is an augmented editor for the functional reactive programming language [Elm](http://www.elm-lang.org/), which lets you evaluate your code right inside the editor, and instantly updates whenever you change anything.

**If you want to find out more about why I created this, [watch my presentation](https://www.youtube.com/watch?v=csRaVagvx0M) or [read my report](https://github.com/lachenmayer/arrowsmith/blob/master/report.pdf). :)**

![Screenshot](https://raw.githubusercontent.com/lachenmayer/arrowsmith/master/screenshot.png)

Arrowsmith loads your Elm project directly from a GitHub repository, and compiles your Elm files for you. It shows you each of the definitions in your Elm file separately. Each definition has a type (either a type you defined yourself, or an inferred type), and a little "play" button. Clicking the "play" button evaluates that definition, displaying it just below the definition. If you change any part of your code, the values will update immediately.

Some values have special representations: values of type `Color` show the color, rather than a textual representations. `List`s and `Dict`s display each item individually. Time-varying numbers (`Signal Float`) are displayed as a graph that changes over time.

## Installation

Installation is ~~instant~~ ~~easy~~ not impossible.

Since the project requires exact versions of GHC (7.8.4) and Cabal (1.22)—don't ask me why, unless you want me to rant at you—you're best off building it with Docker.

I have only tested this on OS X 10.9. Please let me know if this doesn't work in an [issue](https://github.com/lachenmayer/arrowsmith/issues), and I'll see if there's any hope fixing it.

### 0. Install Docker

**On OS X**:

1. Install [VirtualBox](https://www.virtualbox.org/wiki/Downloads)
2. `brew install docker boot2docker`
(Install [Homebrew](http://brew.sh/) first if you don't have it already.)

**On Linux**: Use your favourite package manager to install Docker. If you don't know how to do that, `apt-get install docker` will probably do the trick.

If that doesn't work, check out the [Docker docs](https://docs.docker.com/installation/). Docks. Heh.

### ½. (OS X only) Start a `boot2docker` VM

Run the following commands:

    boot2docker init
    boot2docker up

The second command will tell you to set some environment variables, you need to do that. **Docker will not work otherwise.**

### 1. Build the Docker image

Now, from this repo's root directory run:

    docker build -t lachenmayer/arrowsmith deploy/

Go grab a coffee or go for a run or something, because this will take *forever*: this script downloads an entire operating system, two compilers, two language-specific package managers, about 23498273489234 Haskell project dependencies, builds these 23498273489234 dependencies from scratch, downloads about 782347927348923748923 JavaScript dependencies (including at least 2 more compilers...), mushes most of those into a big ol' JavaScript file, and then finally compiles the main executable. Awesome. The things we do for reproducible builds these days...
The last line should say something like `Successfully built 1a94b5abebbd`, otherwise something messed up.

### 2. Start the Arrowsmith server

    docker run -t -p 8000:8000 -i lachenmayer/arrowsmith cabal run

The last line should look like this:

    Listening on http://0.0.0.0:8000/

If it does, bingo! Nearly ready to go.

### 3. Open an Elm file in Arrowsmith

**On OS X**: The IP address `0.0.0.0` inside the Docker VM does not correspond to `0.0.0.0` on your machine, so you'll have to find out the fake IP that boot2docker creates, using `boot2docker ip`.

Or just copy & paste the following into your terminal, which will open up the "Clock" module from the [sample repo](https://github.com/lachenmayer/arrowsmith-example).

    open http://$(boot2docker ip):8000/lachenmayer/arrowsmith-example/Clock

**On Linux**: You should be able just to use your localhost IP address. If you're lazy, just click this. [http://0.0.0.0:8000/lachenmayer/arrowsmith-example/Clock](http://0.0.0.0:8000/lachenmayer/arrowsmith-example/Clock)

### 4. PROFIT! (kinda)

Loading the first file will probably take a long time, because Arrowsmith has to download & compile a lot of Elm dependencies. If the browser window is completely black, just refresh it a couple of times, it should show up eventually.

If you're feeling adventurous, you can use any GitHub `user/project` as a URL path, as long as the corresponding repo is an Elm project (ie. it has a valid `elm-package.json`). For example, you could load the Elm core library code by going to `http://0.0.0.0:8000/elm-lang/core`.

**Cleaning up**: once you've realised how broken this project is, you'll probably want to remove it. On OS X, you'll want to run `boot2docker destroy`, which will remove the Docker VM image. This will otherwise take up several gigabytes of your sleek MacBook Pro's dainty SSD.

### Caveats

This is *research-quality* code, ie. *totally broken*.
Don't expect anything. If you're feeling nice and/or feeling pity, open an [issue](https://github.com/lachenmayer/arrowsmith/issues).

That being said, there are some known issues:

- Elm files with [ports](http://elm-lang.org/learn/Ports.elm) will not work. Not sure if that will ever be supported.
- If your `main` Element is relying on `Window.dimensions` to set its size, it will not show underneath a definition. (It should however show if you evaluate the whole module)
- Hot-swapping is broken, so disabled.
- Editing definitions will sometimes not do what you expect - I'm still trying to figure out what the edge cases are, [issues](https://github.com/lachenmayer/arrowsmith/issues) are always welcome. If the file seems irredeemably broken, use the plain text view to get you back on track.

The program is glaringly incomplete:

- Editing imports and types is currently not possible from the structured editor view. Use the plain text fallback view instead. (click on the "file" icon in the menu bar).
- There's currently no "undo" feature.
- There's no way to get any changes out of the editor - you should be able to push to GitHub.
- You currently can't create new files from within the editor - to create a file, push it to the relevant GitHub repo.
