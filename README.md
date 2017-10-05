# Rokugan

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [node.js](https://nodejs.org) 6.11 or higher
* A JS package manager: [yarn](https://yarnpkg.com) or [npm](http://npmjs.com/)

Although is not a Fable requirement, on macOS and Linux you'll need [Mono](http://www.mono-project.com/) for other F# tooling like Paket or editor support.

## Technology stack

* F# everywhere
* .net core 2.0
* [Suave](http://suave.io) as web server
* [Fable](http://fable.io) for client - compiling F# into Javascript (using webpack)
* Fable.Elmish for elm-like structure of F# client, Fable.React (and React) for rendering part 


## Editor

VS Code + [Ionide](http://ionide.io/)

## Installing the Fable template

In a terminal, run `dotnet new -i Fable.Template` to install or update the template to the latest version.

## Building and running the app

### Web server

Run `dotnet run` in Rokugan.web.server folder. This will restore packages through `Paket` then it will build all server projects and start Suave web server.

### Web client

> In the commands below, yarn is the tool of choice. If you want to use npm, just replace `yarn` by `npm` in the commands.

* Install JS dependencies: `yarn install`
* **Move to `Rokugan.web.client/src` folder**
* Install F# dependencies: `dotnet restore`
* Build client by `dotnet fable yarn-build`, this will create javascript bundle file `bundle.js`

## Project structure

### Paket

[Paket](https://fsprojects.github.io/Paket/) is the package manager used for F# dependencies. It doesn't need a global installation, the binary is included in the `.paket` folder. Other Paket related files are:

- **paket.dependencies**: contains all the dependencies in the repository.
- **paket.references**: there should be one such a file next to each `.fsproj` file.
- **paket.lock**: automatically generated, but should be committed to source control, [see why](https://fsprojects.github.io/Paket/faq.html#Why-should-I-commit-the-lock-file).
- **Nuget.Config**: prevents conflicts with Paket in machines with some Nuget configuration.

> Paket dependencies will be installed in the `packages` directory. See [Paket website](https://fsprojects.github.io/Paket/) for more info.


### Rokugan.Shared

- Project shared both by client and server codebase.

### Rokugan

- Main game logic code library
- Can be run with `dotnet run` as console app with very basic UI

### Rokugan.Tests

- Test project for the game library
- Contains support for fsi scripting

### Rokugan.web.server

- Web server using Suave
- Provide simple api to Rokugan library and serialization of shared types 

### Rokugan.web.client

- F# codebase compiled by Fable into javascript/react code
- using elm-like architecture

#### yarn/npm 

- **package.json**: contains the JS dependencies together with other info, like development scripts.
- **yarn.lock**: is the lock file created by yarn.
- **package-lock.json**: is the lock file understood by npm 5, if you use it instead of yarn.

> JS dependencies will be installed in `node_modules`. See [yarn](https://yarnpkg.com) and/or [npm](http://npmjs.com/) websites for more info.

#### Webpack

[Webpack](https://webpack.js.org) is a bundler, which links different JS sources into a single file making deployment much easier. It also offers other features, like a static dev server that can automatically refresh the browser upon changes in your code or a minifier for production release. Fable interacts with Webpack through the `fable-loader`.

- **webpack.config.js**: is the configuration file for Webpack. It allows you to set many things: like the path of the bundle, the port for the development server or [Babel](https://babeljs.io/) options. See [Webpack website](https://webpack.js.org) for more info.


