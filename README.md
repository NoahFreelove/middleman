# Middleman
Middleman is a tool to help add more security to AI agents' API requests.

## Why?
Not all services which have API keys have custom permission levels. Github for example has great granular access levels, but some services only have
simple Read and Write toggles. Middleman lets you get inbetween your Agent and the API, holding onto the API key so that the Agent can't access it, and
only forwarding requests from routes you authorize.

Middleman also lets you run scripts (in python or haskell) before data is sent to the API server and before data is sent back to the Agent. This should
theoretically let you scrub it of any info you don't want your agent (like openclaw 🌝 seeing)

## Bells and Whistles

Middleman supports these operations:

* Forwarding API requests
* Parameters in routes ("my_service/{ID}/get")
* Blanket method permissions (Allowing all GET, PATCH, POST, etc.)
* Inverting path permissions (Allow everything BUT listed paths)



## Installation

You need GHC (Haskell compiler) and Cabal (build tool). The easiest way to get both is [GHCup](https://www.haskell.org/ghcup/):

```bash
# Mac or Linux — installs ghcup, ghc, and cabal
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Follow the prompts, then restart your shell (or `source ~/.bashrc` / `source ~/.zshrc`). Verify with:

```bash
ghc --version
cabal --version
```

## Running it

```bash
# Build
cabal build all

# Run with your config
cabal run middleman -- --config config.json
```

## How do I configure it?

Read the config.example.json, for a good example of how to config middleman. 
Services are split into... "services". Namespaces for your routes. Routes have a `path` which is the input
route, i.e if you run middleman locally and want to access `my_service/users` you would curl `localhost:8080/my_service/users` (the `path` in
this case would just be `/users`).

`targetPath` is where you want it to go (relative to the service name). So if I wanted that route to redirect to `my_service.com/get/users`,
assuming my service name is `my_service`, the `targetPath` would just be "/get/users".
