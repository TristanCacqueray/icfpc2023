# JKRT-2023

> Checkout the companion blog post: https://tristancacqueray.github.io/blog/icfpc2023

Run the tool `cabal run -O2 progcon -- --help`

## Usage

- Solve or improve a problem solution: `progcon solve 42`

- Submit an existing solution: `progcon submit 42`

- Visualize a solution: `progcon render 42 [problems/42-solution.json]`

- Run test: `progcon test`

## nix

```ShellSession
# Start a dev env
nix develop -c ghcid

# Run the tool
nix run
```

## SDL_GL_CreateContext error and nix

If the program fails with:

```ShellSession
SDLCallFailed {sdlExceptionCaller = "SDL.Video.glCreateContext", sdlFunction = "SDL_GL_CreateContext", sdlExceptionError = "Invalid window"}
```

You need to use `nixGL`. To install the right version (see nixpkgs input to match the nixpkgs pin):

```ShellSession
nix profile install --override-input nixpkgs github:NixOS/nixpkgs/3176a8460ff51d1fa223e2962b11bd85543a36ba --impure github:guibou/nixGL
```

Then run:

```ShellSession
nixGL ghcid ...
```
