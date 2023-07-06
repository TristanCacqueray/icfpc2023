# JKRT-2023

```ShellSession
# Hot reload the interface
ghcid --command "cabal repl progcon" -W --test ProgCon.GUI.main
```

## nix

```ShellSession
# Start a dev env
nix develop -c ghcid --command "cabal repl progcon"

# Run the tool
nix run
```

## SDL_GL_CreateContext error and nix

If the program fails with:

```ShellSession
SDLCallFailed {sdlExceptionCaller = "SDL.Video.glCreateContext", sdlFunction = "SDL_GL_CreateContext", sdlExceptionError = "Invalid window"}
```

You need to use `nixGL`. To install the right version (see hspkgs input to match the nixpkgs pin):

```ShellSession
nix profile install --override-input nixpkgs github:NixOS/nixpkgs/22c5bd85d8478e24874ff2b80875506f5c3711a6 --impure github:guibou/nixGL
```

Then run:

```ShellSession
nixGL ghcid ...
```
