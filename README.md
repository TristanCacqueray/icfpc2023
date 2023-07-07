# JKRT-2023

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
