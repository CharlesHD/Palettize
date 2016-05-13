# About
Apply the same palette to multiples images in a directory, producing images with the same palette.
# Installation
With stack :
```bash
stack build
stack install // will copy binary in stack bin directory
```

# Usage
If the binary is in your PATH you can just use
```bash
palettize palette dir
```
else you can use `stack exec`
```bash
stack exec palettize palette dir
```

palette must be a N x 1 image with N between 1 and 256.
dir must be the directory containing images you want to apply palette to.

It will create a res directory in dir containing all transformed images.
