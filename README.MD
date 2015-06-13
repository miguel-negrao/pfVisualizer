
#Copyright

pfVisualizer is an Haskell program for opengl visualization of data via incoming osc messages. It is (C)opyright 2013–2015 by Miguel Negrão. All rights reserved. pfVisualizer is released under the GNU General Public License and comes with absolutely no warranties. To contact the author, send an email to miguel dot negrao at friendlyvirus dot org.

# description

pfVisualizer can draw triangles or cubes. Each triangle or cube can be assigned one color.

# executable syntax:

Usage: pfVisualizer [OPTION...]
  -p PORT     --port=PORT      TCP port
  -l LABEL    --label=LABEL    label
  -o ZOOM     --zoom=ZOOM      zoom factor
  -x RADIANS  --rotX=RADIANS   rotation around x axis
  -y RADIANS  --rotY=RADIANS   rotation around y axis
  -z RADIANS  --rotZ=RADIANS   rotation around z axis
  -w PIXELS   --width=PIXELS   window width
  -h PIXELS   --height=PIXELS  window height
              --help           show this help

label                  - string
zoom, rotx, roty, rotz - float
port,w,h               - int

# osc messages:

    /quit -> quit program

    /triangles, point1_x, point1_y, point1_z, point2_x, point2_y, point2_z, point3_x, point3_y, point3_z, ...

The floats come in groups of 9, each triangle requires 3 points or 9 floats.

    /cubes, point1_x, point1_y, point1_z, point2_x, point2_y, point2_z, point3_x, point3_y, point3_z, ...

The floats come in groups of 3, each cube requires 3 floats.

    /colors, color1_r, color1_g, color1_b, color2_r, color2_g, color2_b, ...

/color messages are sent after /cubes or /triangles messages
colors come in groups of 3 floats.
If before you sent a /triangles message then the number of floats passed to /colors should be one third of the number of floats passed to /triangles. It's one color per triangle, and one triangle needs 9 floats.
If before you sent a /cubes message then the number of floats passed to /colors should be the same as the number of floats passed to /triangles. It's one color per cube, 3 floats to define the center of the cube and 3 floats to define the color.

# Install with the nix package manager

1) Install the nix package manager from https://nixos.org/nix/. This can be done by running:

    curl https://nixos.org/nix/install | sh

2) Currently hosc doesn't build from the nixpkgs from nix-channel, you need the latest nixpkgs from github. Clone to somewhere on your harddrive (commit d6cd4c57dc6a3cae3a3675fea38a7d564babdd7e known to work).

    git clone https://github.com/NixOS/nixpkgs.git

3) run:

    nix-build -I nixpkgs=path/to/nixpkgs --option extra-binary-caches http://hydra.nixos.org --option extra-binary-caches http://hydra.cryp.to build.nix

this will fetch all dependencies build the application which will appear as the file "result" in this directory.

# Install with cabal

1) make sure you have haskell and all the libraries needed for opengl installed. If you don't know how to install those libraries, then it's easier to use nix, since it will do this for you.

    cabal sandbox init

    cabal install --only-dependencies

    cabal build

# Compiling a working binary on OSX

In order to compile a binary on OSX (10.10) that runs you need to comment out the line

     actionOnWindowClose $= MainLoopReturns

It uses freglut code not available in OSX which uses normal GLUT.
