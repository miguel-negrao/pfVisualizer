
#Copyright

pfVisualizer is an Haskell program for opengl visualization of data via incoming osc messages. It is (C)opyright 2013–2015 by Miguel Negrão. All rights reserved. pfVisualizer is released under the GNU General Public License and comes with absolutely no warranties. To contact the author, send an email to miguel dot negrao at friendlyvirus dot org.

# description

pfVisualizer can draw triangles or cubes. Each triangle or cube can be assigned one color.n

# executable syntax:

    pfVisualizer port [label] [zoom] [rotx] [roty] [rotz]

label - string
zoom, rotx, roty, rotz - floats

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

2) Currently hosc doesn't build from the nixpkgs from nix-channel, you need the latest nixpkgs from github. Clone to somewhere on your harddrive.

    git clone https://github.com/NixOS/nixpkgs.git

3) run:

    nix-shell -I nixpkgs=path/to/nixpkgs --option extra-binary-caches http://hydra.nixos.org --option extra-binary-caches http://hydra.cryp.to

this will fetch all dependencies and start a shell with everything in place to build pfVisualizer. Then run:

    ghc --make -O2 src/*.hs -o pfVisualizer

# Install with cabal

1) make sure you have haskell and all the libraries needed for opengl installed. If you don't know how to install those libraries, then it's easier to use nix, since it will do this for you.

    cabal sandbox init

    cabal install --only-dependencies

    cabal build
