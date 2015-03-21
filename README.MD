This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Haskell app for opengl visualization of data via incoming osc messages

Can draw triangles and cubes, nothing else.

syntax:

    pfVisualizer port [label] [zoom] [rotx] [roty] [rotz]

label - string
zoom, rotx, roty, rotz - floats


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
