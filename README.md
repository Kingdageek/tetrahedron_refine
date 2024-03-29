# Tetrahedron Refine

**UPDATE:** the program reads an old mesh from the `./src/input` folder: two required files

- `nodes.dat` with same format as above
- `elements.dat` with same format as element file above
- The program can now refine an input mesh irregularly, and the visualization files are now generated on the fly
  i.e. No need to run `visualize_mesh.f90` after a refining process ends.

This program refines a tetrahedron coarse mesh level by level resulting in finer meshes
with level depth.
It starts off with one tetrahedron with hardcoded coordinates, the idea is to eventually
read a tetrahedron mesh of multiple elements, the nodes of those elements and their coordinates
from a given file or files and continue refining.

`tetrahedron_refine.f90` creates a general `nodes.dat` file, an elements file and a file for
hanging nodes level by level.

- `nodes.dat` with format: _global node number_, _local node number_, _x_, _y_, _z_
- `elements_{REFINEMENT_LEVEL}.dat` with format:
  _element_label_, _0_, _node connectivity to form element_
- `hangingnodes_{REFINEMENT_LEVEL}.dat` with format:
  _element_label_, _0_, _global node numbers of hanging nodes for this element_

`visualize_mesh.f90` creates `.vtu` files for visualization in paraview using the nodes and
element files generated from above

- `visualize_mesh_{REFINEMENT_LEVEL}.dat`: First download and install the Paraview app, then
  open this in the app to load the data and to view the mesh

**ALL FILES GENERATED ARE SAVED IN THE `./src/output` directory.**

You can see example outputs in the `./src/example_output` directory, and example inputs in the `./src/example_input` directory.

#### Compilation

You should have _gfortran_ installed. This was created on linux so, the `.out` files are generated.
On windows, `.exe` files are generated I think.

The `-o` flag specifies what the output file should be called
The `-I` flag tells the fortran compiler where the modules included in the main programs are
The `-J` flag tells the compiler where to dump the resulting compiled modules

```sh
cd src
mkdir output
mkdir input
mkdir compiled_modules
# run this line to test with the example input files
cp -r ./example_input/* ./input
gfortran ./modules/geometry_mod.f90 ./modules/helpers.f90 ./modules/visualization.f90 tetrahedron_refine.f90 -o tetrahedron_refine.out -I ./modules -J ./compiled_modules
```

On Linux, you can `cd src` and then just run the `./compile.sh` file. Ensure the file is executable first.
