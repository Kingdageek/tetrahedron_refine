# cd src
mkdir output
mkdir compiled_modules
gfortran ./modules/geometry_mod.f90 tetrahedron_refine.f90 -o tetrahedron_refine.out -I ./modules -J ./compiled_modules

gfortran ./modules/visualization.f90 visualize_mesh.f90 -o visualize_mesh.out -I ./modules -J ./compiled_modules