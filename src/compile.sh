# cd src
mkdir output
mkdir input
mkdir compiled_modules
# cp -r ./example_input/* ./input
# gfortran ./modules/geometry_mod.f90 tetrahedron_refine.f90 -o tetrahedron_refine.out -I ./modules -J ./compiled_modules
gfortran ./modules/geometry_mod.f90 ./modules/helpers.f90 ./modules/visualization.f90 tetrahedron_refine.f90 -o tetrahedron_refine.out -I ./modules -J ./compiled_modules
# gfortran ./modules/visualization.f90 ./modules/helpers.f90 visualize_mesh.f90 -o visualize_mesh.out -I ./modules -J ./compiled_modules