program visualize_mesh
   use visualization
   use helpers
   implicit none

   ! Specify total number of levels
   ! First pass nodes.dat file and build "node_coords" array
   ! Then, for each level:
   ! It scans "output" directory for element files
   ! It parses those files and save appropriate inputs in the "element_arr"
   ! Doesn't have to do this for each level. It can do it only for the level specified
   ! That should be the default behaviour, maybe a flag to force it to do for every level
   ! First parses the node file and stores all coordinates in the array
   ! calculates on the fly the number of nodes expected for the level specified
   ! and also the number of elements for the level specified, then parses the elements_{level} file
   ! and saves the connectivity of the nodes to the "element_arr"
   ! Then passes these variables to the 'generate_paraview_output' subroutine to generate
   ! paraview XML files for display
   integer, dimension(:,:), allocatable :: element_arr
   double precision, dimension(:,:), allocatable :: node_coords
   integer :: num_nodes, num_elements, level
!    integer :: file_status
   integer :: i, j, k
   character(len=100) :: element_filename, nodes_filename, old_mesh_nodes_filename
!    character(100) :: line            ! Character buffer to hold each line of input

   write(*,*) "########### MESH VISUALIZATION PROGRAM ##########"
   write(*,*) "Ensure the element and nodes files exist"
   write(*,*) "Enter level as int to visualize: "

   read(*,*) level

   write(element_filename, "(A18,I1,A4)") "./output/elements_", level, ".dat"
   nodes_filename = "./output/nodes.dat"
   old_mesh_nodes_filename = "./input/nodes.dat"
   ! These calculations will only work for uniform refinement, i.e. no one element in
   ! these level was further refined without refining the others
   ! calculate total number of elements expected for specified level
   ! num_elements = (8 ** (level-1))
   ! just count the elements in the file
   call countLines(element_filename, num_elements)
   ! calculate total number of nodes expected for specified level
   ! Some loop to do this.
   ! Level 1: ! element. 4 nodes. adds 6 midpoint nodes and then has 10 total nodes
   ! Level 2: 8 elements out of 1. each adding 6 midpoint nodes + nodes from previous level
   ! This only applicable if no elements or nodes at the start
   ! num_nodes_i (num_nodes at level i) = 8**(i-1) * 6 + (num_nodes_(i-1))
   ! For old mesh:
   ! num_nodes_i (num_nodes at level_i) = 8**i * 6 + num_nodes_(i-1)

   ! start with the number of nodes from the old input mesh
   call countLines(old_mesh_nodes_filename, num_nodes)
   write(*,*) "before calculating: num_nodes is ", num_nodes
   do i = 1, level
      num_nodes = 8**(i) * 6 + num_nodes
   enddo
   write(*,*) "after calculating: num_nodes is ", num_nodes

   ! allocate space for element_arr and node_coords
   ! 4 nodes per element
   allocate(element_arr(num_elements, 4))

   ! 3 cartesian directions for coordinates (x,y,z)
   allocate(node_coords(num_nodes, 3))

   ! To generate element_arr and node_coords
   open(unit=42, file=element_filename, status='old', action='read')
   do i = 1, num_elements
      ! The current element files have the ordinal no. of the element and a zero first
      ! before the global node numbers describing the element's connectivity
      ! j and k are placeholders for the initial values
      read(42, *) j, k, element_arr(i,1:4)
      write(*,*) "Writing Elements: "
      write(*,*) j, k, element_arr(i, 1:4)
   end do
   close(42)

   open(unit=43, file=nodes_filename, status='old', action='read')
   do i = 1, num_nodes
      ! same here, j and k are placeholders for the global & local node numbers
      ! just the node coordinates are needed
      read(43, *) j, k, node_coords(i,1:3)
      write(*,*) "Writing Nodes: "
      write(*,*) j, k, node_coords(i,1:3)
   end do
   close(43)
   ! dim=0 for size along the horizontal axis (default) and dim=1 for size along the vertical axis
   write(*,*) "Size of elements array", size(element_arr, dim=1)
   write(*,*) "Size of nodes array", size(node_coords, dim=1)
   call generate_paraview_output(level, element_arr, node_coords)

end program visualize_mesh
