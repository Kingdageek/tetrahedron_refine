program tetrahedron_refine
   use geometry_mod
   implicit none

!    type tetrahedron
!       ! vertice nodes
!       integer :: v1, v2, v3, v4
!       ! Nodes at midpoints of the edges
!       integer :: v1v2_mid, v1v3_mid, v1v4_mid, v2v3_mid, v2v4_mid, v3v4_mid
!       ! coordinates of tetrahedron. First 4 rows are vertex coords and last 6 rows are midpoint
!       ! coordinates.
!       ! 10 nodes by 3 cartesian directions (x,y,z)
!       !   rows align as so:
!       !   variable local node number
!       !   v1        1
!       !   v2        2
!       !   v3        3
!       !   v4        4
!       !   v1v2_mid  5
!       !   v1v3_mid  6
!       !   v1v4_mid  7
!       !   v2v3_mid  8
!       !   v2v4_mid  9
!       !   v3v4_mid  10
!       double precision :: coords(10,3)
!    end type tetrahedron
   ! hold array of elements (tetrahedrons) per level
   type(tetrahedron), dimension(:), allocatable :: current_level, next_level
   ! i, j, k are just loop counter variables
   integer :: num_levels, i, j, k, curr_level
   integer :: num_current, num_next, num_nodes
   ! 4 vertices and 6 edges (midpoint nodes)
!    integer :: nodes_per_element = 10
   character(len=128) :: elem_filename, hanging_nodes_filename, nodes_filename
   character(len=9) :: output_dir = "./output/"

   ! Initialize the largest tetrahedron -> coarsest mesh
   allocate(current_level(1))
   ! global node numbers
   current_level(1)%v1 = 1
   current_level(1)%v2 = 2
   current_level(1)%v3 = 3
   current_level(1)%v4 = 4

   ! initialize the coordinates of the vertice nodes
   current_level(1)%coords(1,:) = (/0.0d+0, 0.0d+0, 2.0d+0/)
   current_level(1)%coords(2,:) = (/2.0d+0, 0.0d+0, 0.0d+0/)
   current_level(1)%coords(3,:) = (/0.0d+0, 0.0d+0, 0.0d+0/)
   current_level(1)%coords(4,:) = (/1.0d+0, 2.0d+0, 1.0d+0/)
   ! tracks total number of nodes created
   num_nodes = 0

   nodes_filename = output_dir // "nodes.dat"
   ! Open the output file to save the nodes and their coordinates
   open(unit=19, file=nodes_filename, access="sequential")
   ! just fill for the coarsest mesh vertice nodes just 4. without the midpoint nodes
   do k=1, 4
      num_nodes = num_nodes + 1
      ! Format: global_node_num   local_node_num  x   y   z
      write(19, *) num_nodes, k, current_level(1)%coords(k,:)
   enddo

   ! Get the desired number of levels of refinement
   read(*,*) num_levels

   do curr_level = 1, num_levels
      write(hanging_nodes_filename, "(A9,A13,I1,A4)") output_dir, "hangingnodes_", curr_level, ".dat"
      ! Open the output file to save the hanging nodes
      open(unit=20, file=hanging_nodes_filename, access='sequential')

      ! output file to save elements per level
      write(elem_filename, "(A9,A9,I1,A4)") output_dir, "elements_", curr_level, ".dat"
      ! Open the output file to save the elements
      open(unit=21, file=elem_filename, access='sequential')

      ! Allocate the next level array
      num_current = size(current_level)
      num_next = 8*num_current

      ! deallocate 'next_level' if it still holds values from previous iteration
      if( allocated(next_level) ) deallocate(next_level)
      allocate(next_level(num_next))

      ! 'j' tracks the total number of elements per level
      j = 1
      do i = 1, num_current
         ! first label/set the midpoint global node numbers
         ! The midpoint nodes are set to increment by 1. Increasing the total number of
         ! global nodes
         current_level(i)%v1v2_mid = num_nodes + 1
         current_level(i)%v1v3_mid = current_level(i)%v1v2_mid + 1
         current_level(i)%v1v4_mid = current_level(i)%v1v3_mid + 1
         current_level(i)%v2v3_mid = current_level(i)%v1v4_mid + 1
         current_level(i)%v2v4_mid = current_level(i)%v2v3_mid + 1
         current_level(i)%v3v4_mid = current_level(i)%v2v4_mid + 1

         ! initialize the midpoint node coordinates
         ! for v1v2_mid: x = (v1_x + v2_x) / 2; y = (v1_y + v2_y) / 2; z = (v1_z+v2_z)/2
         current_level(i)%coords(5,1) = (current_level(i)%coords(1,1) + &
            current_level(i)%coords(2,1) )/ 2
         current_level(i)%coords(5,2) = (current_level(i)%coords(1,2) + &
            current_level(i)%coords(2,2) )/ 2
         current_level(i)%coords(5,3) = (current_level(i)%coords(1,3) + &
            current_level(i)%coords(2,3) )/ 2
         ! for v1v3_mid: x = (v1_x + v3_x) / 2; y = (v1_y + v3_y) / 2; z = (v1_z+v3_z)/2
         current_level(i)%coords(6,1) = (current_level(i)%coords(1,1) + &
            current_level(i)%coords(3,1) )/ 2
         current_level(i)%coords(6,2) = (current_level(i)%coords(1,2) + &
            current_level(i)%coords(3,2) )/ 2
         current_level(i)%coords(6,3) = (current_level(i)%coords(1,3) + &
            current_level(i)%coords(3,3) )/ 2
         ! for v1v4_mid: x = (v1_x + v4_x) / 2; y = (v1_y + v4_y) / 2; z = (v1_z+v4_z)/2
         current_level(i)%coords(7,1) = (current_level(i)%coords(1,1) + &
            current_level(i)%coords(4,1) )/ 2
         current_level(i)%coords(7,2) = (current_level(i)%coords(1,2) + &
            current_level(i)%coords(4,2) )/ 2
         current_level(i)%coords(7,3) = (current_level(i)%coords(1,3) + &
            current_level(i)%coords(4,3) )/ 2
         ! for v2v3_mid: x = (v2_x + v3_x) / 2; y = (v2_y + v3_y) / 2; z = (v2_z+v3_z)/2
         current_level(i)%coords(8,1) = (current_level(i)%coords(2,1) + &
            current_level(i)%coords(3,1) )/ 2
         current_level(i)%coords(8,2) = (current_level(i)%coords(2,2) + &
            current_level(i)%coords(3,2) )/ 2
         current_level(i)%coords(8,3) = (current_level(i)%coords(2,3) + &
            current_level(i)%coords(3,3) )/ 2

         ! for v2v4_mid: x = (v2_x + v4_x) / 2; y = (v2_y + v4_y) / 2; z = (v2_z+v4_z)/2
         current_level(i)%coords(9,1) = (current_level(i)%coords(2,1) + &
            current_level(i)%coords(4,1) )/ 2
         current_level(i)%coords(9,2) = (current_level(i)%coords(2,2) + &
            current_level(i)%coords(4,2) )/ 2
         current_level(i)%coords(9,3) = (current_level(i)%coords(2,3) + &
            current_level(i)%coords(4,3) )/ 2

         ! for v3v4_mid: x = (v3_x + v4_x) / 2; y = (v3_y + v4_y) / 2; z = (v3_z+v4_z)/2
         current_level(i)%coords(10,1) = (current_level(i)%coords(3,1) + &
            current_level(i)%coords(4,1) )/ 2
         current_level(i)%coords(10,2) = (current_level(i)%coords(3,2) + &
            current_level(i)%coords(4,2) )/ 2
         current_level(i)%coords(10,3) = (current_level(i)%coords(3,3) + &
            current_level(i)%coords(4,3) )/ 2

         ! write current element to element file
         write(21, *) i, 0, current_level(i)%v1, current_level(i)%v2, current_level(i)%v3, &
            current_level(i)%v4

         ! write the hanging nodes to hanging nodes file
         ! from 2nd level
         if(curr_level > 1)then
            write(20, *) i, 0, current_level(i)%v1v2_mid, current_level(i)%v1v3_mid, &
               current_level(i)%v1v4_mid, current_level(i)%v2v3_mid, current_level(i)%v2v4_mid,&
               current_level(i)%v3v4_mid
         endif

         ! write the newly created nodes to the nodes.dat file i.e. the midpoint nodes -> 6 nodes
         ! the midpoint nodes start from 5 local label to 10
         do k=5, 10
            ! Added nodes must reflect on total node number
            num_nodes = num_nodes + 1
            ! Format: global_node_num   local_node_num  x   y   z
            write(19, *) num_nodes, k, current_level(i)%coords(k,:)

         enddo

         ! Carve out the elements for the next level
         ! global node number labels and then assign coordinates accordingly
         next_level(j)%v1 = current_level(i)%v1
         next_level(j)%v2 = current_level(i)%v1v2_mid
         next_level(j)%v3 = current_level(i)%v1v3_mid
         next_level(j)%v4 = current_level(i)%v1v4_mid

         next_level(j)%coords(1,:) = current_level(i)%coords(1,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(5,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(6,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(7,:)
         j = j + 1

         next_level(j)%v1 = current_level(i)%v1v2_mid
         next_level(j)%v2 = current_level(i)%v2
         next_level(j)%v3 = current_level(i)%v2v3_mid
         next_level(j)%v4 = current_level(i)%v2v4_mid

         next_level(j)%coords(1,:) = current_level(i)%coords(5,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(2,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(8,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(9,:)
         j = j + 1

         next_level(j)%v1 = current_level(i)%v1v3_mid
         next_level(j)%v2 = current_level(i)%v2v3_mid
         next_level(j)%v3 = current_level(i)%v3
         next_level(j)%v4 = current_level(i)%v3v4_mid

         next_level(j)%coords(1,:) = current_level(i)%coords(6,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(8,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(3,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(10,:)
         j = j + 1

         next_level(j)%v1 = current_level(i)%v1v4_mid
         next_level(j)%v2 = current_level(i)%v2v4_mid
         next_level(j)%v3 = current_level(i)%v3v4_mid
         next_level(j)%v4 = current_level(i)%v4

         next_level(j)%coords(1,:) = current_level(i)%coords(7,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(9,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(10,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(4,:)
         j = j + 1

         next_level(j)%v1 = current_level(i)%v2v4_mid
         next_level(j)%v2 = current_level(i)%v1v2_mid
         next_level(j)%v3 = current_level(i)%v1v3_mid
         next_level(j)%v4 = current_level(i)%v1v4_mid

         next_level(j)%coords(1,:) = current_level(i)%coords(9,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(5,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(6,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(7,:)
         j = j + 1

         next_level(j)%v1 = current_level(i)%v1v2_mid
         next_level(j)%v2 = current_level(i)%v1v3_mid
         next_level(j)%v3 = current_level(i)%v2v3_mid
         next_level(j)%v4 = current_level(i)%v2v4_mid

         next_level(j)%coords(1,:) = current_level(i)%coords(5,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(6,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(8,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(9,:)
         j = j + 1

         next_level(j)%v1 = current_level(i)%v1v3_mid
         next_level(j)%v2 = current_level(i)%v2v3_mid
         next_level(j)%v3 = current_level(i)%v2v4_mid
         next_level(j)%v4 = current_level(i)%v3v4_mid

         next_level(j)%coords(1,:) = current_level(i)%coords(6,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(8,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(9,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(10,:)
         j = j + 1

         next_level(j)%v1 = current_level(i)%v1v4_mid
         next_level(j)%v2 = current_level(i)%v2v4_mid
         next_level(j)%v3 = current_level(i)%v3v4_mid
         next_level(j)%v4 = current_level(i)%v1v3_mid

         next_level(j)%coords(1,:) = current_level(i)%coords(7,:)
         next_level(j)%coords(2,:) = current_level(i)%coords(9,:)
         next_level(j)%coords(3,:) = current_level(i)%coords(10,:)
         next_level(j)%coords(4,:) = current_level(i)%coords(6,:)
         j = j + 1
      end do

      ! Deallocate the current level array
      deallocate(current_level)

      ! Update the current level array with the next level
      ! current_level => next_level
      current_level = next_level

      ! Close the hangingnodes & elements file
      close(20)
      close(21)
   end do

   ! close nodes file after the specified number of levels have all been processed
   close(19)
   ! Print the data structure with the local and global node numbers for the refined tetrahedron
   do i = 1, size(current_level)
      write(*,*) current_level(i)%v1, current_level(i)%v2, current_level(i)%v3, current_level(i)%v4
   end do

   ! Print total number of nodes
   write(*,*) "total number of nodes: ", num_nodes

end program tetrahedron_refine

