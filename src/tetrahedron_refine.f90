program tetrahedron_refine
   use geometry_mod
   use helpers
   use visualization
   implicit none

   ! hold array of elements (tetrahedrons) per level
   type(tetrahedron), dimension(:), allocatable :: prev_level, current_level
   ! i, j, k are just loop counter variables
   ! max_num_levels - maximum desired refinement depth for all the elements in the mesh
   integer :: max_num_levels, i, j, k, m, curr_level
   integer :: num_current, num_next, num_nodes
   character(len=128) :: elem_filename, hanging_nodes_filename, nodes_filename
   character(len=9) :: output_dir = "./output/"
   ! To parse the input mesh
   double precision, dimension(:,:), allocatable :: parsed_nodes, old_parsed_nodes
   character(len=128) :: elem_inputfile, nodes_inputfile
   character(len=8) :: input_dir = "./input/"
   ! use to track elements in old mesh, updated for continued meshing | mode (regular-0 | irregular - 1)
   integer :: num_elements, refine_mode

   elem_inputfile = input_dir // "elements.dat"
   nodes_inputfile = input_dir // "nodes.dat"

   write(*,*) "##### MESH REFINEMENT PROGRAM #####"
   write(*,*) "Please ensure the node and element files are in input folder, are properly named, and structured"
   write(*,*) "You can choose to perform either a regular or irregular mesh refinement"
   write(*,*) "****************************************************************************"
   write(*,*)

   ! Get refinement mode
   write(*,*) "Choose refinement mode | Enter 0 for 'REGULAR' & 1 for 'IRREGULAR': "
   read(*,*) refine_mode
   ! parse nodes and elements from input mesh
   write(*,*) "Trying to parse nodes and elements of old mesh..."

   call countLines(nodes_inputfile, num_nodes)
   call countLines(elem_inputfile, num_elements)
   ! allocate nodes array from num_nodes (num_nodes * 3) arr. x,y,z
   allocate(parsed_nodes(num_nodes, 3))

   call parse_nodes(parsed_nodes, num_nodes, nodes_inputfile)
   write(*,*) "Parsing nodes successful..."
   ! Initialize the largest tetrahedron -> coarsest (old) mesh
   allocate(prev_level(num_elements))
   ! assigns elements based on the nodes and initializes the coordinates of the nodes for elements
   ! both vertice nodes and edge nodes
   call parse_elements(prev_level, parsed_nodes, num_elements, elem_inputfile)
   write(*,*) "Parsing elements successful..."

   ! Assign refinement_depth (num_levels) to each element according to the chosen refinement mode
   call process_refine_mode(prev_level, num_elements, refine_mode, max_num_levels)

   nodes_filename = output_dir // "nodes.dat"
   ! Open the output file to save the nodes and their coordinates
   open(unit=19, file=nodes_filename, access="sequential")
   do j=1, num_nodes
      ! Format: global_node_num   local_node_num_(default of 0)  x   y   z
      write(19, *) j, 0, parsed_nodes(j,1:)
   enddo

   write(*,*) "MAX REFINEMENT DEPTH: ", max_num_levels
   do curr_level = 0, max_num_levels
      ! Level 0: The received mesh, with midpoint positions & coords initialized to prep for refinement
      write(*,*) "Refining level: ", curr_level
      write(hanging_nodes_filename, "(A9,A13,I1,A4)") output_dir, "hangingnodes_", curr_level, ".dat"
      ! Open the output file to save the hanging nodes
      open(unit=20, file=hanging_nodes_filename, access='sequential')

      ! output file to save elements per level
      write(elem_filename, "(A9,A9,I1,A4)") output_dir, "elements_", curr_level, ".dat"
      ! Open the output file to save the elements
      open(unit=21, file=elem_filename, access='sequential')

      ! Allocate the next level array
      num_current = size(prev_level)
      num_next = 8*num_current

      ! deallocate 'current_level' if it still holds values from previous iteration
      if( allocated(current_level) ) deallocate(current_level)
      allocate(current_level(num_next))

      ! 'j' tracks the total number of elements per level
      j = 1
      do i = 1, num_current
         ! write(*,*) "In level: ", curr_level
         ! write(*,*) "Current Level tetrahedron at: ", i, "with v1: ", prev_level(i)%v1, "with v1v2_mid: ", &
         !    prev_level(i)%v1v2_mid, "with num_levels: ", prev_level(i)%num_levels
         ! Due to Irregular refinement, current_level might have some empty slots as it is always
         ! initialized to the max no. of elements possible per refinement level
         ! so, break out of this loop when element connectivity nodes are 0
         if (prev_level(i)%v1 == 0 .and. prev_level(i)%v2 == 0) exit

         ! current element should not be refined if its num_levels is 0
         if (prev_level(i)%num_levels == 0) then
            ! Slot it in next level as-is i.e. without refining
            current_level(j) = prev_level(i)

            ! Write it to current level element file
            write(21, *) j, 0, prev_level(i)%v1, prev_level(i)%v2, prev_level(i)%v3, &
               prev_level(i)%v4
            ! Write its hanging nodes, if available, from the previous step too, if they're not zero
            if ( prev_level(i)%v1v2_mid /= 0 .and. prev_level(i)%v1v3_mid /= 0) then
               write(20, *) j, 0, prev_level(i)%v1v2_mid, prev_level(i)%v1v3_mid, &
                  prev_level(i)%v1v4_mid, prev_level(i)%v2v3_mid, prev_level(i)%v2v4_mid,&
                  prev_level(i)%v3v4_mid
            endif
            j = j + 1
            cycle
         endif

         ! To update the parsed_nodes array
         if (allocated(old_parsed_nodes)) deallocate(old_parsed_nodes)
         ! 3 coords - x, y, z
         allocate(old_parsed_nodes(num_nodes, 3))
         old_parsed_nodes = parsed_nodes
         if (allocated(parsed_nodes)) deallocate(parsed_nodes)
         ! 48 more nodes will be added (midpoint nodes) in this level
         ! 6 midpoint nodes * 8 child elements from each refinement step
         ! allocate(parsed_nodes(num_nodes + 48, 3))
         allocate(parsed_nodes(num_nodes + 6, 3))
         ! write elements from old parsed nodes to parsed nodes
         do k=1, num_nodes
            parsed_nodes(k, 1:) = old_parsed_nodes(k, 1:)
         enddo

         call initialize_tetrahedron_midpoints(prev_level(i), num_nodes)
         ! write(*,*) "k is: ", k

         ! Write the elements for this level
         ! Write current element to current level element file
         ! Format: element, 0, vertice nodes connectivity (global node nos)
         write(21, *) i, 0, prev_level(i)%v1, prev_level(i)%v2, prev_level(i)%v3, &
            prev_level(i)%v4

         ! Write hanging nodes for current element
         ! Format: element,0, midpoint nodes (global node nos)
         write(20, *) i, 0, prev_level(i)%v1v2_mid, prev_level(i)%v1v3_mid, &
            prev_level(i)%v1v4_mid, prev_level(i)%v2v3_mid, prev_level(i)%v2v4_mid,&
            prev_level(i)%v3v4_mid

         ! Write midpoint nodes to general nodes file - (local node numbers 5 through 10)
         do m=5,10
            ! Added nodes must reflect on total node number
            num_nodes = num_nodes + 1
            ! Format: global_node_num   local_node_num  x   y   z
            write(19, *) num_nodes, m, prev_level(i)%coords(m,:)
            ! add midpoint node to parsed node
            parsed_nodes(num_nodes, 1:) = prev_level(i)%coords(m,:)
         enddo
         ! Carve out the elements for the next level
         ! global node number labels and then assign coordinates accordingly
         current_level(j)%v1 = prev_level(i)%v1
         current_level(j)%v2 = prev_level(i)%v1v2_mid
         current_level(j)%v3 = prev_level(i)%v1v3_mid
         current_level(j)%v4 = prev_level(i)%v1v4_mid

         current_level(j)%coords(1,:) = prev_level(i)%coords(1,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(5,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(6,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(7,:)

         ! call initialize_tetrahedron_midpoints(current_level(j), num_nodes)
         ! num_nodes = num_nodes + 6
         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         current_level(j)%v1 = prev_level(i)%v1v2_mid
         current_level(j)%v2 = prev_level(i)%v2
         current_level(j)%v3 = prev_level(i)%v2v3_mid
         current_level(j)%v4 = prev_level(i)%v2v4_mid

         current_level(j)%coords(1,:) = prev_level(i)%coords(5,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(2,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(8,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(9,:)

         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         current_level(j)%v1 = prev_level(i)%v1v3_mid
         current_level(j)%v2 = prev_level(i)%v2v3_mid
         current_level(j)%v3 = prev_level(i)%v3
         current_level(j)%v4 = prev_level(i)%v3v4_mid

         current_level(j)%coords(1,:) = prev_level(i)%coords(6,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(8,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(3,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(10,:)

         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         current_level(j)%v1 = prev_level(i)%v1v4_mid
         current_level(j)%v2 = prev_level(i)%v2v4_mid
         current_level(j)%v3 = prev_level(i)%v3v4_mid
         current_level(j)%v4 = prev_level(i)%v4

         current_level(j)%coords(1,:) = prev_level(i)%coords(7,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(9,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(10,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(4,:)

         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         current_level(j)%v1 = prev_level(i)%v2v4_mid
         current_level(j)%v2 = prev_level(i)%v1v2_mid
         current_level(j)%v3 = prev_level(i)%v1v3_mid
         current_level(j)%v4 = prev_level(i)%v1v4_mid

         current_level(j)%coords(1,:) = prev_level(i)%coords(9,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(5,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(6,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(7,:)

         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         current_level(j)%v1 = prev_level(i)%v1v2_mid
         current_level(j)%v2 = prev_level(i)%v1v3_mid
         current_level(j)%v3 = prev_level(i)%v2v3_mid
         current_level(j)%v4 = prev_level(i)%v2v4_mid

         current_level(j)%coords(1,:) = prev_level(i)%coords(5,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(6,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(8,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(9,:)

         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         current_level(j)%v1 = prev_level(i)%v1v3_mid
         current_level(j)%v2 = prev_level(i)%v2v3_mid
         current_level(j)%v3 = prev_level(i)%v2v4_mid
         current_level(j)%v4 = prev_level(i)%v3v4_mid

         current_level(j)%coords(1,:) = prev_level(i)%coords(6,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(8,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(9,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(10,:)

         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         current_level(j)%v1 = prev_level(i)%v1v4_mid
         current_level(j)%v2 = prev_level(i)%v2v4_mid
         current_level(j)%v3 = prev_level(i)%v3v4_mid
         current_level(j)%v4 = prev_level(i)%v1v3_mid

         current_level(j)%coords(1,:) = prev_level(i)%coords(7,:)
         current_level(j)%coords(2,:) = prev_level(i)%coords(9,:)
         current_level(j)%coords(3,:) = prev_level(i)%coords(10,:)
         current_level(j)%coords(4,:) = prev_level(i)%coords(6,:)

         ! Reduce the num_levels by 1 from the parent num_levels
         current_level(j)%num_levels = prev_level(i)%num_levels - 1
         j = j + 1

         ! Initialize the midpoints for all the above child elements
         ! Write to elements, hanging_nodes, and nodes files
         ! The start, step initialization considers that 8 elements were added, and j started from 1
         ! do k=j-8, j-1
         !    call initialize_tetrahedron_midpoints(current_level(k), num_nodes)
         !    write(*,*) "k is: ", k

         !    ! Write the elements for this level
         !    ! Write current element to current level element file
         !    ! Format: element, 0, vertice nodes connectivity (global node nos)
         !    write(21, *) k, 0, current_level(k)%v1, current_level(k)%v2, current_level(k)%v3, &
         !       current_level(k)%v4

         !    ! Write hanging nodes for current element
         !    ! Format: element,0, midpoint nodes (global node nos)
         !    write(20, *) k, 0, current_level(k)%v1v2_mid, current_level(k)%v1v3_mid, &
         !       current_level(k)%v1v4_mid, current_level(k)%v2v3_mid, current_level(k)%v2v4_mid,&
         !       current_level(k)%v3v4_mid

         !    ! Write midpoint nodes to general nodes file - (local node numbers 5 through 10)
         !    do m=5,10
         !       ! Added nodes must reflect on total node number
         !       num_nodes = num_nodes + 1
         !       ! Format: global_node_num   local_node_num  x   y   z
         !       write(19, *) num_nodes, m, current_level(k)%coords(m,:)
         !       ! add midpoint node to parsed node
         !       parsed_nodes(num_nodes, 1:) = current_level(k)%coords(m,:)
         !    enddo
         !    ! Reduce the num_levels by 1 from the parent num_levels
         !    ! current_level(k)%num_levels = prev_level(i)%num_levels - 1
         ! enddo
      end do

      ! Generate Visualization file for current level
      write(*,*) "Generating visualization file..."
      call generate_paraview_file(curr_level, prev_level, parsed_nodes)
      ! Deallocate the prev level array
      deallocate(prev_level)

      ! Update the previous level array with the current level for next iteration
      ! prev_level => current_level
      prev_level = current_level

      ! Close the hangingnodes & elements file
      close(20)
      close(21)
   end do

   ! close nodes file after the specified number of levels have all been processed
   close(19)
   ! Print the data structure with the local and global node numbers for the refined tetrahedron
   ! do i = 1, size(prev_level)
   !    write(*,*) prev_level(i)%v1, prev_level(i)%v2, prev_level(i)%v3, prev_level(i)%v4
   ! end do

   write(*,*) "Refining complete"
   ! Print total number of nodes
   write(*,*) "total number of nodes: ", num_nodes

end program tetrahedron_refine

