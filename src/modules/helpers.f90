module helpers
   use geometry_mod
   implicit none

contains
   subroutine countLines(filename, nLines)
      implicit none
      integer :: nLines, io
      character(len=60) :: filename
      open(unit=922, file=filename, status='old', action='read')
      nLines = 0
      do; read(922,*,iostat=io); if (io/=0) exit; nLines = nLines + 1; end do
      close(922)
      write(*,*) 'File ', filename, ' contains ', nLines, ' lines.'
   end subroutine countLines


   subroutine parse_nodes(parsed_nodes, num_nodes, node_inputfile)
      implicit none
      double precision, dimension(:,:), intent(inout) :: parsed_nodes
      integer, intent(in) :: num_nodes
      character(len=128), intent(in) :: node_inputfile
      integer :: i, j, k
      !   character(len=128) :: filepath
      !   write(filepath, "(A1,A19)") ".", node_inputfile
      open(unit=11, file=node_inputfile, status='old', action='read')
      do i = 1, num_nodes
         ! j and k are placeholders for the initial values i.e. global & local node nos
         read(11, *) j, k, parsed_nodes(i,1:3)
         ! write(*,*) "Writing Elements: "
         write(*,*) j, k, parsed_nodes(i, 1:3)
      end do
      close(11)

   end subroutine parse_nodes

   subroutine parse_elements(current_level, parsed_nodes, num_elements, elem_inputfile)
      implicit none
      type(tetrahedron), dimension(:), intent(inout) :: current_level
      double precision, dimension(:,:), intent(in) :: parsed_nodes
      integer, intent(in) :: num_elements
      character(len=128), intent(in) :: elem_inputfile
      integer :: i, j, k, num_nodes

      num_nodes = 0
      open(unit=76, file=elem_inputfile, status='old', action='read')
      do i=1, num_elements
         ! j and k are placeholders for the initial values i.e. global & local node nos
         read(76, *) j, k, current_level(i)%v1, current_level(i)%v2, current_level(i)%v3, current_level(i)%v4
         ! write(*,*) "Writing Elements: "
         write(*,*) j, k, current_level(i)%v1, current_level(i)%v2, current_level(i)%v3, current_level(i)%v4

         ! initialize node coordinates from parsed_nodes
         ! 4 nodes make one element
         ! use the global node numbers that form the elements to get the coordinates from the nodes
         ! array
         current_level(i)%coords(1,1:) = parsed_nodes(current_level(i)%v1, 1:)
         current_level(i)%coords(2,1:) = parsed_nodes(current_level(i)%v2, 1:)
         current_level(i)%coords(3,1:) = parsed_nodes(current_level(i)%v3, 1:)
         current_level(i)%coords(4,1:) = parsed_nodes(current_level(i)%v4, 1:)
         ! From 4 corner points (vertice) nodes added above
         num_nodes = num_nodes + 4
         ! call initialize_tetrahedron_midpoints(current_level(i), num_nodes)
         ! ! The 6 midpoint nodes
         ! num_nodes = num_nodes + 6
      enddo
      close(76)

   end subroutine parse_elements

   subroutine process_refine_mode(current_level, num_elements, refine_mode, max_num_levels)
      implicit none
      ! array of elements
      type(tetrahedron), dimension(:), intent(inout) :: current_level
      integer, intent(in) :: num_elements
      integer, intent(in) :: refine_mode
      integer, intent(inout) :: max_num_levels

      integer :: num_elems, num_levels, i, element_num
      ! Make 'Regular' Mode the DEFAULT. Unless IRREGULAR is explicitly chosen
      if (refine_mode == 1) then
         ! IRREGULAR MODE
         write(*,*) "Refinement mode: IRREGULAR"
         ! Get the total number of elements to be refined irregularly
         write(*,*) "Number of Elements in Input Mesh: ", num_elements
         write(*,*) "Enter total number of elements to be irregularly refined: "
         read(*,*) num_elems
         ! Get the element and the desired refinement depth
         write(*,*) "In the prompt to choose the elements, enter the element number as labelled from paraview (zero-based indexing)"
         do i=1,num_elems
            write(*,*) "Enter element ", i, ": "
            read(*,*) element_num
            write(*,*) "Desired refinement depth for element ", i, ": "
            read(*,*) num_levels
            ! fetch chosen element from current_level of elements and set it's refinement depth
            ! ONE is added cos paraview uses a zero-based labelling and fortran starts from ONE
            element_num = element_num + 1
            current_level(element_num)%num_levels = num_levels
            ! update max_num_levels
            max_num_levels = max(max_num_levels, num_levels)
         enddo

      else
         ! REGULAR MODE
         write(*,*) "Refinement mode: REGULAR"
         ! Get the desired general/uniform number of levels of refinement
         write(*,*) "Enter number of levels: "
         read(*,*) num_levels
         ! Assign uniform num_levels as refinement depth to every element in mesh
         do i=1,num_elements
            current_level(i)%num_levels = num_levels
         enddo
         max_num_levels = num_levels
      endif
   end subroutine process_refine_mode

   subroutine initialize_tetrahedron_midpoints(element, num_nodes)
      implicit none

      type(tetrahedron), intent(inout) :: element
      integer, intent(in) :: num_nodes

      ! first label/set the midpoint global node numbers
      ! The midpoint nodes are set to increment by 1. Increasing the total number of
      ! global nodes
      element%v1v2_mid = num_nodes + 1
      element%v1v3_mid = element%v1v2_mid + 1
      element%v1v4_mid = element%v1v3_mid + 1
      element%v2v3_mid = element%v1v4_mid + 1
      element%v2v4_mid = element%v2v3_mid + 1
      element%v3v4_mid = element%v2v4_mid + 1

      ! initialize the midpoint node coordinates
      ! for v1v2_mid: x = (v1_x + v2_x) / 2; y = (v1_y + v2_y) / 2; z = (v1_z+v2_z)/2
      element%coords(5,1) = (element%coords(1,1) + &
         element%coords(2,1) )/ 2
      element%coords(5,2) = (element%coords(1,2) + &
         element%coords(2,2) )/ 2
      element%coords(5,3) = (element%coords(1,3) + &
         element%coords(2,3) )/ 2
      ! for v1v3_mid: x = (v1_x + v3_x) / 2; y = (v1_y + v3_y) / 2; z = (v1_z+v3_z)/2
      element%coords(6,1) = (element%coords(1,1) + &
         element%coords(3,1) )/ 2
      element%coords(6,2) = (element%coords(1,2) + &
         element%coords(3,2) )/ 2
      element%coords(6,3) = (element%coords(1,3) + &
         element%coords(3,3) )/ 2
      ! for v1v4_mid: x = (v1_x + v4_x) / 2; y = (v1_y + v4_y) / 2; z = (v1_z+v4_z)/2
      element%coords(7,1) = (element%coords(1,1) + &
         element%coords(4,1) )/ 2
      element%coords(7,2) = (element%coords(1,2) + &
         element%coords(4,2) )/ 2
      element%coords(7,3) = (element%coords(1,3) + &
         element%coords(4,3) )/ 2
      ! for v2v3_mid: x = (v2_x + v3_x) / 2; y = (v2_y + v3_y) / 2; z = (v2_z+v3_z)/2
      element%coords(8,1) = (element%coords(2,1) + &
         element%coords(3,1) )/ 2
      element%coords(8,2) = (element%coords(2,2) + &
         element%coords(3,2) )/ 2
      element%coords(8,3) = (element%coords(2,3) + &
         element%coords(3,3) )/ 2

      ! for v2v4_mid: x = (v2_x + v4_x) / 2; y = (v2_y + v4_y) / 2; z = (v2_z+v4_z)/2
      element%coords(9,1) = (element%coords(2,1) + &
         element%coords(4,1) )/ 2
      element%coords(9,2) = (element%coords(2,2) + &
         element%coords(4,2) )/ 2
      element%coords(9,3) = (element%coords(2,3) + &
         element%coords(4,3) )/ 2

      ! for v3v4_mid: x = (v3_x + v4_x) / 2; y = (v3_y + v4_y) / 2; z = (v3_z+v4_z)/2
      element%coords(10,1) = (element%coords(3,1) + &
         element%coords(4,1) )/ 2
      element%coords(10,2) = (element%coords(3,2) + &
         element%coords(4,2) )/ 2
      element%coords(10,3) = (element%coords(3,3) + &
         element%coords(4,3) )/ 2

   end subroutine initialize_tetrahedron_midpoints
end module helpers
