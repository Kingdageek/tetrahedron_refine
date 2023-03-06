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
      integer :: i, j, k

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
      enddo
      close(76)

   end subroutine parse_elements
end module helpers
