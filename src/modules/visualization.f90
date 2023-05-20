module visualization
   use geometry_mod
   implicit none
contains
   subroutine generate_paraview_output(curr_level, element_arr, node_coords)

      ! use sharedvars
      ! use histvars
      !    use geometry_mod
      implicit none
      ! curr_level: current refinement level;
      integer, intent(in) :: curr_level
      ! Array of elements from previous refinement step for this level
      ! Each row holds the global node numbers to be connected to form an element
      integer, dimension(:,:), intent(in) :: element_arr
      ! holds the x,y,z coords for each global node
      double precision, dimension(:,:), intent(in) :: node_coords
      ! integer,parameter :: nhvrs=3!number of history vars
      ! integer,parameter :: nip=8!nmb int pts per el

      ! integer i,j,k,iel,ip,ihvr
      ! integer :: dimh, pos2

      ! integer :: dimhvrs(nhvrs), nsclrhvrs
      ! num_nodes: total nodes in this level, num_elements: total elements on level
      integer :: num_nodes, num_elements
      character :: output_filename*128
      integer :: i, j
      ! the 4 vertice nodes of a tetrahedron
      integer :: nodes_per_element = 4
      character(len=9) :: output_dir = "./output/"

      ! character :: nmshvrs(nhvrs)*25
      ! double precision :: eav(20)
      ! double precision, allocatable :: omegael(:), omegaip(:,:)
      !omegaip(numel(1),nip)
      !   double precision, allocatable ::

      ! eliminate:
      ! allocate(omegael(numel(1))); allocate(omegaip(numel(1),nip))
      ! check the number of items in the array on the vertical axis
      num_elements = size(element_arr, dim=1)
      num_nodes = size(node_coords, dim=1)
      output_filename = output_dir // 'visualize_mesh_'!Base name of output file. Must have length 4
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! not important:
      ! nmshvrs(1)='omega';    dimhvrs(1)=1
      ! nmshvrs(2)='sigma';    dimhvrs(2)=6
      ! nmshvrs(3)='eps';      dimhvrs(3)=6

      ! nsclrhvrs = sum(dimhvrs)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      if(curr_level<10) then
         write(output_filename, "(A24,I1,A4)") output_filename, curr_level,".vtu"
      elseif(curr_level<100) then
         write(output_filename, "(A24,I2,A4)") output_filename, curr_level,".vtu"
      elseif(curr_level<1000) then
         write(output_filename, "(A24,I3,A4)") output_filename, curr_level,".vtu"
      elseif(curr_level<10000) then
         write(output_filename, "(A24,I4,A4)") output_filename, curr_level,".vtu"
      else
         write(*,*) 'ERROR. Can not do more than 9999 output files.'
         stop
      endif

      open(unit=99,file=output_filename,access='sequential')
      write(*,*) 'Saving PARAVIEW data to ',output_filename
      !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




      ! Write out top header:
      write(99,1000)

      write(99,1010) num_nodes, num_elements  ! Start Mesh/Piece Section
      !numnp1, numnp2: node numbers -> replace numnp1+numnp2 by your number of nodes
      !numel(1): number of elements

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      write(99,1020)                           ! Start Point/Node data
      write(99,1030) 'Float64','nodes',3
      do i = 1,num_nodes
         do j = 1,3
            write(99,5000) node_coords(i,j)
         end do
      end do
      write(99,1031)                           ! Close Node data
      write(99,1021)                           ! Close Points section
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



      !=============================================================
      write(99,1050)                           ! Start Cell Section

      write(99,1030) 'Int32','connectivity',1  ! Start Elements
      do i = 1, num_elements
         do j = 1, nodes_per_element!nen: no of element nodes, in your case: 4
            ! Paraview's count starts at zero and so the -1 to offset the start of 1
            write(99,6000) element_arr(i,j)-1
         end do
      end do
      write(99,1031)                          ! Close Elements

      write(99,1030) 'Int32','offsets',1      ! Start Offsets
      j=nodes_per_element
      do i = 1, num_elements
         write(99,6000) j
         j = j + nodes_per_element
      end do
      write(99,1031)                           ! Close Offsets

      ! Paraview's VTK files use integers to represent different element types
      write(99,1030) 'UInt8','types',1         ! Start Element types
      do i = 1, num_elements
         if (nodes_per_element .eq. 8) then      ! 8 node brick
            write(99,6000) 12
         elseif(nodes_per_element.eq.4) then     !4 node tetrahedron
            write(99,6000) 10
         else
            stop 'ERROR:No implementation in visualization.f90 for this element type'
         endif
      end do
      write(99,1031)                             ! Close Element types

      write(99,1051)                             ! Close Cell Section
      !=============================================================




      !   !-------------------------------------------------------------
      !   !-------------------------------------------------------------
      !   write(99,1060)                             ! Start Point Data

      !   write(99,1030) 'Float64','disp',3        ! Start Displacements
      !   k=0
      !   do i = 1,numnp1+numnp2
      !     do j =0,2
      !       k=k+1
      !       write(99,5000) uu(1)%v(k)
      !     end do
      !   end do
      !   write(99,1031)                             ! Close Displacements

      !   write(99,1061)                             ! Close Point Data Section
      !   !-------------------------------------------------------------
      !   !-------------------------------------------------------------






      !   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      !   write(99,1070)        !Cell Data

      !   do iel=1,numel(1)
      !     k=0
      !     do ip=1,nip
      !       omegaip(iel,ip) = hr(ihr(iel)+nhistn+k)
      !       k=k+nsclrhvrs
      !     end do
      !     omegael(iel) = sum(omegaip(iel,:))
      !   end do

      !   k=0
      !   do ihvr=1,nhvrs
      !     dimh = dimhvrs(ihvr)
      !     write(99,1030) 'Float64',nmshvrs(ihvr),dimh        ! Start

      !     pos2 = ihr(numel(1)+1) + k
      !     do iel=1,numel(1)
      !       eav(1:dimh) = 0.d0
      ! !       k=1
      !       do ip=1,nip
      !         eav(1:dimh) = eav(1:dimh) + hr(pos2:pos2+dimh-1)*omegaip(iel,ip)
      !         pos2 = pos2+nsclrhvrs
      !       end do
      !       eav(1:dimh) = eav(1:dimh)/omegael(iel)
      !       do i=1,dimh
      !         if(dabs(eav(i)).lt.1.d-89) eav(i)=0.d0
      !         write(99,5000) eav(i)
      !       end do
      !     end do
      !     write(99,1031)                             ! Close
      !     k = k + dimhvrs(ihvr)
      !   end do!ihvr

      !   write(99,1071)        !Cell Data
      !   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$







      write(99,1011)                             ! Close Mesh/Piece


      ! Close the XML file:
      write(99,1001)

      !    eliminate:
      !    deallocate(omegael); deallocate(omegaip)



1000  format('<?xml version="1.0"?>'/'<VTKFile type="UnstructuredGrid" version="0.1">'/'<UnstructuredGrid>')
1001  format('</UnstructuredGrid> </VTKFile>')


1010  format('<Piece NumberOfPoints="',i10,'" NumberOfCells="',i10,'">')
1011  format('</Piece>')


1020  format('<Points>')
1021  format('</Points>')

1030  format('<DataArray type="',a,'" Name="',a,'" NumberOfComponents="',i2,'" format="ascii">')
1031  format('</DataArray>')

      ! 1060 format('<PointData>')
      ! 1061 format('</PointData>')

      ! 1070 format('<CellData>')
      ! 1071 format('</CellData>')

1050  format('<Cells>')
1051  format('</Cells>')

5000  format(e15.5,' ',$)
6000  format(i6,' ',$)


   end subroutine generate_paraview_output


   subroutine generate_paraview_file(curr_level, element_arr, node_coords)
      implicit none
      ! curr_level: current refinement level;
      ! num_elements: actual number of elements on this level. Neccessitated by the Irregular refine mode feature
      ! i.e. curr_level will be initialized to the max possible no. of elements that can be broken off per level
      ! But due to irregular refinement, that number might not be met
      integer, intent(in) :: curr_level
      ! Array of tetrahedrons for this level
      type(tetrahedron), dimension(:), intent(in) :: element_arr
      ! holds the x,y,z coords for each global node
      double precision, dimension(:,:), intent(in) :: node_coords
      ! num_nodes: total nodes in this level, num_elements: total elements on level
      integer :: num_nodes, num_elements
      character :: output_filename*128
      integer :: i, j
      ! the 4 vertice nodes of a tetrahedron
      integer :: nodes_per_element = 4
      character(len=9) :: output_dir = "./output/"

      ! count actual number of elements
      num_elements = 0

      do i=1, size(element_arr)
         if (element_arr(i)%v1 == 0 .and. element_arr(i)%v2 == 0) exit
         num_elements = num_elements + 1
      enddo
      ! size on vertical axis
      num_nodes = size(node_coords, dim=1)
      output_filename = output_dir // 'visualize_mesh_'!Base name of output file. Must have length 4

      if(curr_level<10) then
         write(output_filename, "(A24,I1,A4)") output_filename, curr_level,".vtu"
      elseif(curr_level<100) then
         write(output_filename, "(A24,I2,A4)") output_filename, curr_level,".vtu"
      elseif(curr_level<1000) then
         write(output_filename, "(A24,I3,A4)") output_filename, curr_level,".vtu"
      elseif(curr_level<10000) then
         write(output_filename, "(A24,I4,A4)") output_filename, curr_level,".vtu"
      else
         write(*,*) 'ERROR. Can not do more than 9999 output files.'
         stop
      endif

      open(unit=99,file=output_filename,access='sequential')
      write(*,*) 'Saving PARAVIEW data to ',output_filename

      ! Write out top header:
      write(99,1000)

      write(99,1010) num_nodes, num_elements  ! Start Mesh/Piece Section
      !numnp1, numnp2: node numbers -> replace numnp1+numnp2 by your number of nodes
      !numel(1): number of elements

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      write(99,1020)                           ! Start Point/Node data
      write(99,1030) 'Float64','nodes',3
      do i = 1,num_nodes
         do j = 1,3
            write(99,5000) node_coords(i,j)
         end do
      end do
      write(99,1031)                           ! Close Node data
      write(99,1021)                           ! Close Points section
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



      !=============================================================
      write(99,1050)                           ! Start Cell Section

      write(99,1030) 'Int32','connectivity',1  ! Start Elements
      do i = 1, num_elements
         ! Paraview's count starts at zero and so the -1 to offset the start of 1
         write(99,6000) element_arr(i)%v1 -1
         write(99,6000) element_arr(i)%v2 -1
         write(99,6000) element_arr(i)%v3 -1
         write(99,6000) element_arr(i)%v4 -1
      end do
      write(99,1031)                          ! Close Elements

      write(99,1030) 'Int32','offsets',1      ! Start Offsets
      j=nodes_per_element
      do i = 1, num_elements
         write(99,6000) j
         j = j + nodes_per_element
      end do
      write(99,1031)                           ! Close Offsets

      ! Paraview's VTK files use integers to represent different element types
      write(99,1030) 'UInt8','types',1         ! Start Element types
      do i = 1, num_elements
         if (nodes_per_element .eq. 8) then      ! 8 node brick
            write(99,6000) 12
         elseif(nodes_per_element.eq.4) then     !4 node tetrahedron
            write(99,6000) 10
         else
            stop 'ERROR:No implementation in visualization.f90 for this element type'
         endif
      end do
      write(99,1031)                             ! Close Element types

      write(99,1051)                             ! Close Cell Section
      !=============================================================

      write(99,1011)                             ! Close Mesh/Piece


      ! Close the XML file:
      write(99,1001)

1000  format('<?xml version="1.0"?>'/'<VTKFile type="UnstructuredGrid" version="0.1">'/'<UnstructuredGrid>')
1001  format('</UnstructuredGrid> </VTKFile>')


1010  format('<Piece NumberOfPoints="',i10,'" NumberOfCells="',i10,'">')
1011  format('</Piece>')


1020  format('<Points>')
1021  format('</Points>')

1030  format('<DataArray type="',a,'" Name="',a,'" NumberOfComponents="',i2,'" format="ascii">')
1031  format('</DataArray>')

1050  format('<Cells>')
1051  format('</Cells>')

5000  format(e15.5,' ',$)
6000  format(i6,' ',$)


   end subroutine generate_paraview_file
end module visualization


