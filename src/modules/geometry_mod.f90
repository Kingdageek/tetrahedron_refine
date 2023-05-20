module geometry_mod
   implicit none
   type tetrahedron
      ! vertice nodes
      integer :: v1=0, v2=0, v3=0, v4=0
      ! Nodes at midpoints of the edges
      integer :: v1v2_mid=0, v1v3_mid=0, v1v4_mid=0, v2v3_mid=0, v2v4_mid=0, v3v4_mid=0
      ! coordinates of tetrahedron. First 4 rows are vertex coords and last 6 rows are midpoint
      ! coordinates.
      ! 10 nodes by 3 cartesian directions (x,y,z)
      !   rows align as so:
      !   variable local node number
      !   v1        1
      !   v2        2
      !   v3        3
      !   v4        4
      !   v1v2_mid  5
      !   v1v3_mid  6
      !   v1v4_mid  7
      !   v2v3_mid  8
      !   v2v4_mid  9
      !   v3v4_mid  10
      double precision :: coords(10,3)
      ! attribute to track number of levels this object should be refined for -
      ! refinement depth
      integer :: num_levels = 0
   end type tetrahedron

   type octahedron
      integer :: v1, v2, v3, v4, v5, v6
      integer :: mid1, mid2, mid3, mid4, mid5, mid6, mid7, mid8, mid9, mid10, mid11, mid12
   end type octahedron
end module geometry_mod
